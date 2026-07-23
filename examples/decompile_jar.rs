//! Extract a JAR/WAR/SAR archive to a directory, decompiling `.class` files to
//! `.java` and copying every other entry verbatim at its original path.
//!
//! Inner classes (`Foo$Bar.class`) are nested into their outer class's source
//! file, so classes produce one `.java` file per top-level class. Non-class
//! resources (manifests, properties, XML, etc.) are written unchanged. The
//! output mirrors the archive's directory structure.
//!
//! Usage:
//! ```sh
//! cargo run --example decompile_jar --features "jar-utils decompile" -- path/to/app.jar [output-dir]
//! ```
//!
//! Output directory defaults to `decompiled/<archive-name-without-extension>/`.

use std::collections::BTreeMap;
use std::error::Error;
use std::path::{Component, Path, PathBuf};

use classfile_parser::ClassFile;
use classfile_parser::decompile::{DecompileOptions, Decompiler};
use classfile_parser::jar_utils::JarFile;

fn main() -> Result<(), Box<dyn Error>> {
    // The decompiler recurses over control flow and expression trees; real-world
    // classes (e.g. Lombok builders with large concatenated toString()) can nest
    // deep enough to overflow the default 1 MB main stack. Run on a worker thread
    // with a big reserved stack instead of rewriting the recursion.
    // ponytail: fixed 256 MB reserved stack; make the recursion iterative if a
    // class ever exceeds it.
    std::thread::Builder::new()
        .stack_size(256 * 1024 * 1024)
        .spawn(|| run().map_err(|e| e.to_string()))?
        .join()
        .map_err(|_| "decompile worker panicked")??;
    Ok(())
}

fn run() -> Result<(), Box<dyn Error>> {
    let mut args = std::env::args().skip(1);
    let jar_path = PathBuf::from(
        args.next()
            .ok_or("usage: decompile_jar <file.jar|war|sar> [output-dir]")?,
    );
    let out_root = args.next().map(PathBuf::from).unwrap_or_else(|| {
        let stem = jar_path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("archive");
        Path::new("decompiled").join(stem)
    });

    let jar = JarFile::open(&jar_path)?;

    // Group inner classes (Foo$Bar.class, Foo$1.class) under their top-level
    // class. Classes whose outer entry isn't in the archive are treated as
    // top-level themselves.
    let mut inners: BTreeMap<String, Vec<String>> = BTreeMap::new();
    let mut top_level: Vec<String> = Vec::new();
    for name in jar.class_names() {
        match outer_class_path(name) {
            Some(outer) if jar.contains_entry(&outer) => {
                inners.entry(outer).or_default().push(name.to_string());
            }
            _ => top_level.push(name.to_string()),
        }
    }

    // Extract every non-class entry verbatim at its original path.
    let mut resources = 0usize;
    for entry in jar.entry_names() {
        if entry.ends_with(".class") {
            continue;
        }
        let Some(rel) = safe_rel_path(entry) else {
            eprintln!("warning: skipping suspicious entry path: {entry}");
            continue;
        };
        let out_file = out_root.join(rel);
        if let Some(parent) = out_file.parent() {
            std::fs::create_dir_all(parent)?;
        }
        std::fs::write(&out_file, jar.get_entry(entry).unwrap_or_default())?;
        resources += 1;
    }

    let decompiler = Decompiler::new(DecompileOptions::default());
    let (mut ok, mut failed) = (0usize, 0usize);

    for entry in &top_level {
        let Some(rel) = safe_rel_path(entry) else {
            eprintln!("warning: skipping suspicious entry path: {entry}");
            failed += 1;
            continue;
        };

        let result = (|| -> Result<String, Box<dyn Error>> {
            let outer_cf = jar.parse_class(entry)?;
            let inner_cfs: Vec<ClassFile> = inners
                .get(entry)
                .into_iter()
                .flatten()
                .filter_map(|p| jar.parse_class(p).ok())
                .collect();
            if inner_cfs.is_empty() {
                Ok(decompiler.decompile(&outer_cf)?)
            } else {
                let refs: Vec<&ClassFile> = inner_cfs.iter().collect();
                Ok(decompiler.decompile_with_inner_classes(&outer_cf, &refs)?)
            }
        })();

        match result {
            Ok(source) => {
                let out_file = out_root.join(rel.with_extension("java"));
                if let Some(parent) = out_file.parent() {
                    std::fs::create_dir_all(parent)?;
                }
                std::fs::write(&out_file, source)?;
                ok += 1;
            }
            Err(e) => {
                eprintln!("warning: failed to decompile {entry}: {e}");
                failed += 1;
            }
        }
    }

    println!(
        "Decompiled {ok} class{} + extracted {resources} resource{} into {} ({failed} failed)",
        if ok == 1 { "" } else { "es" },
        if resources == 1 { "" } else { "s" },
        out_root.display()
    );
    Ok(())
}

/// For `a/b/Foo$Bar$1.class`, return `a/b/Foo.class`. None if not an inner class.
fn outer_class_path(entry: &str) -> Option<String> {
    let stem = entry.strip_suffix(".class")?;
    let file_start = stem.rfind('/').map_or(0, |i| i + 1);
    let dollar = stem[file_start..].find('$')?;
    Some(format!("{}.class", &stem[..file_start + dollar]))
}

/// Reject absolute paths and `..` components so a malicious archive can't
/// write outside the output directory (zip-slip).
fn safe_rel_path(entry: &str) -> Option<PathBuf> {
    let p = Path::new(entry);
    if p.components().all(|c| matches!(c, Component::Normal(_))) {
        Some(p.to_path_buf())
    } else {
        None
    }
}
