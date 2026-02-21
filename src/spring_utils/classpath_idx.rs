use crate::jar_utils::{JarError, JarResult};

/// Parsed representation of a Spring Boot `classpath.idx` file.
///
/// Format: each line is `- "BOOT-INF/lib/some.jar"`
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ClasspathIndex {
    entries: Vec<String>,
}

impl ClasspathIndex {
    /// Parse a `classpath.idx` file from raw bytes.
    pub fn parse(data: &[u8]) -> JarResult<Self> {
        let text = std::str::from_utf8(data)
            .map_err(|e| JarError::ManifestParse(format!("classpath.idx: invalid UTF-8: {e}")))?;

        let mut entries = Vec::new();
        for line in text.lines() {
            let line = line.trim();
            if line.is_empty() {
                continue;
            }
            // Expected format: - "path"
            let Some(rest) = line.strip_prefix("- \"") else {
                continue;
            };
            let Some(path) = rest.strip_suffix('"') else {
                continue;
            };
            entries.push(path.to_string());
        }

        Ok(ClasspathIndex { entries })
    }

    /// Serialize back to `classpath.idx` format.
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut out = String::new();
        for entry in &self.entries {
            out.push_str("- \"");
            out.push_str(entry);
            out.push_str("\"\n");
        }
        out.into_bytes()
    }

    /// All classpath entries.
    pub fn entries(&self) -> &[String] {
        &self.entries
    }

    /// Number of entries.
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    /// Whether the index is empty.
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    /// Check if a path is in the classpath index.
    pub fn contains(&self, path: &str) -> bool {
        self.entries.iter().any(|e| e == path)
    }

    /// Iterate over entries.
    pub fn iter(&self) -> impl Iterator<Item = &str> {
        self.entries.iter().map(|s| s.as_str())
    }
}
