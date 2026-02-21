use crate::jar_utils::{JarError, JarResult};

/// A single layer in a Spring Boot `layers.idx` file.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Layer {
    pub name: String,
    pub paths: Vec<String>,
}

/// Parsed representation of a Spring Boot `layers.idx` file.
///
/// Format:
/// ```text
/// - "dependencies":
///   - "BOOT-INF/lib/"
/// - "application":
///   - "BOOT-INF/classes/"
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LayersIndex {
    layers: Vec<Layer>,
}

impl LayersIndex {
    /// Parse a `layers.idx` file from raw bytes.
    pub fn parse(data: &[u8]) -> JarResult<Self> {
        let text = std::str::from_utf8(data)
            .map_err(|e| JarError::ManifestParse(format!("layers.idx: invalid UTF-8: {e}")))?;

        let mut layers = Vec::new();
        let mut current: Option<Layer> = None;

        for line in text.lines() {
            if line.is_empty() {
                continue;
            }

            // Layer header: - "name":
            if line.starts_with("- \"") {
                // Flush previous layer
                if let Some(layer) = current.take() {
                    layers.push(layer);
                }
                let rest = &line[3..]; // after `- "`
                if let Some(name) = rest.strip_suffix("\":") {
                    current = Some(Layer {
                        name: name.to_string(),
                        paths: Vec::new(),
                    });
                }
                continue;
            }

            // Path entry:   - "path"
            if line.starts_with("  - \"") {
                let rest = &line[5..]; // after `  - "`
                if let Some(path) = rest.strip_suffix('"') {
                    if let Some(ref mut layer) = current {
                        layer.paths.push(path.to_string());
                    }
                }
            }
        }

        // Flush last layer
        if let Some(layer) = current {
            layers.push(layer);
        }

        Ok(LayersIndex { layers })
    }

    /// Serialize back to `layers.idx` format.
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut out = String::new();
        for layer in &self.layers {
            out.push_str("- \"");
            out.push_str(&layer.name);
            out.push_str("\":\n");
            for path in &layer.paths {
                out.push_str("  - \"");
                out.push_str(path);
                out.push_str("\"\n");
            }
        }
        out.into_bytes()
    }

    /// All layers.
    pub fn layers(&self) -> &[Layer] {
        &self.layers
    }

    /// Find a layer by name.
    pub fn find_layer(&self, name: &str) -> Option<&Layer> {
        self.layers.iter().find(|l| l.name == name)
    }

    /// Iterate over layer names.
    pub fn layer_names(&self) -> impl Iterator<Item = &str> {
        self.layers.iter().map(|l| l.name.as_str())
    }

    /// Find which layer an entry path belongs to (by prefix match).
    pub fn layer_for_path(&self, entry_path: &str) -> Option<&str> {
        for layer in &self.layers {
            for path in &layer.paths {
                if entry_path.starts_with(path.as_str()) {
                    return Some(&layer.name);
                }
            }
        }
        None
    }

    /// Number of layers.
    pub fn len(&self) -> usize {
        self.layers.len()
    }

    /// Whether there are no layers.
    pub fn is_empty(&self) -> bool {
        self.layers.is_empty()
    }
}
