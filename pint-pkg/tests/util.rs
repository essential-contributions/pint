//! Items shared between pint-pkg tests.

use std::{panic::UnwindSafe, path::Path};

/// Create a temporary directory with a random hash based on current timestamp
/// and call the given function with access to it.
///
/// Cleans up the temporary directory after the given function completes.
#[allow(dead_code)]
pub(crate) fn with_temp_dir<F>(f: F)
where
    F: FnOnce(&Path) + UnwindSafe,
{
    use std::hash::{Hash, Hasher};
    let temp_dir = std::env::temp_dir();
    let ts = std::time::Instant::now();
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    ts.hash(&mut hasher);
    let hash = hasher.finish();
    let dirname = format!("{hash:16x}");
    let dir = temp_dir.join(dirname);
    std::fs::create_dir_all(&dir).expect("failed to create temp dir");
    let res = std::panic::catch_unwind(|| f(&dir));
    std::fs::remove_dir_all(&dir).unwrap();
    res.unwrap();
}
