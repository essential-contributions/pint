//! The manifest type and its implementations.

use serde::{Deserialize, Serialize};
use std::{
    collections::{BTreeMap, HashSet},
    fmt, fs, io, ops,
    path::{Path, PathBuf},
    str,
};
use thiserror::Error;

/// A manifest loaded from a file.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ManifestFile {
    /// The deserialized manifest.
    manifest: Manifest,
    /// The canonical path to the manifest file.
    path: PathBuf,
}

/// A package manifest.
///
/// This is the Rust representation of the `pint.toml` manifest file.
#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct Manifest {
    /// High-level information about the package like name, license.
    #[serde(rename = "package")]
    pub pkg: Package,
    /// All library dependencies declared by this package.
    #[serde(default, rename = "dependencies", with = "serde_opt")]
    pub deps: Dependencies,
    /// All contract dependencies declared by this package.
    #[serde(default, rename = "contract-dependencies", with = "serde_opt")]
    pub contract_deps: ContractDependencies,
}

/// High-level information about the package.
#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct Package {
    /// The name of the package.
    pub name: String,
    /// The license for the package.
    pub license: Option<String>,
    /// Whether the package is a contract or library.
    #[serde(default)]
    pub kind: PackageKind,
    /// The location of the file within `src` that is the entry-point to
    /// compilation.
    ///
    /// If unspecified, the default entry point will be derived from the package
    /// kind:
    ///
    /// - `Contract`: "contract.pnt"
    /// - `Library`: "lib.pnt"
    #[serde(rename = "entry-point")]
    pub entry_point: Option<String>,
}

/// Whether the package is to be compiled as a contract or library.
#[derive(Clone, Debug, Default, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum PackageKind {
    /// The package is to be compiled as a contract.
    #[default]
    #[serde(rename = "contract")]
    Contract,
    /// The package is a library of items (types, macros, consts, modules).
    #[serde(rename = "library")]
    Library,
}

/// The table of library dependencies.
pub type Dependencies = BTreeMap<String, Dependency>;
/// The table of contract dependencies.
pub type ContractDependencies = BTreeMap<String, Dependency>;

/// Represents a dependency on another pint package.
#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct Dependency {
    /// The source of the dependency.
    #[serde(flatten)]
    pub source: dependency::Source,
    /// Optionally specify the expected package name in the case that it differs
    /// to the name given to the dependency.
    pub package: Option<String>,
}

/// The manifest specifies an invalid package name.
#[derive(Debug, Error)]
pub enum InvalidName {
    /// Must only contain ASCII non-uppercase alphanumeric chars, dashes or underscores.
    #[error("must only contain ASCII non-uppercase alphanumeric chars, dashes or underscores")]
    InvalidChar,
    /// Must begin with an alphabetic character.
    #[error("must begin with an alphabetic character")]
    NonAlphabeticStart,
    /// Must end with an alphanumeric character.
    #[error("must end with an alphanumeric character")]
    NonAlphanumericEnd,
    /// Must not be a pint language keyword.
    #[error("must not be a pint language keyword")]
    PintKeyword,
    /// The given name is a word reserved by pint.
    #[error("the given name is a word reserved by pint")]
    Reserved,
}

/// The parsed package kind was invalid.
#[derive(Debug, Error)]
#[error(r#"failed to parse package kind, expected "contract" or "library""#)]
pub struct InvalidPkgKind;

/// The manifest failed its validation check.
#[derive(Debug, Error)]
pub enum InvalidManifest {
    /// Manifest specifies an invalid package name.
    #[error("manifest specifies an invalid package name {0:?}: {1}")]
    PkgName(String, InvalidName),
    /// Manifest specifies an invalid dependency name.
    #[error("manifest specifies an invalid dependency name {0:?}: {1}")]
    DepName(String, InvalidName),
    /// Dependency name appears more than once.
    #[error("dependency name {0:?} appears more than once")]
    DupDepName(String),
}

/// Failure to parse and construct a manifest from a string.
#[derive(Debug, Error)]
pub enum ManifestError {
    /// Failed to deserialize the manifest from toml.
    #[error("failed to deserialize manifest from toml: {0}")]
    Toml(#[from] toml::de::Error),
    /// Manifest failed the validation check.
    #[error("invalid manifest: {0}")]
    Invalid(#[from] InvalidManifest),
}

/// Failed to load a manifest from file.
#[derive(Debug, Error)]
pub enum ManifestFileError {
    #[error("an IO error occurred while constructing the `ManifestFile`: {0}")]
    Io(#[from] io::Error),
    /// Failed to construct the manifest.
    #[error("{0}")]
    Manifest(#[from] ManifestError),
}

impl Manifest {
    /// The default contract compilation entry point within the src dir.
    pub const DEFAULT_CONTRACT_ENTRY_POINT: &'static str = "contract.pnt";
    /// The default library compilation entry point within the src dir.
    pub const DEFAULT_LIBRARY_ENTRY_POINT: &'static str = "lib.pnt";

    /// The specified entry point, or the default entry point based on the package kind.
    pub fn entry_point_str(&self) -> &str {
        self.pkg
            .entry_point
            .as_deref()
            .unwrap_or(match self.pkg.kind {
                PackageKind::Contract => Self::DEFAULT_CONTRACT_ENTRY_POINT,
                PackageKind::Library => Self::DEFAULT_LIBRARY_ENTRY_POINT,
            })
    }
}

impl ManifestFile {
    /// The expected file name of the pint manifest file.
    pub const FILE_NAME: &'static str = "pint.toml";

    /// Load a manifest directly from its file path.
    ///
    /// The given path will be [`canonicalize`][std::path::Path::canonicalize]d.
    pub fn from_path(path: &Path) -> Result<Self, ManifestFileError> {
        let path = path.canonicalize()?;
        let string = fs::read_to_string(&path)?;
        let manifest: Manifest = string.parse()?;
        let manifest_file = Self { manifest, path };
        Ok(manifest_file)
    }

    /// The canonical path to the manifest.
    pub fn path(&self) -> &Path {
        &self.path
    }

    /// The parent directory of the manifest file.
    pub fn dir(&self) -> &Path {
        self.path
            .parent()
            .expect("manifest file has no parent directory")
    }

    /// The full path to the package's `src` directory.
    pub fn src_dir(&self) -> PathBuf {
        self.dir().join("src")
    }

    /// The full path to the package's `out` directory for build artifacts.
    pub fn out_dir(&self) -> PathBuf {
        self.dir().join("out")
    }

    /// The path to the compilation entry point src file.
    pub fn entry_point(&self) -> PathBuf {
        self.src_dir().join(self.entry_point_str())
    }

    /// The dependency or contract dependency with the given dependency name.
    pub fn dep(&self, dep_name: &str) -> Option<&Dependency> {
        self.deps
            .get(dep_name)
            .or_else(|| self.contract_deps.get(dep_name))
    }

    /// Given the name of a `path` dependency, returns the full canonical `Path` to the dependency.
    pub fn dep_path(&self, dep_name: &str) -> Option<PathBuf> {
        let dir = self.dir();
        let dep = self.dep(dep_name)?;
        match &dep.source {
            dependency::Source::Path(dep) => match dep.path.is_absolute() {
                true => Some(dep.path.to_owned()),
                false => dir.join(&dep.path).canonicalize().ok(),
            },
        }
    }
}

impl fmt::Display for PackageKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            Self::Contract => "contract",
            Self::Library => "library",
        };
        write!(f, "{}", s)
    }
}

impl str::FromStr for PackageKind {
    type Err = InvalidPkgKind;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let kind = match s {
            "contract" => Self::Contract,
            "library" => Self::Library,
            _ => return Err(InvalidPkgKind),
        };
        Ok(kind)
    }
}

impl str::FromStr for Manifest {
    type Err = ManifestError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let toml_de = toml::de::Deserializer::new(s);
        let mut ignored_paths = vec![];
        let manifest: Self = serde_ignored::deserialize(toml_de, |path| {
            // TODO: Trace these ignored TOML paths as warnings?
            ignored_paths.push(format!("{path}"));
        })?;
        check(&manifest)?;
        Ok(manifest)
    }
}

impl ops::Deref for ManifestFile {
    type Target = Manifest;
    fn deref(&self) -> &Self::Target {
        &self.manifest
    }
}

/// Validate the given manifest.
pub fn check(manifest: &Manifest) -> Result<(), InvalidManifest> {
    // Check the package name.
    check_name(&manifest.pkg.name)
        .map_err(|e| InvalidManifest::PkgName(manifest.pkg.name.to_string(), e))?;

    // Check the dependency names.
    let mut names = HashSet::new();
    for name in manifest.deps.keys().chain(manifest.contract_deps.keys()) {
        // Check name validity.
        check_name(name).map_err(|e| InvalidManifest::DepName(manifest.pkg.name.to_string(), e))?;

        // Check for duplicates.
        if !names.insert(name) {
            return Err(InvalidManifest::DupDepName(name.to_string()));
        }
    }

    Ok(())
}

/// Package names must only contain ASCII non-uppercase alphanumeric chars, dashes or underscores.
pub fn check_name_char(ch: char) -> bool {
    (ch.is_ascii_alphanumeric() && !ch.is_uppercase()) || ch == '-' || ch == '_'
}

/// Check the validity of the given package name.
pub fn check_name(name: &str) -> Result<(), InvalidName> {
    if !name.chars().all(check_name_char) {
        return Err(InvalidName::InvalidChar);
    }

    if matches!(name.chars().next(), Some(ch) if !ch.is_ascii_alphabetic()) {
        return Err(InvalidName::NonAlphabeticStart);
    }

    if matches!(name.chars().last(), Some(ch) if !ch.is_ascii_alphanumeric()) {
        return Err(InvalidName::NonAlphanumericEnd);
    }

    if PINT_KEYWORDS.contains(&name) {
        return Err(InvalidName::PintKeyword);
    }

    if RESERVED.contains(&name) {
        return Err(InvalidName::Reserved);
    }

    Ok(())
}

const PINT_KEYWORDS: &[&str] = &[
    "as",
    "bool",
    "b256",
    "cond",
    "constraint",
    "else",
    "enum",
    "exists",
    "forall",
    "if",
    "in",
    "int",
    "predicate",
    "interface",
    "macro",
    "maximize",
    "minimize",
    "real",
    "satisfy",
    "self",
    "solve",
    "state",
    "storage",
    "string",
    "type",
    "use",
    "var",
    "where",
];

const RESERVED: &[&str] = &[
    "contract",
    "dep",
    "dependency",
    "lib",
    "library",
    "mod",
    "module",
    "root",
];

/// Different dependency types supported by the manifest.
pub mod dependency {
    use serde::{Deserialize, Serialize};

    /// The source from which the dependency may be retrieved.
    #[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
    #[serde(untagged)]
    pub enum Source {
        /// Depends on another package directly via a path to its root directory.
        Path(Path),
    }

    /// A path dependency.
    #[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
    pub struct Path {
        /// The path to the dependency's root directory.
        pub path: std::path::PathBuf,
    }
}

/// Serialize and Deserialize implementations that serialize via `Option`.
/// Designed for use with `#[serde(with = "serde_opt")]`.
mod serde_opt {
    use serde::{Deserialize, Deserializer, Serialize, Serializer};

    // If `t` is equal to the default value, serializes as `None`, otherwise as `Some(t)`.
    pub(crate) fn serialize<S, T>(t: &T, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
        T: Default + PartialEq + Serialize,
    {
        let opt = (t != &T::default()).then_some(t);
        opt.serialize(s)
    }

    // Deserializes into `Option<T>`, then calls `unwrap_or_default`.
    pub(crate) fn deserialize<'de, D, T>(d: D) -> Result<T, D::Error>
    where
        D: Deserializer<'de>,
        T: Default + Deserialize<'de>,
    {
        let opt: Option<T> = <_>::deserialize(d)?;
        Ok(opt.unwrap_or_default())
    }
}
