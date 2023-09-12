use super::{Ident, UseTree};
use crate::span::Span;

#[derive(Clone, PartialEq)]
pub(super) struct UsePath {
    pub(super) path: Vec<Ident>,
    pub(super) has_glob: bool,
    pub(super) alias: Option<Ident>,
    pub(super) span: Span,
}

impl UsePath {
    #[allow(dead_code)]
    pub(super) fn add_prefix(&mut self, mut prefix: Vec<Ident>) {
        prefix.append(&mut self.path);
        self.path = prefix;
    }
}

impl std::fmt::Debug for UsePath {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let path_str = self
            .path
            .iter()
            .map(|p| p.name.to_string())
            .collect::<Vec<_>>()
            .join("::");
        let glob_str = if self.has_glob { "::*" } else { "" };
        let alias_str = self
            .alias
            .as_ref()
            .map(|a| format!(" as {}", &a.name))
            .unwrap_or_else(|| "".to_owned());
        write!(f, "UsePath({path_str}{glob_str}{alias_str})",)
    }
}

impl UseTree {
    #[allow(dead_code)]
    pub(super) fn gather_paths(&self) -> Vec<UsePath> {
        match self {
            UseTree::Glob(span) => {
                // Just a glob suffix.
                vec![UsePath {
                    path: Vec::new(),
                    has_glob: true,
                    alias: None,
                    span: span.clone(),
                }]
            }
            UseTree::Name { name, span } => {
                // A single element.
                vec![UsePath {
                    path: vec![name.clone()],
                    has_glob: false,
                    alias: None,
                    span: span.clone(),
                }]
            }
            UseTree::Path { prefix, suffix, .. } => {
                // A prefix and suffix(es).  Get the suffix(es) and stuff the prefix in their
                // path(s).
                suffix
                    .gather_paths()
                    .into_iter()
                    .map(|mut suffix| {
                        suffix.path.insert(0, prefix.clone());
                        suffix
                    })
                    .collect()
            }
            UseTree::Group { imports, .. } => {
                // A group of imports.  Simply recurse for each one.
                imports
                    .iter()
                    .flat_map(|import| import.gather_paths())
                    .collect()
            }
            UseTree::Alias { name, alias, span } => {
                // A single element with an alias.
                vec![UsePath {
                    path: vec![name.clone()],
                    has_glob: false,
                    alias: Some(alias.clone()),
                    span: span.clone(),
                }]
            }
        }
    }
}

#[cfg(test)]
fn check_use_path(paths: Vec<UsePath>, expect: expect_test::Expect) {
    expect.assert_eq(format!("{paths:?}").as_str());
}

#[test]
fn gather_use_paths() {
    fn to_use_tree(src: &str) -> UseTree {
        let test_file_name = std::rc::Rc::from(std::path::Path::new("test"));
        match crate::parser::parse_str_to_ast(src, test_file_name)
            .expect("Failed to parse test case.")
            .0
            .into_iter()
            .next()
            .expect("Failed to get first decl in parsed result for test case.")
        {
            crate::ast::Decl::Use { use_tree, .. } => use_tree,
            _ => panic!("Parsed use tree source didn't produce a Decl::Use."),
        }
    }

    check_use_path(
        to_use_tree("use a;").gather_paths(),
        expect_test::expect!["[UsePath(a)]"],
    );

    check_use_path(
        to_use_tree("use a::b;").gather_paths(),
        expect_test::expect!["[UsePath(a::b)]"],
    );

    check_use_path(
        to_use_tree("use a::{b, c};").gather_paths(),
        expect_test::expect!["[UsePath(a::b), UsePath(a::c)]"],
    );

    check_use_path(
        to_use_tree("use a::{b, c, d};").gather_paths(),
        expect_test::expect!["[UsePath(a::b), UsePath(a::c), UsePath(a::d)]"],
    );

    check_use_path(
        to_use_tree("use a::{b, c::d};").gather_paths(),
        expect_test::expect!["[UsePath(a::b), UsePath(a::c::d)]"],
    );

    check_use_path(
        to_use_tree("use a as b;").gather_paths(),
        expect_test::expect!["[UsePath(a as b)]"],
    );

    check_use_path(
        to_use_tree("use a::{b as ab, c};").gather_paths(),
        expect_test::expect!["[UsePath(a::b as ab), UsePath(a::c)]"],
    );

    check_use_path(
        to_use_tree("use a::*;").gather_paths(),
        expect_test::expect!["[UsePath(a::*)]"],
    );

    check_use_path(
        to_use_tree("use a::{*, b::c};").gather_paths(),
        expect_test::expect!["[UsePath(a::*), UsePath(a::b::c)]"],
    );

    check_use_path(
        to_use_tree("use a::{*, b::c as abc};").gather_paths(),
        expect_test::expect!["[UsePath(a::*), UsePath(a::b::c as abc)]"],
    );
}
