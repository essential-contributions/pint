use super::Ident;
use crate::span::{self, Span};

#[derive(Clone, PartialEq)]
pub struct UsePath {
    pub(super) path: Vec<String>,
    pub(super) alias: Option<String>,
    pub(super) is_absolute: bool,
    pub(super) span: Span,
}

impl UsePath {
    pub(super) fn add_prefix(&mut self, mut prefix: Vec<String>) {
        prefix.append(&mut self.path);
        self.path = prefix;
    }

    pub fn matches_suffix(&self, suffix: &String) -> bool {
        self.path.last().is_some_and(|last| last == suffix)
            || self.alias.as_ref().map_or(false, |alias| alias == suffix)
    }
}

impl std::fmt::Debug for UsePath {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "UsePath({self})",)
    }
}

impl std::fmt::Display for UsePath {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let path_str = self.path.join("::");
        let alias_str = self
            .alias
            .as_ref()
            .map_or_else(String::new, |a| format!(" as {a}"));
        write!(f, "{path_str}{alias_str}",)
    }
}

#[derive(Clone, Debug)]
pub enum UseTree {
    Name { name: Ident },
    Path { prefix: Ident, suffix: Box<UseTree> },
    Group { imports: Vec<UseTree> },
    Alias { name: Ident, alias: Ident },
}

impl UseTree {
    pub(super) fn gather_paths(&self) -> Vec<UsePath> {
        match self {
            UseTree::Name { name } => {
                // A single element.
                vec![UsePath {
                    path: vec![name.name.clone()],
                    alias: None,
                    is_absolute: false,
                    span: name.span.clone(),
                }]
            }
            UseTree::Path { prefix, suffix } => {
                // A prefix and suffix(es).  Get the suffix(es) and stuff the prefix in their
                // path(s).
                suffix
                    .gather_paths()
                    .into_iter()
                    .map(|mut suffix| {
                        suffix.path.insert(0, prefix.name.clone());
                        suffix
                    })
                    .collect()
            }
            UseTree::Group { imports } => {
                // A group of imports.  Simply recurse for each one.
                imports.iter().flat_map(Self::gather_paths).collect()
            }
            UseTree::Alias { name, alias } => {
                // A single element with an alias.
                vec![UsePath {
                    path: vec![name.name.clone()],
                    alias: Some(alias.name.clone()),
                    is_absolute: false,
                    span: span::join(&name.span, &alias.span),
                }]
            }
        }
    }
}

#[cfg(test)]
fn check_use_path(paths: Vec<UsePath>, expect: expect_test::Expect) {
    expect.assert_eq(format!("{paths:?}").as_str());
}

#[cfg(test)]
use lalrpop_util::lalrpop_mod;

#[cfg(test)]
lalrpop_mod!(#[allow(unused)] pub pint_parser);

#[test]
fn gather_use_paths() {
    use crate::{error::Handler, predicate::Contract};
    use std::collections::BTreeMap;

    let parser = pint_parser::TestDelegateParser::new();
    let filepath = std::rc::Rc::from(std::path::Path::new("test"));
    let mut contract = Contract::default();

    let mut to_use_paths = |src: &str| -> Vec<UsePath> {
        parser
            .parse(
                &mut crate::parser::ParserContext {
                    mod_path: &[],
                    mod_prefix: "",
                    local_scope: None,
                    contract: &mut contract,
                    current_pred_key: None,
                    macros: &mut Vec::default(),
                    macro_calls: &mut BTreeMap::default(),
                    span_from: &|_, _| span::empty_span(),
                    use_paths: &mut Vec::default(),
                    next_paths: &mut Vec::default(),
                },
                &Handler::default(),
                crate::lexer::Lexer::new(src, &filepath, &[]),
            )
            .expect("Failed to parse test case.")
            .gather_paths()
    };

    // Each of these tests are parsing only the use tree, so there is an implicit `use <>;`
    // surrounding each.  I.e., testing just "a" is equivalent to testing `use a;`.

    check_use_path(
        to_use_paths("###usetree### a"),
        expect_test::expect!["[UsePath(a)]"],
    );

    check_use_path(
        to_use_paths("###usetree### a::b"),
        expect_test::expect!["[UsePath(a::b)]"],
    );

    check_use_path(
        to_use_paths("###usetree### a::{b, c}"),
        expect_test::expect!["[UsePath(a::b), UsePath(a::c)]"],
    );

    check_use_path(
        to_use_paths("###usetree### a::{b, c, d}"),
        expect_test::expect!["[UsePath(a::b), UsePath(a::c), UsePath(a::d)]"],
    );

    check_use_path(
        to_use_paths("###usetree### a::{b, c::d}"),
        expect_test::expect!["[UsePath(a::b), UsePath(a::c::d)]"],
    );

    check_use_path(
        to_use_paths("###usetree### a as b"),
        expect_test::expect!["[UsePath(a as b)]"],
    );

    check_use_path(
        to_use_paths("###usetree### a::{b as ab, c}"),
        expect_test::expect!["[UsePath(a::b as ab), UsePath(a::c)]"],
    );
}
