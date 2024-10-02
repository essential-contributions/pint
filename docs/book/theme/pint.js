hljs.registerLanguage("pint", (hljs) => ({
  name: "Pint",
  keywords: {
    keyword:
      "as bool b256 cond const constraint else exists false forall if in int interface macro mut nil predicate pub real satisfy self state storage string true type use union var where",
    literal: "false true",
  },
  contains: [
    hljs.QUOTE_STRING_MODE,
    hljs.C_NUMBER_MODE,
    hljs.C_LINE_COMMENT_MODE,
  ],
}));

hljs.initHighlightingOnLoad();
