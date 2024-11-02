hljs.registerLanguage("pint", (hljs) => ({
  name: "Pint",
  keywords: {
    keyword:
      "as bool b256 cond const constraint else exists false forall if in int interface let macro match mut nil predicate real self storage string true type union use where",
    literal: "false true",
  },
  contains: [
    hljs.QUOTE_STRING_MODE,
    hljs.C_NUMBER_MODE,
    hljs.C_LINE_COMMENT_MODE,
  ],
}));

hljs.initHighlightingOnLoad();
