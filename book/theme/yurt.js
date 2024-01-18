hljs.registerLanguage("yurt", (hljs) => ({
  name: "Yurt",
  keywords: {
    keyword:
      "as bool cond constraint contract else enum extern forall fn if implements in interface int let macro maximize minimize real satisfy solve state string type use where",
    literal: "false true",
  },
  contains: [
    hljs.QUOTE_STRING_MODE,
    hljs.C_NUMBER_MODE,
    hljs.C_LINE_COMMENT_MODE,
  ],
}));

hljs.initHighlightingOnLoad();
