hljs.registerLanguage("bnf", (hljs) => ({
  name: "BNF",
  contains: [
    // Attribute
    {
      className: "attribute",
      begin: /</,
      end: />/,
    },
    hljs.C_LINE_COMMENT_MODE,
    hljs.C_BLOCK_COMMENT_MODE,
    hljs.APOS_STRING_MODE,
    hljs.QUOTE_STRING_MODE,
    // Specific
    {
      begin: /::=/,
      starts: {
        contains: [
          {
            begin: /</,
            end: />/,
          },
          // Common
          hljs.C_LINE_COMMENT_MODE,
          hljs.C_BLOCK_COMMENT_MODE,
          hljs.APOS_STRING_MODE,
          hljs.QUOTE_STRING_MODE,
        ],
      },
    },
  ],
}));

hljs.initHighlightingOnLoad();
