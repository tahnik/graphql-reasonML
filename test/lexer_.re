let testLexer = () => {
  Lexer.setInput("title
  url
  labels() {
    edges {
      node {
        name
      }
    }
  }");
  let endOfTokens = ref(false);
  let prevToken = ref(None);
  while (!endOfTokens^) {
    switch(Lexer.getNextToken(prevToken^)) {
    | Some(token) => {
        prevToken := Some(token);
        if (token.type_ === EOF) {
          endOfTokens := true;
        }
      }
    | None => { endOfTokens := true; }
    }
  }
};

/* for (index in 0 to 50000) { */
  testLexer();
/* }; */