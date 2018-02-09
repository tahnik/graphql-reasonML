let testLexer = () => {
  Lexer.setInput("query GetCityEvents {
    getCity(id: \"id-for-san-francisco\") {
      id
      name
      events {
        edges {
          node {
            id
            name
            date
            sport {
              id
              name
            }
          }
        }
      }
    }
  }");
  let endOfTokens = ref(false);
  let prevToken = ref(None);
  while (!endOfTokens^) {
    let token = Lexer.getNextToken(prevToken^);
    Js.log(token);
    prevToken := Some(token);
    if (token.type_ === EOF) {
      endOfTokens := true;
    };
  }
};

/* for (index in 0 to 50000) { */
  testLexer();
/* }; */