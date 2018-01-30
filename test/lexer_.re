let testLexer = () => {
  Lexer.setInput("query GetFilmInfo {
    film1: film(id: \"ZmlsbXM6NA==\") {
      title
      director
      producers
    }
    film2: film(id: \"ZmlsbXM6Ng==\") {
      title
      director
      producers
    }
  }");
  let endOfTokens = ref(false);
  let prevToken = ref(None);
  while (!endOfTokens^) {
    switch(Lexer.getNextToken(prevToken^)) {
    | Some(token) => {
        /* Js.log(token); */
        prevToken := Some(token);
        if (token.type_ === EOF) {
          endOfTokens := true;
        }
      }
    | None => { endOfTokens := true; }
    }
  }
};

for (index in 0 to 50000) {
  testLexer();
};