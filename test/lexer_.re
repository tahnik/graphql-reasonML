<<<<<<< HEAD
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
=======
Lexer.setInput("{
  # this is a comment
  friend {
    name,
    photo
  }
}");
let endOfTokens = ref(false);
let prevToken = ref(None);
while (!endOfTokens^) {
  switch(Lexer.getNextToken(prevToken^)) {
  | Some(token) => {
      Js.log(token);
      prevToken := Some(token);
      if (token.type_ === EOF) {
        endOfTokens := true;
>>>>>>> 3e4be5a764d833a4e70a3067c9a172b72702a9c4
      }
    | None => { endOfTokens := true; }
    }
  }
};

for (index in 0 to 50000) {
  testLexer();
};