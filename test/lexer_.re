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
      }
    }
  | None => { endOfTokens := true; }
  }
}