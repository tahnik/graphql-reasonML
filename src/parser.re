Tokenizer.setInput("{}");
let endOfTokens = ref(false);
let prevToken = ref(None);
while (!endOfTokens^) {
  switch(Tokenizer.getNextToken(prevToken^)) {
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

