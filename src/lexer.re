type punctuators = 
  | Bang
  | Dollar
  | LeftParen
  | RightParen
  | Spread
  | Colon
  | Equal
  | At
  | LeftBracket
  | RightBracket
  | Pipe
  | LeftBrace
  | RightBrace;

type tokenType = 
  | WhiteSpace
  | LineTerminators
  | Comments
  | Punctuator(punctuators)
  | Name
  | IntValue
  | FloatValue
  | StringValue
  | Comma
  | EOF
  | Undetermined;

type token = {
  type_: tokenType,
  line_: int,
  position_: int,
  /* prev_: token, */
  /* next_: option(token) */
};

let ignoredToken = [
  WhiteSpace,
  LineTerminators,
  Comments,
  Comma
];

let index = ref(0);
let firstTime = ref(true);
let input = ref("");
let line = ref(1);

let getNextIndex = () => {
  switch firstTime^ {
    | true => { firstTime := false; index^; }
    | false => { index := index^ + 1; index^; }
  };
};

let setNextIndex = (nextIndex: int) => {
  index := nextIndex;
};

let getNextToken = (prevToken: option(token)) : option(token) => {
  switch(prevToken) {
  | Some(token) => setNextIndex(token.position_)
  | _ => ()
  };
  let currentToken = { type_: Undetermined, line_: line^, position_: index^ + 1};

  let firstChar = try(String.sub(input^, index^, 1)) {
    | Invalid_argument(_err) => ""
  };

  switch(firstChar) {
  | "{" => Some({...currentToken, type_: Punctuator(LeftBrace)})
  | "}" => Some({...currentToken, type_: Punctuator(RightBrace)})
  | "" => Some({...currentToken, type_: EOF })
  | _ => None
  };
};

let setInput = (inputText: string) => {
  input := inputText;
  index := 0;
};

