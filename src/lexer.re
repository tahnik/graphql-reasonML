exception Invalid_character(string);

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
  | LineTerminator
  | Comment
  | Punctuator(punctuators)
  | Name
  | IntValue
  | FloatValue
  | StringValue
  | Comma
  | EOF
  | Ampersand
  | Undetermined;

type token = {
  type_: tokenType,
  line_: int,
  start_: int,
  end_: int,
  /* prev_: token, */
  /* next_: option(token) */
};

let ignoredToken = [
  WhiteSpace,
  LineTerminator,
  Comment,
  Comma
];

let index = ref(0);
let firstTime = ref(true);
let input = ref("");
let line = ref(1);

let getCharCode =  (position: int) : int => {
  let tokenVal = try(String.sub(input^, position, 1)) {
  | Invalid_argument(_err) => ""
  };
  try(Char.code(tokenVal.[0])) {
  | Invalid_argument(_err) => -1
  }
};

let getNextIndex = () => {
  switch firstTime^ {
    | true => { firstTime := false; index^; }
    | false => { index := index^ + 1; index^; }
  };
};

let positionAfterWhiteSpace = (start: int) : int => {
  let position = ref(start);

  let bodyLength = String.length(input^);

  let break = ref(false);

  while (!break^) {
    let code = getCharCode(position^);
    if (code === 9 || code === 32 || code === 44) {
      position := position^ + 1;
    } else if (code === 10) {
      position := position^ + 1;
      line := line^ + 1;
    } else if ( code === 13) {
      if (getCharCode(position^ + 1) === 10) {
        position := position^ + 2
      } else {
        position := position^ + 1;
      };
      line := line^ + 1;
    } else {
      break := true;
    }
  };

  position^;
};

let setNextIndex = (nextIndex: int) => {
  index := nextIndex;
};

let getNextToken = (prevToken: option(token)) : option(token) => {
  switch(prevToken) {
  | Some(token) => setNextIndex(token.end_)
  | _ => ()
  };

  setNextIndex(positionAfterWhiteSpace(index^));

  let currentToken = { type_: Undetermined, line_: line^, start_: index^, end_: index^ + 1 };

  let code = getCharCode(index^);

  let bodyLength = String.length(input^);

  if (code < 20 && code !== 9 && code !== 10 && code !== 13 && code !== -1) {
    raise(Invalid_character("Invalid character found"));
  };

  switch(code) {
  /* ! */
  | 33 => Some({ ...currentToken, type_: Punctuator(Bang) })
  /*  */
  /* | 34  => {
    if (
      getCharCode(index^ + 1) &&
      getCharCode(index^ + 2)
    ) {

    } else {

    }
  } */
  /* # */
  | 35  => {
    /** start after the # char */
    let position = ref(index^ + 1);

    /** get the current char code */
    let code = ref(getCharCode(position^));

    /** Keep looping until we find the line terminator */
    while(code^ != -1 && (code^ > 31 || code^ === 9)) {
      position := position^ + 1;
      code := getCharCode(position^);
    };

    Some({ ...currentToken, type_: Comment, end_: position^ })
  }
  /* $ */
  | 36  => Some({ ...currentToken, type_: Punctuator(Dollar) })
  /* & */
  | 38  => Some({ ...currentToken, type_: Ampersand })
  /* ( */
  | 40  => Some({ ...currentToken, type_: Punctuator(LeftParen) })
  /* ) */
  | 41  => Some({ ...currentToken, type_: Punctuator(RightParen) })
  /* 46 */
  | 46 => {
    if (
      getCharCode(index^ + 1) === 46 &&
      getCharCode(index^ + 2) === 46
    ) {
      Some({ ...currentToken, type_: Punctuator(Spread) });
    } else {
      raise(Invalid_character("Invalid Character Found"));
    };
  }
  | _ when code >= 65 && code <= 122 => {
    let position = ref(index^);

    let bodyLength = String.length(input^);

    let break = ref(false);

    let code = ref(getCharCode(position^));

    while (
      position^ !== bodyLength &&
      code^ !== -1 &&
      (code^ === 95 || /* _ */
      (code^ >= 48 && code^ <= 57) || /* 0-9 */
      (code^ >= 65 && code^ <= 90) || /* A-Z */
      (code^ >= 97 && code^ <= 122))  /* a-z */
    ) {
      code := getCharCode(position^);
      position := position^ + 1;
    };
    Some({ ...currentToken, type_: Name, end_: position^ });
  }
  /* | _ when code >= 45 && code <= 57 => {

  } */
  /* : */
  | 58 => Some({ ...currentToken, type_: Punctuator(Colon) })
  /* = */
  | 61 => Some({ ...currentToken, type_: Punctuator(Equal) })
  /* @ */
  | 64 => Some({ ...currentToken, type_: Punctuator(At) })
  /* [ */
  | 91 => Some({ ...currentToken, type_: Punctuator(LeftBracket) })
  /* ] */
  | 93 => Some({ ...currentToken, type_: Punctuator(RightBracket) })
  /* { */
  | 123 => Some({ ...currentToken, type_: Punctuator(LeftBrace) })
  /* | */
  | 124 => Some({ ...currentToken, type_: Punctuator(Pipe) })
  /* } */
  | 125 => Some({ ...currentToken, type_: Punctuator(RightBrace) })
  /* End of file */
  | -1 when index^ === bodyLength => Some({ ...currentToken, type_: EOF }) 
  | -1 => raise(Invalid_character("Invalid Character Found"))
  };
};

let setInput = (inputText: string) => {
  input := inputText;
  index := 0;
};

