/**
 * Lexer
 * 
 * This module is responsible for breaking down the input into tokens
 */


/** This is the exception that is generated if there is a token is not valid */
exception Invalid_character(string);

/**
 * All the valid tokens for GraphQl Language
 * Can be found here: http://facebook.github.io/graphql/October2016/#sec-Language
 */
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
  end_: int
};

/** These tokens are ignored if they appear in input */
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
let currentToken = ref({ type_: Undetermined, line_: line^, start_: index^, end_: index^ + 1 });
let previousToken = ref(None);


/** Get the next index in the input */
let getNextIndex = () => {
  switch firstTime^ {
    | true => { firstTime := false; index^; }
    | false => { index := index^ + 1; index^; }
  };
};

/** Sets the next input if necessary */
let setNextIndex = (nextIndex: int) => {
  index := nextIndex;
};

/** Skips whitespace and moves to the next token */
let positionAfterWhiteSpace = () => {
  let position = ref(index^);

  let bodyLength = String.length(input^);

  let break = ref(false);

  while (!break^ && position^ < bodyLength) {
    let code = try(Char.code(input^.[position^])) {
      | Invalid_argument(_err) => -1
    };
    if (code == 9 || code == 32 || code == 44) {
      position := position^ + 1;
    } else if (code == 10) {
      position := position^ + 1;
      line := line^ + 1;
    } else if (code == 13) {
      let code = try(Char.code(input^.[position^ + 1])) {
        | Invalid_argument(_err) => -1
      };
      if (code == 10) {
        position := position^ + 2
      } else {
        position := position^ + 1;
      };
      line := line^ + 1;
    } else {
      break := true;
    }
  };

  setNextIndex(position^);
};

let readString = () : int => {
  let position = ref(index^ + 1);

  let code = ref(Char.code(input^.[position^]));

  let bodyLength = String.length(input^);

  let break = ref(false);

  while (position^ < bodyLength && code^ !== 10 && code^ !== 13 && !break^) {
    code := Char.code(input^.[position^]);
    switch(code^) {
    | 34 => { position := position^ + 1; break := true; }
    | _ when code^ < 20 && code^ !== 9 => {
      raise(Invalid_character("Invalid Character Detected"));
    }
    | 92 => position := position^ + 1;
    | _  => position := position^ + 1;
    };
  };

  position^;
};

let readDigits = (startPos) : int => {
  let position = ref(startPos);

  let code = ref(Char.code(input^.[position^]));

  if (code^ >= 48 && code^ <= 57) {
    position := position^ + 1;
    code := Char.code(input^.[position^]);

    while (code^ >= 48 && code^ <= 57) {
      position := position^ + 1;
      code := Char.code(input^.[position^]);
    };
    position^;
  } else {
    raise(Invalid_character("Invalid Character inside digits"));
  }

};

let readNumber = () : token => {
  let position = ref(index^);

  let code = ref(Char.code(input^.[position^]));

  let type_ = ref(IntValue);

  if (code^ === 45) {
    position := position^ + 1;
    code := Char.code(input^.[position^]);
  };

  if (code^ === 48) {
    position := position^ + 1;
    code := Char.code(input^.[position^]);

    if (code^ >= 48 && code^ <= 57) {
      raise(Invalid_character("Unexpected Character after 0"));
    } else {
      position := readDigits(position^);
    };
  };

  if (code^ === 46) {
    type_ := FloatValue;
    position := position^ + 1;
    code := Char.code(input^.[position^]);

    position := readDigits(position^);
    code := Char.code(input^.[position^]);
  };

  if (code^ === 69 && code^ === 101) {
    type_ := FloatValue;
    position := position^ + 1;
    code := Char.code(input^.[position^]);

    if (code^ === 43 && code^ === 45) {
      position := position^ + 1;
      code := Char.code(input^.[position^]);

      position := readDigits(position^);
    };
  };

  { ...currentToken^, type_: type_^, end_: position^ }
};


/** Gets the next token in the input */
let getNextToken = (prevToken: option(token)) : token => {

  switch(prevToken) {
  | Some(token) => {
    setNextIndex(token.end_);
    previousToken := prevToken;
  }
  | _ => ()
  };


  let bodyLength = String.length(input^);

  positionAfterWhiteSpace();

  currentToken := { type_: Undetermined, line_: line^, start_: index^, end_: index^ + 1 };

  if (index^ === bodyLength) {
    { ...currentToken^, type_: EOF, end_: index^ }
  } else {
    let code = Char.code(input^.[index^]);

    if (code < 20 && code != 9 && code != 10 && code != 13) {
      raise(Invalid_character("Invalid character found"));
    };

    switch(code) {
    /* ! */
    | 33 => { ...currentToken^, type_: Punctuator(Bang) }
    /* */
    | 34  => {
      let end_ = readString();
      { ...currentToken^, type_: StringValue, end_ }
    }
    /* # */
    | 35  => {
      /** start after the # char */
      let position = ref(index^ + 1);

      /** get the current char code */
      let code = ref(Char.code(input^.[position^]));

      /** Keep looping until we find the line terminator */
      while(code^ != -1 && (code^ > 31 || code^ === 9)) {
        position := position^ + 1;
        code := Char.code(input^.[position^]);
      };

      { ...currentToken^, type_: Comment, end_: position^ }
    }
    /* $ */
    | 36  => { ...currentToken^, type_: Punctuator(Dollar) }
    /* & */
    | 38  => { ...currentToken^, type_: Ampersand }
    /* ( */
    | 40  => { ...currentToken^, type_: Punctuator(LeftParen) }
    /* ) */
    | 41  => { ...currentToken^, type_: Punctuator(RightParen) }
    /* 46 */
    | 46 => {
      if (
        Char.code(input^.[index^ + 1]) === 46 &&
        Char.code(input^.[index^ + 2]) === 46
      ) {
        { ...currentToken^, type_: Punctuator(Spread) };
      } else {
        raise(Invalid_character("Invalid Character Found"));
      };
    }
    | _ when code >= 45 && code <= 57 => readNumber()
    | _ when code >= 65 && code <= 122 => {
      let position = ref(index^);

      let bodyLength = String.length(input^);

      let break = ref(false);

      let code = ref(try(Char.code(input^.[position^])) {
      | Invalid_argument(_err) => -1
      });

      while (
        position^ != bodyLength &&
        code^ != -1 &&
        (code^ == 95 || /* _ */
        (code^ >= 48 && code^ <= 57) || /* 0-9 */
        (code^ >= 65 && code^ <= 90) || /* A-Z */
        (code^ >= 97 && code^ <= 122))  /* a-z */
      ) {
        code := Char.code(input^.[position^]);
        /* Js.log(String.sub(input^, currentToken^.start_, position^ - currentToken^.start_));
        Js.log(code^); */
        position := position^ + 1;
      };
      { ...currentToken^, type_: Name, end_: position^ - 1 };
    }
    /* | _ when code >= 45 && code <= 57 => {

    } */
    /* : */
    | 58 => { ...currentToken^, type_: Punctuator(Colon) }
    /* = */
    | 61 => { ...currentToken^, type_: Punctuator(Equal) }
    /* @ */
    | 64 => { ...currentToken^, type_: Punctuator(At) }
    /* [ */
    | 91 => { ...currentToken^, type_: Punctuator(LeftBracket) }
    /* ] */
    | 93 => { ...currentToken^, type_: Punctuator(RightBracket) }
    /* { */
    | 123 => { ...currentToken^, type_: Punctuator(LeftBrace) }
    /* | */
    | 124 => { ...currentToken^, type_: Punctuator(Pipe) }
    /* } */
    | 125 => { ...currentToken^, type_: Punctuator(RightBrace) }
    | _   => raise(Invalid_character("Invalid Character Found"))
    };
  }

};

/** Set the input that will be broken down to tokens */
let setInput = (inputText: string) => {
  input := inputText;
  index := 0;
};


/** Advances to the next token in the token stream */
let advance = () : token => {
  let prevToken = currentToken^;
  if (prevToken.type_ === Undetermined) {
    currentToken := getNextToken(None);
  } else {
    currentToken := getNextToken(Some(currentToken^));
  };
  currentToken^;
};

/** Goes back into the token stream */
let back = () => {
  switch(previousToken^) {
  | Some(token) => currentToken := token;
  }
};

/** Get the value of the current token */
let getValue = () => {
  String.sub(input^, currentToken^.start_, currentToken^.end_ - currentToken^.start_);
};