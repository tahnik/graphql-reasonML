/**
 * Parser
 * 
 * This modules is responsible for parsing the input
 */

open Lexer;

/** The exeception generated if the syntax of the code is wrong */
exception Unexpected_Token(string);

/**
 * The intermediate representation of GraphQL query
 * 
 * All the possible types of GraphQL is defined here.
 */
type name = {
  kind: string,
  value: string
};

type objectFields = {
  key: string,
  value: string,
  nestedObj: option(value)
} and value = {
  kind: string,
  value: string,
  list: list(string),
  object_: list(objectFields),
};

type variable = {
  kind: string,
  name: name
};

type variableDefinition = {
  kind: string,
  variable: string,
  varType: string,
  value: string
};

type argument = {
  kind: string,
  name: name,
  value: value
};

type directive = {
  kind: string,
  name: name,
  arguments: list(argument)
};

type namedType = {
  kind: string,
  name: name
};

type fragment = {
  kind: string,
  name: name,
  condition: option(namedType),
  directives: list(directive),
  selectionSet: option(selectionSet)
} and field = {
  kind: string,
  alias: option(string),
  name: name,
  arguments: list(argument),
  directives: list(directive),
  selectionSet: option(selectionSet)
} and selectionSet = {
  kind: string,
  selections: selection
} and selection = {
  fragments: list(fragment),
  fields: list(field)
};


type fragmentDefinition = {
  kind: string,
  name: name,
  condition: namedType,
  directives: list(directive),
  selectionSets: selectionSet
};

type operationDefinition = {
  kind: string,
  operation: string,
  name: option(name),
  variableDefinitions: list(variableDefinition),
  directives: list(directive),
  selectionSets: option(selectionSet)
};


type definition = {
  kind: string,
  operations: list(operationDefinition),
  fragments: list(fragmentDefinition)
};

type document = {
  kind: string,
  definitions: list(definition)
};


/**
 * Each function here is responsible for parsing different types in the input
 */
let parseOperationType = () => {
  switch(Lexer.getValue()) {
  | "query" | "mutation" => Lexer.getValue()
  | _ => raise(Unexpected_Token("Expected either query or mutation"))
  }
};

let parseList = () => {
  let list = ref([]);
  Lexer.advance();
  while(Lexer.currentToken^.type_ !== Punctuator(RightBracket)) {
    list := [Lexer.getValue(), ...list^];
    Lexer.advance();
  };
  { kind: "List", value: "", list: list^, object_: [] }
};

let rec parseObject = () => {
  Js.log("parseObject");
  Lexer.advance();
  Js.log(Lexer.getValue());
  let objects = ref([]);
  let isNested = ref(false);
  while(Lexer.currentToken^.type_ != Punctuator(RightBrace)) {
    switch(Lexer.currentToken^.type_) {
    | Name => {
      Js.log("Key: " ++ Lexer.getValue());
      let key = Lexer.getValue();
      Lexer.advance();
      switch(Lexer.currentToken^.type_) {
      | Punctuator(Colon) => ()
      | _ => raise(Unexpected_Token("Expected Colon"))
      };
      Lexer.advance();
      isNested := false;
      switch(Lexer.currentToken^.type_) {
      | Name | StringValue => ()
      | Punctuator(LeftBrace) => { isNested := true; Lexer.back() }
      | _ => raise(Unexpected_Token("Expected Name or Leftbrace"))
      };

      switch(isNested^) {
      | true => {
        objects := [{ key, value: "", nestedObj: Some(parseValueLiteral()) }, ...objects^];
        Js.log("Value after nesting: " ++ Lexer.getValue());
        let _notUsed = Lexer.advance();
      }
      | false => {
        let value = Lexer.getValue();
        Js.log("Value is: " ++ Lexer.getValue());
        Lexer.advance();
        Js.log("Value after not nesting: " ++ Lexer.getValue());
        objects := [{ key, value, nestedObj: None }, ...objects^]
      }
      };
    }
    | Punctuator(RightBrace) => ()
    | Punctuator(RightParen) => ()
    | _ => raise(Unexpected_Token("Expected Name"))
    };
  };
  Js.log("Exiting While in parseObject");
  Js.log(Array.of_list(objects^));
  /* switch(objects^) {
  | [a, ...rest] => Js.log(rest)
  }; */
  { kind: "Object", value: "", list: [], object_: objects^ }
} and parseFragment = (): fragment => {
  Lexer.advance();
  let kind = ref("Fragment Spread");
  let selectionSet = ref([]);
  if (Lexer.currentToken^.type_ === Name && Lexer.getValue() !== "on") {
    {
      kind: kind^,
      condition: None,
      name: { kind: "Name", value: Lexer.getValue() },
      directives: parseDirectives(),
      selectionSet: None
    }
  } else {
    let condition = ref(None);
    if (Lexer.getValue() === "on") {
      Lexer.advance() |> ignore;
      condition := Some({ kind: "Named Type", name: { kind: "Name", value: Lexer.getValue() } });
    };
    {
      kind: "Inline Fragment",
      condition: condition^,
      name: { kind: "Name", value: Lexer.getValue() },
      directives: parseDirectives(),
      selectionSet: parseSelectionSet()
    }
  }
} and parseDirectives = () => {
  Lexer.advance();
  /* Js.log("DIRECTIVES"); */
  /* Js.log(Lexer.getValue()); */
  let directives = ref([]);
  while (Lexer.currentToken^.type_ === Punctuator(At)) {
    directives := [
      {
        kind: "Directive",
        name: { kind: "Name", value: Lexer.getValue() },
        arguments: parseArguments()
      },
      ...directives^
    ];
  };
  if (List.length(directives^) == 0) {
    Lexer.back();
  };
  directives^;
} and parseValueLiteral = () : value => {
  Lexer.advance();
  Js.log("parseValueLiteral");
  Js.log(Lexer.getValue());
  switch (Lexer.currentToken^.type_) {
  | Punctuator(LeftBracket) => parseList()
  | Punctuator(LeftBrace) => parseObject()
  | IntValue => { kind: "Integer", value: Lexer.getValue(), list: [], object_: [] }
  | FloatValue => { kind: "Float", value: Lexer.getValue(), list: [], object_: [] }
  | StringValue => { kind: "String", value: Lexer.getValue(), list: [], object_: [] }
  | Name => {
    switch(Lexer.getValue()) {
    | "true" | "false" => { kind: "Boolean", value: Lexer.getValue(), list: [], object_: [] }
    | "null" => { kind: "null", value: "", list: [], object_: [] }
    | _ => { kind: "Enum", value: Lexer.getValue(), list: [], object_: [] }
    }
  }
  | Punctuator(Dollar) => { kind: "Variable", value: Lexer.getValue(), list: [], object_: [] }
  }
} and parseArguments = () => {
  let noArgs = ref(false);
  Lexer.advance();
  switch(Lexer.currentToken^.type_) {
  | Punctuator(LeftParen) => let _ = Lexer.advance()
  | _ => noArgs := true;
  };

  Js.log("ARGUMENTS");
  Js.log(Lexer.getValue());

  switch (noArgs^) {
  | true => {
    Lexer.back();
    []
  }
  | false => {
    let arguments = ref([]);

    Js.log("Going Inside Args");
    while (Lexer.currentToken^.type_ != Punctuator(RightParen)) {
      let name = { kind: "Name", value: Lexer.getValue() };
      Js.log("Name: " ++ name.value);
      Lexer.advance();
      /* Js.log(Lexer.getValue()); */
      switch(Lexer.currentToken^.type_) {
      | Punctuator(Colon) => ()
      | _ => raise(Unexpected_Token("Expected colon"))
      };
      let argument = { kind: "Argument", name, value: parseValueLiteral() };
      arguments := [argument, ...arguments^];
      Js.log("Argument Future Value: " ++ Lexer.getValue());
      Lexer.advance();
      /* if (Lexer.currentToken^.type_ != Punctuator(Colon)) {
        Lexer.back();
      } */
    };
    Js.log("Finished Parsing Argument");
    arguments^
  };
  };

} and parseField = () => {
  let name = ref(Lexer.getValue());
  let alias = ref(None);
  Lexer.advance();
  if (Lexer.currentToken^.type_ == Punctuator(Colon)) {
    alias := Some(name^);
    if (Lexer.advance().type_ == Name) {
      name := Lexer.getValue();
    } else {
      raise(Unexpected_Token("Expected Name"));
    };
  } else {
    Lexer.back();
  };
  /* Js.log(""); */
  /* Js.log("Field"); */
  /* Js.log(name^); */
  {
    kind: "Field",
    name: { kind: "Name", value: name^ },
    alias: alias^,
    arguments: parseArguments(),
    directives: parseDirectives(),
    selectionSet: parseSelectionSet()
  }
} and parseSelectionSet = (): option(selectionSet) => {
  Lexer.advance();
  let noSelectionSet = ref(false);
  if (Lexer.currentToken^.type_ != Punctuator(LeftBrace)) {
    noSelectionSet := true;
  };
  /* Js.log("SELECTION SET"); */
  /* Js.log(Lexer.getValue()); */
  if (noSelectionSet^ == true) {
    /* Js.log("Going back"); */
    Lexer.back();
    /* Js.log(Lexer.getValue()); */
    None;
  } else {
    Lexer.advance();
    let fragments = ref([]);
    let fields = ref([]);
    while (Lexer.currentToken^.type_ != Punctuator(RightBrace)) {
      if (Lexer.currentToken^.type_ == Punctuator(Spread)) {
        fragments := [parseFragment(), ...fragments^];
      } else if (Lexer.currentToken^.type_ == Name) {
        fields := [parseField(), ...fields^];
      } else {
        raise(Unexpected_Token("Expected Name or spread"));
      };
      Lexer.advance();
    };
    Some({ kind: "Selection Set", selections: { fields: fields^, fragments: fragments^ } })
  }
};

let parseOperationDefinition = () : operationDefinition => {
  if (Lexer.currentToken^.type_ === Punctuator(LeftBrace)) {
    {
      kind: "Operation Definition",
      operation: "query",
      name: None,
      variableDefinitions: [],
      directives: [],
      selectionSets: None
    }
  } else {
    let operation = parseOperationType();
    Lexer.advance();
    let name = Some({ kind: "Name", value: Lexer.getValue() });
    {
      kind: "Operation Definition",
      operation: "query",
      name,
      variableDefinitions: [],
      directives: [],
      selectionSets: parseSelectionSet()
    }
  }
};

/* let parseFragmentDefinition = () : fragmentDefinition => {
  { kind: "Fragment Definition", operation: "query", name: None }
}; */

let parseDefinition = () => {
  let operations = ref([]);
  let fragments = ref([]);
  switch(Lexer.currentToken^.type_) {
  | Name => {
    switch(Lexer.getValue()) {
    | "query" | "mutation" => operations := [parseOperationDefinition(), ...operations^];
    /* | "fragment" => fragments := [parseFragmentDefinition(), ...fragments^]; */
    | _ => raise(Unexpected_Token("Unexpected Token"))
    };
  }
  | Punctuator(LeftBrace) => operations := [parseOperationDefinition(), ...operations^];
  | _ => raise(Unexpected_Token("Unexpected Token"))
  };
  /* Js.log({ kind: "Definition", operations: operations^, fragments: fragments^ }); */
  { kind: "Definition", operations: operations^, fragments: fragments^ }
};

let parseDocument = () : document => {
  let definitions = ref([]);
  while (Lexer.advance().type_ !== EOF) {
    let definition = parseDefinition();
    definitions := [definition, ...definitions^];
  };
  { kind: "Document", definitions: definitions^ };
};

let parse = (input: string) => {
  Lexer.setInput(input);

  parseDocument();
};