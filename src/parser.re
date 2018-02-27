open Lexer;

exception Unexpected_Token(string);

type name = {
  kind: string,
  value: string
};

type objectFields = {
  key: string,
  value: string
};

type value = {
  kind: string,
  value: string,
  list: list(string),
  object_: list(objectFields)
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
  selectionSet: selectionSet
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
  { kind: "List", value: "", list: list^, object_: []  }
};

let parseObject = () => {
  let objects = ref([]);
  while(Lexer.currentToken^.type_ === Punctuator(RightBrace)) {
    Lexer.advance();
    switch(Lexer.currentToken^.type_) {
    | Name => ()
    | _ => raise(Unexpected_Token("Expected Name"))
    };
    let key = Lexer.getValue();
    switch(Lexer.currentToken^.type_) {
    | Punctuator(Colon) => ()
    | _ => raise(Unexpected_Token("Expected Name"))
    };
    Lexer.advance();
    switch(Lexer.currentToken^.type_) {
    | Name => ()
    | _ => raise(Unexpected_Token("Expected Name"))
    };
    let value = Lexer.getValue();
    objects := [{ key, value}, ...objects^];
  };
  { kind: "List", value: "", list: [], object_: objects^ }
};


let rec parseFragment = (): fragment => {
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
      selectionSet: Some(parseSelectionSet())
    }
  }
} and parseDirectives = () => {
  Lexer.advance();
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
  directives^;
} and parseValueLiteral = () : value => {
  Lexer.advance();
  switch (Lexer.currentToken^.type_) {
  | Punctuator(LeftBracket) => parseList()
  | Punctuator(LeftBrace) => parseObject()
  | IntValue => { kind: "Integer", value: Lexer.getValue(), list: [], object_: [] }
  | FloatValue => { kind: "Float", value: Lexer.getValue(), list: [], object_: [] }
  | StringValue => { kind: "String", value: Lexer.getValue(), list: [], object_: [] }
  | Name => {
    switch(Lexer.getValue()) {
    | "true" | "false" => { kind: "Boolean", value: Lexer.getValue(), list: [], object_: []  }
    | "null" => { kind: "null", value: "", list: [], object_: [] }
    | _ => { kind: "Enum", value: Lexer.getValue(), list: [], object_: []  }
    }
  }
  | Punctuator(Dollar) => { kind: "Variable", value: Lexer.getValue(), list: [], object_: [] }
  }
} and parseArguments = () => {
  switch(Lexer.currentToken^.type_) {
  | Punctuator(LeftParen) => let _ = Lexer.advance()
  | Name => ()
  | _ => raise(Unexpected_Token("Expected Name"))
  };

  let arguments = ref([]);

  while (Lexer.currentToken^.type_ !== Punctuator(RightParen)) {
    let name = { kind: "Name", value: Lexer.getValue() };
    switch(Lexer.advance().type_) {
    | Punctuator(Colon) => ()
    | _ => raise(Unexpected_Token("Expected colon"))
    };
    let argument = { kind: "Argument", name, value: parseValueLiteral() };
    arguments := [argument, ...arguments^];
    Lexer.advance();
  };

  arguments^
} and  parseField = () => {
  let name = ref(Lexer.getValue());
  let alias = ref(None);
  if (Lexer.advance().type_ === Punctuator(Colon)) {
    alias := Some(name^);
    if (Lexer.advance().type_ === Name) {
      name := Lexer.getValue();
    } else {
      raise(Unexpected_Token("Expected Name"));
    };
  };
  {
    kind: "Field",
    name: { kind: "Name", value: name^ },
    alias: alias^,
    arguments: parseArguments(),
    directives: parseDirectives(),
    selectionSet: parseSelectionSet()
  }
} and parseSelectionSet = (): selectionSet => {
  Lexer.advance();
  let fragments = ref([]);
  let fields = ref([]);
  while (Lexer.currentToken^.type_ !== Punctuator(RightBrace)) {
    if (Lexer.currentToken^.type_ === Punctuator(Spread)) {
      fragments := [parseFragment(), ...fragments^];
    } else if (Lexer.currentToken^.type_ === Name) {
      fields := [parseField(), ...fields^];
    } else {
      raise(Unexpected_Token("Expected Name or spread"));
    };
  };
  { kind: "Selection Set", selections: { fields: fields^, fragments: fragments^ } }
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
    Js.log(Lexer.currentToken^.type_);
    Js.log(Lexer.getValue());
    {
      kind: "Operation Definition",
      operation: "query",
      name,
      variableDefinitions: [],
      directives: [],
      selectionSets: Some(parseSelectionSet())
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

let parse = () => {
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

  parseDocument();
};