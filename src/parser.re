open Lexer;

exception Unexpected_Token(string);

type name = {
  kind: string,
  value: string
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
  value: string
};

type directive = {
  kind: string,
  name: string,
  arguments: list(argument)
};

type namedType = {
  kind: string,
  name: string
};

type fragment = {
  kind: string,
  name: name,
  directives: list(directive),
  selectionSets: list(selectionSet)
} and field = {
  kind: string,
  alias: option(string),
  name: name,
  arguments: list(argument),
  directives: list(directive),
  selectionSet: list(selectionSet)
} and selectionSet = {
  kind: string,
  selections: list(selection)
} and selection = {
  fragments: list(fragment),
  fields: list(field)
};


type fragmentDefinition = {
  kind: string,
  name: name,
  condition: namedType,
  directives: list(directive),
  selectionSets: list(selectionSet)
};

type operationDefinition = {
  kind: string,
  operation: string,
  name: option(name),
  variableDefinitions: list(variableDefinition),
  directives: list(directive),
  selectionSets: list(selectionSet)
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

let parseMany = (openTok: tokenType, closeTok: tokenType) => {
   
};

let parseOperationType = () => {
  switch(Lexer.getValue()) {
  | "query" | "mutation" => Lexer.getValue()
  | _ => raise(Unexpected_Token("Expected either query or mutation"))
  }
};

let parseFragment = () => {
  []
};

let parseValueLiteral = () => {
  
};

let parseArguments = () => {
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
};

let parseDirective = () => {
  []
};

let rec parseField = () => {
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
    directives: parseDirective(),
    selectionSet: parseSelectionSet()
  }
} and parseSelectionSet = () => {
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
      selectionSets: []
    }
  } else {
    let operation = parseOperationType();
    let name = { kind: "Name", value: Lexer.getValue() };
    Lexer.advance();
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