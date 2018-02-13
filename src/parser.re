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
  name: string,
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
  name: string,
  directives: list(directive),
  selectionSets: list(selectionSet)
} and field = {
  kind: string,
  alias: string,
  name: string,
  arguments: list(argument),
  directives: list(directive),
  selectionSet: list(selectionSet)
} and selectionSet = {
  kind: string,
  fragments: list(fragment),
  fields: list(field)
};

type fragmentDefinition = {
  kind: string,
  name: string,
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

let parseOperationType = () => {
  switch(Lexer.getValue()) {
  | "query" | "mutation" => Lexer.getValue()
  | _ => raise(Unexpected_Token("Expected either query or mutation"))
  }
};

let parseSelectionSet = () => {
  { kind: "Selection Set", fragments: "",  }
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
    {
      kind: "Operation Definition",
      operation: "query",
      name: None,
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