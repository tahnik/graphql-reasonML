type tokenClass = 
  | LexicalTokens
  | IgnoredTokens
  | Punctuators;

type token = {
  class_: tokenClass,
  value_: string,
  line_: int,
  position_: int
};

let sof = "<sof>";
let eof = "<eof>";
let bang = "!";
let dollar = "$";
let paren_l = "(";
let paren_r = ")";
let spread = "...";
let colon = ":";
let equals = "=";
let at = "@";
let bracket_l = "[";
let bracket_r = "]";
let brace_l = "{";
let pipe = "|";
let brace_r = "}";
let name = "name";
let int = "int";
let float = "float";
let string = "string";
let comment = "comment";
let space = " ";
let newLine = "\n";

let separators = [
  brace_l,
  brace_r,
  paren_l,
  paren_r,
  colon
];

let edibleSeparators = [
  space,
  newLine
];