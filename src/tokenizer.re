
type tokenType = 
  | WhiteSpace
  | LineTerminators
  | Comments
  | Punctuator
  | Name
  | IntValue
  | FloatValue
  | StringValue;


type token = {
  type_: tokenType,
  value_: string,
  line_: int,
  position_: int,
  prev_: token,
  next_: option(token)
};

let index = ref(0);
let firstTime = ref(true);
let input = ref("");

/* let getNextIndex = () => {
  switch firstTime^ {
    | true => { firstTime := false; index^; }
    | false => { index := index^ + 1; index^; }
  };
};

let setNextIndex = (nextIndex: int) => {
  index := nextIndex;
};

let getNextToken = () : option(token) => {
  let token = ref("");
  let nextTokenFound = ref(false);
  while (!nextTokenFound^) {
    let index = getNextIndex();
    let subStr = try(String.sub(input^, index, 1)) {
      | Invalid_argument(err) => ""
    };
    if (subStr === "") {
      setNextIndex(index - 1);
      nextTokenFound := true;
      token := "";
    } else {
      let tokenable = List.exists((a) => { a == subStr }, TokenTypes.separators);
      let edible = List.exists((a) => { a == subStr }, TokenTypes.edibleSeparators);
      if (!tokenable && !edible) {
        token := token^ ++ subStr;
      } else if (tokenable && token^ !== "") {
        setNextIndex(index - 1);
        nextTokenFound := true;
      } else if (edible && token^ !== "") {
        nextTokenFound := true;
      } else if (tokenable) {
        token := token^ ++ subStr;
        nextTokenFound := true;
      }
    };
  };
  token^ === "" ? None : Some({ type_: "", value_: token^ });
}; */

let setInput = (inputText: string) => {
  input := inputText;
  index := 0;
};