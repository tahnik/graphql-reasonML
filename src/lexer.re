
type token = {
  tokenType: string,
  value: string
};

let lexer = {
  val index = ref(0);
  val firstTime = ref(true);
  val input = ref("");

  pri getNextIndex = () => {
    switch firstTime^ {
      | true => { firstTime := false; index^; }
      | false => { index := index^ + 1; index^; }
    };
  };

  pri getNextToken = () : token => {
    let index = this#getNextIndex();
    { tokenType: "", value: "" };
  };

  pub setInput = (inputText: string) => {
    input := inputText;
  };
};

