
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

  pri setNextIndex = (nextIndex: int) => {
    index := nextIndex;
  };

  pub getNextToken = () : option(token) => {
    let token = ref("");
    let nextTokenFound = ref(false);
    while (!nextTokenFound^) {
      let index = this#getNextIndex();
      let subStr = try(String.sub(input^, index, 1)) {
        | Invalid_argument(err) => ""
      };
      if (subStr === "") {
        this#setNextIndex(index - 1);
        nextTokenFound := true;
        token := "";
      } else {
        let tokenable = List.exists((a) => { a == subStr }, TokenTypes.separators);
        let edible = List.exists((a) => { a == subStr }, TokenTypes.edibleSeparators);
        if (!tokenable && !edible) {
          token := token^ ++ subStr;
        } else if (tokenable && token^ !== "") {
          this#setNextIndex(index - 1);
          nextTokenFound := true;
        }
        else if (edible && token^ !== "") {
          nextTokenFound := true;
        } 
        else if (tokenable) {
          token := token^ ++ subStr;
          nextTokenFound := true;
        }
      };
    };
    token^ === "" ? None : Some({ tokenType: "", value: token^ });
  };

  pub setInput = (inputText: string) => {
    input := inputText;
    index := 0;
  };
};

lexer#setInput("{
  empireHero: hero(episode: EMPIRE) {
    name
  }
  jediHero: hero(episode: JEDI) {
    name
  }
}");

let endOfTokens = ref(false);
while (!endOfTokens^) {
  switch (lexer#getNextToken()) {
  | Some(token) => { Js.log(token); }
  | None => { endOfTokens := true; }
  };
};