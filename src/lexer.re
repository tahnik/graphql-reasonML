type tesla = {
  .
  drive: int
};

let obj: tesla = {
  val hasEnvy = ref(false);
  pub drive = 9;
  pri enableEnvy = (envy) => hasEnvy := envy
};