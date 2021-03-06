open Framework;
open Source.Ast;
open Source.Main;

let compare = (expected, result, {expect, _}) => {
  let expected = show_expression(expected);
  let result = show_expression(result);
  expect.string(result).toEqual(expected);
};

let tests: list((string, expression)) = [
  /* (".[1]", Pipe(Identity, Index(1))), */
  (". | [1]", Pipe(Identity, Index(1))),
  (".store.books", Pipe(Key("store", false), Key("books", false))),
  (".books[1]", Pipe(Key("books", false), Index(1))),
  (
    ".books[1].author",
    Pipe(Key("books", false), Pipe(Index(1), Key("author", false))),
  ),
  (".store", Key("store", false)),
  (".", Identity),
  (".store | .books", Pipe(Key("store", false), Key("books", false))),
  (
    ". | map(.price + 1)",
    Pipe(
      Identity,
      Map(Addition(Key("price", false), Literal(Number(1.)))),
    ),
  ),
  (".WAT", Key("WAT", false)),
  ("head", Head),
  (".WAT?", Key("WAT", true)),
];

describe("correctly parse value", ({test, _}) => {
  let runTest = (index, (result, expected)) =>
    test(
      "parse: " ++ string_of_int(index),
      payload => {
        let result =
          switch (parse(~debug=true, result)) {
          | Ok(r) => r
          | Error(err) => failwith(err)
          };
        compare(expected, result, payload);
      },
    );

  List.iteri(runTest, tests);
});
