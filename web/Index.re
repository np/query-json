/* open Source;
   open Source.Compiler;
   open Source.Console;

   type inputKind =
     | File
     | Inline;
    */
let run = (query: option(string), json: option(string)) => {
  switch (query, json) {
  | (Some(q), Some(j)) => Js.log(q ++ " " ++ j)
  | (None, None) => Js.log("OK")
  | _ => Js.log("OK")
  };
};
