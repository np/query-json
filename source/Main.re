open Ast;
open Tokenizer;

let positionToString = (start, end_) =>
  Printf.sprintf(
    "[line: %d, char: %d-%d]",
    start.Lexing.pos_lnum,
    start.Lexing.pos_cnum - start.Lexing.pos_bol,
    end_.Lexing.pos_cnum - end_.Lexing.pos_bol,
  );

let makeError = (~input, ~start: Lexing.position, ~end_: Lexing.position, exn) =>
  "\n\n"
  ++ input
  ++ "\n"
  ++ String.make(start.pos_cnum, ' ')
  ++ String.make(end_.pos_cnum - start.pos_cnum, '^')
  ++ "\nProblem parsing at position "
  ++ positionToString(start, end_)
  ++ "\n"
  ++ Printexc.to_string(exn);

let menhir = MenhirLib.Convert.Simplified.traditional2revised(Parser.prog);

let last_position = ref(Location.none);

let provider = (buf, ()) => {
  let (start, stop) = Sedlexing.lexing_positions(buf);
  let token =
    switch (tokenize(buf)) {
    | Ok(token) => token
    | Error(error) => failwith(error)
    };
  last_position :=
    Location.{loc_start: start, loc_end: stop, loc_ghost: false};

  print_endline(show_token(token));

  (token, start, stop);
};

let parse = (input: string): option(expression) => {
  let fn = Sedlexing.Utf8.from_string(input) |> provider;
  try(menhir(fn)) {
  | exn =>
    let Location.{loc_start, loc_end, _} = last_position^;
    failwith(makeError(~input, ~start=loc_start, ~end_=loc_end, exn));
  };
};