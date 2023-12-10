module Game = struct
  module Round = struct
    type t = { red : int; green : int; blue : int } [@@deriving show]

    let empty = { red = 0; green = 0; blue = 0 }

    let can_play round =
      round.red <= 12 && round.green <= 13 && round.blue <= 14
  end

  type t = { id : int; rounds : Round.t list } [@@deriving show]

  module Parser = struct
    open Angstrom

    let is_digit = function '0' .. '9' -> true | _ -> false
    let integer = take_while1 is_digit >>| int_of_string
    let red = integer <* string " red" >>| fun i -> `Red i
    let green = integer <* string " green" >>| fun i -> `Green i
    let blue = integer <* string " blue" >>| fun i -> `Blue i
    let card = red <|> green <|> blue

    let round =
      sep_by1 (string ", ") card
      >>| List.fold_left
            Round.(
              fun round card ->
                match card with
                | `Red i -> { round with red = round.red + i }
                | `Green i -> { round with green = round.green + i }
                | `Blue i -> { round with blue = round.blue + i })
            Round.empty

    let rounds = sep_by1 (string "; ") round

    let game =
      string "Game " *> integer <* string ": " >>= fun id ->
      rounds >>| fun rounds -> { id; rounds }

    let parse input = parse_string ~consume:All game input |> Result.get_ok
  end

  let parse = Parser.parse
  let can_play game = List.for_all Round.can_play game.rounds
end

module Part_1 = struct
  let sample =
    {|
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
    |}
    |> String.trim

  let solve input =
    input
    |> String.split_on_char '\n'
    |> List.map Game.parse
    |> List.filter Game.can_play
    |> List.fold_left Game.(fun acc game -> acc + game.id) 0

  let%test "part 1" = Alcotest.(check int) "part 1 sample" 8 (solve sample)
end
