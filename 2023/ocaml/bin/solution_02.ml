let is_digit = function '0' .. '9' -> true | _ -> false
let to_integer char = Char.code char - Char.code '0'

module Game = struct
  type t = { id : int; red : int; green : int; blue : int }
  [@@deriving show, make, eq]

  let parse source =
    let rec parse_aux game = function
      | 'G' :: 'a' :: 'm' :: 'e' :: ' ' :: ch :: rest when is_digit ch ->
          parse_aux { game with id = to_integer ch } rest
      | ch :: ' ' :: 'r' :: 'e' :: 'd' :: rest when is_digit ch ->
          parse_aux { game with red = game.red + to_integer ch } rest
      | ch :: ' ' :: 'g' :: 'r' :: 'e' :: 'e' :: 'n' :: rest when is_digit ch ->
          parse_aux { game with green = game.green + to_integer ch } rest
      | ch :: ' ' :: 'b' :: 'l' :: 'u' :: 'e' :: rest ->
          parse_aux { game with blue = game.blue + to_integer ch } rest
      | _ :: rest -> parse_aux game rest
      | [] -> game
    in
    parse_aux
      (make ~id:0 ~red:0 ~green:0 ~blue:0)
      (source |> String.to_seq |> List.of_seq)

  let id t = t.id

  let can_play_with game ~red ~green ~blue =
    game.red <= red && game.green <= green && game.blue <= blue

  let%test "game" =
    Alcotest.(check bool)
      "parse game" true
      (equal
         { id = 1; red = 5; green = 4; blue = 9 }
         (parse "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"))
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

  let can_play = Game.can_play_with ~red:12 ~green:13 ~blue:14

  let solve input =
    input
    |> String.split_on_char '\n'
    |> List.map Game.parse
    |> List.filter can_play
    |> List.map Game.id
    |> List.fold_left ( + ) 0

  let%test "part 1" = Alcotest.(check int) "part 1 sample" 8 (solve sample)
end
