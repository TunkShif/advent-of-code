open Base
module IntSet = Set.Make (Int)

let sample =
  {|
Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
  |}
  |> String.trim

module Card = struct
  type t = { id : int; win : IntSet.t; got : int list }

  module Parser = struct
    open Angstrom

    let inetger = take_while1 is_digit >>| int_of_string
    let space = many1 (char ' ')
    let seprator = space *> char '|' <* space
    let numbers = sep_by1 space inetger
    let win = numbers >>| IntSet.of_list
    let got = numbers
    let card_id = string "Card" *> space *> inetger <* char ':' <* space

    let card =
      card_id >>= fun id ->
      win >>= fun win ->
      seprator *> got >>| fun got -> { id; win; got }

    let parse input = parse_string ~consume:All card input |> Result.get_ok
  end

  let parse input = input |> split_lines |> List.map Parser.parse
  let win t number = IntSet.mem number t.win

  let points t =
    List.filter (win t) t.got |> List.length |> fun n ->
    match n with 0 -> 0 | _ -> int_of_float (2. ** float_of_int (n - 1))
end

module Part_1 = struct
  let solve input = input |> Card.parse |> List.map Card.points |> sum
  let%test "part 1" = Alcotest.(check int) "part 1 sample" 13 (solve sample)
end
