open Base

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
  module IntSet = Set.Make (Int)

  type t = { id : int; win : IntSet.t; got : int list; result : int }

  let compare a b = a.id - b.id

  let points t =
    match t.result with
    | 0 -> 0
    | _ -> int_of_float (2. ** float_of_int (t.result - 1))

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
      seprator *> got >>| fun got ->
      {
        id;
        win;
        got;
        result = List.filter (fun n -> IntSet.mem n win) got |> List.length;
      }

    let parse input = parse_string ~consume:All card input |> Result.get_ok
  end

  let parse input = input |> split_lines |> List.map Parser.parse
end

module Part_1 = struct
  let solve input = input |> Card.parse |> List.map Card.points |> sum
  let%test "part 1" = Alcotest.(check int) "part 1 sample" 13 (solve sample)
end

module Part_2 = struct
  let solve input =
    let card_table = Hashtbl.create 128 in
    let count_table = Hashtbl.create 128 in
    let cards = Card.parse input in
    let () =
      List.iter
        Card.(
          fun card ->
            Hashtbl.add card_table card.id card.result;
            Hashtbl.add count_table card.id 1)
        cards
    in
    let count = Hashtbl.length card_table in
    for i = 1 to count do
      let won = Hashtbl.find card_table i in
      let count = Hashtbl.find count_table i in
      if won != 0 then
        for j = i + 1 to i + won do
          let current = Hashtbl.find count_table j in
          Hashtbl.replace count_table j (current + count)
        done
    done;
    count_table |> Hashtbl.to_seq_values |> List.of_seq |> sum

  let%test "part 2" = Alcotest.(check int) "part 2 sample" 30 (solve sample)
end
