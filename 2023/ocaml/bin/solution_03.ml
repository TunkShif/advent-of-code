let sample =
  {|
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
  |}
  |> String.trim

module Engine = struct
  type t = {
    schematic : string array;
    rows : int;
    cols : int;
    marker : bool array array;
  }

  let dirs =
    [ (-1, -1); (0, -1); (1, -1); (-1, 0); (1, 0); (-1, 1); (0, 1); (1, 1) ]

  let parse input =
    let schematic =
      input |> String.split_on_char '\n' |> List.to_seq |> Array.of_seq
    in
    let rows = Array.length schematic in
    let cols = String.length schematic.(0) in
    let marker = Array.make_matrix rows cols false in
    { schematic; rows; cols; marker }

  let is_digit c = c >= '0' && c <= '9'
  let is_symbol c = not (is_digit c || c == '.')
  let mark t i j = t.marker.(i).(j) <- true

  let get_part t i j =
    let row = t.schematic.(i) in
    let length = String.length row in
    let rec get_part_aux s l r acc =
      if l > 0 && is_digit s.[l - 1] then
        let () = mark t i (l - 1) in
        get_part_aux s (l - 1) r (Char.escaped s.[l - 1] ^ acc)
      else if r < length - 1 && is_digit s.[r + 1] then
        let () = mark t i (r + 1) in
        get_part_aux s l (r + 1) (acc ^ Char.escaped s.[r + 1])
      else int_of_string acc
    in
    get_part_aux row j j (Char.escaped row.[j])

  let get_parts t =
    let parts = ref [] in
    let is_inbound x y = x < t.rows && y < t.cols in
    for i = 0 to t.rows - 1 do
      for j = 0 to t.cols - 1 do
        if is_symbol t.schematic.(i).[j] then
          List.iter
            (fun (x, y) ->
              let i' = i + x in
              let j' = j + y in
              if
                is_inbound i' j'
                && is_digit t.schematic.(i').[j']
                && not t.marker.(i').(j')
              then
                let part =
                  mark t i' j';
                  get_part t i' j'
                in
                parts := part :: !parts)
            dirs
      done
    done;
    !parts

  let%test "engine" =
    Alcotest.(check int) "get_part" 467 (get_part (parse "467..114.") 0 1)
end

module Part_1 = struct
  let solve input =
    input |> Engine.parse |> Engine.get_parts |> List.fold_left ( + ) 0

  let%test "part 1" = Alcotest.(check int) "part 1 sample" 4361 (solve sample)
end
