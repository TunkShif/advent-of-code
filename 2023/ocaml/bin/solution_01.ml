let is_digit = function '0' .. '9' -> true | _ -> false
let to_integer char = Char.code char - Char.code '0'

module Part_1 = struct
  let sample = {|
1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
    |} |> String.trim

  let find_value line =
    let chars = line |> String.to_seq |> Array.of_seq in
    let left = ref 0 in
    let right = ref (String.length line - 1) in
    while (not (is_digit chars.(!left))) || not (is_digit chars.(!right)) do
      if not (is_digit chars.(!left)) then left := !left + 1;
      if not (is_digit chars.(!right)) then right := !right - 1
    done;
    let left_digit = chars.(!left) in
    let right_digit = chars.(!right) in
    (to_integer left_digit * 10) + to_integer right_digit

  let solve input =
    input
    |> String.split_on_char '\n'
    |> List.map find_value
    |> List.fold_left ( + ) 0

  let%test "part 1" = Alcotest.(check int) "part 1 sample" 142 (solve sample)

  let%test "find value" =
    Alcotest.(check int) "parse value" 77 (find_value "treb7uchet")
end

module Part_2 = struct
  let sample =
    {|
two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
    |}
    |> String.trim

  let find_value line =
    let chars = line |> String.to_seq |> List.of_seq in
    let rec find_aux digits = function
      | 'o' :: 'n' :: 'e' :: rest -> find_aux (1 :: digits) ('e' :: rest)
      | 't' :: 'w' :: 'o' :: rest -> find_aux (2 :: digits) ('o' :: rest)
      | 't' :: 'h' :: 'r' :: 'e' :: 'e' :: rest ->
          find_aux (3 :: digits) ('e' :: rest)
      | 'f' :: 'o' :: 'u' :: 'r' :: rest -> find_aux (4 :: digits) rest
      | 'f' :: 'i' :: 'v' :: 'e' :: rest -> find_aux (5 :: digits) ('e' :: rest)
      | 's' :: 'i' :: 'x' :: rest -> find_aux (6 :: digits) rest
      | 's' :: 'e' :: 'v' :: 'e' :: 'n' :: rest ->
          find_aux (7 :: digits) ('n' :: rest)
      | 'e' :: 'i' :: 'g' :: 'h' :: 't' :: rest ->
          find_aux (8 :: digits) ('t' :: rest)
      | 'n' :: 'i' :: 'n' :: 'e' :: rest -> find_aux (9 :: digits) ('e' :: rest)
      | ('0' .. '9' as char) :: rest ->
          find_aux (to_integer char :: digits) rest
      | _ :: rest -> find_aux digits rest
      | [] -> digits
    in
    let digits = find_aux [] chars in
    (digits |> List.hd) + ((digits |> List.rev |> List.hd) * 10)

  let solve input =
    input
    |> String.split_on_char '\n'
    |> List.map find_value
    |> List.fold_left ( + ) 0

  let%test "part 1" = Alcotest.(check int) "part 2 sample" 281 (solve sample)

  let%test "find value" =
    Alcotest.(check int) "parse value" 29 (find_value "two1nine")
end
