open Solution_01

let input = Aoc.read_input "./bin/input_01.txt" |> String.trim

let () =
  let result = Part_1.solve input in
  Printf.printf "%d\n" result

let () =
  let result = Part_2.solve input in
  Printf.printf "%d\n" result
