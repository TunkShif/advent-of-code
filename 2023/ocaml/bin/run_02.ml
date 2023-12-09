open Solution_02

let input = Aoc.read_input "./bin/input_02.txt" |> String.trim

let () =
  let result = Part_1.solve input in
  Printf.printf "%d\n" result
