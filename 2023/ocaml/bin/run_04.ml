open Solution_04

let () = Printexc.record_backtrace true

let input = Aoc.read_input "./bin/input_04.txt" |> String.trim

let () =
  let result = Part_1.solve input in
  Printf.printf "%d\n" result

let () =
  let result = Part_2.solve input in
  Printf.printf "%d\n" result
