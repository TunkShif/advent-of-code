open Base

let sample =
  {|
seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
  |}
  |> String.trim

module Almanac = struct
  type range = int * int * int
  type t = { seeds : int list; mappings : range list list }

  module Parser = struct
    open Angstrom

    let space = char ' '
    let newline = char '\n'
    let integer = take_while1 is_digit >>| int_of_string
    let numbers = sep_by1 space integer

    let range =
      numbers >>| function
      | dest :: src :: len :: _ -> (dest, src, len)
      | _ -> failwith "Unreachable"

    let seeds = string "seeds: " *> numbers <* newline <* newline
    let heading = skip_while (fun ch -> ch != '\n')
    let mapping = heading *> newline *> sep_by1 newline range
    let mappings = sep_by1 (string "\n\n") mapping

    let almanac =
      seeds >>= fun seeds ->
      mappings >>| fun mappings -> { seeds; mappings }

    let parse input = parse_string ~consume:All almanac input |> Result.get_ok
  end

  let parse = Parser.parse

  let find_match ranges src =
    List.find_map
      (fun (desti, srci, len) ->
        if src >= srci && src <= srci + len - 1 then Some (desti + (src - srci))
        else None)
      ranges
    |> Option.value ~default:src
end

module Part_1 = struct
  let solve input =
    let open Almanac in
    let { seeds; mappings } = parse input in
    let locations =
      List.map
        (fun seed ->
          List.fold_left (fun next map -> find_match map next) seed mappings)
        seeds
    in
    List.fold_left min max_int locations

  let%test "part 1" = Alcotest.(check int) "part 1 sample" 35 (solve sample)
end
