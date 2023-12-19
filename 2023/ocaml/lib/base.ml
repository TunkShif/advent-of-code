let is_digit c = c >= '0' && c <= '9'
let split_lines = String.split_on_char '\n'
let sum = List.fold_left ( + ) 0
