let read_input file =
  In_channel.with_open_bin file In_channel.input_all |> String.trim
