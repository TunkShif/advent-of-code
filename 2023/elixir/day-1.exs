defmodule Day1 do
  @input File.read!("./input-1.txt") |> String.trim()
  @sample1 ~S"""
           1abc2
           pqr3stu8vwx
           a1b2c3d4e5f
           treb7uchet
           """
           |> String.trim()

  @sample2 ~S"""
           two1nine
           eightwothree
           abcone2threexyz
           xtwone3four
           4nineeightseven2
           zoneight234
           7pqrstsixteen
           """
           |> String.trim()

  def test do
    142 = find_calibration_sum(@sample1)
    281 = find_real_calibration_sum(@sample2)
  end

  def solve do
    find_calibration_sum(@input) |> IO.puts()
    find_real_calibration_sum(@input) |> IO.puts()
  end

  defp find_calibration_sum(input) do
    input
    |> String.split("\n")
    |> Enum.map(&find_digits/1)
    |> Enum.map(&(List.first(&1) + List.last(&1) * 10))
    |> Enum.sum()
  end

  defp find_real_calibration_sum(input) do
    input
    |> String.split("\n")
    |> Enum.map(&find_digits_with_name/1)
    |> Enum.map(&(List.first(&1) + List.last(&1) * 10))
    |> Enum.sum()
  end

  defp find_digits(line), do: do_find_digits(line, [])

  defp do_find_digits(<<digit::8, rest::binary>>, acc) when digit > ?0 and digit <= ?9,
    do: do_find_digits(rest, [digit - ?0 | acc])

  defp do_find_digits(<<_char::8, rest::binary>>, acc),
    do: do_find_digits(rest, acc)

  defp do_find_digits("", acc), do: acc

  defp find_digits_with_name(line), do: do_find_digits_with_name(line, [])

  defp do_find_digits_with_name(<<"one", rest::binary>>, acc),
    do: do_find_digits_with_name("e" <> rest, [1 | acc])

  defp do_find_digits_with_name(<<"two", rest::binary>>, acc),
    do: do_find_digits_with_name("o" <> rest, [2 | acc])

  defp do_find_digits_with_name(<<"three", rest::binary>>, acc),
    do: do_find_digits_with_name("e" <> rest, [3 | acc])

  defp do_find_digits_with_name(<<"four", rest::binary>>, acc),
    do: do_find_digits_with_name(rest, [4 | acc])

  defp do_find_digits_with_name(<<"five", rest::binary>>, acc),
    do: do_find_digits_with_name("e" <> rest, [5 | acc])

  defp do_find_digits_with_name(<<"six", rest::binary>>, acc),
    do: do_find_digits_with_name(rest, [6 | acc])

  defp do_find_digits_with_name(<<"seven", rest::binary>>, acc),
    do: do_find_digits_with_name("n" <> rest, [7 | acc])

  defp do_find_digits_with_name(<<"eight", rest::binary>>, acc),
    do: do_find_digits_with_name("t" <> rest, [8 | acc])

  defp do_find_digits_with_name(<<"nine", rest::binary>>, acc),
    do: do_find_digits_with_name("e" <> rest, [9 | acc])

  defp do_find_digits_with_name(<<digit::8, rest::binary>>, acc) when digit > ?0 and digit <= ?9,
    do: do_find_digits_with_name(rest, [digit - ?0 | acc])

  defp do_find_digits_with_name(<<_char::8, rest::binary>>, acc),
    do: do_find_digits_with_name(rest, acc)

  defp do_find_digits_with_name("", acc), do: acc
end

Day1.test()
Day1.solve()
