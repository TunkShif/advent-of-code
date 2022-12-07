defmodule Day1 do
  @input File.read!("./input-1.txt")
  @sample ~S"""
  1000
  2000
  3000

  4000

  5000
  6000

  7000
  8000
  9000

  10000
  """

  def test() do
    24000 = max_calories(@sample)
  end

  def solve() do
    max_calories(@input) |> IO.puts()
    total_calories_of_top_three(@input) |> IO.puts()
  end

  defp max_calories(input) do
    inventories_from(input)
    |> Enum.max()
  end

  defp total_calories_of_top_three(input) do
    inventories_from(input)
    |> Enum.sort(:desc)
    |> Enum.take(3)
    |> Enum.sum()
  end

  defp inventories_from(input) do
    input
    |> String.trim_trailing()
    |> String.split("\n\n")
    |> Enum.map(&String.split(&1, "\n"))
    |> Enum.map(fn inv -> Enum.map(inv, &String.to_integer/1) end)
    |> Enum.map(fn inv -> Enum.reduce(inv, &(&1 + &2)) end)
  end
end

Day1.test()
Day1.solve()
