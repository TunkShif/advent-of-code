defmodule Day3 do
  @input File.read!("./input-3.txt")
  @sample ~S"""
  vJrwpWtwJgWrhcsFMMfFFhFp
  jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
  PmmdzqPrVvPwwTWBwg
  wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
  ttgJtRGJQctTZtZT
  CrZsJsPPZsGzwwsLwLmpwMDw
  """

  def test() do
    157 = sum_of_items(@sample)
    70 = sum_of_badges(@sample)
  end

  def solve() do
    sum_of_items(@input) |> IO.puts()
    sum_of_badges(@input) |> IO.puts()
  end

  defp priority(item) when item in ?a..?z, do: item - 96
  defp priority(item) when item in ?A..?Z, do: item - 38

  defp rucksacks_from(input) do
    input
    |> String.trim_trailing()
    |> String.split("\n")
    |> Enum.map(&{String.to_charlist(&1), String.length(&1)})
    |> Enum.map(fn {chars, len} -> {Enum.map(chars, &priority/1), len} end)
  end

  defp sum_of_items(input) do
    for {items, len} <- rucksacks_from(input) do
      {first, last} = Enum.split(items, div(len, 2))
      MapSet.intersection(MapSet.new(first), MapSet.new(last)) |> MapSet.to_list() |> List.first()
    end
    |> Enum.sum()
  end

  defp sum_of_badges(input) do
    for group <- rucksacks_from(input) |> Enum.chunk_every(3) do
      [{first, _}, {second, _}, {third, _}] = group

      MapSet.intersection(MapSet.new(first), MapSet.new(second))
      |> MapSet.intersection(MapSet.new(third))
      |> MapSet.to_list()
      |> List.first()
    end
    |> Enum.sum()
  end
end

Day3.test()
Day3.solve()
