defmodule Day4 do
  @input File.read!("./input-4.txt")
  @sample ~S"""
  2-4,6-8
  2-3,4-5
  5-7,7-9
  2-8,3-7
  6-6,4-6
  2-6,4-8
  """

  def test() do
    2 = count_pairs(@sample)
    4 = count_overlap(@sample)
  end

  def solve() do
    count_pairs(@input) |> IO.puts()
    count_overlap(@input) |> IO.puts()
  end

  defp pairs_from(input) do
    for line <-
          input
          |> String.trim_trailing()
          |> String.split("\n") do
      [first, second] = line |> String.split(",")
      to_pair = fn item -> item |> String.split("-") |> Enum.map(&String.to_integer/1) end
      [to_pair.(first), to_pair.(second)]
    end
  end

  defp count_pairs(input) do
    pairs_from(input)
    |> Enum.filter(fn [[i, j], [k, l]] -> (i >= k && j <= l) || (k >= i && l <= j) end)
    |> Enum.count()
  end

  defp count_overlap(input) do
    pairs_from(input)
    |> Enum.reject(fn [[i, j], [k, l]] -> MapSet.disjoint?(MapSet.new(i..j), MapSet.new(k..l)) end)
    |> Enum.count()
  end
end

Day4.test()
Day4.solve()
