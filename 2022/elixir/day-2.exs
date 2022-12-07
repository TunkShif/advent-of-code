defmodule Day2 do
  @input File.read!("./input-2.txt")
  @sample ~S"""
  A Y
  B X
  C Z
  """

  def test() do
    15 = total(@sample)
    12 = total_new(@sample)
  end

  def solve() do
    total(@input) |> IO.puts()
    total_new(@input) |> IO.puts()
  end

  defp mapping("X"), do: :rock
  defp mapping("Y"), do: :paper
  defp mapping("Z"), do: :scissors
  defp mapping("A"), do: :rock
  defp mapping("B"), do: :paper
  defp mapping("C"), do: :scissors

  defp score(elf, me), do: judge(elf, me) + score(me)

  defp score(:rock), do: 1
  defp score(:paper), do: 2
  defp score(:scissors), do: 3

  defp judge(:rock, :paper), do: 6
  defp judge(:paper, :scissors), do: 6
  defp judge(:scissors, :rock), do: 6
  defp judge(same, same), do: 3
  defp judge(_, _), do: 0

  defp strategy_for(elf, "Y"), do: elf
  defp strategy_for(:rock, "X"), do: :scissors
  defp strategy_for(:paper, "X"), do: :rock
  defp strategy_for(:scissors, "X"), do: :paper
  defp strategy_for(:rock, "Z"), do: :paper
  defp strategy_for(:paper, "Z"), do: :scissors
  defp strategy_for(:scissors, "Z"), do: :rock

  defp rounds_from(input) do
    input
    |> String.trim_trailing()
    |> String.split("\n")
    |> Enum.map(&String.split(&1, " "))
  end

  defp total(input) do
    rounds_from(input)
    |> Enum.map(fn [elf, me] -> score(mapping(elf), mapping(me)) end)
    |> Enum.sum()
  end

  defp total_new(input) do
    rounds_from(input)
    |> Enum.map(fn [elf, strategy] ->
      score(mapping(elf), strategy_for(mapping(elf), strategy))
    end)
    |> Enum.sum()
  end
end

Day2.test()
Day2.solve()
