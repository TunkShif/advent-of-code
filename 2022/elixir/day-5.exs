defmodule Day5 do
  @input File.read!("./input-5.txt")
  @sample ~S"""
      [D]    
  [N] [C]    
  [Z] [M] [P]
  1   2   3 

  move 1 from 2 to 1
  move 3 from 1 to 3
  move 2 from 2 to 1
  move 1 from 1 to 2
  """

  def test() do
    @sample
    |> String.trim_trailing()
    |> String.split("\n")
    |> Enum.take(3)
    |> parse_stacks()
    |> IO.inspect()
  end

  def solve() do
  end

  defp parse_stacks(input) do
    for line <- input do
      Regex.named_captures(~r/(\s{3}\s?)|(\[(?<val>[A-Z])\]\s?)/, String.trim_trailing(line),
        capture: :list
      )
    end
  end

  defp parse_moves(input) do
  end
end

Day5.test()
Day5.solve()
