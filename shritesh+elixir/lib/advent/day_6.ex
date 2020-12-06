defmodule Advent.Day6 do
  def part_1(input) do
    input
    |> String.split("\n\n", trim: true)
    |> Enum.map(&String.replace(&1, "\n", ""))
    |> Enum.map(&String.codepoints/1)
    |> Enum.map(&MapSet.new/1)
    |> Enum.map(&Enum.count/1)
    |> Enum.sum()
  end
end
