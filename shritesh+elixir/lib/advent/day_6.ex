defmodule Advent.Day6 do
  def part_1(input) do
    for group <- String.split(input, "\n\n", trim: true) do
      String.replace(group, "\n", "")
      |> String.codepoints()
      |> MapSet.new()
      |> Enum.count()
    end
    |> Enum.sum()
  end

  def part_2(input) do
    for group <- String.split(input, "\n\n", trim: true) do
      for line <- String.split(group, "\n", trim: true) do
        String.codepoints(line)
        |> MapSet.new()
      end
      |> Enum.reduce(&MapSet.intersection/2)
      |> Enum.count()
    end
    |> Enum.sum()
  end
end
