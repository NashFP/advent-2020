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

  def part_2(input) do
    input
    |> String.split("\n\n", trim: true)
    |> Enum.map(&String.split(&1, "\n", trim: true))
    |> Enum.map(fn group ->
      Enum.map(group, fn answer ->
        answer
        |> String.codepoints()
        |> MapSet.new()
      end)
    end)
    |> Enum.map(fn mapsets ->
      Enum.reduce(mapsets, &MapSet.intersection/2)
    end)
    |> Enum.map(&Enum.count/1)
    |> Enum.sum()
  end
end
