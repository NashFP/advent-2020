defmodule Advent.Day5 do
  def seat_number(pass) do
    pass
    |> String.replace(["F", "L"], "0")
    |> String.replace(["B", "R"], "1")
    |> String.to_integer(2)
  end

  def part_1(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(&seat_number/1)
    |> Enum.max()
  end

  def part_2(input) do
    seat_set =
      input
      |> String.split("\n", trim: true)
      |> Enum.map(&seat_number/1)
      |> MapSet.new()

    {min, max} = Enum.min_max(seat_set)
    Enum.filter(min..max, &(not MapSet.member?(seat_set, &1)))
  end
end
