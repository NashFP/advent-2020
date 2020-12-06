defmodule Advent.Day1 do
  def part_1(input) do
    numbers =
      String.split(input, "\n", trim: true)
      |> Enum.map(&String.to_integer/1)

    numbers
    |> Stream.flat_map(fn x -> Stream.map(numbers, fn y -> [x, y] end) end)
    |> Enum.find(&(Enum.sum(&1) == 2020))
    |> Enum.reduce(1, &*/2)
  end

  def part_2(input) do
    numbers =
      String.split(input, "\n", trim: true)
      |> Enum.map(&String.to_integer/1)

    Stream.flat_map(numbers, fn x ->
      Stream.flat_map(numbers, fn y ->
        Stream.map(numbers, fn z ->
          [x, y, z]
        end)
      end)
    end)
    |> Enum.find(&(Enum.sum(&1) == 2020))
    |> Enum.reduce(1, &*/2)
  end
end
