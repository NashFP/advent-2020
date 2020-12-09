defmodule Advent.Day9 do
  defp input_to_numbers(input) do
    String.split(input, "\n", trim: true)
    |> Enum.map(&String.to_integer/1)
  end

  def valid_chunk?(input) do
    [sum | numbers] = Enum.reverse(input)

    Stream.flat_map(numbers, fn x -> Stream.map(numbers, fn y -> {x, y} end) end)
    |> Stream.filter(fn {x, y} -> x != y end)
    |> Enum.any?(fn {x, y} -> x + y == sum end)
  end

  def part_1(input, preamble_length \\ 25) do
    input_to_numbers(input)
    |> Enum.chunk_every(preamble_length + 1, 1, :discard)
    |> Enum.find(&(!valid_chunk?(&1)))
    |> List.last()
  end
end
