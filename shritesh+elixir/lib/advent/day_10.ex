defmodule Advent.Day10 do
  defp input_to_numbers(input) do
    String.split(input, "\n", trim: true)
    |> Enum.map(&String.to_integer/1)
  end

  def part_1(input) do
    numbers = input_to_numbers(input)

    device_jolt = Enum.max(numbers) + 3

    %{1 => ones, 3 => threes} =
      [0, device_jolt | numbers]
      |> Enum.sort()
      |> Enum.chunk_every(2, 1, :discard)
      |> Enum.frequencies_by(fn [x, y] -> y - x end)

    ones * threes
  end
end
