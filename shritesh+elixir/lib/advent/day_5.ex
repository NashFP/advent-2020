defmodule Advent.Day5 do
  defp upper_half({min, max}), do: {div(max + min, 2) + 1, max}

  defp lower_half({min, max}), do: {min, div(max + min, 2)}

  def parse_seat(pass) do
    {{row, row}, {column, column}} =
      pass
      |> String.codepoints()
      |> Enum.reduce(
        {{0, 127}, {0, 7}},
        fn
          "F", {row, column} -> {lower_half(row), column}
          "B", {row, column} -> {upper_half(row), column}
          "R", {row, column} -> {row, upper_half(column)}
          "L", {row, column} -> {row, lower_half(column)}
        end
      )

    {row, column, row * 8 + column}
  end

  def part_1(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(&parse_seat/1)
    |> Enum.map(fn {_row, _col, seat_number} -> seat_number end)
    |> Enum.max()
  end

  def part_2(input) do
    seat_set =
      input
      |> String.split("\n", trim: true)
      |> Enum.map(&parse_seat/1)
      |> Enum.map(fn {_row, _col, seat_number} -> seat_number end)
      |> MapSet.new()

    {min, max} = Enum.min_max(seat_set)

    Enum.filter(min..max, &(not MapSet.member?(seat_set, &1)))
  end
end
