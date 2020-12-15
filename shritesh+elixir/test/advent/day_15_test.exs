defmodule Advent.Day15Test do
  use ExUnit.Case
  alias Advent.Day15

  test "part_1" do
    [
      {[0, 3, 6], 436},
      {[1, 3, 2], 1},
      {[2, 1, 3], 10},
      {[1, 2, 3], 27},
      {[2, 3, 1], 78},
      {[3, 2, 1], 438},
      {[3, 1, 2], 1836}
    ]
    |> Enum.each(fn {input, expectation} ->
      assert Day15.part_1(input) == expectation
    end)
  end
end
