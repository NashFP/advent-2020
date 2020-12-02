defmodule Advent.Day1 do
  def part_1(numbers) do
    [answer | _] = for x <- numbers, y <- numbers, x + y == 2020, do: x * y
    answer
  end

  def part_2(numbers) do
    [answer | _] = for x <- numbers, y <- numbers, z <- numbers, x + y + z == 2020, do: x * y * z
    answer
  end
end
