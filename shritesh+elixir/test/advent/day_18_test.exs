defmodule Advent.Day18Test do
  use ExUnit.Case
  alias Advent.Day18

  @examples [
    {"1 + 2 * 3 + 4 * 5 + 6", 71},
    {"1 + (2 * 3) + (4 * (5 + 6))", 51},
    {"2 * 3 + (4 * 5)", 26},
    {"5 + (8 * 3 + 9 + 3 * 4 * 3)", 437},
    {"5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))", 12240},
    {"((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2", 13632}
  ]

  test "exec" do
    for {exp, result} <- @examples do
      assert Day18.exec(exp) == result
    end
  end

  test "part_1" do
    input = Enum.map(@examples, &elem(&1, 0)) |> Enum.join("\n")
    expectation = Enum.map(@examples, &elem(&1, 1)) |> Enum.sum()

    assert Day18.part_1(input) == expectation
  end
end
