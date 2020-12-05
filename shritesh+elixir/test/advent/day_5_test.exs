defmodule Advent.Day5Test do
  use ExUnit.Case
  alias Advent.Day5

  @passes %{
    "FBFBBFFRLR" => {44, 5, 357},
    "BFFFBBFRRR" => {70, 7, 567},
    "FFFBBBFRRR" => {14, 7, 119},
    "BBFFBBFRLL" => {102, 4, 820}
  }

  test "parse_seat" do
    @passes
    |> Enum.each(fn {pass, result} ->
      assert Day5.parse_seat(pass) == result
    end)
  end

  test "part_1" do
    assert 820 ==
             @passes
             |> Map.keys()
             |> Enum.join("\n")
             |> Day5.part_1()
  end
end
