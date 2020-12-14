defmodule Advent.Day14Test do
  use ExUnit.Case
  alias Advent.Day14

  @example """
  mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
  mem[8] = 11
  mem[7] = 101
  mem[8] = 0
  """

  test "part_1" do
    assert Day14.part_1(@example) == 165
  end
end
