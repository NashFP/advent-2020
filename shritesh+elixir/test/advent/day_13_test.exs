defmodule Advent.Day13Test do
  use ExUnit.Case
  alias Advent.Day13

  @example """
  939
  7,13,x,x,59,x,31,19
  """

  test "part_1" do
    assert Day13.part_1(@example) == 295
  end
end
