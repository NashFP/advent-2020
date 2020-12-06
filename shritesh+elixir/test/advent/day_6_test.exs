defmodule Advent.Day6Test do
  use ExUnit.Case
  alias Advent.Day6

  @example """
  abc

  a
  b
  c

  ab
  ac

  a
  a
  a
  a

  b
  """

  test "part_1" do
    assert Day6.part_1(@example) == 11
  end
end
