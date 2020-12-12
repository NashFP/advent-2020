defmodule Advent.Day1Test do
  use ExUnit.Case
  alias Advent.Day1

  test "solves day 1 sample" do
    input = """
    1721
    979
    366
    299
    675
    1456
    """

    assert 514_579 == Day1.run(input)
  end
end
