defmodule Advent.Day7Test do
  use ExUnit.Case
  alias Advent.Day7

  @example """
  light red bags contain 1 bright white bag, 2 muted yellow bags.
  dark orange bags contain 3 bright white bags, 4 muted yellow bags.
  bright white bags contain 1 shiny gold bag.
  muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
  shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
  dark olive bags contain 3 faded blue bags, 4 dotted black bags.
  vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
  faded blue bags contain no other bags.
  dotted black bags contain no other bags.
  """

  test "parse_rules" do
    expectation = %{
      "light red" => %{"bright white" => 1, "muted yellow" => 2},
      "dark orange" => %{"bright white" => 3, "muted yellow" => 4},
      "bright white" => %{"shiny gold" => 1},
      "muted yellow" => %{"shiny gold" => 2, "faded blue" => 9},
      "shiny gold" => %{"dark olive" => 1, "vibrant plum" => 2},
      "dark olive" => %{"faded blue" => 3, "dotted black" => 4},
      "vibrant plum" => %{"faded blue" => 5, "dotted black" => 6},
      "faded blue" => %{},
      "dotted black" => %{}
    }

    assert Day7.parse_rules(@example) == expectation
  end

  test "part_1" do
    assert Day7.part_1(@example) == 4
  end
end
