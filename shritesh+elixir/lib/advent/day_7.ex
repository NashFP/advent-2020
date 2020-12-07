defmodule Advent.Day7 do
  @rule_regex ~r/(?<color>[\w ]*) bags contain (?<contents>.*)\./
  @contents_regex ~r/((?<none>no other)|(?<count>\d)+ (?<color>[a-z ]+)) bags?/

  def parse_rules(input) do
    for sentence <- String.split(input, "\n", trim: true), into: %{} do
      %{"color" => color, "contents" => contents} = Regex.named_captures(@rule_regex, sentence)

      bags =
        for [none, count, color] <-
              Regex.scan(@contents_regex, contents, capture: ["none", "count", "color"]),
            none != "no other",
            into: %{},
            do: {color, String.to_integer(count)}

      {color, bags}
    end
  end

  defp does_hold(rules, container_color, bag_color) do
    holding_colors = Map.keys(rules[container_color])

    bag_color in holding_colors || Enum.any?(holding_colors, &does_hold(rules, &1, bag_color))
  end

  def part_1(input, bag_color \\ "shiny gold") do
    rules = parse_rules(input)

    Map.keys(rules)
    |> Enum.filter(&(&1 != bag_color))
    |> Enum.count(&does_hold(rules, &1, bag_color))
  end
end
