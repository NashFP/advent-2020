defmodule Advent.Day16 do
  @rule_regex ~r/.*: (\d+)-(\d+) or (\d+)-(\d+)/

  defp ticket_to_list(line), do: String.split(line, ",") |> Enum.map(&String.to_integer/1)

  def parse_input(input) do
    [rules, your_ticket, nearby_tickets] = String.split(input, "\n\n", trim: true)

    rules =
      for numbers <- Regex.scan(@rule_regex, rules, capture: :all_but_first) do
        [a, b, c, d] = Enum.map(numbers, &String.to_integer/1)
        {a..b, c..d}
      end

    ["your ticket:", your_ticket] = String.split(your_ticket, "\n", trim: true)
    your_ticket = ticket_to_list(your_ticket)

    ["nearby tickets:" | nearby_tickets] = String.split(nearby_tickets, "\n", trim: true)
    nearby_tickets = Enum.map(nearby_tickets, &ticket_to_list/1)

    {rules, your_ticket, nearby_tickets}
  end

  defp valid_ticket?(rules, n) do
    Enum.any?(rules, fn {range_1, range_2} -> n in range_1 || n in range_2 end)
  end

  def part_1(input) do
    {rules, _your, nearby} = parse_input(input)

    Enum.map(nearby, fn tickets ->
      Enum.find(tickets, &(!valid_ticket?(rules, &1)))
    end)
    |> Enum.filter(& &1)
    |> Enum.sum()
  end
end
