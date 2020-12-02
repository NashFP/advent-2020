defmodule Advent.Day2 do
  @input_regex ~r/(?<min>\d+)-(?<max>\d+) (?<char>\S): (?<password>.+)/
  def part_1(passwords) do
    passwords
    |> parse()
    |> Enum.count(&validate?/1)
  end

  defp validate?(%{min: min, max: max, char: char, password: password}) do
    count =
      password
      |> String.codepoints()
      |> Enum.count(&(&1 == char))

    count in min..max
  end

  defp parse(input) do
    input
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(fn line ->
      %{"min" => min, "max" => max, "char" => char, "password" => password} =
        Regex.named_captures(@input_regex, line)

      %{min: String.to_integer(min), max: String.to_integer(max), char: char, password: password}
    end)
  end
end
