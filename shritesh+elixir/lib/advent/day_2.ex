defmodule Advent.Day2 do
  @input_regex ~r/(?<min>\d+)-(?<max>\d+) (?<char>\S): (?<password>.+)/
  def part_1(passwords) do
    passwords
    |> parse()
    |> Enum.count(&validate_1?/1)
  end

  def part_2(passwords) do
    passwords
    |> parse()
    |> Enum.count(&validate_2?/1)
  end

  defp validate_1?(%{min: min, max: max, char: char, password: password}) do
    count =
      password
      |> String.codepoints()
      |> Enum.count(&(&1 == char))

    count in min..max
  end

  defp validate_2?(%{min: min, max: max, char: char, password: password}) do
    codepoints = String.codepoints(password)

    case {Enum.at(codepoints, min - 1), Enum.at(codepoints, max - 1)} do
      {^char, ^char} -> false
      {^char, _} -> true
      {_, ^char} -> true
      {_, _} -> false
    end
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
