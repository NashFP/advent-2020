defmodule Advent.Day4 do
  @required_fields ~w{byr iyr eyr hgt hcl ecl pid}

  defp passport_fields(passport_lines) do
    passport_lines
    |> String.split(["\n", " "], trim: true)
    |> Enum.map(&hd(String.split(&1, ":")))
  end

  defp valid_passport?(fields) do
    Enum.all?(@required_fields, &(&1 in fields))
  end

  def part_1(input) do
    input
    |> String.split("\n\n")
    |> Enum.map(&passport_fields/1)
    |> Enum.count(&valid_passport?/1)
  end
end
