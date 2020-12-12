defmodule Advent.Day1 do
  def run(input_string) when is_binary(input_string) do
    input_string
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(fn x -> String.to_integer(x) end)
    |> run()
  end

  def run(input) when is_list(input) do
    input
    |> get_pairs()
    |> Enum.find(fn pair -> pair |> Enum.sum() == 2020 end)
    |> multiply()
  end

  defp get_pairs(input) do
    for x <- input, y <- input |> List.delete(x) do
      [x, y] |> Enum.sort()
    end
    |> Enum.uniq()
  end

  defp multiply([x, y]) do
    x * y
  end
end
