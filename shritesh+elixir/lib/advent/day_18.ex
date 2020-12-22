defmodule Advent.Day18 do
  defp tokenize(input) do
    input
    |> String.replace("(", "( ")
    |> String.replace(")", " )")
    |> String.split(" ", trim: true)
    |> Enum.map(fn
      "+" -> :add
      "*" -> :mul
      "(" -> :open
      ")" -> :close
      n -> String.to_integer(n)
    end)
  end

  defp parse([:open | rest], acc) do
    {parens, rest} = parse(rest, [])
    parse(rest, [parens | acc])
  end

  defp parse([:close | rest], acc), do: {Enum.reverse(acc), rest}
  defp parse([n | rest], acc), do: parse(rest, [n | acc])
  defp parse([], acc), do: Enum.reverse(acc)

  defp eval([n]), do: n
  defp eval([a, :add, b | rest]), do: eval([eval(a) + eval(b) | rest])
  defp eval([a, :mul, b | rest]), do: eval([eval(a) * eval(b) | rest])
  defp eval(n), do: n

  def exec(input) do
    input
    |> tokenize()
    |> parse([])
    |> eval()
  end

  def part_1(lines) do
    String.split(lines, "\n", trim: true)
    |> Enum.map(&exec/1)
    |> Enum.sum()
  end
end
