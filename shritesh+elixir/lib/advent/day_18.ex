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

  defp eval(tokens) do
    eval(tokens, nil, nil, [])
  end

  # tokens, op, acc, stack
  defp eval([], nil, acc, []) do
    acc
  end

  defp eval([:open | rest], op, acc, stack) do
    eval(rest, nil, nil, [{op, acc} | stack])
  end

  defp eval([:close | rest], nil, acc, [{nil, nil} | stack]) do
    eval(rest, nil, acc, stack)
  end

  defp eval([:close | rest], nil, acc, [{:add, old_acc} | stack]) do
    eval(rest, nil, old_acc + acc, stack)
  end

  defp eval([:close | rest], nil, acc, [{:mul, old_acc} | stack]) do
    eval(rest, nil, old_acc * acc, stack)
  end

  defp eval([:add | rest], nil, acc, stack) do
    eval(rest, :add, acc, stack)
  end

  defp eval([:mul | rest], nil, acc, stack) do
    eval(rest, :mul, acc, stack)
  end

  defp eval([n | rest], :add, acc, stack) do
    eval(rest, nil, acc + n, stack)
  end

  defp eval([n | rest], :mul, acc, stack) do
    eval(rest, nil, acc * n, stack)
  end

  defp eval([n | rest], nil, nil, stack) do
    eval(rest, nil, n, stack)
  end

  def exec(input) do
    input
    |> tokenize()
    |> eval()
  end

  def part_1(lines) do
    String.split(lines, "\n", trim: true)
    |> Enum.map(&exec/1)
    |> Enum.sum()
  end
end
