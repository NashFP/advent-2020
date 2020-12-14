defmodule Advent.Day14 do
  use Bitwise

  defp input_to_assignments(input) do
    for line <- String.split(input, "\n", trim: true) do
      [location, value] = String.split(line, " = ")

      case location do
        "mask" ->
          zero_mask = String.replace(value, ["X"], "1") |> String.to_integer(2)
          one_mask = String.replace(value, ["X"], "0") |> String.to_integer(2)
          {:mask, {zero_mask, one_mask}}

        "mem[" <> address ->
          {address, "]"} = Integer.parse(address)
          value = String.to_integer(value)
          {address, value}
      end
    end
  end

  def part_1(input) do
    Enum.reduce(input_to_assignments(input), {%{}, {0, 0}}, fn
      {:mask, new_mask}, {memory, _mask} ->
        {memory, new_mask}

      {address, value}, {memory, {zero_mask, one_mask} = mask} ->
        masked_value =
          value
          |> band(zero_mask)
          |> bor(one_mask)

        new_memory = Map.put(memory, address, masked_value)

        {new_memory, mask}
    end)
    |> elem(0)
    |> Enum.map(&elem(&1, 1))
    |> Enum.sum()
  end
end
