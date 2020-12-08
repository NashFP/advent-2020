defmodule Advent.Day8 do
  defp instruction_map(input) do
    String.split(input, "\n", trim: true)
    |> Enum.map(&String.split/1)
    |> Enum.with_index()
    |> Enum.into(%{}, fn {[instruction, offset], idx} ->
      {idx, {instruction, String.to_integer(offset)}}
    end)
  end

  defp execution_stream(instructions) do
    Stream.iterate(
      {0, 0},
      fn {idx, acc} ->
        case instructions[idx] do
          {"nop", _} -> {idx + 1, acc}
          {"jmp", offset} -> {idx + offset, acc}
          {"acc", offset} -> {idx + 1, acc + offset}
        end
      end
    )
  end

  def part_1(input) do
    instruction_map(input)
    |> execution_stream()
    |> Enum.reduce_while(MapSet.new(), fn {idx, acc}, executed_ids ->
      if MapSet.member?(executed_ids, idx),
        do: {:halt, acc},
        else: {:cont, MapSet.put(executed_ids, idx)}
    end)
  end
end
