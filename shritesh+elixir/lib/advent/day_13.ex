defmodule Advent.Day13 do
  defp process_input(input) do
    [line_1, line_2] = String.split(input, "\n", trim: true)

    earliest = String.to_integer(line_1)

    bus_ids =
      for number <- String.split(line_2, ","), number != "x", do: String.to_integer(number)

    {earliest, bus_ids}
  end

  defp bus_at(bus_ids, tick) do
    Enum.find(bus_ids, &(rem(tick, &1) == 0))
  end

  defp bus_stream(bus_ids) do
    Stream.iterate(1, &(&1 + 1))
    |> Stream.flat_map(
      &case bus_at(bus_ids, &1) do
        nil -> []
        bus_id -> [{&1, bus_id}]
      end
    )
  end

  def part_1(input) do
    {earliest, bus_ids} = process_input(input)

    Enum.find_value(bus_stream(bus_ids), fn {tick, bus_id} ->
      if tick > earliest do
        (tick - earliest) * bus_id
      end
    end)
  end
end
