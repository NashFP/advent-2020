defmodule Advent.Day13 do
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
    [line_1, line_2] = String.split(input, "\n", trim: true)

    earliest = String.to_integer(line_1)

    bus_ids =
      for number <- String.split(line_2, ","), number != "x", do: String.to_integer(number)

    Enum.find_value(bus_stream(bus_ids), fn {tick, bus_id} ->
      if tick > earliest do
        (tick - earliest) * bus_id
      end
    end)
  end

  defp find_earliest_timestamp([{start, 0} | rest]) do
    Stream.iterate(1, &(&1 + 1))
    |> Enum.find_value(fn multiplier ->
      guess = multiplier * start

      if Enum.all?(rest, fn {n, offset} -> rem(guess + offset, n) == 0 end) do
        guess
      end
    end)
  end

  def part_2(input) do
    String.split(input, ",")
    |> Enum.with_index()
    |> Enum.filter(fn {n, _idx} -> n != "x" end)
    |> Enum.map(fn {n, idx} -> {String.to_integer(n), idx} end)
    |> find_earliest_timestamp()
  end
end
