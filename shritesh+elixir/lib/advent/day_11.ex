defmodule Advent.Day11 do
  defmodule Grid do
    defstruct [:rows, :cols, :map]

    @floor "."
    @empty "L"
    @occupied "#"

    def new(input) do
      lines = String.split(input, "\n", trim: true)

      map =
        for {line, row} <- Enum.with_index(lines),
            {char, col} <- Enum.with_index(String.codepoints(line)),
            into: %{},
            do: {{row, col}, char}

      %__MODULE__{
        rows: Enum.count(lines),
        cols: String.length(hd(lines)),
        map: map
      }
    end

    def count_total_occupied(%__MODULE__{map: map}) do
      Enum.count(map, fn {_coords, seat} -> seat == @occupied end)
    end

    defp count_adjacent_occupied(%__MODULE__{map: map}, {row, col}) do
      [
        {row - 1, col - 1},
        {row - 1, col},
        {row - 1, col + 1},
        {row, col - 1},
        {row, col + 1},
        {row + 1, col - 1},
        {row + 1, col},
        {row + 1, col + 1}
      ]
      |> Enum.count(&(map[&1] == @occupied))
    end

    defp next_at(%__MODULE__{map: map} = grid, coords) do
      case map[coords] do
        @floor -> @floor
        @empty -> if count_adjacent_occupied(grid, coords) == 0, do: @occupied, else: @empty
        @occupied -> if count_adjacent_occupied(grid, coords) >= 4, do: @empty, else: @occupied
      end
    end

    def next(%__MODULE__{map: map} = grid) do
      new_map = for coords <- Map.keys(map), into: %{}, do: {coords, next_at(grid, coords)}
      %{grid | map: new_map}
    end
  end

  def part_1(input) do
    input
    |> Grid.new()
    |> Stream.iterate(&Grid.next/1)
    |> Stream.chunk_every(2, 1)
    |> Enum.find_value(fn [prev, current] -> if prev == current, do: current end)
    |> Grid.count_total_occupied()
  end
end
