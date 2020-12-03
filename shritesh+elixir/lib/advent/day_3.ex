defmodule Advent.Day3 do
  defmodule TobogganMap do
    defstruct [:rows, :columns, :map]

    def new(input) do
      lines =
        input
        |> String.trim()
        |> String.split("\n")

      map =
        for {line, row} <- Enum.with_index(lines),
            {char, col} <- Enum.with_index(String.codepoints(line)),
            into: %{} do
          content =
            case char do
              "." -> :open
              "#" -> :tree
            end

          {{row, col}, content}
        end

      %__MODULE__{
        rows: Enum.count(lines),
        columns: hd(lines) |> String.length(),
        map: map
      }
    end

    def at(%__MODULE__{rows: rows, columns: columns, map: map}, row, col)
        when row in 0..(rows - 1) and col in 0..(columns - 1) do
      Map.get(map, {row, col})
    end

    def at(%__MODULE__{rows: rows, columns: columns} = tm, row, col) do
      at(tm, rem(row, rows), rem(col, columns))
    end
  end

  def part_1(map_input) do
    tm = TobogganMap.new(map_input)

    {_col, encounters} =
      Enum.reduce(0..(tm.rows - 1), {0, []}, fn row, {col, encounters} ->
        {col + 3, [TobogganMap.at(tm, row, col) | encounters]}
      end)

    Enum.count(encounters, &(&1 == :tree))
  end
end
