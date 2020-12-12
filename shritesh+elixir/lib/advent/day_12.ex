defmodule Advent.Day12 do
  defmodule Ship do
    defstruct [:x, :y, :rotation]

    def new do
      %__MODULE__{x: 0, y: 0, rotation: {1, 0}}
    end

    def action(%__MODULE__{x: x, y: y} = ship, {:move, {dx, dy}}) do
      %{ship | x: x + dx, y: y + dy}
    end

    def action(%__MODULE__{rotation: {rx, ry}} = ship, {:rotate, angle}) do
      rotation =
        case angle do
          90 -> {-ry, rx}
          180 -> {-rx, -ry}
          270 -> {ry, -rx}
        end

      %{ship | rotation: rotation}
    end

    def action(%__MODULE__{rotation: {rx, ry}} = ship, {:forward, units}) do
      action(ship, {:move, {rx * units, ry * units}})
    end

    def manhattan_distance(%__MODULE__{x: x, y: y}) do
      abs(x) + abs(y)
    end
  end

  defp input_to_directions(input) do
    for line <- String.split(input, "\n", trim: true) do
      {action, units} = String.split_at(line, 1)
      units = String.to_integer(units)

      case action do
        "N" -> {:move, {0, units}}
        "S" -> {:move, {0, -units}}
        "E" -> {:move, {units, 0}}
        "W" -> {:move, {-units, 0}}
        "L" -> {:rotate, units}
        "R" -> {:rotate, 360 - units}
        "F" -> {:forward, units}
      end
    end
  end

  def part_1(input) do
    input_to_directions(input)
    |> Enum.reduce(Ship.new(), &Ship.action(&2, &1))
    |> Ship.manhattan_distance()
  end
end
