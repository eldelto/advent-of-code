defmodule AdventOfCode do
  @moduledoc """
  Documentation for `AdventOfCode`.
  """

  defp mul(instruction) do
    with [_, a, b] <- Regex.run(~r/^\((\d+),(\d+)\)/, instruction),
         {x, _} <- Integer.parse(a),
         {y, _} <- Integer.parse(b) do
      x * y
    else
      _ -> 0
    end
  end

  def day3_part1 do
    {:ok, memory} = File.read("inputs/03.txt")

    String.split(memory, "mul")
    |> Stream.map(&mul/1)
    |> Enum.sum()
  end

  def day3_part2 do
    {:ok, memory} = File.read("inputs/03.txt")

    memory =
      String.split("do()" <> memory, "don't()")
      |> Stream.map(fn x -> String.split(x, "do()") end)
      |> Stream.flat_map(fn x ->
        case x do
          [_ | instructions] -> instructions
          _ -> []
        end
      end)
      |> Enum.join()

    String.split(memory, "mul")
    |> Stream.map(&mul/1)
    |> Enum.sum()
  end
end
