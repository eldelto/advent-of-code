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

  def valid_print?([_page], _ruleDict) do
    true
  end

  def valid_print?([page | pagesBefore], ruleDict) do
    pagesAfter = Map.get(ruleDict, page, [])

    case Enum.find(pagesBefore, fn pageBefore ->
           Enum.find(pagesAfter, &(&1 == pageBefore)) != nil
         end) do
      nil -> valid_print?(pagesBefore, ruleDict)
      _ -> false
    end
  end

  def weight_pages(ruleDict, pageSet, weights \\ %{}) do
    newWeights =
      Enum.reduce(ruleDict, weights, fn {page, pagesAfter}, acc ->
        if Enum.find(pageSet, &(&1 == page)) != nil do
          weight = Map.get(acc, page, 1000.0)

          Enum.reduce(pagesAfter, acc, fn pageAfter, acc ->
            Map.update(acc, pageAfter, 1000.0, fn oldWeight -> min(oldWeight, weight - 1) end)
          end)
        else
          acc
        end
      end)

    if weights == newWeights do
      weights
    else
      weight_pages(ruleDict, pageSet, newWeights)
    end
  end

  def sort_print(print, weights) do
    weighted_print =
      Enum.map(print, fn page ->
        weight = Map.get(weights, page, 1000.0)
        {page, weight}
      end)

    Enum.sort_by(weighted_print, fn {_, weight} -> -weight end)
    |> Enum.map(fn {page, _} -> page end)
  end

  def get_middle_page(print) do
    middle = ceil(length(print) / 2 - 1)
    {value, _} = Integer.parse(Enum.at(print, middle))
    value
  end

  def parse_prints_and_rules(input) do
    [rawRules, rawPrints] = String.split(input, "\n\n")

    ruleDict =
      String.split(rawRules, "\n")
      |> Enum.reduce(%{}, fn rule, acc ->
        [key, value] = String.split(rule, "|")
        Map.update(acc, key, [value], fn old -> [value | old] end)
      end)

    prints =
      String.split(rawPrints, "\n")
      |> Stream.filter(&(&1 != ""))
      |> Stream.map(fn line -> String.split(line, ",") end)
      |> Enum.to_list()

    {ruleDict, prints}
  end

  def day5_part1 do
    {:ok, input} = File.read("inputs/05.txt")
    {ruleDict, prints} = parse_prints_and_rules(input)

    Enum.filter(prints, fn print -> valid_print?(Enum.reverse(print), ruleDict) end)
    |> Enum.map(&get_middle_page/1)
    |> Enum.sum()
  end

  def day5_part2 do
    {:ok, input} = File.read("inputs/05.txt")
    {ruleDict, prints} = parse_prints_and_rules(input)

    Enum.filter(prints, fn print ->
      !valid_print?(Enum.reverse(print), ruleDict)
    end)
    |> Enum.map(fn print -> sort_print(print, weight_pages(ruleDict, print)) end)
    |> Enum.map(&get_middle_page/1)
    |> Enum.sum()
  end

  def concat_numbers(a, b) do
	{number, _} = Integer.parse("#{a}#{b}")
	number
  end

  def valid_equation?(expected, [number | tail]) do
	valid_equation?(expected, tail, number)
	end

  def valid_equation?(expected, [], acc) do
	expected == acc
  end

  def valid_equation?(expected, [number | tail], acc) do
	if acc > expected do
	  false
	else
	  valid_equation?(expected, tail, acc + number) or
		valid_equation?(expected, tail, acc * number)
	end
  end

  def day7_part1 do
	{:ok, input} = File.read("inputs/07.txt")

	equations = String.split(input, "\n")
	|> Enum.filter(&(&1 != ""))
	|> Enum.map(fn line ->
	  [sum, numbers] = String.split(line, ": ")
	  {sum, _} = Integer.parse(sum)

	  numbers = String.split(numbers, " ")
	  |> Enum.map(fn x ->
		{int, _ } = Integer.parse(x)
		int
	  end)

	  {sum, numbers}
	end)

	Stream.filter(equations, fn {expected, numbers} ->
	  valid_equation?(expected, numbers)
	  end)
	|> Stream.map(fn {expected, _} -> expected end)
	|> Enum.sum

	# Result: 1545311493300
  end
end
