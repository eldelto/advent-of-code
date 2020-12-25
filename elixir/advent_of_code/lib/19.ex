defmodule AdventOfCode.Nineteen do
  defmodule Rule do
    @enforce_keys [:number]
    defstruct @enforce_keys ++ [leftRules: [], rightRules: [], value: nil]
  end

  def find_matches(input) do
    [rawRules | rawWords] = String.split(input, "\n\n")
    regex = input_to_regex(rawRules)
    words = input_to_words(List.first(rawWords))

    Enum.map(words, &String.match?(&1, regex))
    |> Enum.filter(& &1)
    |> Enum.count()
  end

  def input_to_words(input) do
    String.split(input, "\n")
    |> Enum.filter(&(String.length(&1) > 0))
  end

  def input_to_regex(input) do
    rawRegex =
      input_to_rules(input)
      |> rule_to_regex()

    {:ok, regex} = Regex.compile("^" <> rawRegex <> "$")
    regex
  end

  def input_to_rules(input) do
    input_to_rule_map(input)
    |> substitute_rule("0", 0)
  end

  def substitute_rule(ruleMap, number, level) do
    {:ok, rule} = Map.fetch(ruleMap, number)
    substitute_sub_rules(ruleMap, rule, level)
  end

  def substitute_sub_rules(_ruleMap, %Rule{leftRules: [], value: value}, _level), do: value

  def substitute_sub_rules(
        ruleMap,
        rule = %Rule{leftRules: leftRules, rightRules: rightRules},
        level
      ) do
    rule = %{rule | leftRules: substitute_sub_rules(ruleMap, leftRules, level)}
    rule = %{rule | rightRules: substitute_sub_rules(ruleMap, rightRules, level)}
    rule
  end

  def substitute_sub_rules(_ruleMap, rules, 11) when is_list(rules) do
    []
  end

  def substitute_sub_rules(ruleMap, rules, level) when is_list(rules) do
    Enum.map(rules, &substitute_rule(ruleMap, &1, level + 1))
  end

  def input_to_rule_map(input) do
    String.split(input, "\n")
    |> Enum.filter(&(String.length(&1) > 0))
    |> Enum.map(&row_to_rule/1)
    |> Enum.reduce(%{}, fn r, acc -> Map.put(acc, r.number, r) end)
  end

  def row_to_rule(row) do
    [number | rules] = String.split(row, ":")
    number = String.trim(number)
    rules = List.first(rules)

    if String.contains?(rules, ["a", "b"]) do
      value =
        String.trim(rules)
        |> String.trim("\"")

      %Rule{number: number, value: value}
    else
      [leftRules | rightRules] =
        String.split(rules, "|")
        |> Enum.map(&String.split(&1, " "))

      leftRules = Enum.map(leftRules, &String.trim/1)
        |> Enum.filter(&(String.length(&1) > 0))

      rightRules =
        Enum.flat_map(rightRules, & &1)
        |> Enum.map(&String.trim/1)
        |> Enum.filter(&(String.length(&1) > 0))

      %Rule{number: number, leftRules: leftRules, rightRules: rightRules}
    end
  end

  def rule_to_regex(value) when is_bitstring(value), do: value

  def rule_to_regex(%Rule{leftRules: leftRules, rightRules: []}) do
    left =
      Enum.map(leftRules, &rule_to_regex/1)
      |> Enum.join()

    "(" <> left <> ")"
  end

  def rule_to_regex(%Rule{leftRules: leftRules, rightRules: rightRules}) do
    left =
      Enum.map(leftRules, &rule_to_regex/1)
      |> Enum.join()

    right =
      Enum.map(rightRules, &rule_to_regex/1)
      |> Enum.join()

    "(" <> left <> "|" <> right <> ")"
  end
end
