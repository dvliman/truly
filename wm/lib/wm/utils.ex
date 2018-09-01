defmodule Utils do

  def format_changeset_errors(errors) do
    errors
    |> Enum.map(fn {k, {m, _}} -> {k, m} end)
    |> Map.new
  end

  def changeset_errors(changeset) do
    changeset.errors
    |> format_changeset_errors
  end

  def keywordize_keys(map) do
    Enum.reduce(map, %{}, fn ({k, v}, acc) ->
      Map.put(acc, String.to_atom(k), v)
    end)
  end
end