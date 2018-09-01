defmodule WmWeb.MedrecController do
  use WmWeb, :controller
  use WmWeb.ApiHelper

  def add(conn, params) do
    caller = conn.assigns[:user_id]
    attrs  = Map.put(params, "user_id", caller)

    case Wm.Schemas.Medrec.add(attrs) do
      {:ok, medrec} ->
        reply = %{:medrec => to_map(medrec)}
        success(conn, reply)

      {:error, changeset} ->
        error(conn, :BAD_REQUEST, Utils.changeset_errors(changeset))
    end
  end

  def fetch(conn, params) do
    args = Utils.keywordize_keys(params) # %{[id|number] => [id|number]}

    try do
      case Wm.Schemas.Medrec.fetch(args) do
        nil ->
          error(conn, :BAD_REQUEST, :not_found)

        medrec ->
          reply = %{:medrec => maybe_expired(to_map(medrec))}
          success(conn, reply)
      end
    rescue
      e in [Ecto.Query.CastError, Ecto.Query.QueryError]->
        error(conn, :BAD_REQUEST, :invalid_query)
    end
  end

  # user association is not loaded
  # only extract medical recommendation fields as necessary
  defp to_map(%Wm.Schemas.Medrec{} = card) do
    card |> Map.take([:id, :user_id, :number, :issuer, :image, :expiration])
  end

  defp maybe_expired(%{:expiration => expiration} = medrec) do
    expired = Date.compare(expiration, Date.utc_today()) != :gt
    format_expired(expired, medrec)
  end

  defp format_expired(true, medrec), do: "expired"
  defp format_expired(false, medrec), do: medrec

  def delete(conn, params) do
    caller = conn.assigns[:user_id]

    # check: can delete?
    attrs = params
#    attrs  = Map.put(params, "user_id", caller)

    case Wm.Schemas.Medrec.delete(attrs) do
      {:ok, _} ->
        success(conn, %{"medrecs/delete" => "ok"})

      {:error, changeset} ->
        error(conn, :BAD_REQUEST, Utils.changeset_errors(changeset))
    end
  end
end