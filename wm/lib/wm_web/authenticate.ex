defmodule WmWeb.Authenticate do
  import Plug.Conn
  import Phoenix.Controller, only: [json: 2]
  use WmWeb.ApiHelper

  def init(opts), do: opts

  def call(conn, _opts) do
    case get_req_header(conn, "authorization") do
      [] ->
        conn
        |> error(:INVALID_AUTH, "bearer <token> required")
        |> halt

      [authorization| _] ->
        token =
          authorization
          |> String.replace("Bearer ", "")

        case Wm.AccessToken.verify(token) do
          {:error, _} ->
            conn
            |> error(:INVALID_AUTH, "invalid bearer <token>")
            |> halt

          {:ok, user_id} ->
            conn
            |> assign(:user_id, user_id)
        end
    end
  end
end