defmodule WmWeb.UserController do
  use WmWeb, :controller
  use WmWeb.ApiHelper
  alias Wm.Schemas
  alias Wm.AccessToken

  def add(conn, params) do
    case Schemas.User.add(params) do
      {:ok, user} ->
        reply = %{
          :user => user,
          :token => AccessToken.sign(user)
        }
        success(conn, reply)

      {:error, changeset} ->
        error(conn, :BAD_REQUEST, Utils.changeset_errors(changeset))
    end
  end
end