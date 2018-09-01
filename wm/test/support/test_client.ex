defmodule TestClient do
  use Phoenix.ConnTest
  @endpoint WmWeb.Endpoint

  def user_add(conn, params) do
    conn |> http_post("/api/users/add", params)
  end

  def medrec_add(conn, params, access_token) do
    conn |> http_post("/api/medrecs/add", params, access_token)
  end

  def medrec_fetch(conn, id, access_token) do
    conn |> http_post("api/medrecs/fetch", %{"id" => id}, access_token)
  end

  def medrec_delete(conn, id, access_token) do
    conn |> http_post("/api/medrecs/delete", %{"id" => id}, access_token)
  end

  # note: this call has side-effect if run within same test suite
  # use faker library to randomize
  def user_created(conn) do
    user = %{
      "name"  => "david",
      "email" => "david@david.com",
      "dob"   => "1988-01-01"}

    TestClient.user_add(conn, user) |> json_response(200)
  end

  def http_post(conn, endpoint, params) when is_map(params) do
    conn
    |> recycle
    |> post(endpoint, params)
  end

  def http_post(conn, endpoint, params, access_token) when is_map(params) do
    conn
    |> recycle
    |> auth(access_token)
    |> post(endpoint, params)
  end

  def auth(conn, access_token) do
    conn
    |> put_req_header("authorization", "Bearer #{access_token}")
  end
end

