defmodule WmWeb.MedrecTest do
  use WmWeb.ConnCase

  test "[medrecs/add] requires auth", %{conn: conn} do
    response = TestClient.medrec_add(conn, %{}, "bogus-access-token") |> json_response(200)

    assert "error" == response["status"]
    assert %{"code" => "INVALID_AUTH",
             "reason" => "invalid bearer <token>"} == response["error"]
  end

  test "[medrecs/add] happy path (add, get, delete)", %{conn: conn} do
    user_created = TestClient.user_created(conn)
    user  = user_created["user"]
    token = user_created["token"]

    medrec = %{
      "number" => 123456,
      "issuer" => "california state",
      "image" => "http://image.com",
      "expiration" => "2020-02-02"}

    r1 = TestClient.medrec_add(conn, medrec, token) |> json_response(200)
    assert "ok" = r1["status"]
    medrec_id   = r1["medrec"]["id"]

    # assert: after user add medical recommendation, user can fetch back
    r2 = TestClient.medrec_fetch(conn, medrec_id, token) |> json_response(200)
    assert "ok" = r2["status"]
    fetched_medrec = r2["medrec"]
    assert medrec == fetched_medrec |> Map.delete("id") |> Map.delete("user_id")

    # assert: after delete, fetch will be: not_found
    r3 = TestClient.medrec_delete(conn, medrec_id, token) |> json_response(200)
    assert "ok" = r3["status"]

    r4 = TestClient.medrec_fetch(conn, medrec_id, token) |> json_response(200)
    IO.inspect(r4)


  end

end