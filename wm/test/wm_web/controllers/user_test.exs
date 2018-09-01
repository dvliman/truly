defmodule WmWeb.UserTest do
  use WmWeb.ConnCase

  test "[users/add] missing fields", %{conn: conn} do
    user = %{
      "name"  => "david",
      "email" => "david@david.com",
      "dob"   => "1988-01-01"}

    missing_name  = Map.delete(user, "name")
    missing_email = Map.delete(user, "email")
    missing_dob   = Map.delete(user, "dob")

    r1 = TestClient.user_add(conn, missing_name) |> json_response(200)
    r2 = TestClient.user_add(conn, missing_email) |> json_response(200)
    r3 = TestClient.user_add(conn, missing_dob) |> json_response(200)

    assert "error" == r1["status"]
    assert %{"name" => "can't be blank"} == r1["error"]["reason"]

    assert "error" == r2["status"]
    assert %{"email" => "can't be blank"} == r2["error"]["reason"]

    assert "error" == r2["status"]
    assert %{"dob" => "can't be blank"} == r3["error"]["reason"]
  end

  test "[users/add] invalid dob format", %{conn: conn} do
    user = %{
      "name"  => "david",
      "email" => "david@david.com",
      "dob"   => "1988"}

    response = TestClient.user_add(conn, user) |> json_response(200)

    assert "error" == response["status"]
    assert %{"dob" => "is invalid"} == response["error"]["reason"]
  end

  test "[users/add] invalid email format", %{conn: conn} do
    user = %{
      "name"  => "david",
      "email" => "david",
      "dob"   => "1988-01-01"}

    response = TestClient.user_add(conn, user) |> json_response(200)

    assert "error" == response["status"]
    assert %{"email" => "has invalid format"} == response["error"]["reason"]
  end

  test "[users/add] duplicate email", %{conn: conn} do
    user = %{
      "name"  => "david",
      "email" => "david@david.com",
      "dob"   => "1988-01-01"}

    _        = TestClient.user_add(conn, user) |> json_response(200)
    response = TestClient.user_add(conn, user) |> json_response(200)

    assert "error" == response["status"]
    assert %{"email" => "has already been taken"} == response["error"]["reason"]
  end

  test "[users/add] email case insensitive", %{conn: conn} do
    user = %{
      "name"  => "david",
      "dob"   => "1988-01-01"}

    version1 = Map.put(user, "email", "cASE@insensitive.com")
    version2 = Map.put(user, "email", "case@insensitive.com")

    _        = TestClient.user_add(conn, version1) |> json_response(200)
    response = TestClient.user_add(conn, version2) |> json_response(200)

    assert "error" == response["status"]
    assert %{"email" => "has already been taken"} == response["error"]["reason"]
  end

  test "[users/add] happy path", %{conn: conn} do
    user = %{
      "name"  => (name  = "david"),
      "email" => (email = "david@david.com"),
      "dob"   => (dob   = "1988-01-01")}

    response = TestClient.user_add(conn, user) |> json_response(200)

    assert "ok"  == response["status"]
    assert name  == response["user"]["name"]
    assert email == response["user"]["email"]
    assert dob   == response["user"]["dob"]

    id = response["user"]["id"]
    readBack = Wm.Repo.get(Wm.Schemas.User, id)
    assert readBack != nil
  end
end