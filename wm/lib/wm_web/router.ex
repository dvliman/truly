defmodule WmWeb.Router do
  use WmWeb, :router

  pipeline :api do
    plug :accepts, ["json"]
  end

  pipeline :authenticated do
    plug :accepts, ["json"]
    plug WmWeb.Authenticate
  end

  scope "/api", WmWeb do
    pipe_through :api

    post "/users/add", UserController, :add
    post "/users/all", UserController, :all
  end

  scope "/api", WmWeb do
    pipe_through :authenticated

    post "/medrecs/add", MedrecController, :add
    post "/medrecs/update", MedrecController, :update
    post "/medrecs/fetch", MedrecController, :fetch
    post "/medrecs/delete", MedrecController, :delete

    post "/stateids/add", StateIdController, :add
    post "/stateids/update", StateIdController, :update
    post "/stateids/fetch", StateIdController, :fetch
    post "/stateids/delete", StateIdController, :delete
  end
end
