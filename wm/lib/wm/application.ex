defmodule Wm.Application do
  use Application

  def start(_type, _args) do
    import Supervisor.Spec

    children = [
      supervisor(Wm.Repo, []),
      supervisor(WmWeb.Endpoint, []),
    ]

    opts = [strategy: :one_for_one, name: Wm.Supervisor]
    Supervisor.start_link(children, opts)
  end

  def config_change(changed, _new, removed) do
    WmWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
