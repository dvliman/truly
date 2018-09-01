use Mix.Config

config :wm,
  ecto_repos: [Wm.Repo]

config :wm, WmWeb.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "TE58d2Pgr727iNpZD7QjzVL2ehA0a1t+VjxqfPVj/tl/RW/Wd0oeurNsuf4ysQyv",
  render_errors: [view: WmWeb.ErrorView, accepts: ~w(html json)],
  pubsub: [name: Wm.PubSub,
           adapter: Phoenix.PubSub.PG2]

config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:user_id]

import_config "#{Mix.env}.exs"
