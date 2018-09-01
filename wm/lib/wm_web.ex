defmodule WmWeb do

  def model do
    quote do

      use Ecto.Schema
      import Ecto.Query
      import Ecto.Changeset

      alias Wm.Repo
    end
  end

  def controller do
    quote do
      use Phoenix.Controller, namespace: WmWeb
      import Plug.Conn
      import WmWeb.Router.Helpers
      import WmWeb.Gettext
    end
  end

  def view do
    quote do
      use Phoenix.View, root: "lib/wm_web/templates",
                        namespace: WmWeb

      import Phoenix.Controller, only: [get_flash: 2, view_module: 1]

      use Phoenix.HTML

      import WmWeb.Router.Helpers
      import WmWeb.ErrorHelpers
      import WmWeb.Gettext
    end
  end

  def router do
    quote do
      use Phoenix.Router
      import Plug.Conn
      import Phoenix.Controller
    end
  end

  def channel do
    quote do
      use Phoenix.Channel
      import WmWeb.Gettext
    end
  end

  defmacro __using__(which) when is_atom(which) do
    apply(__MODULE__, which, [])
  end
end
