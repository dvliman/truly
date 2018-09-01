defmodule WmWeb.ApiHelper do

  defmacro __using__(_opts) do
    quote do
      def error(conn, code, msg) do
        json(conn, %{:status => "error",
                     :error => %{:code => code, :reason => msg}})
      end

      def success(conn, response) do
        json(conn, Map.merge(%{:status => "ok"}, response))
      end
    end
  end
end