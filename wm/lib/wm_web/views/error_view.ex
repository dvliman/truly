defmodule WmWeb.ErrorView do
  use WmWeb, :view

  def template_not_found(template, _assigns) do
    Phoenix.Controller.status_message_from_template(template)
  end
end
