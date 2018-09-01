defmodule Wm.AccessToken do
  alias Wm.Schemas

  def secret_salt() do
    props = Application.get_env(:wm, WmWeb.Endpoint)
    Keyword.get(props, :secret_key_base)
  end

  def sign(%Schemas.User{} = user) do
    Phoenix.Token.sign(secret_salt(), secret_salt(), user.id)
  end

  def verify(token) do
    Phoenix.Token.verify(secret_salt(), secret_salt(), token, max_age: 86400)
  end
end