defmodule Wm.Schemas.User do
  @derive {Poison.Encoder, except: [:__meta__]}
  use WmWeb, :model

  schema "users" do
    field :email, :string
    field :name, :string
    field :dob, :date

    timestamps()
  end

  @required_fields ~w(email name dob)a
  @reserved_names  ~w(admin erlang elixir otp)

  def changeset(user, params) do
    user
    |> cast(params, @required_fields)
    |> validate_required(@required_fields)
    |> validate_format(:email, ~r/@/)
    |> unique_constraint(:email, name: :user_email_idx)
    |> validate_exclusion(:name, @reserved_names)
  end

  def add(params) do
    %__MODULE__{}
    |> changeset(params)
    |> Repo.insert
  end
end