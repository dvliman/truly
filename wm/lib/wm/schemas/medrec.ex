defmodule Wm.Schemas.Medrec do
  @derive {Poison.Encoder, except: [:__meta__]}
  use WmWeb, :model

  schema "medrecs" do
    belongs_to :user, Wm.Schemas.User, foreign_key: :user_id

    field :number, :integer
    field :issuer, :string
    field :image, :string
    field :expiration, :date

    timestamps()
  end

  @required_fields [:user_id, :number, :issuer, :image, :expiration]

  def changeset(module, params) do
    module
    |> cast(params, @required_fields)
    |> validate_required(@required_fields)
    |> foreign_key_constraint(:user_id)
    |> validate_expiration_date
  end

  defp validate_expiration_date(changeset) do
    exp = get_field(changeset, :expiration)

    if Date.compare(exp, Date.utc_today()) != :gt do
      add_error(changeset, :expiration, "expiration date can not be in the past")
    else
      changeset
    end
  end

  def add(params) do
    %__MODULE__{}
    |> changeset(params)
    |> Repo.insert
  end

  def fetch(params) do
    Repo.get_by(Wm.Schemas.Medrec, params)
  end

  def delete(params) do
    Repo.delete(Wm.Schemas.Medrec, params)
  end
end