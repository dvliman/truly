defmodule Wm.Schemas.StateId do
  @derive {Poison.Encoder, except: [:__meta__]}
  use WmWeb, :model

  schema "state_ids" do
    belongs_to :user, Wm.Schemas.User, foreign_key: :user_id

    field :number, :integer
    field :state, :string
    field :image, :string
    field :expiration, :date

    timestamps()
  end

  @required_fields [:user_id, :state, :image, :expiration]

  def changeset(module, params) do
    module
    |> cast(params, @required_fields)
    |> validate_required(@required_fields)
    |> foreign_key_constraint(:user_id)
    |> validate_states
  end

  @states ["AL", "AK", "AZ", "AR", "CA", "CO", "CT"] ++
      ["DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA"] ++
      ["KS", "KS", "KY", "LA", "ME", "MD", "MA", "MI"] ++
      ["MN", "MO", "MT", "NE", "NV", "NH", "NJ", "NM"] ++
      ["NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI"] ++
      ["SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA"] ++
      ["WV", "WI", "WY"]

  defp validate_states(changeset) do
    state = get_field(changeset, :state)

    case Enum.member?(state, @states) do
      true -> changeset
      false -> add_error(changeset, :state, "not valid state abbrv")
    end
  end

  def add(params) do
    %__MODULE__{}
    |> changeset(params)
    |> Repo.insert
  end

  def fetch(params) do
    Repo.get_by(Wm.Schemas.StateId, params)
  end

  def delete(params) do
    Repo.delete(Wm.Schemas.StateId, params)
  end
end