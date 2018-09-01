defmodule Wm.Repo.Migrations.Medrecs do
  use Ecto.Migration

  def change do
    create table(:medrecs) do
      add :user_id, references(:users, on_delete: :delete_all), null: false

      add :number, :bigint, null: false
      add :issuer, :string, null: false
      add :image, :string, null: false
      add :expiration, :date, null: false

      timestamps()
    end

    create index(:medrecs, [:user_id])
  end
end
