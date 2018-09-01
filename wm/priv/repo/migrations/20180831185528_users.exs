defmodule Wm.Repo.Migrations.Users do
  use Ecto.Migration

  def change do
    create table(:users) do
      add :email, :string, null: false
      add :name, :string, null: false
      add :dob, :date, null: false

      timestamps()
    end

    execute "CREATE UNIQUE INDEX users_email_idx ON users (lower(email))"
  end
end
