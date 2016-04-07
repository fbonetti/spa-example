class ConvertCaloriesToInt < ActiveRecord::Migration
  def change
    remove_column :meals, :calories
    add_column :meals, :calories, :integer
  end
end
