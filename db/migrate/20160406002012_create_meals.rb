class CreateMeals < ActiveRecord::Migration
  def change
    create_table :meals do |t|
      t.integer :user_id
      t.string :description
      t.string :calories

      t.timestamps
    end
  end
end
