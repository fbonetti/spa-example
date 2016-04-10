class AddDailyLimitToUser < ActiveRecord::Migration
  def change
    add_column :users, :daily_limit, :integer, default: 2000
  end
end
