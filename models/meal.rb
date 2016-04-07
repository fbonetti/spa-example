class Meal < ActiveRecord::Base
  belongs_to :user

  def safe_attributes
    {
      'id' => id,
      'description' => description,
      'calories' => calories,
      'created_at' => created_at.to_i
    }
  end
end