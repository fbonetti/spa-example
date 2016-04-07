class User < ActiveRecord::Base
  has_secure_password
  has_many :meals

  validates :first_name, :last_name, presence: true
  validates :email, presence: true, uniqueness: { case_sensitive: false }
  validates :type, presence: true, inclusion: { in: ['RegularUser', 'UserManager', 'Admin'] }

  def regular_user?
    type == 'RegularUser'
  end

  def user_manager?
    type == 'UserManager'
  end

  def admin?
    type == 'Admin'
  end

  def safe_attributes
    attrs = attributes.slice('first_name', 'last_name')
    attrs['meals'] = meals.map do |meal|
      {
        'description' => meal.description,
        'calories' => meal.calories,
        'created_at' => meal.created_at.to_i
      }
    end

    attrs
  end
end