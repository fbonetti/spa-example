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
    attrs = attributes.slice('id', 'first_name', 'last_name', 'daily_limit')
    attrs['meals'] = meals.map(&:safe_attributes)
    attrs
  end
end