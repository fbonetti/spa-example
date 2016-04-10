require 'dotenv'
Dotenv.load

require 'sinatra'
require 'sinatra/base'
require 'sinatra/activerecord'
require 'dotenv'
require 'sinatra'

Dir.glob('./models/*.rb').each { |r| require r}

class SpaExampleApp < Sinatra::Base
  register Sinatra::ActiveRecordExtension

  set :server, :puma

  use Rack::Session::Cookie, expire_after: 604800, secret: ENV['SESSION_SECRET']

  before do
    content_type :json

    request.body.rewind
    body = request.body.read
    @payload = body.empty? ? {} : JSON.parse(body)
  end

  post '/api/v1/register' do
    user = User.new(
      @payload.slice(
        'first_name',
        'last_name',
        'email',
        'password',
        'password_confirmation',
        'type'
      )
    )

    if user.save
      session[:user_id] = user.id
      { user_id: user.id }.to_json
    else
      status 400
      { error: user.errors.full_messages.join(', ') }.to_json
    end
  end

  post '/api/v1/login' do
    user = User.find_by(email: @payload['email'])

    if user && user.authenticate(@payload['password'])
      session[:user_id] = user.id
      { user_id: user.id }.to_json
    else
      status 400
      { error: 'Email or password invalid' }.to_json
    end
  end

  post '/api/v1/logout' do
    session.clear
    { message: 'You successfully logged out' }.to_json
  end

  get '/api/v1/users' do
    if !logged_in?
      status 401
      return { error: 'You need to be logged in to see this page' }.to_json
    end

    if !(current_user.user_manager? || current_user.admin?)
      status 403
      return { error: "You must be a User Manager or an Admin to view this page" }.to_json
    end

    User.select("users.*, COUNT(*) as meal_count")
      .joins("LEFT JOIN meals ON meals.user_id = users.id")
      .group("users.id")
      .order("users.id")
      .map { |u| u.attributes.slice('id', 'first_name', 'last_name', 'type', 'meal_count') }.to_json
  end

  get '/api/v1/users/:id' do
    if logged_in?
      user = User.includes(:meals).order("meals.created_at DESC").find_by(id: params[:id])
      if user
        if current_user.id == user.id || current_user.user_manager? || current_user.admin?
          user.safe_attributes.to_json
        else
          status 403
          { error: "You're not allowed to view this user" }.to_json
        end
      else
        status 404
        { error: 'User not found' }.to_json
      end
    else
      status 401
      { error: 'You need to be logged in to see this page' }.to_json
    end
  end

  patch '/api/v1/users/:id' do
    if logged_in?
      user = User.includes(:meals).order("meals.created_at DESC").find_by(id: params[:id])
      if user
        if current_user.id == user.id || current_user.user_manager? || current_user.admin?
          if user.update(@payload.slice('first_name', 'last_name', 'daily_limit'))
            user.safe_attributes.to_json
          else
            { error: user.errors.full_messages.join(', ') }.to_json
          end
        else
          status 403
          { error: "You're not allowed to modify this user" }.to_json
        end
      else
        status 404
        { error: 'User not found' }.to_json
      end
    else
      status 401
      { error: 'You need to be logged in to see this page' }.to_json
    end
  end

  post '/api/v1/meals' do
    if logged_in?
      user = User.find_by(id: @payload['user_id'])
      if user
        if current_user.id == user.id || current_user.admin?
          meal = user.meals.create(@payload.slice('description', 'calories'))
          meal.safe_attributes.to_json
        else
          status 403
          { error: "You're not allowed to perform this action" }.to_json
        end
      else
        status 404
        { error: 'User not found' }.to_json
      end
    else
      status 401
      { error: 'You need to be logged in to perform this action' }.to_json
    end
  end

  delete '/api/v1/meals/:id' do
    if logged_in?
      meal = Meal.find_by(id: params['id'])
      if meal
        if current_user.id == meal.user_id || current_user.admin?
          meal.destroy
          { meal_id: meal.id }.to_json
        else
          status 403
          { error: "You're not allowed to perform this action" }.to_json
        end
      else
        status 404
        { error: 'Meal not found' }.to_json
      end
    else
      status 401
      { error: 'You need to be logged in to perform this action' }.to_json
    end
  end

  # Route all non data requests to the frontend app

  get '/*' do
    content_type :html
    File.read('./frontend/index.html')
  end

  private

  def logged_in?
    !current_user.nil?
  end

  def current_user
    @current_user ||= User.find_by(id: session[:user_id])
  end
end