require 'dotenv'
Dotenv.load

require 'sinatra'
require 'sinatra/base'
require 'sinatra/activerecord'
require 'dotenv'

Dir.glob('./models/*.rb').each { |r| require r}

class SpaExampleApp < Sinatra::Base
  register Sinatra::ActiveRecordExtension

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
      { message: 'Success' }.to_json
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
    { message: 'You successfully logged out' }
  end

  get '/api/v1/users' do

  end

  get '/api/v1/users/:id' do
    if logged_in?
      user = User.find_by(id: params[:id])
      if user
        if current_user.id == user.id || user.admin?
          user.safe_attributes.to_json
        else
          status 403
          { message: "You're not allowed to view this user" }
        end
      else
        status 404
        { message: 'User not found' }.to_json
      end
    else
      status 401
      { message: 'You need to be logged in to see this page' }.to_json
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