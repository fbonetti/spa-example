require 'sinatra'
require 'sinatra/base'
require 'sinatra/activerecord'
require './models/user'
require './models/meal'

class SpaExampleApp < Sinatra::Base
  register Sinatra::ActiveRecordExtension

  set :sessions, true

  before do
    content_type :json
  end

  post '/api/v1/login' do
    if user
      status 404
      { error: 'Email or password invalid' }.to_json
    else
      session[:user_id] = user.id
      user.to_json
    end
  end

  post '/api/v1/logout' do
    session.clear
    { message: 'You successfully logged out' }
  end

  get '/api/v1/users/:id/meals' do
    if logged_in?
      { message: 'asdf' }.to_json
    else
      { message: 'one' }.to_json
    end
  end

  # Route all non data requests to the frontend app

  get '/*' do
    content_type :html
    File.read('./frontend/src/index.html')
  end

  private

  def logged_in?
    !current_user.nil?
  end

  def current_user
    @current_user ||= begin
      user = User.find_by(email: params[:email])

      if user && user.authenticate(params[:password])
        user
      else
        nil
      end
    end
  end
end