require 'rubygems'
require 'bundler'
require 'fileutils'

Bundler.require

require './app'

log_path = "log/#{ENV['RACK_ENV']}.log"
FileUtils.mkdir_p("log")
FileUtils.touch(log_path)
log = File.new(log_path, "a+")
$stdout.reopen(log)
$stderr.reopen(log)

run SpaExampleApp