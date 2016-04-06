require 'filewatcher'

namespace :elm do
  task :build do
    Dir.chdir("frontend") do
      sh("elm make Main.elm --yes --output ../public/elm.js")
    end
  end

  task :watch do
    Dir.chdir("frontend") do
      FileWatcher.new("**/*.elm").watch do |filename|
        sh("elm make Main.elm --yes --output ../public/elm.js")
      end
    end
  end
end