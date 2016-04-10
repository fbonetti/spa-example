require 'filewatcher'
require 'uglifier'

namespace :elm do
  task :build do
    Dir.chdir("frontend") do
      sh("elm make Main.elm --yes --output ../public/elm.js")
    end
  end

  task :build_prod do
    Rake::Task["elm:build"].invoke
    minified = Uglifier.compile(File.read("public/elm.js"))
    File.open("public/elm.js", 'w') do |f|
      f.write minified
    end
  end

  task :watch do
    Dir.chdir("frontend") do
      FileWatcher.new("**/*.elm").watch do |filename|
        system("elm make Main.elm --yes --output ../public/elm.js")
      end
    end
  end
end