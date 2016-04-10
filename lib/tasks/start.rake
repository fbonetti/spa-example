namespace :start do
  task :dev do
    Rake::Task["elm:build"].invoke
    sh("rerun -p app.rb 'rackup -p 3000'")
  end

  task :debug do
    Rake::Task["elm:build"].invoke
    sh("rackup -p 3000")
  end

  task :prod do
    Rake::Task["elm:build_prod"].invoke
    sh("rackup -p 80")
  end
end