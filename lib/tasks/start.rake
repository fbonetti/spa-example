namespace :start do
  task :dev do
    Rake::Task["elm:build"].invoke
    system("rerun -p app.rb 'rackup -p 3000'")
  end

  task :prod do
    Rake::Task["elm:build"].invoke
    system("rackup")
  end
end