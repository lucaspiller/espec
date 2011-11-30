describe "sillyness" do
  before(:all) do
    puts "output from before all"
  end

  before(:each) do
    puts "before each"
  end

  after(:each) do
    puts "after each"
  end

  it "should do stuff" do
    puts "output: from should do stuff"
  end

  describe "nested" do
    it "should do nested stuff" do
      puts "output from nested stuff"
    end

    it "should do other nested stuff" do
      puts "output from other nested stuff"
    end
  end

  after(:all) do
    puts "output from after all"
  end
end
