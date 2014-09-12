def greet
  puts 'beeee'
  yield
end
greet { puts 'Hi' }

ar = %w{a b c d}
# ar.each {|x| puts x}
ar.each do |x|
  puts x
end
3.upto(6) {|i| print i }

puts

class X
  def initialize(m)
    @m = m
  end
end

class X
  def to_s
    "my X #@m"
  end
end

puts X.new(100)
