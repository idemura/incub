def testYield
  yield 10, 20
end
testYield {|x, y| puts(x + y) }

ar = %w{a b c d}
ar.each do |x|
  puts x
end
3.upto(6) {|i| print i }
puts

a = 10
b = a
puts "a=#{a}"
puts "set b to 20"
b = 20
puts "a=#{a}"
puts "b=#{b}"

puts '------'
a = [1, 2, 1000]
b = 'cat'
a.each {|b| c = b * a[1] }
puts a
puts b
