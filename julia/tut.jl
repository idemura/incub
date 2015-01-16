n = 5059381772892821531
# n = typemax(Uint64)
println("n=$(n)")
println("$(hex(n))")

# function bd_prob(k::Int)
#   p::Float64 = 1.0
#   for i in 0 .. k - 1
#     printlln(365.0 - i)
#     p *= (365.0 - i) / 365
#   end
#   1.0 - p
# end

# function same_sign(x1::Float64, x2::Float64)::Bool = sign(x1) == sign(x2)

# function bisection_eq(f::Function, x1::Float64, x2::Float64, eps::Float64)::Float64
#   f1 = f(x1)
#   f2 = f(x2)
#   if (f1 > 0 && f2 > 0) || (f1 < 0 && f2 < 0)
#     exit(-1)
#   end
#   while abs(x2 - x1) < eps
#     xm = (x1 + x2) * 0.5
#     fm = f(xm)
#     if same_sign(f1, fm)
#       x1 = xm
#       f1 = fm
#     else
#       x2 = xm
#       f2 = fm
#     end
#   end
#   x0
# end

# function p_nk(n::Int, k::Int)::BigInt = div(factorial(BigInt(n)) / factorial(BigInt(k)))

# println(p_nk(3, 2))
