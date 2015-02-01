# This is an interpretable inline Julia tutorial I based on the official
# documentation.

same_sign(x1::Float64, x2::Float64)::Bool = sign(x1) == sign(x2)
println(same_sign(10.0, 4.0))
