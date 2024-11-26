using ModelingToolkit
using DifferentialEquations
using Random
using CairoMakie
using BifurcationKit

iv = @variables t
states = @variables x(t)
@variables y(t)
ps = @parameters α = 0.0045 β = 0.00015 s = 10 N = 600
D = Differential(t)

eqs = [
    D(x) ~ (α + β * x) * (N - x) - s * x / (s + x)
]
obs_eq = [y ~ x]

@mtkbuild model = ODESystem(eqs, t, states, ps, observed=obs_eq)

# 初期条件と時間範囲の設定
u0 = [x => 200.0]  # 初期条件
tspan = (0.0, 200.0)  # 時間範囲

# 微分方程式を解く
prob = ODEProblem(model, u0, tspan)
sol = DifferentialEquations.solve(prob, Tsit5(), abstol=1e-6, reltol=1e-6)

# 結果をプロット
fig = Figure(size=(1000, 800))
ax = Axis(fig[1, 1], xlabel="Time", ylabel="x", xlabelsize=40, ylabelsize=40, xticklabelsize=30, yticklabelsize=30, limits=(nothing, nothing, 0, 600))
lines!(ax, sol.t, sol[x], linewidth=5)
set_theme!(figure_padding=(50, 50, 50, 50))
display(fig)

save("plot3.png", fig)