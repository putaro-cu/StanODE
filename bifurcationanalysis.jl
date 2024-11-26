using ModelingToolkit
using DifferentialEquations
using Random
using Plots
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

bif_par = N
plot_var = x
p_start = [N => 1.0]
u0_guess = [x => 0.0]

bprob = BifurcationProblem(
    model,
    u0_guess,
    p_start,
    bif_par;
    plot_var = plot_var,
    jac = false)

p_span = (0.0, 500.0)
opts_br = ContinuationPar(nev = 2,
    p_max = p_span[2], p_min = p_span[1])

bf = bifurcationdiagram(bprob, PALC(), 2, (args...) -> opts_br; bothside = true)
using Plots
plot(bf;
    putspecialptlegend = false,
    markersize = 2,
    plotfold = false,
    xguide = "N",
    yguide = "x")