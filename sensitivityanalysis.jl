using ModelingToolkit
using DifferentialEquations
using StructuralIdentifiability
using Random
using GlobalSensitivity
using Plots

iv = @variables t
states = @variables x1(t)
@variables y(t)
ps = @parameters α=0.0045 β=0.00015 s=10
D = Differential(t)

eqs = [
    D(x1) ~ (α + β * x1) * (300 - x1) - s * x1 / (s + x1)
]

obs_eq = [y ~ x1]
measured_quantities = [y ~ x1]

@named model = ODESystem(eqs, t, states, ps; observed = obs_eq)

sia_result = assess_identifiability(model, measured_quantities = measured_quantities, p = 0.99) # 識別可能性解析の実行
println(sia_result)

function model_func(p)
    prob = ODEProblem(model, [102.2], (0.0, 100.0), p)
    sol = solve(prob, Tsit5())
    return sol[1, end]
end


param_ranges = [(0.0001, 0.01), (0.00001, 0.001), (1, 15.0)] # 動かすパラメータ範囲

sobol_result = gsa(model_func, Sobol(), param_ranges, samples=1000) # 感度分析の実行
println(sobol_result)

first_order = sobol_result.S1[:]
total_order = sobol_result.ST[:]

param_names = ["α", "β", "s"]
p1 = bar(param_names, first_order, legend=:none)
xlabel!(p1, "Parameters")
ylabel!(p1, "First-Order Sensitivity Index")
p2 = bar(param_names, total_order, legend=:none)
xlabel!(p2, "Parameters")
ylabel!(p2, "Total-Order Sensitivity Index")
p_combined = plot(p1, p2, layout=(1, 2), size=(800, 400), left_margin=5Plots.mm, right_margin=5Plots.mm, bottom_margin=5Plots.mm, top_margin=5Plots.mm)

savefig(p_combined, "sobol_sensitivity_analysis_combined.png")

iv = @variables t
states = @variables x1(t) x2(t)
@variables y1(t) y2(t)
ps = @parameters a b c d
D = Differential(t)

eqs = [
    D(x1) ~ a * x1 - b * x1 * x2,
    D(x2) ~ -c * x2 + d * x1 * x2
]

obs_eq = [y1 ~ x1]
measured_quantities = [y1 ~ x1]

@named model = ODESystem(eqs, t, states, ps; observed = obs_eq)

sia_result = assess_identifiability(model, measured_quantities = measured_quantities, p = 0.99) # 識別可能性解析の実行
println(sia_result)

find_result = find_identifiable_functions(model, measured_quantities = measured_quantities, with_states = true)
println(find_result)
