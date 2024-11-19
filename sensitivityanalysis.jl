module SensitivityAnalysis
    __precompile__(false)

    using PumasQSP
    using ModelingToolkit
    using DifferentialEquations
    using DataSets
    using DataFrames
    using StructuralIdentifiability: assess_identifiability, assess_local_identifiability
    using Random
    using StatsPlots
    using GlobalSensitivity
    using Plots

    iv = @variables t
    states = @variables x1(t) = 480
    @variables y(t)
    ps = @parameters α=0.0045 β=0.00015 s=10 N=600
    D = Differential(t)

    eqs = [
        D(x1) ~ (α + β * x1) * (N - x1) - s * x1 / (s + x1)
    ]

    obs_eq = [y ~ x1]
    measured_quantities = [y ~ x1]

    @named model = ODESystem(eqs, t, states, ps; observed = obs_eq)

    sia_result = assess_identifiability(model, measured_quantities = measured_quantities, p = 0.99) # 識別可能性解析の実行
    println(sia_result)

    function model_func(p)
        prob = ODEProblem(model, [480.0], (0.0, 100.0), p)
        sol = solve(prob, Tsit5())
        return sol[1, end]
    end

    
    param_ranges = [(0.001, 0.01), (0.0001, 0.001), (1.0, 20.0), (500.0, 700.0)] # 動かすパラメータ範囲

    sobol_result = gsa(model_func, Sobol(), param_ranges, samples=1000) # 感度分析の実行
    println(sobol_result)

    first_order = sobol_result.S1[:]
    total_order = sobol_result.ST[:]

    param_names = ["α", "β", "s", "N"]
    p1 = bar(param_names, first_order, legend=:none)
    xlabel!(p1, "Parameters")
    ylabel!(p1, "First Order")
    p2 = bar(param_names, total_order, legend=:none)
    xlabel!(p2, "Parameters")
    ylabel!(p2, "Total Order")
    p_combined = plot(p1, p2, layout=(1, 2), size=(800, 400), left_margin=5Plots.mm, right_margin=5Plots.mm, bottom_margin=5Plots.mm, top_margin=5Plots.mm)

    savefig(p_combined, "sobol_sensitivity_analysis_combined.png")
end