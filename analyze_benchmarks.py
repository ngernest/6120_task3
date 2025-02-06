# Displays brench results as evidence that LVN actually optimizes programs

import matplotlib.pyplot as plt

if __name__ == '__main__':

    data = []
    with open("brench.out", "r") as file:
        next(file)
        for line in file:
            benchmark, run, result = line.strip().split(",")
            if result != "timeout":
                data.append((benchmark, run, int(result)))

    # Sort the benchmarks by the name of the benchmark file 
    benchmarks = list(sorted({ benchmark for (benchmark, _, _) in data }))
    results_baseline = []
    results_full = []
    results_dkp = []
    results_tdce = []
    results_tdce_plus = []
    results_lvn = []
    num_optimized = 0
    total = 0

    for benchmark in benchmarks:
        baseline_result = next(result for (bench_name, run, result) in data if bench_name == benchmark and run == "baseline")
        tdce_result = next(result for (bench_name, run, result) in data if bench_name == benchmark and run == "tdce")
        dkp_result = next(result for (bench_name, run, result) in data if bench_name == benchmark and run == "dkp")
        tdce_plus_result = next(result for (bench_name, run, result) in data if bench_name == benchmark and run == "tdce_plus")
        lvn_result = next(result for (bench_name, run, result) in data if bench_name == benchmark and run == "lvn")
        full_result = next(result for (bench_name, run, result) in data if bench_name == benchmark and run == "full")
        
        results_baseline.append(baseline_result)
        results_full.append(full_result)
        results_tdce.append(tdce_result)
        results_tdce_plus.append(tdce_plus_result)
        results_dkp.append(dkp_result)
        results_lvn.append(lvn_result)


        if full_result < baseline_result: 
            num_optimized += 1
        total += 1

    print(f"{num_optimized}/{total} benchmarks optimized")

    x = range(len(benchmarks))
    plt.figure(figsize=(10, 6))

    # Plot baseline
    plt.scatter(x, results_baseline, color="red", label="Baseline", zorder=2)
    
    plt.scatter(x, results_tdce, color="green", label="tdce", zorder=2)

    plt.scatter(x, results_tdce_plus, color="purple", label="tdce+", zorder=2)

    plt.scatter(x, results_dkp, color="grey", label="dkp", zorder=2)

    plt.scatter(x, results_lvn, color="orange", label="lvn", zorder=2)
    
    # Plot full
    plt.scatter(x, results_full, color="blue", label="Full", zorder=2)

    plt.xticks(x, benchmarks, rotation=45, ha="right")
    plt.xlabel("Benchmarks")
    plt.ylabel("No. of dynamic instructions")
    plt.legend()
    plt.grid(zorder=1, linestyle="--", alpha=0.6)

    plt.ylim(0, 1000)

    plt.tight_layout()
    plt.savefig('plot.png')
    plt.close()