using Pkg
Pkg.add("Coverage")
Pkg.add("LocalCoverage")

using Coverage
using LocalCoverage

coverage = process_folder("../src") 
coverage = merge_coverage_counts(coverage)
covered_lines, total_lines = get_summary(coverage)

lcov = LocalCoverage.generate_coverage(; run_test = false)
html_coverage(lcov)

Pkg.rm("Coverage")
Pkg.rm("LocalCoverage")
