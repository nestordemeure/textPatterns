
# export symbol map for perf
export COMPlus_PerfMapEnabled=1

# compiles
dotnet build

# run it with perf
perf record -F 97 -g dotnet run

# analyses the result
perf report --no-children -g 'graph,0.5,callee'
