#!/bin/bash

# Initialize Conda
eval "$(conda shell.bash hook)"
conda activate plsGP

# Array of models and dimensions
models=("str1_high" "str1_small" "str1_med")
dimensions=100
pop=50
eps=(50 100 200 300)
end=29

MAX_JOBS=30
jobcount=0

for model in "${models[@]}"; do
    for ep in "${eps[@]}"; do
        echo "Running Rscript for model $model with modeDim $dimensions, maxiter=$ep"
        Rscript src/main.R --model="$model" --modeDim="$dimensions" --popSize="$pop" --maxiter="$ep" --seed_end="$end" &
        ((jobcount++))
        if (( jobcount % MAX_JOBS == 0 )); then
            wait   # aspetta che finiscano i 4 in corso
        fi
    done
done

wait
echo "All tasks completed."

