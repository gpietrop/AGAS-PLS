#!/bin/bash

# Initialize Conda
eval "$(conda shell.bash hook)"
conda activate plsGP

# Array of models and dimensions
models=("str3_small" "str3_med" "str3_high")
dimensions=(100 250 500)
pop=100
ep=200
start=0
end=4

# Run R scripts in parallel
for model in "${models[@]}"; do
    for dim in "${dimensions[@]}"; do
        echo "Running Rscript for model $model with modeDim $dim"
        Rscript src/main.R --model=$model --modeDim=$dim --popSize=$pop --maxiter=$ep --seed_start=$start --seed_end=$end &
    done
done

# Wait for all background processes to finish
wait

echo "All tasks have been launched."

