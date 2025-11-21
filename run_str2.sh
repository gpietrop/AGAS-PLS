#!/bin/bash

# Initialize Conda
eval "$(conda shell.bash hook)"
conda activate plsGP

# Array of models and dimensions
models=("str2_small" "str2_med" "str2_high")
dimensions=(100 250 500)
pop=100
ep=200
end=10

# Run R scripts in parallel
for model in "${models[@]}"; do
    for dim in "${dimensions[@]}"; do
        echo "Running Rscript for model $model with modeDim $dim"
        Rscript src/main.R --model=$model --modeDim=$dim --popSize=$pop --maxiter=$ep --seed_end=$end &
    done
done

# Wait for all background processes to finish
wait

echo "All tasks have been launched."

