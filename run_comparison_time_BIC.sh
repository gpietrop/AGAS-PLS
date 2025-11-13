#!/bin/bash

# Initialize Conda
eval "$(conda shell.bash hook)"
conda activate plsGP

# Array of models and dimensions
models=("str1_high" "str1_small" "str1_med")
dimensions=100
pop=50
eps=(50, 100, 200, 400)
end=99

# Run R scripts in parallel
for model in "${models[@]}"; do
    for ep in "${eps[@]}"; do
        echo "Running Rscript for model $model with modeDim $dim"
        Rscript src/main.R --model=$model --modeDim=$dim --popSize=$pop --maxiter=$ep --seed_end=$end &
    done
done

# Wait for all background processes to finish
wait

echo "All tasks have been launched."

