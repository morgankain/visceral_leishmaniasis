#!/bin/bash

export IFS=","

cat VL_fit.csv | while read a b c d e f g h i; do 

job_file="VL_${a}${b}${c}${d}${e}${f}${g}${h}.job"

echo "#!/bin/bash
#SBATCH  -p hns
#SBATCH --nodes=1               
#SBATCH --ntasks-per-node=3      
#SBATCH --time=24:00:00
#SBATCH --mail-type=ALL
#SBATCH --output=VL_${a}${b}${c}${d}${e}${f}${g}${h}.log

ml R/4.0.2
Rscript users/kainm/VL/VL_stan.R "$a" "$b" "$c" "$d" "$e" "$f" "$g" "$h" 1" > $job_file

    sbatch $job_file

done

