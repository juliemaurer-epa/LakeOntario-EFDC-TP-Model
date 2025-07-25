#!/bin/csh
#SBATCH -J TP_Conc
#SBATCH -t 01:00:00
#SBATCH --ntasks=1
#SBATCH --mem=80gb
#SBATCH --gid=glfbreez
#SBATCH -A glfbreez
#SBATCH -e TP_Conc_error
#SBATCH -o TP_Conc_out
cd /work/GLFBREEZ/Lake_Ontario/Model_Runs/2018/LO_10
Rscript ./TP_Conc_Comparison.R
