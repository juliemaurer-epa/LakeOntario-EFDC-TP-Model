#!/bin/csh
#SBATCH -J EFDC_Tran
#SBATCH -t 03:00:00
#SBATCH -N 1
#SBATCH --mem=80gb
#SBATCH --gid=glfbreez
#SBATCH -A glfbreez
#SBATCH -e EFDC_Tran_error
#SBATCH -o EFDC_Tran_out
cd /work/GLFBREEZ/Lake_Ontario
date
./Read_EFDC_Tran.exe
date
