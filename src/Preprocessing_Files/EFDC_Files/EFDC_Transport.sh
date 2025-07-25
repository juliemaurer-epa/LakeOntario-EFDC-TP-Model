#!/bin/csh
#SBATCH -J EFDC_Transport
#SBATCH -t 03:00:00
#SBATCH -N 1
#SBATCH --mem=80gb
#SBATCH --gid=glfbreez
#SBATCH -A glfbreez
#SBATCH -e EFDC_Transport_error
#SBATCH -o EFDC_Transport_out
cd /work/GLFBREEZ/Lake_Ontario
date
./EFDC_Transport-1.0
date
