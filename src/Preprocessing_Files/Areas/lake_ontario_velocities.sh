#!/bin/csh
#SBATCH -J LO_Vel
#SBATCH -t 1-00:00:00
#SBATCH --ntasks=8
#SBATCH --mem=80gb
#SBATCH --gid=glfbreez
#SBATCH -A glfbreez
#SBATCH -e lake_ontario_vels_error
#SBATCH -o lake_ontario_vels_out
cd /work/GLFBREEZ/Lake_Ontario/Areas
date
Rscript Calculate_Velocities.R
date
