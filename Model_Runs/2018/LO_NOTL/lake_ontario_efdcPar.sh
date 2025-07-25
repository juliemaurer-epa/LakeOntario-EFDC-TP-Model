#!/bin/csh
#SBATCH -J LO_EFDC_Par
#SBATCH -t 2-00:00:00
#SBATCH --ntasks=8
#SBATCH --mem=80gb
#SBATCH --gid=glfbreez
#SBATCH -A glfbreez
#SBATCH -e lake_ontario_efdcPar_error
#SBATCH -o lake_ontario_efdcPar_out
cd /work/GLFBREEZ/Lake_Ontario/Model_Runs/2018/LO_11
source ./modules_intel.sh
date
mpirun ./CGEM gomdom GD_InputFile_TP Initial_Conditions.nc
date
