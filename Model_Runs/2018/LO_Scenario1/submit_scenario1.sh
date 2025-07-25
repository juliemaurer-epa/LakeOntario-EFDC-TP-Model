#!/bin/csh
#SBATCH --job-name=LOTP
#SBATCH --time=2-00:00:00
#SBATCH --ntasks=8
#SBATCH --mem=80gb
#SBATCH --account=glhabs
#SBATCH --partition=compute
#SBATCH --error=LOTP_error
#SBATCH --output=LOTP_out

#Before running make sure you copy compiled executable into directory!

cd /work/GLHABS/GreatLakesEco/LakeOntario/Model_Runs/2018/LO_scenario1

module load intel/23.2
module load intelmpi/23.2
module load netcdf/4.9.2
module load pnetcdf/1.12.3

mkdir -p NETCDF 

date

mpirun ./CGEM gomdom GD_InputFile_TP Initial_Conditions.nc

date
