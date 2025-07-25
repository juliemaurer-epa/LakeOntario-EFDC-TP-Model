#!/bin/csh
#SBATCH --job-name=LOTP
#SBATCH --time=2-00:00:00
#SBATCH --ntasks=8
#SBATCH --mem=80gb
#SBATCH --account=glhabs
#SBATCH --partition=compute
#SBATCH --error=LOTP_error
#SBATCH --output=LOTP_out

cd /work/GLHABS/GreatLakesEco/LakeOntario/Model_Runs/2018/LO2

module load intel
module load intelmpi
module load netcdf
module load pnetcdf

mkdir -p NETCDF 

date

mpirun ./CGEM gomdom GD_InputFile_TP Initial_Conditions.nc

date
