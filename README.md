# LakeOntario-EFDC-TP-Model
Repository for model files, run scenarios, and methods for visualizing/analyzing output

What you need to run the model:
1. A model run directory in GLHABs or GLFBREEZ (GLHABS preferred)
    Create a subdirectory in either directory depending on which year you want to run:
           /work/GLHABS/GreatLakesEco/LakeOntario/Model_Runs/2013
           /work/GLHABS/GreatLakesEco/LakeOntario/Model_Runs/2018

```
mkdir /work/GLHABS/GreatLakesEco/LakeOntario/Model_Runs/2018/LO_test
```

2. A compiled executable (CGEM) copied into the model run directory

```
cp /work/GLFBREEZ/Lake_Ontario/MPI_Code/CGEM/CGEM/CGEM .
```

3. MyFiles.inp - This file contains the path to initial conditions, hydrodynamics, and TP river loads.  It must be stored in a "data" subdirectory within your model run directory. Copy from a previous run into your directory.

```
mkdir data
cp /work/GLHABS/GreatLakesEco/LakeOntario/Model_Runs/2018/LO2/data/MyFiles.inp ./data/
```

4. GD_InputFile_TP - the input file containing start/end simulation dates and kinetic parameters.
    This file can be copied from a previous run. The first line can be modified to indicate the loading scenario used and whether it is a test run or full run.
    If you want to run the model for a shorter amount of time, modify line 6 (simulation end time)

```
cp /work/GLHABS/GreatLakesEco/LakeOntario/Model_Runs/2018/LO2/GD_InputFile_TP
```

5. A script for submitting the run (can copy and modify an existing script)
    Example:

```
cp /work/GLHABS/GreatLakesEco/LakeOntario/Model_Runs/2018/LO2/lake_ontario_02.sh .
```

Within lake_ontario02.sh:
    You can modify job name and the _error and _out names if you want to.
    You MUST change the directory after the cd command to match your model run directory!
    You can modify the time, number and tasks and memory if you know what you need but what is already requested here is sufficient for a full run and will be fine to keep as-is for a partial run.

```
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

module load intel/23.2
module load intelmpi/23.2
module load netcdf/4.9.2
module load pnetcdf/1.12.3

mkdir -p NETCDF

date

mpirun ./CGEM gomdom GD_InputFile_TP Initial_Conditions.nc

date
~


cp  /work/GLHABS/GreatLakesEco/LakeOntario/Model_Runs/2018/LO2/
```

# To update river loads:

Netcdf files for river load scenarios exist in /work/GLFBREEZ/Lake_Ontario/River_Loads

The "default" scenario is TP_RiverLoads_2013.nc and TP_RiverLoads_2018.nc

1. Before running the model, check which scenario is currently loaded in the executable in the RiverLoad.F90 file in:

```
/work/GLFBREEZ/Lake_Ontario/MPI_code
```

2. Open RiverLoad.F90 with favorite text editor

Modify line 115 (search 'set filenames for netCDF')
Edit the netcdf file to the scenario you choose.
Save and close file.

3. Go up one directory. (cd ..)
4. recompile the executable to update it with current river load scenario
Run the following:

```
module load intel/23.2
module load intelmpi/23.2
module load netcdf/4.9.2
module load pnetcdf/1.12.3

make clean

make -f Makefile_par
```

5. Once the program finishes compiling, copy the executable to your model run directory.

# To create a new river load scenario:

We have an R script for updating the netcdfs from .csv files.
Loading data starts on 1/1 and ends on 12/31/
Units are in kg/s
Rivers are stored as numbers in a specific sequence:
1 = Niagara River
2 = Eighteenmile Creek
3 = Oak Orchard Creek
4 = Genesee River
5 = Oswego River
6 = Black River
7 = Twelve Mile Creek
8 = Trent River
9 = Humber River

1. Update the netcdf file by copying a previous version and renaming it.
Example;

cp TP_RiverLoads_2018.nc TP_RiverLoads_2018_newscenario.nc

2. Run R script to update the new netcdf

3. copy updated netcdf into /work/GLFBREEZ/Lake_Ontario/CGEM/CGEM/data/Lake_Ontario/INPUT

4. Follow steps in update river loads (above) to update RiverLoad.F90 and recompile executable.
