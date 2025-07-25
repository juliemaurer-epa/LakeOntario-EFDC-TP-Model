library(ncdf4)
library(openxlsx)

# Set path and name of Excel file containing river loads.
TP_InputFile <- "/work/GLHABS/GreatLakesEco/LakeOntario/LOEM/LOEM_TP_Daily_Loads_2018.xlsx"

# Set path and name of netcdf containing TP loads.
# JM: copied TP_RiverLoads_2013.nc and renamed to new version.
Netcdf_InputFile <- "/work/GLFBREEZ/Lake_Ontario/River_Loads/TP_RiverLoads_LOEM_2018.nc"

#Netcdf_OutputFile <- "/work/GLFBREEZ/Lake_Ontario/River_Loads/TP_RiverLoads_LOEM_2013.nc"

# Read in Excel file containing river loads.
# The command below assumes user is reading input data from an Excel file.  
# The "sheet" parameter should be adjusted to whatever name the Excel file 
# has for the sheet containing the data.
# If using a CSV file, use the "read.csv" function instead.
dfTP <- read.xlsx(xlsxFile = TP_InputFile,
                  sheet = "TP_Loads_kg_s",
                  colNames = TRUE,
                  detectDates = TRUE)


# Create data frame for Niagara River (or some other river).  
# Data frame is defined for 365 days.  
# If updating loads for leap year, set length.out to 366.
# Set date to appropriate year. Current example assumes year is 2013.
# Note: If updating additional rivers, add more columns to data frame below.
dfLD <- data.frame(Date = seq(as.Date("2018/1/1"), by = "day", length.out = 365),
                   Load1 = 0.0, Load2 = 0.0, Load3 = 0.0, Load4 = 0.0, Load5 = 0.0)

# Obtain the day numbers for the simulation time frame.
dayNumbers <- match(dfTP$Date, dfLD$Date)

# Assign river loads to the corresponding day numbers in data frame.
dfLD$Load1[dayNumbers] <- dfTP$Niagara
dfLD$Load2[dayNumbers] <- dfTP$Eighteenmile
dfLD$Load3[dayNumbers] <- dfTP$OakOrchard
dfLD$Load4[dayNumbers] <- dfTP$Genesee
dfLD$Load5[dayNumbers] <- dfTP$Oswego

# Repeat the April 1st TP load for the January 1 - March 31 time frame.
date1 <- match("2018-01-01", dfLD$Date)
date2 <- match("2018-03-31", dfLD$Date)
date3 <- match("2018-04-01", dfLD$Date)
dfLD$Load1[date1:date2] <- dfLD$Load1[date3]
dfLD$Load2[date1:date2] <- dfLD$Load2[date3]
dfLD$Load3[date1:date2] <- dfLD$Load3[date3]
dfLD$Load4[date1:date2] <- dfLD$Load4[date3]
dfLD$Load5[date1:date2] <- dfLD$Load5[date3]

# Repeat the September 30th TP load for the October 1 - December 31 time frame.
date4 <- match("2018-10-01", dfLD$Date)
date5 <- match("2018-12-31", dfLD$Date)
date6 <- match("2018-09-30", dfLD$Date)
dfLD$Load1[date4:date5] <- dfLD$Load1[date6]
dfLD$Load2[date4:date5] <- dfLD$Load2[date6]
dfLD$Load3[date4:date5] <- dfLD$Load3[date6]
dfLD$Load4[date4:date5] <- dfLD$Load4[date6]
dfLD$Load5[date4:date5] <- dfLD$Load5[date6]
  
# Open netcdf file. 
nc <- nc_open(Netcdf_InputFile, write = TRUE)

# Read in TP variable from netcdf file.
tp <- ncvar_get(nc, varid = "TP")

# Update Niagara River. Note Niagara River corresponds to index 1.
# If updating more rivers, add corresponding entries below.
tp[1,] <- dfLD$Load1
tp[2,] <- dfLD$Load2
tp[3,] <- dfLD$Load3
tp[4,] <- dfLD$Load4
tp[5,] <- dfLD$Load5

# Put updated values under TP variable in netcdf file.
ncvar_put(nc, varid = "TP", vals = tp)

# Close netcdf file.
nc_close(nc)
