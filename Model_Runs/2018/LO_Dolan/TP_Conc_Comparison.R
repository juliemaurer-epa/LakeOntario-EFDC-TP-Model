library(ncdf4)
# library(lubridate)
# library(dplyr) 

im <- 256
jm <- 133
rec_num <- 106

# input_file <- "Ontario2_current_EFDC_IJ.csv"
input_file <- "/work/GLFBREEZ/Lake_Ontario/Initial_Conditions/Initial_Conditions_TP_2018.csv"

df <- read.csv(input_file, 
               header = TRUE, 
               stringsAsFactors = FALSE, 
               check.names = FALSE)
nr <- nrow(df)
ix <- array(0, dim = c(nr))
jy <- array(0, dim = c(nr))

for (k in 1:nr)
{
  ix[k] <- df$I_Value[k]
  jy[k] <- df$J_Value[k]
}

# Open netCDF files
nc1 <- nc_open("/work/GLFBREEZ/Lake_Ontario/Model_Runs/2018/LO_10/NETCDF/gomdom.000000.nc")

# Extract TP concentration for sigma layer = 1 and record = 106
tp1 <- ncvar_get(nc1, varid = "TR")
tp1a <- tp1[,,1,106]
tp_Base <- array(0.0, dim = c(nr))
for (k in 1:nr)
{
  i <- ix[k]
  j <- jy[k]
  tp_Base[k] <- tp1a[i,j]
}

nc_close(nc1)

nc2 <- nc_open("/work/GLFBREEZ/Lake_Ontario/Model_Runs/2018/LO_30/NETCDF/gomdom.000000.nc")

# Extract TP concentration for sigma layer = 1 and record = 106
tp2 <- ncvar_get(nc2, varid = "TR")
tp2a <- tp2[,,1,106]
tp_Scenario <- array(0.0, dim = c(nr))
for (k in 1:nr)
{
  i <- ix[k]
  j <- jy[k]
  tp_Scenario[k] <- tp2a[i,j]
}

nc_close(nc2)

# Calculate percent change
conc_change <- array(0.0, dim = c(nr))
percent_change <- array(0.0, dim = c(nr))
for (k in 1:nr)
{
  conc_change[k] <- tp_Scenario[k] - tp_Base[k]
  percent_change[k] = ((tp_Scenario[k] - tp_Base[k])/tp_Base[k]) * 100
}

# Create dataframe
dfTP <- data.frame(I_Value = ix,
                   J_Value = jy,
                   TP_Base = tp_Base,
                   TP_Slow_Settling = tp_Scenario,
                   Conc_Change = conc_change,
                   Percent_Change = percent_change)

write.csv(dfTP, file = "TP_Base_SlowSettling_PercentChanges.csv", row.names = FALSE)
