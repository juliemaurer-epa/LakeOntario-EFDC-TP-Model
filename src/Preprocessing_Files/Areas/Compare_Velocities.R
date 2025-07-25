library(ncdf4)
library(lubridate)
library(dplyr) 

im <- 256
jm <- 133
dx <- array(0.0, dim = c(im,jm))
dy <- array(0.0, dim = c(im,jm))

# input_file <- "Ontario2_current_EFDC_IJ.csv"
input_file <- "n2018LOntario1_current_test_ij_id.txt"

df <- read.csv(input_file, 
               header = TRUE, 
               stringsAsFactors = FALSE, 
               check.names = FALSE)

# Convert dates/times to julian days
df$JulianDate <- as.numeric(as.POSIXct(dmy_hms(df$Var1)))

df1 <- read.csv("dx_dy.csv",
                header = TRUE,
                stringsAsFactors = FALSE,
                check.names = FALSE)


ic <- 0
for (i in 1:im)
{
  for (j in 1:jm)
  {
    ic <- ic + 1
    ix <- df1$i[ic]
    jy <- df1$j[ic]
    dx[ix,jy] <- df1$dx[ic]
    dy[ix,jy] <- df1$dy[ic]
  }
}

ldepth <- "/work/GLFBREEZ/Lake_Ontario/CGEM/CGEM/data/Lake_Ontario_2018/INPUT/LayerDepth.nc"
uflows <- "/work/GLFBREEZ/Lake_Ontario/CGEM/CGEM/data/Lake_Ontario_2018/INPUT/UFlow.nc"
vflows <- "/work/GLFBREEZ/Lake_Ontario/CGEM/CGEM/data/Lake_Ontario_2018/INPUT/VFlow.nc"
temp <- "/work/GLFBREEZ/Lake_Ontario/CGEM/CGEM/data/Lake_Ontario_2018/INPUT/Temp.nc"

nc1 <- nc_open(ldepth)
nc2 <- nc_open(uflows)
nc3 <- nc_open(vflows)
nc4 <- nc_open(temp)

dz <- ncvar_get(nc1, varid = "LayerDepth")
stime <- ncvar_get(nc1, varid = "Time")
maxDate <- max(stime)
minDate <- min(stime)
npoints <- length(stime)
u <- ncvar_get(nc2, varid = "UFlow")
v <- ncvar_get(nc3, varid = "VFlow")
wt <- ncvar_get(nc4, varid = "Temp")

# Only use field data within model run time period
df <- df[df$JulianDate >= minDate,]
df <- df[df$JulianDate <= maxDate,]

# Calculate velocities
nr <- nrow(df)
vel <- array(0.0, dim = c(nr))
angle <- array(0.0, dim = c(nr))
wtemp <- array(0.0, dim = c(10, nr))
ld <- array(0.0, dim = c(10, nr))
icounter <- 0

for (i in 1:nr)
{
  # Find index of closest model output date to each field data date
  date_index <- which.min(abs(df$JulianDate[i] - stime))
  
  i1 <- df$I_Value[i]
  j1 <- df$J_Value[i]
  nl <- df$Num_layers[i]
  
  netU <- sum(u[i1,j1,1:nl,date_index])
  netV <- sum(v[i1,j1,1:nl,date_index])
  netAreaU <- sum(dz[i1,j1,1:nl,date_index]) * dy[i1,j1]
  netAreaV <- sum(dz[i1,j1,1:nl,date_index]) * dx[i1,j1]
  velu <- netU / netAreaU 
  velv <- netV / netAreaV
  vel[i] <- sqrt(velu**2 + velv**2)
  
  angvel <- atan2(velv, velu) * 180 / pi
  if (angvel < 0.0) 
  {
    angvel <- angvel + 360
  }
  angle[i] <- angvel
  
  for (k in 1:nl)
  {
    icounter <- icounter + 1
    wtemp[k, i] <- wt[i1, j1, k, date_index]
    if (k > 1)
    {
      ld[k,i] <- ld[k-1,i] + dz[i1, j1, k, date_index]
    }else
    {
      ld[k,i] <- dz[i1, j1, k, date_index] / 2.0
    }
    
  }
}


df$Modeled_Velocity <- vel
df$Modeled_Direction <- angle

dfwt <- data.frame(
  date = array("", dim = c(icounter)),
  latitude = array(0.0, dim = c(icounter)),
  longitude = array(0.0, dim = c(icounter)),
  i_value = array(0, dim = c(icounter)),
  j_value = array(0, dim = c(icounter)),
  vertical_layer = array(0, dim = c(icounter)),
  temperature = array(0.0, dim = c(icounter)),
  layer_depth = array(0.0, dim = c(icounter))
)

j <- 0
for (i in 1:nr)
{
  nl <- df$Num_layers[i]
  for (k in 1:nl)
  {
    j <- j + 1
    dfwt$i_value[j] <- df$I_Value[i]
    dfwt$j_value[j] <- df$J_Value[i]
    dfwt$date[j] <- df$Var1[i]
    dfwt$latitude[j] <- df$Lat[i]
    dfwt$longitude[j] <- df$Long[i]
    dfwt$vertical_layer[j] <- k
    dfwt$temperature[j] <- wtemp[k,i]
    dfwt$layer_depth[j] <- ld[k,i]
  }
}

write.csv(df, file = "New_Lake_Ontario_ModelvsObserved_Vels.csv", row.names = FALSE)
write.csv(dfwt, file = "New_Lake_Ontario_WaterTemperature_LayerDepths.csv", row.names = FALSE)

nc_close(nc1)
nc_close(nc2)
nc_close(nc3)
nc_close(nc4)
