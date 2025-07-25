library(ncdf4)

im <- 256
jm <- 133
dx <- array(0.0, dim = c(im,jm))
dy <- array(0.0, dim = c(im,jm))

i1 <- 138
j1 <- 52



# Convert dates/times to julian days
# df$JulianDate <- as.numeric(as.POSIXct(dmy_hms(df$Var1)))

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

nc1 <- nc_open(ldepth)
nc2 <- nc_open(uflows)
nc3 <- nc_open(vflows)

nl <- 10
dz <- ncvar_get(nc1, varid = "LayerDepth")
time <- ncvar_get(nc1, varid = "Time")
npoints <- length(time)
u <- ncvar_get(nc2, varid = "UFlow")
v <- ncvar_get(nc3, varid = "VFlow")

# Calculate velocities
vel <- array(0.0, dim = c(npoints))
velu <- array(0.0, dim = c(npoints))
velv <- array(0.0, dim = c(npoints))
angle <- array(0.0, dim = c(npoints))

for (i in 1:npoints)
{
  date_index <- i
  netU <- sum(u[i1,j1,1:nl,date_index], na.rm = TRUE)
  netV <- sum(v[i1,j1,1:nl,date_index], na.rm = TRUE)
  cat("i = ", i, "\n")
  cat("netU = ", netU, "\n")
  cat("netV = ", netV, "\n")
  netAreaU <- sum(dz[i1,j1,1:nl,date_index], na.rm = TRUE) * dy[i1,j1]
  netAreaV <- sum(dz[i1,j1,1:nl,date_index], na.rm = TRUE) * dx[i1,j1]
  cat("netAreaU = ", netAreaU, "\n")
  cat("netAreaV = ", netAreaV, "\n")
  velu[i] <- netU / netAreaU
  velv[i] <- netV / netAreaV
  vel[i] <- sqrt(velu[i]**2 + velv[i]**2)

  cat("i = ", i, "\n")
  cat("velv = ", velv[i], "\n")
  cat("velu = ", velu[i], "\n")
  angvel <- atan2(velv[i], velu[i]) * 180 / pi
  if (angvel < 0.0)
  {
    angvel <- angvel + 360
  }
  angle[i] <- angvel

}


df <- data.frame(interval_number = 1:npoints, velocity_meters_per_second = vel, U_Component_Vel = velu, V_Compoment_Vel = velv, direction_degrees = angle)

write.csv(df, file = "Lake_Ontario_Velocities_138_52_2018.csv", row.names = FALSE)

nc_close(nc1)
nc_close(nc2)
nc_close(nc3)
