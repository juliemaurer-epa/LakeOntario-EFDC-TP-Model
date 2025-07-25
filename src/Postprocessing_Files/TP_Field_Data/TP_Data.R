library(lubridate)
library(ncdf4)
library(dplyr) 

factor_ug <- 1000000

# Input files
file1 <- "/work/GLFBREEZ/Lake_Ontario/TP_Field_Data/GLNPO_2013_2018_ID_ijk_Version2_9_3_2020.csv"
file2 <- "/work/GLFBREEZ/Lake_Ontario/TP_Field_Data/LOLA2013id2_ijk.csv"
file3 <- "/work/GLFBREEZ/Lake_Ontario/TP_Field_Data/LONNS_2013_Makarewicz_ID_ijk.csv"
file4 <- "/work/GLFBREEZ/Lake_Ontario/TP_Field_Data/all_ECC_13_18TP_ID_ijk.csv"
file5 <- "/work/GLFBREEZ/Lake_Ontario/TP_Field_Data/CSMI_WQ2018LOLA2_ijk.csv"
file6 <- "/work/GLFBREEZ/Lake_Ontario/TP_Field_Data/LONNS2018_CSMI_ID_ijk.csv"
file7 <- "/work/GLFBREEZ/Lake_Ontario/TP_Field_Data/USGSCladophorWQ2018_UTM17_ID_ijk.csv"

# Output directory
output_folder <- "/work/GLFBREEZ/Lake_Ontario/TP_Field_Data"

# Name of combined output file
combined_file <- "Lake_Ontario_TP_Data_Combined_File.csv"

# Read CSV files
df1 <- read.csv(file = file1, header = TRUE, stringsAsFactors = FALSE)
df2 <- read.csv(file = file2, header = TRUE, stringsAsFactors = FALSE)
df3 <- read.csv(file = file3, header = TRUE, stringsAsFactors = FALSE)
df4 <- read.csv(file = file4, header = TRUE, stringsAsFactors = FALSE)
df5 <- read.csv(file = file5, header = TRUE, stringsAsFactors = FALSE)
df6 <- read.csv(file = file6, header = TRUE, stringsAsFactors = FALSE)
df7 <- read.csv(file = file7, header = TRUE, stringsAsFactors = FALSE)

# Define data frames
df1[df1$I_Value == 0 | df1$J_Value == 0, ] <- NA
dfA <- data.frame(Date = df1$SAMPLING_D,
                  JulianDate = NA,
                  Latitude = df1$LATITUDE,
                  Longitude = df1$LONGITUDE,
                  I_Index = df1$I_Value, 
                  J_Index = df1$J_Value, 
                  K_Index = df1$sigma_layer,
                  Number_Layers = df1$Num_layers,
                  Sigma_Layer_Thickness = df1$sig_thick,
                  TP = df1$TP,
                  Sample_Depth = df1$SAMPLE_DEP,
                  Water_Depth = df1$Wat_depth,
                  stringsAsFactors = FALSE)
dfA$Date <- mdy(dfA$Date)
dfA$JulianDate <- as.numeric(as.POSIXct(dfA$Date))
dfA <- subset(dfA, !is.na(dfA$I_Index))

df2[df2$TP_ug_L < 1.0E-12 | df2$Depth_m < 1.0E-12 | df2$I_index == 0 | df2$J_Index == 0, ] <- NA
df2 <- subset(df2, !is.na(df2$TP_ug_L) | !is.na(df2$Depth_m))
dfB <- data.frame(Date = df2$Date, 
                  JulianDate = NA,
                  Latitude = df2$Lat,
                  Longitude = df2$Long,
                  I_Index = df2$I_index, 
                  J_Index = df2$J_Index, 
                  K_Index = df2$Sig_layer.K,
                  Number_Layers = df2$Number_Lay,
                  Sigma_Layer_Thickness = df2$sig_thick,
                  TP = df2$TP_ug_L,
                  Sample_Depth = df2$Depth_m,
                  Water_Depth = df2$Water_Dept,
                  stringsAsFactors = FALSE)
dfB$Date <- mdy(dfB$Date)
dfB$JulianDate <- as.numeric(as.POSIXct(dfB$Date))
# dfB <- subset(dfB, !is.na(dfB$TP))

dfC <- data.frame(Date = df3$Date_, 
                  JulianDate = NA,
                  Latitude = df3$Lat,
                  Longitude = df3$Long2,
                  I_Index = df3$I_index, 
                  J_Index = df3$J_Index, 
                  K_Index = df3$Sig_layer.K,
                  Number_Layers = df3$Number_Lay,
                  Sigma_Layer_Thickness = df3$sig_thick,
                  TP = df3$TP_ug_L,
                  Sample_Depth = df3$Smpl_Dpth_,
                  Water_Depth = df3$Water_Dept,
                  stringsAsFactors = FALSE)
dfC$Date <- mdy(dfC$Date)
dfC$JulianDate <- as.numeric(as.POSIXct(dfC$Date))

dfD <- data.frame(Date = df4$STN_DATE, 
                  JulianDate = NA,
                  Latitude = df4$LAT,
                  Longitude = df4$LONG,
                  I_Index = df4$I_index, 
                  J_Index = df4$J_Index, 
                  K_Index = df4$sig_Layer_K,
                  Number_Layers = df4$Number_Layers,
                  Sigma_Layer_Thickness = df4$sig_thick,
                  TP = df4$TP__ug_L_,
                  Sample_Depth = df4$DEPTH__m_,
                  Water_Depth = df4$Water_Depth_meters,
                  stringsAsFactors = FALSE)
dfD$Date <- gsub(" 0.00", "", dfD$Date)
dfD$Date <- mdy(dfD$Date)
dfD$JulianDate <- as.numeric(as.POSIXct(dfD$Date))
dfD <- subset(dfD, !is.na(dfD$I_Index) & !is.na(dfD$J_Index))

dfE <- data.frame(Date = df5$Collection, 
                  JulianDate = NA,
                  Latitude = df5$LatitudeDD,
                  Longitude = df5$LongitudeD,
                  I_Index = df5$I_Value, 
                  J_Index = df5$J_Value, 
                  K_Index = df5$Sig_layer.K,
                  Number_Layers = df5$Number_Lay,
                  Sigma_Layer_Thickness = df5$sig_thick,
                  TP = df5$TP_ug,
                  Sample_Depth = df5$Ave_dep,
                  Water_Depth = df5$Water_Dept,
                  stringsAsFactors = FALSE)
dfE$Date <- mdy(dfE$Date)
dfE$JulianDate <- as.numeric(as.POSIXct(dfE$Date))

dfF <- data.frame(Date = df6$Collection, 
                  JulianDate = NA,
                  Latitude = df6$LatitudeDD,
                  Longitude = df6$LongitudeD,
                  I_Index = df6$I_Value, 
                  J_Index = df6$J_Value, 
                  K_Index = df6$Sig_layer.K,
                  Number_Layers = df6$Number_Lay,
                  Sigma_Layer_Thickness = df6$sig_thick,
                  TP = df6$TotalPhosp,
                  Sample_Depth = df6$Depths,
                  Water_Depth = df6$Water_Dept,
                  stringsAsFactors = FALSE)
dfF$Date <- mdy(dfF$Date)
dfF$JulianDate <- as.numeric(as.POSIXct(dfF$Date))

n7 <- nrow(df7)
for (i in 1:n7)
{
  if (is.na(df7$TP_ug_PL[i]) || df7$TP_ug_PL[i] == "BDL")
  {
    df7$TP_ug_PL[i] <- NA
  }
  if (df7$I_Value[i] == 0 || df7$J_Value[i] == 0)
  {
    df7$I_Value[i] <- NA
    df7$J_Value[i] <- NA
    df7$sigma_Layer[i] <- NA
  }
}

dfG <- data.frame(Date = df7$Collection, 
                  JulianDate = NA,
                  Latitude = df7$Latitude,
                  Longitude = df7$Longitude,
                  I_Index = df7$I_Value, 
                  J_Index = df7$J_Value, 
                  K_Index = as.numeric(df7$sigma_Layer),
                  Number_Layers = df7$Num_layers,
                  Sigma_Layer_Thickness = df7$sig_thick,
                  TP = as.numeric(df7$TP_ug_PL),
                  Sample_Depth = df7$Depths_m,
                  Water_Depth = df7$Wat_depth,
                  stringsAsFactors = FALSE)
dfG$Date <- mdy(dfG$Date)
dfG$JulianDate <- as.numeric(as.POSIXct(dfG$Date))
dfG <- subset(dfG, !is.na(dfG$TP) & !is.na(dfG$I_Index) & !is.na(dfG$J_Index))

df <- rbind(dfA, dfB, dfC, dfD, dfE, dfF, dfG)

write.csv(df, file = combined_file, row.names = FALSE)

InputFile <- "/work/GLFBREEZ/Lake_Ontario/Model_Runs/2018/LO_10/NETCDF/gomdom.000000.nc"

nc <- nc_open(InputFile)
stime <- ncvar_get(nc, varid = "time")
tp <- ncvar_get(nc, varid = "TR")

maxDate <- max(stime)
minDate <- min(stime)

nc_close(nc)

# Only use field data within model run time period
df <- df[df$JulianDate >= minDate,]
df <- df[df$JulianDate <= maxDate,]

#################################################################################
#  Generate modeled vs measured TP plots
#################################################################################
TP_title <- file.path("P_MvM_TP", "LO_10", fsep = "_")
TP_title1 <- file.path(output_folder, TP_title, fsep = "/")
TP_title2 <- file.path(TP_title1, "png", fsep = ".")

# TP
png(TP_title2, height = 850, width = 850, res = 100)

nTP <- length(df$TP)

modelvalue_TP  <- c(1:nTP)

for (i in 1:nTP)
{
  # Find index of closest model output date to each field data date
  date_index <- which.min(abs(df$JulianDate[i] - stime))
  
  # Find modeled value closest to field data time and location
  modelvalue_TP[i] <- tp[df$I_Index[i], df$J_Index[i], df$K_Index[i], date_index] 
}

modeled_TP <- modelvalue_TP * factor_ug
measured_TP <- df$TP

dfOut <- df
dfOut$Modeled_TP <- modeled_TP
write.csv(dfOut, file = "Modeled_vs_Measured_TP_2018_LO_10.csv", row.names = FALSE)

max_TP <- max(modeled_TP, measured_TP) * 1.1

title <- file.path("Lake Ontario TP Model: Modeled vs. Measured", "Total P", "2018", fsep = ", ")
plot(modeled_TP,
     measured_TP, 
     xlab = "Modeled Value, ug/L", 
     ylab = "Measured Value, ug/L", 
     main = title, 
     xlim=c(0, max_TP), 
     ylim=c(0, max_TP))
abline(0,1) 

# Adding Statistics To Plot
# sets up a data frame to calculate the statistics for the model vs. data plot
stats <- data.frame(BIAS = rep(NA, 1),
                    BIAS_PERC = rep(NA, 1),
                    RAE = rep(NA, 1),
                    RSQ = rep(NA, 1),
                    RMSE = rep(NA, 1),
                    MEF = rep(NA, 1),
                    R_SP = rep(NA, 1),
                    N = rep(NA, 1)
)

ndata <- length(measured_TP)


# Calculating the statistics
bias <- sum(modeled_TP  - measured_TP) / ndata
stats$BIAS[1] <- format(round(bias,2), nsmall=2)

bias_perc <- sum(modeled_TP - measured_TP) / abs(sum(measured_TP)) * 100
stats$BIAS_PERC[1] <- format(round(bias_perc,2), nsmall=2)

rae <- sum(abs(modeled_TP - measured_TP)) / ndata
stats$RAE[1] <- format(round(rae,2), nsmall=2)

rsq <- cor(measured_TP, modeled_TP)^2
stats$RSQ[1] <- format(round(rsq,2), nsmall = 2)

r_sp <- cor(measured_TP, modeled_TP, 
            method = c("spearman"), 
            use = "na.or.complete")
stats$R_SP[1] <- format(round(r_sp,2), nsmall = 2)

error <- (modeled_TP - measured_TP)^2
rmse <- sqrt(sum(error) / ndata)
stats$RMSE[1] <- format(round(rmse,2), nsmall = 2)

mef <- 1 - sum(error) / ( sum( (measured_TP - mean(measured_TP))^2 ) )
stats$MEF[1] <- format(round(mef,2), nsmall = 2)

n <- ndata
stats$N[1] <- format(round(n,0), nsmall = 0)

mtext(paste("Mean Bias ", stats$BIAS[1]),
      side = 1, 
      adj = 0.98, 
      line = -10, 
      cex = 1.0, 
      col = "blue")

mtext(paste("Normalized % Bias ", stats$BIAS_PERC[1]), 
      side = 1, 
      adj = 0.98, 
      line = -9, 
      cex = 1.0, 
      col = "blue")

mtext(paste("RAE ",stats$RAE[1]),
      side = 1,
      adj = 0.98,
      line = -8,
      cex = 1.0, 
      col = "blue")

mtext(paste("RSQ ",stats$RSQ[1]),
      side = 1,
      adj = 0.98,
      line = -7,
      cex = 1.0, 
      col = "blue")

mtext(paste("RMSE ",stats$RMSE[1]),
      side = 1,
      adj = 0.98,
      line = -6,
      cex = 1.0, 
      col = "blue")

mtext(paste("MEF ",stats$MEF[1]),
      side = 1,
      adj = 0.98,
      line = -5,
      cex = 1.0, 
      col = "blue")

mtext(paste("Spearman R ", stats$R_SP[1]), 
      side = 1, 
      adj = 0.98, 
      line = -4, 
      cex = 1.0, 
      col = "blue")

mtext(paste("N ",stats$N[1]),
      side = 1,
      adj = 0.98,
      line = -3,
      cex = 1.0, 
      col = "blue")

dev.off()

################################################################################
#  Generate modeled vs measured TP plots for grid cell locations < 20 m.
################################################################################
maxDepth <- 20
dfWD <- df[df$Water_Depth < maxDepth,]

TP_title <- file.path("P_MvM_20m_TP", "LO_10", fsep = "_")
TP_title1 <- file.path(output_folder, TP_title, fsep = "/")
TP_title2 <- file.path(TP_title1, "png", fsep = ".")

# TP
png(TP_title2, height = 850, width = 850, res = 100)

nTP <- length(dfWD$TP)

modelvalue_TP  <- c(1:nTP)

for (i in 1:nTP)
{
  # Find index of closest model output date to each field data date
  date_index <- which.min(abs(dfWD$JulianDate[i] - stime))
  
  # Find modeled value closest to field data time and location
  modelvalue_TP[i] <- tp[dfWD$I_Index[i], dfWD$J_Index[i], dfWD$K_Index[i], date_index] 
}

modeled_TP <- modelvalue_TP * factor_ug
measured_TP <- dfWD$TP

dfWDOut <- dfWD
dfWDOut$Modeled_TP <- modeled_TP
write.csv(dfWDOut, file = "Modeled_vs_Measured_TP_20m_2018_LO_10.csv", row.names = FALSE)

max_TP <- max(modeled_TP, measured_TP) * 1.1

title <- file.path("Lake Ontario TP Model: Modeled vs. Measured", "Total P", "2018", fsep = ", ")
plot(modeled_TP,
     measured_TP, 
     xlab = "Modeled Value, ug/L", 
     ylab = "Measured Value, ug/L", 
     main = title, 
     xlim=c(0, max_TP), 
     ylim=c(0, max_TP))
abline(0,1) 

# Adding Statistics To Plot
# sets up a data frame to calculate the statistics for the model vs. data plot
stats <- data.frame(BIAS = rep(NA, 1),
                    BIAS_PERC = rep(NA, 1),
                    RAE = rep(NA, 1),
                    RSQ = rep(NA, 1),
                    RMSE = rep(NA, 1),
                    MEF = rep(NA, 1),
                    R_SP = rep(NA, 1),
                    N = rep(NA, 1)
)

ndata <- length(measured_TP)


# Calculating the statistics
bias <- sum(modeled_TP  - measured_TP) / ndata
stats$BIAS[1] <- format(round(bias,2), nsmall=2)

bias_perc <- sum(modeled_TP - measured_TP) / abs(sum(measured_TP)) * 100
stats$BIAS_PERC[1] <- format(round(bias_perc,2), nsmall=2)

rae <- sum(abs(modeled_TP - measured_TP)) / ndata
stats$RAE[1] <- format(round(rae,2), nsmall=2)

rsq <- cor(measured_TP, modeled_TP)^2
stats$RSQ[1] <- format(round(rsq,2), nsmall = 2)

r_sp <- cor(measured_TP, modeled_TP, 
            method = c("spearman"), 
            use = "na.or.complete")
stats$R_SP[1] <- format(round(r_sp,2), nsmall = 2)

error <- (modeled_TP - measured_TP)^2
rmse <- sqrt(sum(error) / ndata)
stats$RMSE[1] <- format(round(rmse,2), nsmall = 2)

mef <- 1 - sum(error) / ( sum( (measured_TP - mean(measured_TP))^2 ) )
stats$MEF[1] <- format(round(mef,2), nsmall = 2)

n <- ndata
stats$N[1] <- format(round(n,0), nsmall = 0)

mtext(paste("Mean Bias ", stats$BIAS[1]),
      side = 1, 
      adj = 0.98, 
      line = -10, 
      cex = 1.0, 
      col = "blue")

mtext(paste("Normalized % Bias ", stats$BIAS_PERC[1]), 
      side = 1, 
      adj = 0.98, 
      line = -9, 
      cex = 1.0, 
      col = "blue")

mtext(paste("RAE ",stats$RAE[1]),
      side = 1,
      adj = 0.98,
      line = -8,
      cex = 1.0, 
      col = "blue")

mtext(paste("RSQ ",stats$RSQ[1]),
      side = 1,
      adj = 0.98,
      line = -7,
      cex = 1.0, 
      col = "blue")

mtext(paste("RMSE ",stats$RMSE[1]),
      side = 1,
      adj = 0.98,
      line = -6,
      cex = 1.0, 
      col = "blue")

mtext(paste("MEF ",stats$MEF[1]),
      side = 1,
      adj = 0.98,
      line = -5,
      cex = 1.0, 
      col = "blue")

mtext(paste("Spearman R ", stats$R_SP[1]), 
      side = 1, 
      adj = 0.98, 
      line = -4, 
      cex = 1.0, 
      col = "blue")

mtext(paste("N ",stats$N[1]),
      side = 1,
      adj = 0.98,
      line = -3,
      cex = 1.0, 
      col = "blue")

dev.off()