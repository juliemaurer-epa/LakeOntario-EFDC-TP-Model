library(ggplot2)

# Initialize constant paremeters

# Lake volume is in units of cubic meters (m3).  
# The factor "1E+09" converts km3 to m3.
Vol <- 1631 * 1E+09

# Outflow is in units of m3/day.
# The factor "86400" converts m3/s to m3/day.
Q <- 8000 * 86400

# Surface area is in units of square meters (m2).
# The factor "1E+06" converts km2 to m2.
As <- 18960 * 1E+06

# Settling velocity is in units of m/day.
# The division by "365" converts m/yr to m/day.
v <- 29 / 365

# Fraction of TP that remains in the sediments
# k = 0 corresponds to Chapra's equation
k <- 0

# v prime
vp <- v * (1 - k)

# Annual load is in units of kg/day.
# The factor "1E+03 / 365" converts metric-tons/year to kg/day.
W <- 4500 * 1E+03 / 365

# Initial TP concentration is in units of kg/m3.
# The factor "1E-06" converts ug/L to kg/m3.
p0 <- 6.0 * 1E-06

# Depth is in units of meters.
# This parameter does not appear in Chapra's equation.
Ave_Lake_Depth <- 86

no_years <- 10
no_days <- no_years * 365

# Declare data frame
df <- data.frame(t = array(0.0, dim = c(no_days+1)),
                 TP_conc = array(0.0, dim = c(no_days+1)),
                 TP_conc_ug_L = array(0.0, dim = c(no_days+1)),
                 mass_settled_kg = array(0.0, dim = c(no_days+1)))

# Chapra/Katsev's P equation
j <- 0
for (i in 0:no_days)
{
  j <- j + 1
  df$t[j] <- i
  df$TP_conc[j] <- (W - (W - (Q + vp * As) * p0) * exp(-((Q + vp * As) / Vol) * df$t[j])) / (Q + vp * As)
  df$TP_conc_ug_L[j] <- df$TP_conc[j] * 1E+06
  df$mass_settled_kg[j] <- vp * As * df$TP_conc[j]
}

# Write data frame to CSV file
write.csv(df, file = "TP_conc.csv", row.names = FALSE)

# Plot TP concentration
gg1 <- ggplot(df, aes(x = df$t, y = df$TP_conc_ug_L)) 
gg2 <- gg1 + geom_point() + geom_smooth() 
gg3 <- gg2 + labs(title = "TP concentration vs number of days", x = "Days", y = "ug/L TP")
print(gg3)
