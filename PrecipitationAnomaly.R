# Load libraries
library(tidyverse)
library(lubridate)
library(zoo)
library(mrfDepth)

# Load the CSV files
data <- read_csv("../Data/Covariates/Precip/VDCPrecipitation.csv")
dataB <- read_csv("../Data/Covariates/Precip/BaselineVDCPrecipitation.csv")

# Extract columns
vdcs <- data[[2]]
dates <- as.Date(data[[3]])
values <- data[[4]]
vdcsB <- dataB[[2]]
datesB <- as.Date(dataB[[3]])
valuesB <- dataB[[4]]

# Get unique dates and VDCs and sort them
ascii_sort <- function(x) {
  x[order(sapply(strsplit(x, NULL), function(chars) paste(sprintf("%03d", utf8ToInt(paste(chars, collapse = ""))), collapse = "")))]
} # function to sort vdcs alphabetically, with uppercase letters taking precedence
uniqueDates <- sort(unique(dates))
uniqueDatesB <- sort(unique(datesB))
uniquevdcs <- ascii_sort(unique(vdcs))

# Create date/VDC indices
dateIdx <- match(dates, uniqueDates)
vdcIdx <- match(vdcs, uniquevdcs)
dateIdxB <- match(datesB, uniqueDatesB)
vdcIdxB <- match(vdcsB, uniquevdcs)

# Create 2 matrices of daily precipitation data, one for 2018-2023, the other for baseline
resultMatrix <- matrix(0, nrow = length(uniqueDates), ncol = length(uniquevdcs))
for (i in seq_along(values)) {
  resultMatrix[dateIdx[i], vdcIdx[i]] <- resultMatrix[dateIdx[i], vdcIdx[i]] + values[i]
}
resultMatrix <- t(resultMatrix)  # vdc x Day for 2018-2023

resultMatrix2 <- matrix(0, nrow = length(uniqueDatesB), ncol = length(uniquevdcs))
for (i in seq_along(valuesB)) {
  resultMatrix2[dateIdxB[i], vdcIdxB[i]] <- resultMatrix2[dateIdxB[i], vdcIdxB[i]] + valuesB[i]
}
resultMatrix2 <- t(resultMatrix2)  # vdc x Day for baseline period

# Number of days in a non leap-year
N <- 365

# Function to convert daily data to normalised cumulative daily data
# leap years adjusted for by interpolating from grid of 366 to 365
extract_year_data <- function(mat, start_day, leap = FALSE) {
  if (leap) {
    raw <- mat[, start_day:(start_day + 365)]
    cum <- t(apply(raw, 1, cumsum))
    # Interpolate to 365 days
    interp_days <- approx(1:366, cum[1, ], xout = seq(366/365, 366, length.out = 365))$y
    cum_interp <- t(apply(cum, 1, function(row) approx(1:366, row, xout = seq(366/365, 366, length.out = 365))$y))
    norm <- cum_interp / cum_interp[, 365]
  } else {
    raw <- mat[, start_day:(start_day + 364)]
    cum <- t(apply(raw, 1, cumsum))
    norm <- cum / cum[, 365]
  }
  return(norm)
}

# Run function for each year in study period
PV2018N <- extract_year_data(resultMatrix, 1)
PV2019N <- extract_year_data(resultMatrix, 366)
PV2020N <- extract_year_data(resultMatrix, 731, leap = TRUE)
PV2021N <- extract_year_data(resultMatrix, 1097)
PV2022N <- extract_year_data(resultMatrix, 1462)
PV2023N <- extract_year_data(resultMatrix, 1827)

# Function to convert daily baseline data to normalised cumulative daily data
extract_baseline <- function(mat, leap_years = c(1984, 1988)) {
  years_data <- list()
  start <- 1
  for (i in 1:10) {
    if ((1980 + i) %in% leap_years) {
      # Interpolate to 365 days
      raw <- mat[, start:(start + 365)]
      cum <- t(apply(raw, 1, cumsum))
      interp <- t(apply(cum, 1, function(row) approx(1:366, row, xout = seq(366/365, 366, length.out = 365))$y))
      daily <- t(apply(interp, 1, function(row) c(row[1], diff(row))))
      years_data[[i]] <- daily
      start <- start + 366
    } else {
      raw <- mat[, start:(start + 364)]
      years_data[[i]] <- raw
      start <- start + 365
    }
  }
  base <- Reduce(`+`, years_data)
  cum_base <- t(apply(base, 1, cumsum))
  norm_base <- cum_base / cum_base[, 365]
  return(norm_base)
}

# Run function for baseline period
PVBaseN <- extract_baseline(resultMatrix2)

# Compute difference to baseline for each year of study
AA1 <- t(PV2018N) - t(PVBaseN)
AA2 <- t(PV2019N) - t(PVBaseN)
AA3 <- t(PV2020N) - t(PVBaseN)
AA4 <- t(PV2021N) - t(PVBaseN)
AA5 <- t(PV2022N) - t(PVBaseN)
AA6 <- t(PV2023N) - t(PVBaseN)

# Functional halfspace depth by VDC for each year of study
p<- 3976 # number of vdcs
FD <- array(0, dim = c(N-1, p, 1))
FD[,,1] <- AA1[1:N-1,]
PREPO1 <- mfd(FD)
FD[,,1] <- AA2[1:N-1,]
PREPO2 <- mfd(FD)
FD[,,1] <- AA3[1:N-1,]
PREPO3 <- mfd(FD)
FD[,,1] <- AA4[1:N-1,]
PREPO4 <- mfd(FD)
FD[,,1] <- AA5[1:N-1,]
PREPO5 <- mfd(FD)
FD[,,1] <- AA6[1:N-1,]
PREPO6 <- mfd(FD)
# aggregate all halfspace depths for final metric
VDCPRANOM <- PREPO1$MFDdepthX + PREPO2$MFDdepthX + PREPO3$MFDdepthX + PREPO4$MFDdepthX + PREPO5$MFDdepthX + PREPO6$MFDdepthX
write.csv(VDCPRANOM,file="VDCPRANOM.csv",row.names=F)
write.csv(uniquevdcs,file="VDCnames.csv",row.names=F)

# Figure S5a
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))  # adjust margins as needed
# Plot 1: Normalized cumulative precipitation for 2019
matplot(t(PV2019N), type = "l", lty = 1, xlab = "Day of year", 
        ylab = "Normalized Cumulative Precipitation", main = "2019")
# Plot 2: Baseline normalized cumulative precipitation
matplot(t(PVBaseN), type = "l", lty = 1, xlab = "Day of year", 
        ylab = "Normalized Cumulative Precipitation", main = "Baseline (1981-1990)")
# Plot 3: Difference 2019 vs Baseline
matplot(AA2, type = "l", lty = 1, xlab = "Day of year", ylab = "Difference", 
        main = "2019 vs Baseline")
# Plot 4: Highlight outliers based on PREPO2$MFDdepthX
matplot(AA2, type = "l", col = "black", xlab = "Day of year", 
        ylab = "Difference", main = "red = most outlying, green = least outlying")
lines(AA2[, which.min(PREPO2$MFDdepthX)], col = "red", lwd = 2)
lines(AA2[, which.max(PREPO2$MFDdepthX)], col = "green", lwd = 2)

# Figure S5b
par(mfrow = c(2, 3), mar = c(4, 4, 2, 1))  # adjust margins as needed
# Plot 1: 2018
matplot(AA1, type = "l", col = "black", xlab = "Day of year", 
        ylab = "Difference", main = "2018")
lines(AA1[, which.min(PREPO1$MFDdepthX)], col = "red", lwd = 2)
lines(AA1[, which.max(PREPO1$MFDdepthX)], col = "green", lwd = 2)
# Plot 2: 2019
matplot(AA2, type = "l", col = "black", xlab = "Day of year", 
        ylab = "Difference", main = "2019")
lines(AA2[, which.min(PREPO2$MFDdepthX)], col = "red", lwd = 2)
lines(AA2[, which.max(PREPO2$MFDdepthX)], col = "green", lwd = 2)
# Plot 3: 2020
matplot(AA3, type = "l", col = "black", xlab = "Day of year", 
        ylab = "Difference", main = "2020")
lines(AA3[, which.min(PREPO3$MFDdepthX)], col = "red", lwd = 2)
lines(AA3[, which.max(PREPO3$MFDdepthX)], col = "green", lwd = 2)
# Plot 4: 2021
matplot(AA4, type = "l", col = "black", xlab = "Day of year", 
        ylab = "Difference", main = "2021")
lines(AA4[, which.min(PREPO4$MFDdepthX)], col = "red", lwd = 2)
lines(AA4[, which.max(PREPO4$MFDdepthX)], col = "green", lwd = 2)
# Plot 5: 2022
matplot(AA5, type = "l", col = "black", xlab = "Day of year", 
        ylab = "Difference", main = "2022")
lines(AA5[, which.min(PREPO5$MFDdepthX)], col = "red", lwd = 2)
lines(AA5[, which.max(PREPO5$MFDdepthX)], col = "green", lwd = 2)
# Plot 6: 2023
matplot(AA6, type = "l", col = "black", xlab = "Day of year", 
        ylab = "Difference", main = "2023")
lines(AA6[, which.min(PREPO6$MFDdepthX)], col = "red", lwd = 2)
lines(AA6[, which.max(PREPO6$MFDdepthX)], col = "green", lwd = 2)
