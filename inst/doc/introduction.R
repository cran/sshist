## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  fig.width = 7
)

## ----setup, message=FALSE, warning=FALSE--------------------------------------
library(sshist)
options(sshist.ncores = 2) # Use 2 cores for vignette building

## ----comparison, fig.asp = 0.5------------------------------------------------
data(faithful)
x_data <- faithful$eruptions

oldpar <- par(mfrow = c(1, 2))

# Standard R histogram (Sturges rule)
hist(x_data,
     main = "Standard hist()  (Sturges)",
     xlab = "Eruption duration (min)", col = "grey90")

# Shimazaki-Shinomoto optimal histogram
res <- sshist(x_data)
hist(x_data, breaks = res$edges,
     main = paste0("sshist()  (N = ", res$opt_n, " bins)"),
     xlab = "Eruption duration (min)", col = "grey90")

par(oldpar)

## ----sshist-print-------------------------------------------------------------
print(res)
plot(res)

## ----sskernel, fig.asp = 0.6--------------------------------------------------
res_k <- sskernel(x_data)
print(res_k)
plot(res_k, xlab = "Eruption duration (min)")

## ----ssvkernel, fig.asp = 0.6-------------------------------------------------
res_sv <- ssvkernel(x_data)
print(res_sv)
plot(res_sv, xlab = "Eruption duration (min)")

## ----bootstrap-example, fig.asp = 0.6-----------------------------------------
# Run 300 bootstrap iterations using 2 cores
res_boot <- sskernel(x_data, nbs = 300)
print(res_boot)

# The plot function automatically detects and renders the confb95 band
plot(res_boot, 
     main = "Optimal KDE with 90% Bootstrap Confidence Band", 
     xlab = "Eruption duration (min)",
     col = "#d6604d", 
     band_col = adjustcolor("#d6604d", alpha.f = 0.2))

res_sv_boot <- ssvkernel(x_data, nbs = 300, WinFunc = "Gauss")

print(res_sv_boot)

# The plot function automatically detects and renders the confb95 band
plot(res_sv_boot, 
     main = "Locally Adaptive KDE with 90% Bootstrap Confidence Band", 
     xlab = "Eruption duration (min)",
     col = "#d6604d", 
     band_col = adjustcolor("#d6604d", alpha.f = 0.2))

## ----sshist-2d, fig.asp = 0.85------------------------------------------------
res_2d <- sshist_2d(faithful$waiting, faithful$eruptions)
print(res_2d)

# plot() renders the optimal 2D histogram as a heatmap
plot(res_2d, xlab = "Waiting time (min)", ylab = "Eruption duration (min)")

## ----kde2d-fixed, fig.asp = 1-------------------------------------------------
df <- faithful   # eruptions: ~2-5 min,  waiting: ~40-90 min

# Fixed global bandwidth (C++ accelerated)
res_2d_fixed <- sskernel2d(df$eruptions, df$waiting, n_grid = 256)
print(res_2d_fixed)
plot(res_2d_fixed,
     xlab = "Eruption duration (min)",
     ylab = "Waiting time (min)")

## ----kde2d-adap, fig.asp = 1--------------------------------------------------
# Locally adaptive bandwidth (Abramson's method)
res_2d_adap <- ssvkernel2d(df$eruptions, df$waiting,
                            n_grid = 256, sensitivity = 0.5)
print(res_2d_adap)
plot(res_2d_adap,
     xlab = "Eruption duration (min)",
     ylab = "Waiting time (min)")

