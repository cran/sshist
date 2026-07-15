## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse  = TRUE,
  comment   = "#>",
  fig.width = 7,
  fig.align = "center"
)

## ----setup, message=FALSE, warning=FALSE--------------------------------------
library(sshist)
options(sshist.ncores = 2)

has_ggplot2 <- requireNamespace("ggplot2", quietly = TRUE)
if (has_ggplot2) {
  library(ggplot2)
  library(patchwork)
  theme_set(theme_minimal(base_size = 12))
}

## ----ggplot-sshist, fig.height = 4, eval = has_ggplot2------------------------
data(faithful)

# 1. Calculate optimal binning parameters
res_hist <- sshist(faithful$waiting)

# 2. Plot using raw data but mapping to the optimized breaks
ggplot(faithful, aes(x = waiting)) +
  geom_histogram(
    breaks = res_hist$edges,
    fill = "steelblue",
    color = "white",
    alpha = 0.85,
    aes(y = after_stat(density))
  ) +
  geom_rug(alpha = 0.5, color = "slategrey") +
  labs(
    title = "Optimal 1D Histogram (Shimazaki-Shinomoto)",
    subtitle = sprintf("Optimized bins: %d | Bin width: %.2f", res_hist$opt_n, res_hist$opt_d),
    x = "Waiting time (minutes)",
    y = "Density"
  )

## ----ggplot-kde-1d, fig.height = 4, eval = has_ggplot2------------------------
# Define a shared grid for perfect alignment
shared_grid <- seq(min(faithful$waiting), max(faithful$waiting), length.out = 500)

# Run fixed and adaptive estimators
res_fixed <- sskernel(faithful$waiting, tin = shared_grid)
res_adaptive <- ssvkernel(faithful$waiting, tin = shared_grid)

# Create a tidy data frame for density lines
df_density <- data.frame(
  time      = c(res_fixed$x, res_adaptive$x),
  density   = c(res_fixed$y, res_adaptive$y),
  Estimator = rep(c("Fixed Global", "Locally Adaptive"), each = length(shared_grid))
)

# Visualize the comparison
ggplot(df_density, aes(x = time, y = density, color = Estimator)) +
  geom_line(linewidth = 1) +
  geom_area(aes(fill = Estimator), alpha = 0.1, position = "identity") +
  scale_color_manual(values = c("Fixed Global" = "#4575b4", "Locally Adaptive" = "#d73027")) +
  scale_fill_manual(values = c("Fixed Global" = "#4575b4", "Locally Adaptive" = "#d73027")) +
  labs(
    title = "1D KDE Comparison: Fixed vs. Adaptive",
    x = "Waiting time (minutes)", 
    y = "Density"
  ) +
  theme(legend.position = "top")

## ----ggplot-winfunc-comparison, fig.height = 4, eval = has_ggplot2------------
# Run adaptive estimators with all available window functions
res_boxcar  <- ssvkernel(faithful$waiting, tin = shared_grid, WinFunc = "Boxcar")
res_gauss   <- ssvkernel(faithful$waiting, tin = shared_grid, WinFunc = "Gauss")
res_laplace <- ssvkernel(faithful$waiting, tin = shared_grid, WinFunc = "Laplace")
res_cauchy  <- ssvkernel(faithful$waiting, tin = shared_grid, WinFunc = "Cauchy")

# Create a tidy data frame for plotting
df_winfunc <- data.frame(
  time    = rep(shared_grid, 4),
  density = c(res_boxcar$y, res_gauss$y, res_laplace$y, res_cauchy$y),
  Window  = factor(
    rep(c("Boxcar", "Gauss", "Laplace", "Cauchy"), each = length(shared_grid)),
    levels = c("Boxcar", "Gauss", "Laplace", "Cauchy")
  )
)

# Visualize the effect of different weight functions
ggplot(df_winfunc, aes(x = time, y = density, color = Window)) +
  geom_line(linewidth = 0.8, alpha = 0.85) +
  scale_color_viridis_d(option = "plasma", end = 0.9) +
  labs(
    title = "Effect of Window Functions on Adaptive KDE",
    subtitle = "Comparing Boxcar, Gauss, Laplace, and Cauchy weight distributions",
    x = "Waiting time (minutes)",
    y = "Density"
  ) +
  theme(legend.position = "right")

## ----ggplot-sshist2d, fig.width = 6, fig.height = 6, eval = has_ggplot2-------
res_hist2d <- sshist_2d(faithful$eruptions, faithful$waiting)

# Extract optimal bin counts or widths if explicit
# Since ggplot2 handles bins natively, we supply the calculated breaks or counts
ggplot(faithful, aes(x = eruptions, y = waiting)) +
  stat_bin2d(
    breaks = list(
      x = seq(min(faithful$eruptions), max(faithful$eruptions), length.out = res_hist2d$opt_nx + 1L),
      y = seq(min(faithful$waiting), max(faithful$waiting), length.out = res_hist2d$opt_ny + 1L)
    ),
    color = "black",
    linewidth = 0.2
  ) +
  scale_fill_distiller(palette = "YlGnBu", direction = -1) +
  labs(
    title = "Optimal 2D Histogram (Old Faithful)",
    subtitle = sprintf("Grid: %d \u00d7 %d bins", res_hist2d$opt_nx, res_hist2d$opt_ny),
    x = "Eruption duration (min)",
    y = "Waiting time (min)",
    fill = "Count"
  )

## ----ggplot-kde2d, fig.width = 11, fig.height = 5.5, eval = has_ggplot2-------
# Compute densities on a 150x150 evaluation grid
grid_res <- 150
res_2d_fixed <- sskernel2d(faithful$waiting, faithful$eruptions, n_grid = grid_res)
res_2d_adap  <- ssvkernel2d(faithful$waiting, faithful$eruptions, n_grid = grid_res)

# Helper function to wrangle the list output into a tidy data frame
wrangle_2d <- function(obj) {
  df <- expand.grid(x = obj$x_grid, y = obj$y_grid)
  df$density <- as.vector(obj$z) 
  return(df)
}

df_fixed <- wrangle_2d(res_2d_fixed)
df_adap  <- wrangle_2d(res_2d_adap)

# Plot 1: Fixed Isotropic KDE
p1 <- ggplot(df_fixed, aes(x = x, y = y, fill = density)) +
  geom_raster(interpolate = TRUE) +
  scale_fill_viridis_c(option = "mako") +
  labs(title = "Fixed Global Bandwidth (sskernel2d)", x = "Waiting time (min)", y = "Eruption duration (min)") +
  theme(legend.position = "none")

# Plot 2: Locally Adaptive KDE
p2 <- ggplot(df_adap, aes(x = x, y = y, fill = density)) +
  geom_raster(interpolate = TRUE) +
  scale_fill_viridis_c(option = "mako") +
  labs(title = "Locally Adaptive Bandwidth (ssvkernel2d)", x = "Waiting time (min)", y = "Eruption duration (min)", fill = "Density")

# Combine side by side with a shared legend
p1 + p2 + plot_layout(guides = "collect")

## ----fallback, eval = !has_ggplot2, echo = FALSE, results = "asis"------------
# cat("**Note:** The figures in this vignette require the **ggplot2** and **patchwork** packages, which are not available in this environment. Install them with `install.packages(c(\"ggplot2\", \"patchwork\"))` and rebuild the vignette.")

