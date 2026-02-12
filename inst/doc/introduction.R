## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7, 
  fig.height = 3.5
)

## ----setup--------------------------------------------------------------------
library(sshist)

## ----comparision--------------------------------------------------------------
data(faithful)
x_data <- faithful$eruptions

# 1. Standard R histogram (Sturges rule by default)
par(mfrow = c(1, 2))
hist(x_data, main = "Standard hist()", xlab = "Duration of Eruptions", col = "gray90")

# 2. Shimazaki-Shinomoto Optimization
# The function returns an S3 object with optimal parameters
res <- sshist(x_data)
hist(x_data, breaks=res$edges,
       main=paste("Optimal Hist (N=", res$opt_n, ")"),
       xlab = "Duration of Eruptions", col = "gray90")

par(mfrow = c(1, 1))

## ----optimal params-----------------------------------------------------------
print(res)

# The plot method shows the Cost Function Graph and the Optimal Histogram
plot(res)

## ----sshist_2d----------------------------------------------------------------
# Run 2D optimization
# This calculates the cost function for a grid of (Nx, Ny) combinations
res_2d <- sshist_2d(iris$Petal.Length, iris$Petal.Width)

# optimal number of bins
print(res_2d)

# The plot method shows the Cost Function Landscape and the Optimal Histogram
plot(res_2d)

