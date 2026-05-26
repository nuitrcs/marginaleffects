# Slide 11

install.packages("marginaleffects")
library(marginaleffects)
mtcars$cyl <- as.factor(mtcars$cyl)

# P(am=1): manual vs automatic
mod <- glm(am ~ hp + wt,
           data = mtcars,
           family = binomial)

# Average Marginal Effect
avg_slopes(mod)

--------------------------------------------------------------------------------

# Slide 14

mod <- glm(am ~ hp + wt + cyl,
           data = mtcars,
           family = binomial)

# Specific hp values; wt at its mean
avg_slopes(mod, variables = "hp",
           newdata = datagrid(hp = c(100, 200)))

# Cross product: 2 hp × 2 wt values
avg_slopes(mod, variables = "hp",
           newdata = datagrid(hp = c(100, 200),
                              wt = c(2.5, 4.0)))

# Marginal effect at the mean
avg_slopes(mod, newdata = "mean")

--------------------------------------------------------------------------------

# Slide 15

mod <- glm(am ~ hp + cyl, data = mtcars, 
           family = binomial(link = "probit"))
           
# Unit-level (one per observation)
slopes(mod, variables = "hp")
           
# Sample average (the AME)
avg_slopes(mod, variables = "hp")
           
# Average by subgroup
avg_slopes(mod, variables = "hp",
                by = "cyl")
           
# Formal equality test
avg_slopes(mod, variables = "hp",
                by = "cyl", hypothesis = "b2 = b3")

--------------------------------------------------------------------------------

# Slide 16
mod <- glm(am ~ hp + wt,
           data = mtcars, family = binomial)

# Default (delta method)
avg_slopes(mod)

# HC3 robust SEs
avg_slopes(mod, vcov = "HC3")

# Clustered SEs
avg_slopes(mod, vcov = ~carb)

# Bootstrap (500 resamples) 
# Draws warnings due to convergence failures, but appears for illustration
avg_slopes(mod) |>
  inferences(method = "boot", R = 500)

--------------------------------------------------------------------------------

# Slide 17

mod <- glm(am ~ hp + wt,
           data = mtcars, family = binomial)

# Equal AMEs for hp and wt?
avg_slopes(mod, hypothesis = "hp = wt")
# Test vs. benchmark (0.003)
avg_slopes(mod, variables = "hp",
           hypothesis = "b1 = 0.003")

# Are subgroup AMEs equal?
avg_slopes(mod, variables = "hp",
           by = "cyl", hypothesis = "b1 = b2")

# Equivalence: within ±0.002?
avg_slopes(mod, variables = "hp",
           equivalence = c(-0.002, 0.002))

--------------------------------------------------------------------------------

# Slide 19

mod <- glm(am ~ hp + wt, data = mtcars,
           family = binomial)

# Predict at specific covariate values
predictions(mod,
            newdata = datagrid(hp = c(100, 200),
                               wt = 3.0))

# Average predicted probability
avg_predictions(mod)

# Comparisons:

# Default: approx. +1 unit change per predictor
avg_comparisons(mod)

# Discrete change: wt from 2.5 to 3.5
avg_comparisons(mod,
                variables = list(wt = c(2.5, 3.5)))

# Ratio instead of difference
avg_comparisons(mod,
                variables = "wt",
                comparison = "ratio")

--------------------------------------------------------------------------------

# Slide 20

mod <- glm(am ~ hp + wt, data = mtcars,
           family = binomial)

# Unit-level slopes
slopes(mod, variables = "hp")

# Average Marginal Effect (AME)
avg_slopes(mod, variables = "hp")

# AME by subgroup
avg_slopes(mod, variables = "hp",
           by = "cyl")

--------------------------------------------------------------------------------

# Slide 21

library(ggplot2)
install.packages("patchwork")   # run once to install
library(patchwork)    # provides the / operator

mod <- glm(am ~ hp + wt, data = mtcars, family = binomial)

# Prediction curve
p1 <- plot_predictions(mod, condition = "hp")

# Slope of hp across its range
p2 <- plot_slopes(mod, variables = "hp", condition = "hp")

# Stack vertically (patchwork syntax)
p1 / p2

--------------------------------------------------------------------------------

# Slide 22

library(marginaleffects)
dat <- get_dataset("thornton")  
mod <- glm(outcome ~ incentive * distance,
           data = dat, family = binomial)

# Q1 Quantity:    slope w.r.t. distance
# Q2 Predictors:  empirical grid (default)
# Q3 Aggregation: average by incentive
# Q4 Uncertainty: cluster by village
# Q5 Test:        are group slopes equal?

avg_slopes(mod,
           variables  = "distance",     # Q1
           newdata    = NULL,           # Q2
           by         = "incentive",    # Q3
           vcov       = ~ village,      # Q4
           hypothesis = "b1 - b2 = 0")  # Q5

--------------------------------------------------------------------------------
  
# Slide 25 (Exercise 1)

library(marginaleffects)
mtcars$cyl <- as.factor(mtcars$cyl)

mod <- glm(am ~ hp + wt + cyl,
           data = mtcars,
           family = binomial)

# Inspect the model
summary(mod)

# Q1 (Predicted probability for a 4-cylinder car weighing 2.5 and 100 hp)
predictions(mod, newdata = datagrid(cyl=4, hp=100, wt=2.5))

# Q2 (Average marginal effect of hp)
avg_slopes(mod, variables = "hp")

# Q3
plot_slopes(mod, variables = "wt",
            condition = "wt")

--------------------------------------------------------------------------------

# Slide 26 (Exercise 2)

mod <- glm(am ~ hp + wt + cyl,
           data = mtcars,
           family = binomial)

# Q1: discrete change in wt
avg_comparisons(mod,
                variables = list(wt = c(2, 2.5)))

# Q2: AME of hp by cylinder
avg_slopes(mod,
           variables = "hp",
           by = "cyl")

# Q3: formal test of equality
avg_slopes(mod,
           variables = "hp",
           by = "cyl",
           hypothesis = "b1 = b3")
