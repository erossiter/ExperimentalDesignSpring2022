library(DeclareDesign)

# Example drawing from Table 2.1 in GG
# N villages
# Outcome is the budget share of the village
# Potential outcomes are the budget share if the village head is a man Y_i(0) or woman Y_i(1)
set.seed(522)
N <- 100
control_mean <- 15
control_sd <- 2
treatment_mean <- 20
treatment_sd <- 2

population <- declare_population(N = N,
                                 u_0 = rnorm(N)*control_sd,
                                 u_1 = rnorm(N)*treatment_sd)

potential_outcomes <- declare_potential_outcomes(Y ~ + Z * (u_1 + treatment_mean)
                                                     + (1 - Z) * (u_0 + control_mean))

estimand <- declare_estimand(ATE = mean(Y_Z_1) - mean(Y_Z_0))

assignment <- declare_assignment(Z = complete_ra(N, prob = .5))

reveal_Y <- declare_reveal() # the switching equation

estimator <- declare_estimator(Y ~ Z,
                               model = estimatr::difference_in_means,
                               inquiry = "ATE")

design1 <- population + potential_outcomes + estimand + assignment + reveal_Y + estimator

# Recall from last time, we can draw a hypothetical experiment
# Now we see Z and Y as added columns
# Which columns are observed and which are unobserved?
single_draw <- draw_data(design1)
single_draw

# We can also use our design object to draw estimates from the
# hypothetical experiment using our declared estimator
# Note: this function completes all prior steps in the design, too,
# so it includes the drawing data step and just adds estimation.
single_est <- draw_estimates(design1)
single_est


# Exercises:
# 1. Describe why the ATE is a useful or interesting estimand in your own words.


# 2. When we "declare" each step, we make a function.  So, above we created
# a function that calculates our declared estimand for any
# single draw of the data.  See how I use our function `estimand()` below.
#
# Task:
# Given what you know about the ATE as an estimand,
# write your own code to calculate it from the `single_draw` dataset.  You'll
# know if you get it right because it should match the results of the `estimand()`
# function!
set.seed(1234) # make sure to run these lines so we're all working with the same data
single_draw <- draw_data(design1)
estimand(single_draw)
mean(single_draw$Y_Z_1 - single_draw$Y_Z_0)

# 3. As I said in #2, we create functions when we "declare" a step, so we can
# calculate our estimate as well since we've added that step!
#
# Task:
# How close is the estimate to the estimand from our hypothetical experiment?
# In your own words, why are these quantities not identical?
estimator(single_draw)
estimator(single_draw)$estimate #estimate
estimand(single_draw) #estimand
# Our estimate is off by about .8
# We're trying our best to calculate the true ATE, but we're only guaranteed that
# *in expectation* our estimator is will get it right.  It's an unbiased estimator,
# but we only get one experiment!

# --------------------------------

# Extra, if time

# Simulating the experiment

# Simulating 1000 different worlds and random assignments
# We can see how our experiment would turn out if we ran it a bunch of times
diagnosands <- declare_diagnosands(mean_estimand = mean(estimand),
                                   mean_estimate = mean(estimate),
                                   sd_estimate = sd(estimate))
diagnosis <- diagnose_design(design1, diagnosands = diagnosands, sims = 1000, bootstrap_sims = 0)
diagnosis
hist(diagnosis$simulations_df$estimate)
