# install.packages(c(
#   "DeclareDesign",
#   "fabricatr",
#   "randomizr",
#   "estimatr",
#   "DesignLibrary"
# ))

library(DeclareDesign)

# Example drawing from Table 2.1 in GG
# N villages
# Outcome is the budget share of the village
# Potential outcomes are the budget share if the village head is a man Y_i(0) or woman Y_i(1)
set.seed(522)
N <- 20
assignment_prob <- 0.5
control_mean <- 15
control_sd <- 2
treatment_mean <- 20
treatment_sd <- 2

population <- declare_population(N = N,
                                 u_0 = rnorm(N)*control_sd,
                                 u_1 = rnorm(N)*treatment_sd)

potential_outcomes <- declare_potential_outcomes(Y ~ + Z * (u_1 + treatment_mean)
                                                     + (1 - Z) * (u_0 + control_mean))

design1 <- population + potential_outcomes
single_draw <- draw_data(design1)
single_draw

# Exercises:
# Why? A little R practice so I can gauge where we're at. Also,
# more engagement with the concepts so far.

# 1. Create a column for the causal effect on unit i. Call it `tau`.


# 2. Describe in words the fundamental problem of causal inference
# in terms of the columns in our dataframe `single_draw`


# 3. Calculate the ATE.

