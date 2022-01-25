library(DeclareDesign)

# Note: ATE is 3
# Assume something, maybe age, is a big cause of
# individual-level variation in treatment effects.
population <- declare_population(blocks = add_level(N = 2,
                                                    age = c("boomer", "genx"),
                                                    tau = c(5, 1)),
                                 units = add_level(N = 10,
                                                   u = rnorm(N)))
                                 
potential_outcomes <- declare_potential_outcomes(Y ~ Z*tau + u)

estimand <- declare_inquiry(ATE = mean(Y_Z_1) - mean(Y_Z_0))

# Two different assignment procedures
assignment_block <- declare_assignment(Z = block_ra(blocks = blocks))
assignment_complete <- declare_assignment(Z = complete_ra(N = 20))

reveal_Y <- declare_reveal()

# Two different estimation procedures
estimator_block <- declare_estimator(Y ~ Z,
                               model = estimatr::difference_in_means,
                               blocks = blocks,
                               inquiry = "ATE",
                               label = "Blocking")
estimator_pooled <- declare_estimator(Y ~ Z,
                                     model = estimatr::difference_in_means,
                                     inquiry = "ATE",
                                     label = "Pooling")

# Design for block RA
design_block <- (population + potential_outcomes + estimand + 
              assignment_block + reveal_Y + estimator_block)

# Design for complete RA
design_complete <- (population + potential_outcomes + estimand + 
              assignment_complete + reveal_Y + estimator_pooled)

# Check things out
set.seed(522)
single_draw_block <- draw_data(design_block)
single_draw_complete <- draw_data(design_complete)

single_est_block <- draw_estimates(design_block)
single_est_complete <- draw_estimates(design_complete)

# Simulation
diagnosands <- declare_diagnosands(mean_estimand = mean(estimand),
                                   mean_estimate = mean(estimate),
                                   bias = mean(estimate - estimand),
                                   sd_estimate = sd(estimate), #true standard error
                                   mean_se = mean(std.error))  #average standard error estimate

diagnosis_block <- diagnose_design(design_block, diagnosands = diagnosands, sims = 1000, bootstrap_sims = 0)
diagnosis_block

diagnosis_complete <- diagnose_design(design_complete, diagnosands = diagnosands, sims = 1000, bootstrap_sims = 0)
diagnosis_complete

# Questions:

# 1. Examine this visualization of our simulation.  What do we learn
# about estimating the ATE?
boxplot(diagnosis_block$simulations_df$estimate,
        diagnosis_complete$simulations_df$estimate,
        names = c("Block RA", "Complete RA"),
        main = "Estimate ATEs from our Design,\nSimulated 1000 times")

  # 1. Blocking can still be unbiased
  # 2. But it reduced our sampling variability
  #   (Note: the visual is an approximation of sampling distribution of the ATE!)

# 2. Note the vectors used in question 1 are the estimates from
# simulating our design 1000 times.  Calculate the true standard error
# using those vectors?

sd(diagnosis_block$simulations_df$estimate)
sd(diagnosis_complete$simulations_df$estimate)

  # Recall, the standard error is the standard deviation of the sampling distribution!
  # So we simply us the sd() command.
  # We see doing so matches the summary of our simulation above under the column
  # 'sd_estimate'

diagnosis_block$diagnosands_df$sd_estimate
diagnosis_complete$diagnosands_df$sd_estimate

# 3. What does this visual represent?  In other words,
# if you had to add a title to this plot, what would you put?
boxplot(diagnosis_block$simulations_df$std.error,
        diagnosis_complete$simulations_df$std.error,
        names = c("Block RA", "Complete RA"),
        main = "?")

  # Title: Estimated standard error of the ATE from our design across 1000 simulations
  # We see how, on average, we're estimating the true standard error in both
  # designs.  These are the standard errors we get back in our t.test or DIM tables.
  # In other words, these are the value we use for hypothesis testing.  I'd much rather have
  # the standard errors from the blocked design!! More likely to reject null!!
  # (More on power in another class.)


