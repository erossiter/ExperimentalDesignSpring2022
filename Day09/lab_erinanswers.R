library(DeclareDesign)

# Assume there are 500 registered voters from each region of a city
# We are interested in generalizing our experimental results to this target population.

# Question: what PATE does this imply?
population <- declare_population(regions = add_level(N = 4,
                                                    region = c("N", "E", "S", "W"),
                                                    tau = c(1,0,2,-1)), 
                                 units = add_level(N = 500,
                                                   u = rnorm(N)))

potential_outcomes <- declare_potential_outcomes(Y ~ tau*Z + u)

# New material -------------

# Question: explain this estimand
estimand_PATE <- declare_inquiry(PATE = mean(Y_Z_1 - Y_Z_0))

# Question: what does this step do?
sampling_complete <- declare_sampling(S = complete_rs(N = N, n = 100),
                                    filter = S == 1)

# Question: explain this estimand
estimand_SATE <- declare_inquiry(SATE = mean(Y_Z_1[S == 1] - Y_Z_0[S == 1]))

#-------------------------------

# Question: how is this different than the step on line 21?
assignment <- declare_assignment(Z = complete_ra(N, prob = .5))

reveal_Y <- declare_reveal()

estimator_dim <- declare_estimator(Y ~ Z,
                               model = estimatr::difference_in_means,
                               label = "DIM")

# Order matters
design_PATE <- (population + 
                  potential_outcomes + 
                  estimand_PATE + #PATE
                  sampling_complete + #sample
                  assignment + 
                  reveal_Y + 
                  estimator_dim)

design_SATE <- (population + 
                  potential_outcomes + 
                  sampling_complete + #sample
                  estimand_SATE + #SATE
                  assignment + 
                  reveal_Y + 
                  estimator_dim)

# Question 1:
# Inspect the designs and single draw examples.
# What's the difference (other than the random noise) between
# a single draw of data for these two designs?
# (This is a trick question...)
draw_PATE <- draw_data(design_PATE)
draw_SATE <- draw_data(design_SATE)

# Erin answer -- there is no difference!  The difference between the designs
# is our *quantity of interest*.  The resulting data is the same.

# Question 2:
# Inspect the diagnosis for each design.  Recall the *estimands are different*.
# Yet, the diagnoses look nearly identical.  Why?
diag_PATE <- diagnose_design(design_PATE)
diag_PATE

diag_SATE <- diagnose_design(design_SATE)
diag_SATE

# Erin answer -- our sampling procedure *randomly selects* units
# from the population, so, the SATE is identical to the PATE!
# It is possible the SATE won't equal the PATE, though...
# See question 3

# Question 3:
# Let's say we have to change our sampling procedure.
# We're still interested in the PATE!  But, our community partner
# in the Southern part of the city won't sample as many units :(

# 3a:
# What is going on in this sampling step?  What is knowing this
# sampling procedure important?
sampling_strata <- declare_sampling(S = strata_rs(strata = regions,
                                                  strata_n = c(100, 100, 25, 100)),
                                    filter = S == 1)

# 3b:
# Let's estimate the PATE.  We have to make a custom function for this to align
# with the estimation procedure we talking about in the slides.
# Examine this function.  Note this is NOT blocking.
# Can you tell what it is doing instead to estimate the PATE?

# Erin answer--weighting by this covariate profile *in the population*, whereas,
# blocking weights by the block size in the sample.
pate_estimator_func <- function(data){
  est_n <- t.test(Y ~ Z, data = data[data$region == "N",])
  est_e <- t.test(Y ~ Z, data = data[data$region == "E",])
  est_s <- t.test(Y ~ Z, data = data[data$region == "S",])
  est_w <- t.test(Y ~ Z, data = data[data$region == "W",])
  
  est <- diff(est_n$estimate)*.25 + diff(est_e$estimate)*.25 + diff(est_s$estimate)*.25 + diff(est_w$estimate)*.25
  se <- sqrt(est_n$stderr*(.25^2) + est_e$stderr*(.25^2 )+ est_s$stderr*(.25^2) + est_w$stderr*(.25^2))
  
  data.frame(estimate = est,
             std.error = se)
}

# Define a custom estimator.
estimator_weights <- declare_estimator(handler = label_estimator(pate_estimator_func), label = "weight DIM")


# Question 4:
# Let's edit our design.  We'll REPLACE our sampling procedure for both
# designs.
design_PATE2 <- replace_step(design_PATE,
                             step = sampling_complete,
                             new_step = sampling_strata)

design_SATE2 <- replace_step(design_SATE,
                             step = sampling_complete,
                             new_step = sampling_strata)

# Then, we'll ADD our new estimator to constrast it with a simple DIM.
design_PATE3 <- insert_step(design_PATE2,
                             new_step = estimator_weights,
                             after = estimator_dim)

# 4a:
# What goes wrong using DIM to estimate the PATE?  Why?  How does using
# the weighted DIM fix our estimate?

# Erin answer -- put simply, our sample isn't representative of the population
# in ways that affect the outcome of interest.
# We know how we could adjust it to look like the population, though.
# People in different regions have heterogeneous treatment effects.  So,
# if we weight *our SATE's per region* according to the population distribution
# of units in the region, we can adjust our estimate of the PATE.
diag_PATE3 <- diagnose_design(design_PATE3)
diag_PATE3

# 4b:
# Why is the DIM an unbiased estimator of the SATE? (trick question!)

# Erin answer -- it always is when treatment is randomly assigned! :)
# It is a biased estimator of the PATE because that's an observational 
# causal inference exercise.  The SATE is what we controlled experimentally.
# Back to internal vs. external validity tension.
diag_SATE2 <- diagnose_design(design_SATE2)
diag_SATE2
