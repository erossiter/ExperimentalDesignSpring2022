library(DeclareDesign)

# Declare -----------------

#M
N <- 500
treatment_mean <- 65
control_mean <- 60
std_dev <- 20

population <- declare_population(N = N)

po <- declare_potential_outcomes(Y_Z_0 = rnorm(N, control_mean, std_dev),
                                 Y_Z_1 = rnorm(N, treatment_mean, std_dev))

#I
estimand <- declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0))

#D
assignment <- declare_assignment(Z = complete_ra(N, m = N/2))

reveal_Y <- declare_reveal()

#A
estimator <- declare_estimator(Y ~ Z,
                               model = estimatr::difference_in_means)

design <- (population + po + 
             estimand + assignment + reveal_Y + estimator)

# Diagnose -----------------
diagnosands <- declare_diagnosands(mean_estimand = mean(estimand),
                                   mean_estimate = mean(estimate),
                                   bias = mean(estimate - estimand),
                                   power = mean(p.value < .05), 
                                   sd_estimate = sd(estimate), #true standard error
                                   mean_se = mean(std.error))  #average standard error estimate

diagnosis <- diagnose_design(design,
                             diagnosands = diagnosands,
                             sims = 500,
                             bootstrap_sims = 0)

# Redesign -----------------
design_ates <- redesign(design, treatment_mean = 61:66)
diagnosis_ates <- diagnose_design(design_ates,
                                  diagnosands = diagnosands,
                                  sims = 250, #Reduce this if needed
                                  bootstrap_sims = 100)
diagnosis_ates


# Question 1
# In your own words, what is the diagnostic statistic
# we use when assessing power in the above simulation?

        # Whether the estimate was deemed statistically significant at the 0.05 level.
        # p < .05 *for a single simulation*

# Question 2
# In your own words, what is the diagnosand
# we use when assessing power in the above simulation?

        # The *rate at which* the estimate was deemed statistically significant
        # Across all simulations: mean(p value < .05)
p_vals <- c(.02,.03, .001, .5)
mean(p_vals < .05)

# Question 3
# In your own words, why is it important to have standard errors
# associated with our diagnosand estimates?

        # We are running a finite number of simulations, so
        # there is uncertainty associated with our estimate of power,
        # for example, and we should quantify that uncertainty.

