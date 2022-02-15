library(DeclareDesign)

N <- 500
treatment_mean <- 65
control_mean <- 60
std_dev <- 20

population <- declare_population(N = N)

po <- declare_potential_outcomes(Y_Z_0 = rnorm(N, control_mean, std_dev),
                                 Y_Z_1 = rnorm(N, treatment_mean, std_dev))

estimand <- declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0))

assignment <- declare_assignment(Z = complete_ra(N, m = N/2))

reveal_Y <- declare_reveal()

estimator <- declare_estimator(Y ~ Z,
                               model = estimatr::difference_in_means)

design <- (population + po + 
             estimand + assignment + reveal_Y + estimator)


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

# Question 1
# Use the `redesign` function to vary the treatment effect size.
# Let's assume the control potential outcomes have a mean of 60,
# but the treated potential outcomes might have a mean from 61-66.
# (Therefore, the ATE varies from 1-6.)

design_ates <- redesign(design, treatment_mean = 61:66)
diagnosis_ates <- diagnose_design(design_ates,
                                  diagnosands = diagnosands,
                                  sims = 250, #Reduce this if needed
                                  bootstrap_sims = 0)
diagnosis_ates

# Question 2
# Create a simple plot to visualize how varying ATE affects power.
# The ATE should be on the x-axis and the power should be on the
# y-axis.
plot(x = 1:6,
     y = diagnosis_ates$diagnosands_df$power,
     xlab = "ATE",
     ylab = "Power",
     type = "b",
     ylim = c(0,1))
abline(h=.8, col = "blue")

# Question 3
# Comment on the power.  How sensitive is power to ATE size?
# Also look at all other columns in the diagnosis.
# These should be familiar!  Comment on any other patterns you see.
# (Keep in mind the lower number of M simulations we are doing for speed reasons)


# If time ------------
# Visualizing power when varying design elements together
set.seed(453)
design_ates_sd <- redesign(design,
                           treatment_mean = 61:66,
                           std_dev = c(10,20))
diagnosis_ates_sd <- diagnose_design(design_ates_sd,
                                     diagnosands = diagnosands,
                                     sims = 200,
                                     bootstrap_sims = 0)
diagnosis_ates_sd

power_sd_10 <- diagnosis_ates_sd$diagnosands_df$power[diagnosis_ates_sd$diagnosands_df$std_dev == 10]
power_sd_20 <- diagnosis_ates_sd$diagnosands_df$power[diagnosis_ates_sd$diagnosands_df$std_dev == 20]

plot(x = 1:6,
     y = power_sd_10,
     pch = 16,
     xlab = "ATE",
     ylab = "Power",
     type = "b",
     ylim = c(0,1))
points(x = 1:6,
     y = power_sd_20,
     pch = 15,
     type = "b")
abline(h=.8, col = "blue")
legend("bottomright", pch = c(16,15), legend = c("sd=10", "sd=20"), bty="n")

# What are the competing considerations here?
