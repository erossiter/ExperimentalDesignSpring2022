# Based on script in DeclareDesign Library
# https://declaredesign.org/r/designlibrary/articles/pretest_posttest.html
N <- 100
ate <- 0.5
rho <- .75

# How to generate correlated numbers
# https://stackoverflow.com/questions/29997994/how-to-generate-correlated-numbers
# Let the random variables X ~ N(0,1) and Y ~ N(0,1) independently.
# Then the random variables X and rho*X + sqrt(1 - rho^2)*Y are both distributed N(0,1),
# but now with correlation rho. 
population <- declare_population(N = N,
                                 Y_t1 = rnorm(N),
                                 u_t2 = rnorm(N),
                                 u_t2 = rho*Y_t1 + sqrt(1-rho^2)*u_t2) #redefine u_t2

potential_outcomes <- declare_potential_outcomes(Y_t2 ~ ate*Z + u_t2)

estimand <- declare_inquiry(ATE = mean(Y_t2_Z_1 - Y_t2_Z_0))

assignment <- declare_assignment(Z = complete_ra(N))

reveal_t2 <- declare_reveal(Y_t2)

manipulation <- declare_step(difference = (Y_t2 - Y_t1), handler = fabricate)

pretest_lhs <- declare_estimator(difference ~ Z,
                                 model = lm_robust, 
                                 inquiry = estimand,
                                 label = "Change score")

pretest_rhs <- declare_estimator(Y_t2 ~ Z + Y_t1,
                                 model = lm_robust, 
                                 inquiry = estimand,
                                 label = "Condition on pretest")

posttest_only <- declare_estimator(Y_t2 ~ Z,
                                   model = lm_robust, 
                                   inquiry = estimand,
                                   label = "Posttest only")

# Include the 3 different estimators
pretest_posttest_design <- (population + 
                              potential_outcomes + 
                              estimand + 
                              assignment + 
                              reveal_t2 + 
                              manipulation + 
                              pretest_lhs + 
                              pretest_rhs + 
                              posttest_only)

# Check it out
single_draw <- draw_data(pretest_posttest_design)
cor(single_draw$Y_t2, single_draw$Y_t1)

# Simulation
diagnosands <- declare_diagnosands(mean_estimand = mean(estimand),
                                   mean_estimate = mean(estimate),
                                   bias = mean(estimate - estimand),
                                   power = mean(p.value < .05), #next week!
                                   sd_estimate = sd(estimate), #true standard error
                                   mean_se = mean(std.error))  #average standard error estimate

diagnosis <- diagnose_design(pretest_posttest_design,
                             diagnosands = diagnosands,
                             sims = 500,
                             bootstrap_sims = 0)
diagnosis

# Questions
# 1. What do you notice about the estimators' bias?

# 2. What do you notice about the estimators' standard errors?

# 3. Would use of a pre-treatment measure be something
#    you would consider for an experiment you have in mind?
#    If so, why?  What variable?  What gains do you predict?
#    If no, why?  Are there other concerns at play?
