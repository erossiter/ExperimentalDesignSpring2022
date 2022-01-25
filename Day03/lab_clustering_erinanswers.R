library(DeclareDesign)

N_clusters <- 10
cluster_size <- 20

population <- declare_population(clusters = add_level(N = N_clusters,
                                                      u_c = rnorm(N_clusters)),
                                 units = add_level(N = cluster_size,
                                                   u_i = rnorm(N)))

potential_outcomes <- declare_potential_outcomes(Y ~ 2*Z + u_c + u_i)

estimand <- declare_inquiry(ATE = mean(Y_Z_1) - mean(Y_Z_0))

assignment <- declare_assignment(Z = cluster_ra(clusters = clusters))

reveal_Y <- declare_reveal()

estimator <- declare_estimator(Y ~ Z,
                               model = estimatr::lm_robust,
                               clusters = clusters,
                               inquiry = "ATE")

design1 <- population + potential_outcomes + estimand + assignment + reveal_Y + estimator

set.seed(522)
single_draw <- draw_data(design1)

set.seed(522)
single_est <- draw_estimates(design1)


# In the DeclareDesign framework, we think of the true standard error as a
# diagnosand (a feature of a design to be "diagnosed"). The thing that is
# reported in an estimator (like lm_robust) is an estimate of that diagnosand.

diagnosands <- declare_diagnosands(mean_estimand = mean(estimand),
                                   mean_estimate = mean(estimate),
                                   bias = mean(estimate - estimand),
                                   sd_estimate = sd(estimate), #true standard error
                                   mean_se = mean(std.error))  #average standard error estimate
diagnosis <- diagnose_design(design1, diagnosands = diagnosands, sims = 1000, bootstrap_sims = 0)
diagnosis


# Simulation:
cluster_designs <- redesign(design1,
                            N_clusters = c(6, 12, 30), 
                            cluster_size = c(5, 25))
set.seed(2342)
cluster_diagnosis <- diagnose_design(cluster_designs, diagnosands = diagnosands, sims = 500, bootstrap_sims = 0)
cluster_diagnosis

# Questions:
# Examine the output of our diagnosis.  We varied the number of clusters as well as how many
# people were in each cluster.  Focus on the `SD Estimate` and `Mean Se` columns, and answer
# the following questions:

# How does the number of clusters affect the true standard error associated with
# our design?

  # As the number of clusters goes up, our sampling variability decreases
  # More people in the study!


# How does the cluster size affect the true standard error associated with our
# design?

  # It doesn't that much!
  # For example, with 30 clusters, increasing each cluster from 5 to 25 people
  # has only .01 decrease on true standard error.


# How does the number of clusters affect our estimation of the standard error?
# Specifically, is it a good estimate?

  # When the number of clusters is small, we see that the average estimate is too small:
  # the standard error estimators are downwardly biased!


# How does the cluster size affect our estimation of the standard error?

  # It doesn't that much! You don't improve the biased estimation by
  # adding more people per cluster... you improve it by adding more clusters!
