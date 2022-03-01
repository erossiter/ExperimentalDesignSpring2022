library(DeclareDesign)

# Based on Coppock gist and Tweet
# https://gist.github.com/acoppock/ca5491e243ac70422f75864b777f5416
# https://twitter.com/aecoppock/status/1145453360633565189?lang=en

# Also see Gelman blog
# https://statmodeling.stat.columbia.edu/2018/03/15/need-16-times-sample-size-estimate-interaction-estimate-main-effect/

# For DeclareDesign 2x2 help
# https://declaredesign.org/r/designlibrary/articles/two_by_two.html

#M
N <- 500


population <- declare_population(N = N)

# Example
# Factor A is conversation or not
# Factor B is topic (non-political or political)

po <- declare_potential_outcomes(
  # Difference 1: 0.6 - 0.4 = 0.2
  Y_A_0_B_1 = draw_binary(prob = 0.4, N = N),
  Y_A_0_B_0 = draw_binary(prob = 0.6, N = N),
  # Difference 2: 0.6 - 0.5 = 0.1
  Y_A_1_B_1 = draw_binary(prob = 0.5, N = N),
  Y_A_1_B_0 = draw_binary(prob = 0.6, N = N))
  # Difference-in-differences (also called the interaction effect): 0.2 - 0.1 = 0.1

#I
estimands <- declare_inquiries(HTE = mean(Y_A_1_B_1 - Y_A_1_B_0) -
                                     mean(Y_A_0_B_1 - Y_A_0_B_0))

#D
assign_A <- declare_assignment(A = complete_ra(N, conditions = c(0,1)))
assign_B <- declare_assignment(B = block_ra(blocks = A, conditions = c(0,1)))
reveal_Y <- declare_reveal(Y, assignment_variables = c(A,B))


#A
estimator_hte <- declare_estimator(Y ~ A + B + A*B,
                                   term = "A:B",
                                   inquiry = "HTE",
                                   model = estimatr::lm_robust,
                                   label = "HTE est")

design <- (population
           + po
           + estimands
           + assign_A
           + assign_B
           + reveal_Y
           + estimator_hte)

draw_estimates(design)

single_draw <- draw_data(design)

# Diagnose -----------------
diagnosis <- diagnose_design(design, sims = 100)
diagnosis

# Redesign -----------------
vary_N <- c(500, 1000, 3000, 5000)
design_N <- redesign(design, N = vary_N)
diagnosis_N <- diagnose_design(design_N,
                                  sims = 200,
                                  bootstrap_sims = F)
power <- diagnosis_N$diagnosands_df$power
plot(x = vary_N, y = power, ylim = c(0,1), type = "b")
abline(h = .8, col = "red")
