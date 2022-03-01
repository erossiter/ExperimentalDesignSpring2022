library(DeclareDesign)

# Imagine a job training program intervention (Z)
# Outcome is income (Y)
# Imagine a covariate we think moderates the treatment effect
#   - Education, defined as high (H=1) if graduate high school
#   - Education, defined as low (H=0) if did not graduate high school

#M
N <- 500

# (I could think of reasons this might be opposite. I'm
# imagining a social network and social capital story.)
cate_high <- 6
cate_low <- 1

population <- declare_population(N = N,
                                 H = sample(c(1,0), N, replace = T), #education high=1, low=0
                                 u_i = rnorm(N, 50, 15))

po <- declare_potential_outcomes(Y ~ cate_high*(H == 1)*Z
                                   + cate_low*(H == 0)*Z
                                   + u_i)

#I
estimands <- declare_inquiries(ATE = mean(Y_Z_1 - Y_Z_0),
                               CATE_H = mean(Y_Z_1[H == 1] - Y_Z_0[H == 1]),
                               CATE_L = mean(Y_Z_1[H == 0] - Y_Z_0[H == 0]),
                               HTE = CATE_H - CATE_L)

#D
assignment <- declare_assignment(Z = complete_ra(N))

reveal_Y <- declare_reveal()

#A
estimator_ate <- declare_estimator(Y ~ Z,
                               term = "Z",
                               inquiry = "ATE",
                               model = estimatr::difference_in_means,
                               label = "ATE est")

estimator_cate_h <- declare_estimator(Y ~ Z,
                                   term = "Z",
                                   inquiry = "CATE_H",
                                   subset = H == 1,
                                   model = estimatr::difference_in_means,
                                   label = "CATE high educ est")

estimator_cate_l <- declare_estimator(Y ~ Z,
                                      term = "Z",
                                      inquiry = "CATE_L",
                                      subset = H == 0,
                                      model = estimatr::difference_in_means,
                                      label = "CATE low educ est")

estimator_hte <- declare_estimator(Y ~ Z*H,
                                   term = "Z:H",
                                   inquiry = "HTE",
                                   model = estimatr::lm_robust,
                                   label = "Interaction est")

design <- (population
           + po
           + estimands
           + assignment
           + reveal_Y
           + estimator_ate
           + estimator_cate_h
           + estimator_cate_l
           + estimator_hte)

draw_estimates(design)

# Diagnose -----------------
diagnosis <- diagnose_design(design, sims = 1000)
diagnosis


# Questions

# (1) Explain why CATE_L is not powered but CATE_H is.

# (2) Explain how the ATE relates to CATE_L and CATE_H.

# (3) Interpret the interaction term in words.

# (4) What would you do given these simulation results if you were
#    most interested in the interaction term?  How might you readjust your design?


# Redesign -----------------
design_htes <- redesign(design, cate_high = 1:11)
diagnosis_htes <- diagnose_design(design_htes,
                                  sims = 250)
power <- diagnosis_htes$diagnosands_df$power[diagnosis_htes$diagnosands_df$inquiry == "HTE"]
hte <- 0:10
plot(x = hte, y = power, ylim = c(0,1), type = "b")
abline(h = .8, col = "red")
