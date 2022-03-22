library(DeclareDesign)

types <- c("Complier", "Never-Taker")
proportion_compliers <- .75
N <- 100

population <- declare_population(N = N,
                                 type = sample(types,
                                               N,
                                               replace = TRUE,
                                               prob = c(proportion_compliers, 1-proportion_compliers)),
                                 u = rnorm(N))

# Function of how assignment (Z) relates to what you actually received (D)
# See how this encodes slide 14?
po_compliance <- declare_potential_outcomes(
  D ~ dplyr::case_when(
    type == "Complier" & Z == 1 ~ 1,
    type == "Never-Taker" & Z == 1 ~ 0,
    type %in% c("Never-Taker", "Complier") & Z == 0 ~ 0,
  )
)

test_data <- po_compliance(population())

# Outcome depends on what you actually received (D)
po_turnout <- declare_potential_outcomes(Y ~ .5*D*Z + u,
                                         assignment_variables = c("D", "Z"))

# New potential outcomes for Y based on what is revealed
# See how it is a switching equation?
# EX: for Y_Z_1:
#     for compliers, d(z=1)=1, so switch on Y_D_1_Z_1 (when assigned 1, I receive 1) 
#     for never-takers, d(z=1)=0, switch on Y_D_0_Z_1 (when assigned 1, I receive 0) 
po_simplified <- declare_potential_outcomes(
    Y_Z_1 = Y_D_1_Z_1 * D_Z_1 + Y_D_0_Z_1 * (1 - D_Z_1),
    Y_Z_0 = Y_D_1_Z_0 * D_Z_0 + Y_D_0_Z_0 * (1 - D_Z_0)
  )

estimand_CACE <- declare_inquiry(CACE = mean(Y_Z_1[type == "Complier"] - Y_Z_0[type == "Complier"]))

estimand_ITT <- declare_inquiry(ITT = mean(Y_Z_1 - Y_Z_0))

assign <- declare_assignment(Z = complete_ra(N = N))
revealD <- declare_reveal(D, assignment_variable = "Z")
revealY <- declare_reveal(Y, assignment_variables = c("D", "Z"))

estimate_CACE <- declare_estimator(Y ~ D | Z,
                                   model = iv_robust,
                                   inquiry = "CACE",
                                   label='2SLS') 

estimate_ITT <- declare_estimator(Y ~ Z,
                                  model = lm_robust,
                                  inquiry = "ITT",
                                  label = 'OLS')

design <- (population 
           + po_compliance
           + po_turnout
           + po_simplified
           + estimand_CACE
           + estimand_ITT
           + assign
           + revealD
           + revealY
           + estimate_CACE
           + estimate_ITT)
  
diagnosis <- diagnose_design(design)

d_vary_comp <- redesign(design, proportion_compliers = seq(.2, 1, .2))
diagnosis_vary_comp <- diagnose_design(d_vary_comp)

df_itt <- diagnosis_vary_comp$diagnosands_df[diagnosis_vary_comp$diagnosands_df$inquiry == "ITT", ]
plot(x = df_itt$proportion_compliers,
     y = df_itt$mean_estimand,
     type = "b",
     col = "blue",
     xlab = "Proportion compliers",
     ylab = "Mean estimand across 500 simulations")

df_cace <- diagnosis_vary_comp$diagnosands_df[diagnosis_vary_comp$diagnosands_df$inquiry == "CACE", ]
lines(x = df_cace$proportion_compliers,
     y = df_cace$mean_estimand,
     type = "b",
     col = "red")

legend("bottomright", legend = c("CACE", "ITT"), col = c("red", "blue"), lty = 1, bty = "n")

