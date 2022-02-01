library(DeclareDesign)
library(estimatr)

# Data from homework ----
term_length_tx <- c(rep(0, 16),
                    rep(1, 15))
term_length_ak <- c(rep(0, 16),
                    rep(1, 18), 0)
bills_tx <- c(18,29,41,53,60,67,75,79,79,
              88,93,101,103,106,107,131,
              29,37,42,45,45,54,54,58,61,
              64,69,73,75,92,104)
bills_ak <- c(11,15,23,24,25,26,28,31,33,
              34,35,35,36,38,52,59,9,
              10,14,15,15,17,18,19,19,
              20,21,23,23,24,28,30,32,34,17)
df <- data.frame(state = c(rep("TX", 31),
                           rep("AK", 35)),
                 term_length = c(term_length_tx, term_length_ak),
                 bills = c(bills_tx, bills_ak))


# Calculating ATE estimate ----
ate_tx <- mean(df$bills[df$state == "TX" & df$term_length == 1]) -
                 mean(df$bills[df$state == "TX" & df$term_length == 0])
ate_ak <- mean(df$bills[df$state == "AK" & df$term_length == 1]) -
                 mean(df$bills[df$state == "AK" & df$term_length == 0])
# Interpret this estimate
observed_estimate <- ate_tx*(31/66) + ate_ak*(35/66)


# Declare Design for RI ----
population <- declare_population(df)

potential_outcomes <- declare_potential_outcomes(Y ~ 0 * Z + bills)

# note, assuming prob of treatment assignment
# should not do in practice! you should know!
assignment <- declare_assignment(Z = block_ra(blocks = state,
                                              block_prob = c(15/31, 18/35)))

reveal_Y <- declare_reveal()

estimator <- declare_estimator(Y ~ Z)

design <- population + potential_outcomes + assignment + reveal_Y + estimator

p_value <- declare_diagnosands(p_value = mean(abs(estimate) >= abs(observed_estimate)))

set.seed(423)
diagnosis <- diagnose_design(design, diagnosands = p_value, sims = 500, bootstrap_sims = 0)
diagnosis

table(abs(diagnosis$simulations_df$estimate) >= abs(observed_estimate))



# How you would do RI in practice ----
state_rand_assign <- declare_ra(N = nrow(df),
                                blocks = df$state,
                                block_prob = c(15/31, 18/35))
dim_fun <- function(data) {
  summary(difference_in_means(bills ~ term_length,
                              blocks = state,
                              data = data))$coefficients[,"Estimate"]
}
set.seed(133)
out <- ri2::conduct_ri(test_function = dim_fun,
                       declaration = state_rand_assign,
                       outcome = "bills",
                       assignment = "term_length",
                       data = df)
plot(out)
out

# Compare to this p-value
difference_in_means(bills ~ term_length,
                    blocks = state,
                    data = df)

# Questions
# 1. Write the sharp null hypothesis in words.
#    What does this mean in terms of these data?
#    H_0: Y_i(1) = Y_i(0)

# 2. Write the null hypothesis of no treatment effect in words.
#    What does this mean in terms of these data?
#    H_0: ATE = 0

# 3. What is the RI pvalue?

# 4. What is the t-test pvalue?

# 5. Why might these tests produce similar (or different) results? In other words,
#    why might it be a good idea to test the hypothesis both ways?

