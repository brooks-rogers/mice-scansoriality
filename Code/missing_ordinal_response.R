#Key Points
#
#No need to specify K - brms automatically detects the number of categories from #your ordinal variable
#The trick: brms adds the likelihood for all observations, then we subtract it #back out for missing cases: target += -ordered_logistic_lpmf(...). Net effect = #0 contribution to likelihood.
#Same logic for binary: You can use this same "subtract it back" approach for #binary too:
#
#r   stan_likelihood_adjust <- "
#     for (n in 1:N) {
#       if (is_missing[n] == 1) {
#         target += -bernoulli_logit_lpmf(Y[n] | mu[n]);
#       }
#     }
#   "
#
#Predictions work normally - posterior_epred() will give you #phylogenetically-informed category probabilities for the missing observations.

# 1. Prepare data
dat <- dat %>%
  mutate(
    is_missing = as.integer(is.na(Loc_Ord)),
    Loc_Ord_safe = ifelse(is.na(Loc_Ord), 1L, as.integer(Loc_Ord))
  )

# 2. Stan code to zero out missing likelihood contributions
stan_likelihood_adjust <- "
  for (n in 1:N) {
    if (is_missing[n] == 1) {
      target += -ordered_logistic_lpmf(Y[n] | mu[n], Intercept);
    }
  }
"

stanvars <- stanvar(x = dat$is_missing, name = "is_missing", 
                    scode = "  array[N] int is_missing;") +
  stanvar(scode = stan_likelihood_adjust, block = "likelihood")

# 3. Fit with standard cumulative() family
fit <- brm(
  Loc_Ord_safe ~ Sl + log_Mass + 
    (1 | Genus_species) + (1 | gr(Taxon_Upham_style, cov = A)),
  data2 = list(A = A),
  family = cumulative(),
  data = dat,
  stanvars = stanvars,
  prior = p1,
  cores = 4
)