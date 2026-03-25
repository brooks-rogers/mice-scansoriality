#The Key Insight
#For a missing binary response, the marginalization is trivially simple:
#  P(data | Loc_bin unknown) = P(Loc_bin=1|X) + P(Loc_bin=0|X) = p + (1-p) = 1
#  log P(data) = 0

#S o the custom Stan function is just:
# stanreal bernoulli_missing_lpmf(int y_placeholder, real mu, int y_obs) {
#  if (y_obs >= 0) {
#    return bernoulli_lpmf(y_obs | mu);  // observed: standard likelihood
#  } else {
#    return 0;  // missing: contributes nothing to likelihood
#  }
#}
#


# 1. Create indicator
dat <- dat %>%
  mutate(
    Loc_bin_obs = ifelse(is.na(Loc_bin), -1L, as.integer(Loc_bin)),
    y_for_brms = ifelse(is.na(Loc_bin), 0L, as.integer(Loc_bin))
  )

# 2. Define custom family
bernoulli_missing <- custom_family(
  name = "bernoulli_missing",
  dpars = "mu",
  links = "logit",
  lb = 0, ub = 1,
  type = "int",
  vars = "vint1[n]"
)

stan_funs <- "
  real bernoulli_missing_lpmf(int y_placeholder, real mu, int y_obs) {
    if (y_obs >= 0) {
      return bernoulli_lpmf(y_obs | mu);
    } else {
      return 0;
    }
  }
"
stanvars <- stanvar(scode = stan_funs, block = "functions")

# 3. Fit
fit <- brm(
  y_for_brms | vint(Loc_bin_obs) ~ Sl + log_Mass + 
    (1 | Genus_species) + (1 | gr(Taxon_Upham_style, cov = A)),
  data2 = list(A = A),
  family = bernoulli_missing,
  stanvars = stanvars,
  data = dat,
  prior = p1,
  cores = 4
)