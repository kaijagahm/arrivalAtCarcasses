# %ST vs predictability
library(targets)
library(future)
library(furrr)
library(progressr)
library(tidyverse)
library(STbayes)
library(sf)
library(loo)
library(posterior)
library(patchwork)
library(meta) # for meta-regression, frequentist
library(brms) # for Bayesian analyses, including meta-regression
handlers(global = TRUE)
source("R/functions.R")

tar_load(model_averaged_estimates_stn)
tar_load(model_averaged_estimates_wild)
tar_load(stn_carcs_modified)
tar_load(wild_carcs)
names(model_averaged_estimates_stn) <- map_dbl(stn_carcs_modified, "carcID")
names(model_averaged_estimates_wild) <- map_dbl(wild_carcs, "carcID")
mae <- purrr::list_rbind(model_averaged_estimates_stn, names_to = "carcID") %>% 
  mutate(carcType = "stn") %>%
  bind_rows(purrr::list_rbind(model_averaged_estimates_wild, names_to = "carcID") %>% 
              mutate(carcType = "wild")) %>%
  mutate(carcID = as.numeric(carcID))

pred <- readRDS("data/created/predictability_results.RDS")

mae <- left_join(mae, pred, by = c("carcID", "carcType"))

# Moment of truth ---------------------------------------------------------
# Median %ST by carcass type
# Have not yet excluded the ones where it wasn't valid
carcasscolors <- c("#DE9C0D", "#16697A")
blues <- c("#134790", "#77A9ED") # flight wild vs stn
greens <- c("#62680D", "#E5ED77") # roost wild vs. stn

pctst <- mae %>%
  filter(grepl("percent_ST", coef_label)) %>%
  mutate(param = case_when(coef_label == "percent_ST_net1" ~ "Flight",
                           coef_label == "percent_ST_net2" ~ "Roost"))
pctst_soc <- pctst %>%
  filter(CI_Lower > 0)
nrow(pctst_soc)/nrow(pctst) # 53% of carcasses/networks have social transmission detected

# Does predictability predict whether or not we detect social transmission?
pctst %>%
  mutate(detected = case_when(CI_Lower > 0 ~ 1, .default = 0)) %>%
  ggplot(aes(x = prop_days_covered, y = detected, color = interaction(carcType, param)))+
  geom_point(size = 4, alpha = 0.5, pch = 1)+
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE, linewidth = 1.5, alpha = 0.2)+
  theme_minimal()+
  theme(text = element_text(size = 14),
        legend.position = "none")+
  labs(y = "Detected", x = "Predictability", color = "Carcass type")+
  facet_wrap(~param)+
  scale_color_manual(values = c(blues, greens))

# %ST by carcass type, when ST > 0
pctst_soc %>%
  ggplot(aes(x = factor(year), y = Median, fill = interaction(carcType, param)))+
  geom_boxplot(outlier.size = 0.5)+
  facet_wrap(~param)+
  theme_minimal()+
  theme(text = element_text(size = 16))+
  labs(y = "%ST (Median)",
       x = "Year",
       fill = "Carcass type")+
  scale_fill_manual(values = c(blues, greens))

# %ST by carcass type and predictability, when ST > 0
pctst_soc %>%
  ggplot(aes(x = prop_days_covered, y = Median, color = interaction(carcType, param)))+
  geom_point(size = 3, pch = 1, alpha = 0.8)+
  theme_minimal()+
  geom_smooth(method = "lm", linewidth = 1.5, alpha = 0.2)+
  theme(text = element_text(size = 16),
        legend.position = "bottom")+
  labs(y = "%ST (Median)",
       x = "Predictability",
       color = "Carcass type")+
  scale_color_manual(values = c(blues, greens))+
  facet_wrap(~param)

# %ST by carcass type, predictability, and year, when ST > 0
pctst_soc %>%
  ggplot(aes(x = prop_days_covered, y = Median, color = interaction(carcType, param)))+
  facet_wrap(~year)+
  geom_point(size = 2, pch = 1, alpha = 0.8)+
  theme_minimal()+
  geom_smooth(method = "lm", linewidth = 1.5, alpha = 0.2)+
  theme(text = element_text(size = 16),
        legend.position = "bottom")+
  labs(y = "%ST (Median)",
       x = "Predictability",
       color = "Carcass type")+
  scale_color_manual(values = c(blues, greens))

library(lme4)
mod <- lm(Median ~ prop_days_covered*carcType + coef_label, data = pctst_soc)
summary(mod)

# Test workflow for meta/metafor ------------------------------------------
# Need to back-calculate the variance estimates
# From Claude: #"The core problem to solve first: getting a variance (or SE) out of Median/MAD/CI metagen() and rma.uni() both need yi (effect size) and vi (sampling variance) or seTE/SE. You have: Median → use directly as your effect size (yi/TE) MAD → convert to an SD-equivalent: SE ≈ 1.4826 × MAD. That 1.4826 constant is the standard scale factor that makes MAD consistent with SD under normality (MAD ≈ 0.6745·SD, so SD = MAD/0.6745). CI_Lower/CI_Upper → an independent SE estimate: SE ≈ (CI_Upper - CI_Lower) / (2 × 1.96) if these are 95% equal-tailed credible intervals (adjust the 1.96 if STbayes reports a different interval width, or if these are HPD intervals rather than equal-tailed — worth double-checking STbayes' default)."
#
#"You now have two independent ways to estimate SE. I'd compute both and compare them per study — if they're close, you're safe assuming approximate posterior normality. If they diverge a lot for some models, that's a flag that those posteriors are skewed (common for NBDA social transmission parameters, which are often bounded at zero), and a plain median/MAD/CI summary is hiding that asymmetry. In that case, consider whether the parameter should be log-transformed before meta-regression (rescaling to something more symmetric), the way ratio effect sizes normally are in meta-analysis."

# Code:
df_flight <- pctst_soc %>%
  filter(param == "Flight") # starting with just one of the perecent_ST estimates for now.
df_roost <- pctst_soc %>%
  filter(param == "Roost")

df_flight_all <- pctst %>%
  filter(param == "Flight")
df_roost_all <- pctst %>%
  filter(param == "Roost")

df_flight %>%
  ggplot(aes(x = log(MAD), color = carcType))+
  geom_density() # This makes the estimates look much more straightforwardly normally distributed. Claude suggests log-transforming before meta-analysis. I'm not sure how to figure out if I should do that, although I suspect the logic would be similar to just running other models where you need to log-transform coefficients before modeling.
# I don't understand how I would deal with the standard errors in case of log-transformation.

df_flight$SE_mad <- 1.4826 * df_flight$MAD
df_roost$SE_mad <- 1.4826 * df_roost$MAD
df_flight_all$SE_mad <- 1.4826 * df_flight_all$MAD
df_roost_all$SE_mad <- 1.4826 * df_roost_all$MAD

df_flight$SE_ci  <- (df_flight$CI_Upper - df_flight$CI_Lower) / (2 * 1.96)
df_roost$SE_ci  <- (df_roost$CI_Upper - df_roost$CI_Lower) / (2 * 1.96)
df_flight_all$SE_ci  <- (df_flight_all$CI_Upper - df_flight_all$CI_Lower) / (2 * 1.96)
df_roost_all$SE_ci  <- (df_roost_all$CI_Upper - df_roost_all$CI_Lower) / (2 * 1.96)

# sanity check — how much do the two SE estimates disagree?
plot(df_flight$SE_mad, df_flight$SE_ci); abline(0, 1) # they diverge considerably. Should we log-transform? How would we even do that?

# pick one (MAD-based is usually preferred, since CI width is more
# sensitive to how STbayes truncates tails)
df_flight$seTE <- df_flight$SE_mad
df_roost$seTE <- df_roost$SE_mad
df_flight_all$seTE <- df_flight_all$SE_mad
df_roost_all$seTE <- df_roost_all$SE_mad

m_flight <- metagen(
  TE      = Median,
  seTE    = seTE,
  studlab = carcID,
  data    = df_flight,
  sm      = "MD",     # generic label; doesn't change the math
  method.tau = "REML" # or "DL", "PM", etc.
)

m_roost <- metagen(
  TE      = Median,
  seTE    = seTE,
  studlab = carcID,
  data    = df_roost,
  sm      = "MD",     # generic label; doesn't change the math
  method.tau = "REML" # or "DL", "PM", etc.
)

m_flight_all <- metagen(
  TE      = Median,
  seTE    = seTE,
  studlab = carcID,
  data    = df_flight_all,
  sm      = "MD",     # generic label; doesn't change the math
  method.tau = "REML" # or "DL", "PM", etc.
)

m_roost_all <- metagen(
  TE      = Median,
  seTE    = seTE,
  studlab = carcID,
  data    = df_roost_all,
  sm      = "MD",     # generic label; doesn't change the math
  method.tau = "REML" # or "DL", "PM", etc.
)

reg_flight <- metareg(m_flight, ~ carcType + prop_days_covered)
summary(reg_flight)
# We see that the estimate of the residual heterogeneity variance, the variance that is not explained by the predictor (tau^2), is 0.0177
# I^2 equivalent: tells us that after inclusion of the predictor, 99.65% of the variability in our data can be attributed to the remaining between-study heterogeneity (YIKES). [Should compare this directly to a normal mixed-effects model that's not a meta-regression]
#In the last line, we see the value of R^2∗, which in our example is 0%. This means that 0% of the difference in true effect sizes can be explained by the carcass predictability (YIKES again)
# Don't rely too much on Test for Residual Heterogeneity, "(which is essentially the Q-test we already got to know previously (see Chapter 5.1.1)). Now, however, we test if the heterogeneity not explained by the predictor is significant. We see that this is the case, with p < 0.001. However, we know the limitations of the Q-test (Chapter 5.1.1), and should therefore not rely too heavily on this result."

#The next part shows the Test of Moderators. We see that this test is not significant (p = 0.3879). This means that we do not have evidence that predictability predicts effect size.
bubble(reg, studlab = TRUE) # not sure how to read this

# What about an interaction term?
reg_flight_int <- metareg(m_flight, ~ carcType*prop_days_covered)
summary(reg_flight_int) # still absolutely zilch.

# Now let's try the same thing, but without restricting it to significant effects only.
reg_flight_all <- metareg(m_flight_all, ~ carcType + prop_days_covered) # why is it omitting some carcasses?
summary(reg_flight_all) # this still explains barely anything at all.

# Roost networks
reg_roost <- metareg(m_roost, ~ carcType + prop_days_covered) # this one isn't omitting any
summary(reg_roost) # absolutely nothing. Why??

reg_roost_all <- metareg(m_roost_all, ~ carcType + prop_days_covered) # this time it omits 40 studies instead of 30.
summary(reg_roost_all) # still nothing.

# Why are our meta-regressions showing absolutely no effect whatsoever, while the regular plots suggest something going on?
# Probably some sort of confound--amount of standard error etc., which is exactly what the metaregression is supposed to account for.


# What about the coefficients? --------------------------------------------
mae %>% glimpse()
betas <- mae %>%
  filter(grepl("age", coef_label) | grepl("dist", coef_label))

betas %>%
  filter(CI_Lower > -60) %>%
  ggplot(aes(x = factor(carcID), y = Median, color = carcType))+
  geom_point()+
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper))+
  theme_minimal()+
  facet_wrap(~coef_label, scales = "free")+
  theme(axis.text.x = element_blank())

betas %>%
  filter(coef_label == "mean_dist_to_carcass_norm__on_asocial") %>%
  filter(CI_Lower > -60) %>%
  ggplot(aes(x = factor(carcID), y = Median, color = carcType))+
  geom_point()+
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper))+
  theme_minimal()+
  theme(axis.text.x = element_blank(), text = element_text(size = 16))+
  geom_hline(aes(yintercept = 0))+
  labs(y = "Median", x = "CarcID", color = "Carcass type")
# None positive, some significantly negative. This is the only one that has any significant results.
