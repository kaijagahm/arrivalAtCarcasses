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

tar_load(social_fits_DistI_2nets)
tar_load(social_fits_DistI_wild_2nets)
summs <- map(social_fits_DistI_2nets, ~{
  if(!is.null(.x)){
    STb_summary(.x)
  }else{
    NULL
  }})

summs_wild <- map(social_fits_DistI_wild_2nets, ~{
  if(!is.null(.x)){
    STb_summary(.x)
  }else{
    NULL
  }})
tar_load(stn_carcs_modified)
tar_load(wild_carcs)
names(summs) <- map_dbl(stn_carcs_modified, "carcID")
names(summs_wild) <- map_dbl(wild_carcs, "carcID")
summs_df <- purrr::list_rbind(summs, names_to = "carcID") %>% mutate(carcType = "stn")
summs_df_wild <- purrr::list_rbind(summs_wild, names_to = "carcID") %>% mutate(carcType = "wild")

summs_all <- bind_rows(summs_df, summs_df_wild) %>%
  mutate(carcID = as.numeric(carcID))

pred <- readRDS("data/created/predictability_results.RDS")

summs_all <- left_join(summs_all, pred, by = c("carcID", "carcType"))

# Test workflow for meta/metafor ------------------------------------------
# Need to back-calculate the variance estimates
# From Claude: #"The core problem to solve first: getting a variance (or SE) out of Median/MAD/CI metagen() and rma.uni() both need yi (effect size) and vi (sampling variance) or seTE/SE. You have: Median → use directly as your effect size (yi/TE) MAD → convert to an SD-equivalent: SE ≈ 1.4826 × MAD. That 1.4826 constant is the standard scale factor that makes MAD consistent with SD under normality (MAD ≈ 0.6745·SD, so SD = MAD/0.6745). CI_Lower/CI_Upper → an independent SE estimate: SE ≈ (CI_Upper - CI_Lower) / (2 × 1.96) if these are 95% equal-tailed credible intervals (adjust the 1.96 if STbayes reports a different interval width, or if these are HPD intervals rather than equal-tailed — worth double-checking STbayes' default)."
#
#"You now have two independent ways to estimate SE. I'd compute both and compare them per study — if they're close, you're safe assuming approximate posterior normality. If they diverge a lot for some models, that's a flag that those posteriors are skewed (common for NBDA social transmission parameters, which are often bounded at zero), and a plain median/MAD/CI summary is hiding that asymmetry. In that case, consider whether the parameter should be log-transformed before meta-regression (rescaling to something more symmetric), the way ratio effect sizes normally are in meta-analysis."

# Code:
df <- summs_all %>%
  filter(Parameter == "percent_ST[1]") # starting with just one of the perecent_ST estimates for now.

df %>%
  ggplot(aes(x = MAD, color = carcType))+
  geom_density()

df %>%
  ggplot(aes(x = log(MAD), color = carcType))+
  geom_density() # This makes the estimates look much more straightforwardly normally distributed. Claude suggests log-transforming before meta-analysis. I'm not sure how to figure out if I should do that, although I suspect the logic would be similar to just running other models where you need to log-transform coefficients before modeling.
# I don't understand how I would deal with the standard errors in case of log-transformation.

df$SE_mad <- 1.4826 * df$MAD
df$SE_ci  <- (df$CI_Upper - df$CI_Lower) / (2 * 1.96)

# sanity check — how much do the two SE estimates disagree?
plot(df$SE_mad, df$SE_ci); abline(0, 1) # they diverge considerably. Should we log-transform? How would we even do that?

# pick one (MAD-based is usually preferred, since CI width is more
# sensitive to how STbayes truncates tails)
df$seTE <- df$SE_mad

m <- metagen(
  TE      = Median,
  seTE    = seTE,
  studlab = carcID,
  data    = df,
  sm      = "MD",     # generic label; doesn't change the math
                                                                                                                                          method.tau = "REML" # or "DL", "PM", etc.
)

reg <- metareg(m, ~ carcType + stationName + predictability)
summary(reg)


# Test workflow for metaregression with brms ------------------------------
# Trying to follow the steps in https://doing-meta.guide/bayesian-ma, but also getting some guidance from Claude because I don't know what I'm doing.
df <- summs_all %>%
  filter(Parameter == "percent_ST[1]")
df <- df %>%
  mutate(
    seTE_mad = 1.4826 * MAD,
    seTE_ci  = (CI_Upper - CI_Lower) / (2 * 1.96),
    seTE     = seTE_mad   # pick one after comparing them, as discussed earlier
  ) %>%
  mutate(carcType = factor(carcType, levels = c("stn", "wild")),
         stationName = factor(stationName))

fit1 <- brm(
  Median | se(seTE, sigma = TRUE) ~ 1 + carcType + prop_days_covered,
  data    = df,
  family  = gaussian(),
  chains  = 4,
  cores   = 4,
  iter    = 4000,
  warmup  = 1000,
  control = list(adapt_delta = 0.95)
)


# Moment of truth ---------------------------------------------------------
# Median %ST by carcass type
# Have not yet excluded the ones where it wasn't valid
pctst <- summs_all %>%
  filter(grepl("percent_ST", Parameter)) %>%
  mutate(param = case_when(Parameter == "percent_ST[1]" ~ "Flight",
                           Parameter == "percent_ST[2]" ~ "Roost"))
pctst %>%
  ggplot(aes(x = factor(year), y = Median, fill = carcType))+
  geom_boxplot(outlier.size = 0.5)+
  facet_wrap(~param)+
  theme_minimal()+
  theme(text = element_text(size = 16),
        legend.position = "bottom")+
  labs(y = "%ST (Median)",
       x = "Year",
       fill = "Carcass type")+
  scale_fill_manual(values = c("darkorange", "olivedrab"))

pctst %>%
  ggplot(aes(x = prop_days_covered, y = Median, color = carcType))+
  geom_point(size = 2, pch = 1, alpha = 0.8)+
  theme_minimal()+
  geom_smooth(method = "lm")+
  theme(text = element_text(size = 16),
        legend.position = "bottom")+
  labs(y = "%ST (Median)",
       x = "Predictability",
       color = "Carcass type")+
  scale_color_manual(values = c("darkorange", "olivedrab"))

pctst %>%
  filter(CI_Lower > 0) %>%
  ggplot(aes(x = prop_days_covered, y = Median, color = carcType))+
  geom_point(size = 2, pch = 1, alpha = 0.8)+
  theme_minimal()+
  geom_smooth(method = "lm")+
  theme(text = element_text(size = 16),
        legend.position = "bottom")+
  labs(y = "%ST (Median)",
       x = "Predictability",
       color = "Carcass type")+
  scale_color_manual(values = c("darkorange", "olivedrab"))

pctst %>%
  filter(CI_Lower > 0) %>%
  ggplot(aes(x = prop_days_covered, y = Median, color = carcType))+
  facet_wrap(~year)+
  geom_point(size = 2, pch = 1, alpha = 0.8)+
  theme_minimal()+
  geom_smooth(method = "lm")+
  theme(text = element_text(size = 16),
        legend.position = "bottom")+
  labs(y = "%ST (Median)",
       x = "Predictability",
       color = "Carcass type")+
  scale_color_manual(values = c("darkorange", "olivedrab"))


