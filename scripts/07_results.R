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
library(ggExtra)
source("R/functions.R")

tar_load(model_averaged_estimates_stn)
tar_load(model_averaged_estimates_wild)
tar_load(stn_carcs_modified)
tar_load(wild_carcs)
tar_load(carc_summs_stn)
tar_load(carc_summs_wild)
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
                           coef_label == "percent_ST_net2" ~ "Roost")) %>%
  mutate(detected = case_when(CI_Lower > 0 ~ 1, .default = 0),
         carctype = case_when(carcType == "stn" ~ "SFS",
                              carcType == "wild" ~ "Non-SFS"),
         param_lbl = case_when(param == "Flight" ~ "Flight network",
                               param == "Roost" ~ "Roost network"))
pctst_soc <- pctst %>%
  filter(CI_Lower > 0)
nrow(pctst_soc)/nrow(pctst) # 53% of carcasses/networks have social transmission detected

# Does predictability predict whether or not we detect social transmission?
detected_yn <- pctst %>%
  ggplot(aes(x = prop_days_covered, y = detected, color = carctype))+
  geom_jitter(height = 0.03, 
              width = 0, 
              alpha = 0.3,
              size = 3)+
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE, linewidth = 1.5, alpha = 0.1)+
  theme_classic()+
  theme(text = element_text(size = 20),
        strip.background = element_rect(color = "white",fill = "white"),
        strip.text = element_text(face = "bold"))+
  labs(y = "Social transmission detected?", 
       x = "Predictability", color = "Carcass type")+
  facet_wrap(~param_lbl)+
  scale_color_manual(values = carcasscolors)+
  scale_y_continuous(
    breaks = c(0, 1), # Replace 0 and 1 with your axis' min/max
    labels = c("No", "Yes")
  )
detected_yn
ggsave(detected_yn, file = "fig/ISBEplots/detected_yn.png", width = 9, height = 4.5)

# %ST by carcass type, when ST > 0
pctst_boxplot <- pctst_soc %>%
  ggplot(aes(x = carctype, y = Median, fill = carctype, color = carctype))+
  geom_boxplot(alpha = 0.3, outlier.shape = NA)+
  geom_jitter(width = 0.2, aes(color = carctype), alpha = 0.4, size = 2)+
  facet_wrap(~param_lbl, scales = "free_y")+
  labs(y = "%ST (Median)",
       x = "Carcass type",
       fill = "Carcass type",
       color = "Carcass type")+
  scale_fill_manual(values = carcasscolors)+
  scale_color_manual(values = carcasscolors)+
  theme_classic()+
  theme(text = element_text(size = 20),
        strip.background = element_rect(color = "white",fill = "white"),
        strip.text = element_text(face = "bold"))+
  scale_y_continuous(limits = c(0, 1))
pctst_boxplot
ggsave(pctst_boxplot, file = "fig/ISBEplots/pctst_boxplot.png", width = 9, height = 4.5)

# %ST by carcass type and predictability, when ST > 0
pctst_scatter <- pctst_soc %>%
  ggplot(aes(x = prop_days_covered, y = Median, color = carctype))+
  geom_point(size = 4, alpha = 0.4)+
  theme_classic()+
  theme(text = element_text(size = 20),
        strip.background = element_rect(color = "white",fill = "white"),
        strip.text = element_text(face = "bold"))+
  geom_smooth(method = "lm", linewidth = 1.5, alpha = 0.2)+
  labs(y = "%ST (Median)",
       x = "Predictability",
       color = "Carcass type")+
  scale_color_manual(values = carcasscolors)+
  facet_wrap(~param_lbl, scales = "free_y")+
  scale_y_continuous(limits = c(0, 1))
pctst_scatter
ggsave(pctst_scatter, file = "fig/ISBEplots/pctst_scatter.png", width = 9, height = 4.5)

# Composite plot: scatterplot with marginal violins or half-violins
## Scatterplots: flight
scatter_fl <- pctst_soc %>%
  filter(param_lbl == "Flight network") %>%
  ggplot(aes(x = prop_days_covered, y = Median, color = carctype))+
  geom_point(size = 4, alpha = 0.4)+
  theme_classic()+
  theme(text = element_text(size = 20))+
  geom_smooth(method = "lm", linewidth = 1.5, alpha = 0.2)+
  labs(y = "%ST (Median)",
       x = "Predictability",
       color = "Carcass type")+
  scale_color_manual(values = carcasscolors)+
  scale_y_continuous(limits = c(0, 1))

## Scatterplots: roost
scatter_ro <- pctst_soc %>%
  filter(param_lbl == "Roost network") %>%
  ggplot(aes(x = prop_days_covered, y = Median, color = carctype))+
  geom_point(size = 4, alpha = 0.4)+
  theme_classic()+
  theme(text = element_text(size = 20))+
  geom_smooth(method = "lm", linewidth = 1.5, alpha = 0.2)+
  labs(y = "%ST (Median)",
       x = "Predictability",
       color = "Carcass type")+
  scale_color_manual(values = carcasscolors)+
  scale_y_continuous(limits = c(0, 1))

## Violins: flight
violin_fl <- pctst_soc %>%
  filter(param_lbl == "Flight network") %>%
  ggplot(aes(x = carctype, y = Median, color = carctype, fill = carctype))+
  geom_violin(alpha = 0.2, linewidth = 1)+
  theme_classic()+
  theme(text = element_text(size = 20),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank())+
  labs(y = "%ST (Median)",
       x = NULL)+
  scale_color_manual(values = carcasscolors)+
  scale_fill_manual(values = carcasscolors)+
  scale_y_continuous(limits = c(0, 1))

## Violins: roost
violin_ro <- pctst_soc %>%
  filter(param_lbl == "Roost network") %>%
  ggplot(aes(x = carctype, y = Median, color = carctype, fill = carctype))+
  geom_violin(alpha = 0.2, linewidth = 1)+
  theme_classic()+
  theme(text = element_text(size = 20),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank())+
  scale_color_manual(values = carcasscolors)+
  scale_fill_manual(values = carcasscolors)+
  scale_y_continuous(limits = c(0, 1))


(scatter_fl & theme(legend.position = "none")) + violin_fl

## Density: flight
density_fl <- pctst_soc %>%
  filter(param_lbl == "Flight network") %>%
  mutate(carctype = factor(carctype, levels = c("SFS", "Non-SFS"))) %>%
  ggplot(aes(x = Median, color = carctype, fill = carctype))+
  geom_density(alpha = 0.2, linewidth = 1, outline.type = "full")+
  theme_classic()+
  coord_flip()+
  theme(text = element_text(size = 20),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank())+
  scale_color_manual(values = rev(carcasscolors))+
  scale_fill_manual(values = rev(carcasscolors))

## Density: roost
density_ro <- pctst_soc %>%
  filter(param_lbl == "Roost network") %>%
  mutate(carctype = factor(carctype, levels = c("SFS", "Non-SFS"))) %>%
  ggplot(aes(x = Median, color = carctype, fill = carctype))+
  geom_density(alpha = 0.2, linewidth = 1, outline.type = "full")+
  theme_classic()+
  coord_flip()+
  theme(text = element_text(size = 20),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank())+
  scale_color_manual(values = rev(carcasscolors))+
  scale_fill_manual(values = rev(carcasscolors))

combined_fl <- ((scatter_fl & theme(legend.position = "none")) | density_fl) & theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"))

combined_ro <- ((scatter_ro & theme(legend.position = "none")) | density_ro) & theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"))

combined_both <- ((scatter_fl & theme(legend.position = "none")) | density_fl | plot_spacer() | (scatter_ro & theme(legend.position = "none", axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())) | density_ro) + plot_layout(widths = c(4, 0.75, 0.3, 4, 0.75), guides = "collect") & theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"))

combined_both
ggsave(combined_both, file = "fig/ISBEplots/scatter_density_combined.png", width = 8, height = 4.5)


carcwt <- pctst %>%
  filter(carctype == "SFS") %>%
  ggplot(aes(x = carcassWeight, y = detected, color = carctype))+
  geom_jitter(height = 0.03, 
              width = 0, 
              alpha = 0.3,
              size = 3)+
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE, linewidth = 1.5, alpha = 0.1)+
  theme_classic()+
  theme(text = element_text(size = 20),
        strip.background = element_rect(color = "white",fill = "white"),
        strip.text = element_text(face = "bold"))+
  labs(y = "Social transmission detected?", 
       x = "Carcass weight", color = "Carcass type")+
  facet_wrap(~param_lbl)+
  scale_color_manual(values = carcasscolors)+
  scale_y_continuous(
    breaks = c(0, 1), # Replace 0 and 1 with your axis' min/max
    labels = c("No", "Yes")
  )
carcwt
ggsave(carcwt, file = "fig/carcwt.png", width = 9, height = 4.5)

nfound <- pctst %>%
  left_join(bind_rows(carc_summs_stn, carc_summs_wild), by = c("carcID" = "trial")) %>%
  ggplot(aes(x = n_found, y = detected, color = carctype))+
  geom_jitter(height = 0.03, 
              width = 0, 
              alpha = 0.3,
              size = 3)+
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE, linewidth = 1.5, alpha = 0.1)+
  theme_classic()+
  theme(text = element_text(size = 20),
        strip.background = element_rect(color = "white",fill = "white"),
        strip.text = element_text(face = "bold"))+
  labs(y = "Social transmission detected?", 
       x = "# found", color = "Carcass type")+
  facet_wrap(~param_lbl)+
  scale_color_manual(values = carcasscolors)+
  scale_y_continuous(
    breaks = c(0, 1), # Replace 0 and 1 with your axis' min/max
    labels = c("No", "Yes")
  )
nfound
ggsave(nfound, file = "fig/nfound.png", width = 9, height = 4.5)

ninnet <- pctst %>%
  left_join(bind_rows(carc_summs_stn, carc_summs_wild), by = c("carcID" = "trial")) %>%
  ggplot(aes(x = n_total, y = detected, color = carctype))+
  geom_jitter(height = 0.03, 
              width = 0, 
              alpha = 0.3,
              size = 3)+
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE, linewidth = 1.5, alpha = 0.1)+
  theme_classic()+
  theme(text = element_text(size = 20),
        strip.background = element_rect(color = "white",fill = "white"),
        strip.text = element_text(face = "bold"))+
  labs(y = "Social transmission detected?", 
       x = "N in network", color = "Carcass type")+
  facet_wrap(~param_lbl)+
  scale_color_manual(values = carcasscolors)+
  scale_y_continuous(
    breaks = c(0, 1), # Replace 0 and 1 with your axis' min/max
    labels = c("No", "Yes")
  )
ninnet
ggsave(ninnet, file = "fig/ninnet.png", width = 9, height = 4.5)

## What about the other carcasses that are active at the same time?

summary(lm(Median ~ prop_days_covered + carcassWeight + coef_label, data = filter(pctst_soc, carctype == "SFS")))
summary(lm(Median ~ prop_days_covered*carctype + coef_label, data = pctst_soc))






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

# Age only
age_betas <- betas %>%
  filter(grepl("age", coef_label)) %>%
  mutate(coef_label = str_replace(coef_label, "age\\[1\\]", "subadult"),
         coef_label = str_replace(coef_label, "age\\[2\\]", "adult")) %>%
  mutate(CI_Lower = case_when(CI_Lower < -4 ~ NA, .default = CI_Lower)) %>%
  ggplot(aes(x = factor(carcID), y = Median, color = carcType))+
  scale_color_manual(values = carcasscolors)+
  geom_point()+
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0)+
  theme_classic()+
  theme(axis.text.x = element_blank(),
        strip.background = element_rect(color = "white", fill = "white"),
        axis.ticks.x = element_blank(),
        text = element_text(size = 20),
        legend.position = "bottom")+
  facet_wrap(~coef_label, scales = "free")+
  labs(y = "Effect size",
       x = "Carcass",
       color = "Carcass type")
age_betas
ggsave(age_betas, file = "fig/age_betas.png", width = 10, height = 5)

# Distance only
dist_betas <- betas %>%
  filter(grepl("dist", coef_label)) %>%
  mutate(CI_Lower = case_when(CI_Lower < -4 ~ NA, .default = CI_Lower)) %>%
  ggplot(aes(x = factor(carcID), y = Median, color = carcType))+
  scale_color_manual(values = carcasscolors)+
  geom_point()+
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0)+
  theme_classic()+
  theme(axis.text.x = element_blank(),
        strip.background = element_rect(color = "white", fill = "white"),
        axis.ticks.x = element_blank(),
        text = element_text(size = 20),
        legend.position = "bottom")+
  facet_wrap(~coef_label, scales = "free")+
  labs(y = "Effect size",
       x = "Carcass",
       color = "Carcass type")
dist_betas
ggsave(dist_betas, file = "fig/dist_betas.png", width = 10, height = 5)
# None positive, some significantly negative. This is the only one that has any significant results.