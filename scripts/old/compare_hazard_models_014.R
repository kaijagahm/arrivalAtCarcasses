# ---------------------------------------------------------------------------
# Compare three model specifications on carc014
#   1. constant baseline hazard   (Kaija's original fit_social_014)
#   2. Weibull baseline hazard    (declining hazard, extra shape param gamma)
#   3. individual varying effects (frailty: per-id random effect on the rate)
#   4. Weibull + individual varying effects
#
# Fits (2) and (3), saves them, then loads all three and plots the PPC
# cumulative-diffusion curves side by side.  Reuses Kaija's own get_plotdata /
# get_curveplots.
# ---------------------------------------------------------------------------

library(tidyverse)
library(STbayes)
library(posterior)
library(patchwork) # for side-by-side panels; install.packages("patchwork") if needed

# --- paths -----------------------------------------------------------------
DATA_DIR <- "data/forMichael_2026-07-30"
FIT_DIR <- file.path(DATA_DIR, "saved_fits")
PLOT_DIR <- "plots"
dir.create(PLOT_DIR, showWarnings = FALSE)

# --- carcass to work on ----------------------------------------------------
# idxs = c(4, 5, 14, 19); carc014 is the 3rd element.
i <- 3

events <- readRDS(file.path(DATA_DIR, "events.RDS"))
data_lists <- readRDS(file.path(DATA_DIR, "data_lists.RDS"))
carcs <- readRDS(file.path(DATA_DIR, "carcs.RDS"))

dl <- data_lists[[i]]
cid <- carcs[[i]]$carcID

# --- sampler settings (shared by the new fits) -------------------------
SAMPLE_ARGS <- list(
    chains          = 2,
    parallel_chains = 2,
    iter_warmup     = 500,
    iter_sampling   = 500,
    adapt_delta     = 0.95,
    seed            = 1
)

# ===========================================================================
# 1. FIT (or reuse) the models
# ===========================================================================

# --- (2) Weibull declining-hazard model ------------------------------------
# Already saved by an earlier run; only refit if the file is missing.
weibull_path <- file.path(FIT_DIR, "fit_weibull_014.rds")
if (!file.exists(weibull_path)) {
    message(">>> Fitting Weibull model ...")
    mod_w <- generate_STb_model(dl, est_acqTime = TRUE, intrinsic_rate = "weibull")
    fit_w <- do.call(fit_STb, c(list(dl, mod_w), SAMPLE_ARGS))
    STb_save(fit_w, output_dir = FIT_DIR, name = "fit_weibull_014")
} else {
    message(">>> Weibull fit already exists, skipping refit: ", weibull_path)
}

# --- (3) Individual varying-effects (frailty) model ------------------------
# Per-id random effect on the intrinsic (asocial) rate lambda_0 -- this is the
# frailty term that lets a susceptible subset acquire while the rest do not.
# Add "s" to veff_params to also let social susceptibility vary by individual.
veff_path <- file.path(FIT_DIR, "fit_veff_014.rds")
if (!file.exists(veff_path)) {
    message(">>> Fitting individual varying-effects model ...")
    mod_veff <- generate_STb_model(
        dl,
        est_acqTime = TRUE,
        intrinsic_rate = "constant",
        veff_params = c("lambda_0"), # try c("lambda_0", "s") for both rates
        veff_type = "id"
    )
    fit_veff <- do.call(fit_STb, c(list(dl, mod_veff), SAMPLE_ARGS))
    STb_save(fit_veff, output_dir = FIT_DIR, name = "fit_veff_014")
} else {
    message(">>> Varying-effects fit already exists, skipping refit: ", veff_path)
}

veff_path_s <- file.path(FIT_DIR, "fit_veff_014_s.rds")
if (!file.exists(veff_path_s)) {
    message(">>> Fitting individual varying-effects model ...")
    mod_veff_s <- generate_STb_model(
        dl,
        est_acqTime = TRUE,
        intrinsic_rate = "constant",
        veff_params = c("s"), # try c("lambda_0", "s") for both rates
        veff_type = "id"
    )
    fit_veff_s <- do.call(fit_STb, c(list(dl, mod_veff_s), SAMPLE_ARGS))
    STb_save(fit_veff_s, output_dir = FIT_DIR, name = "fit_veff_014_s")
} else {
    message(">>> Varying-effects fit already exists, skipping refit: ", veff_path)
}

veff_weibull_path <- file.path(FIT_DIR, "fit_veff_weibull_014.rds")
if (!file.exists(veff_weibull_path)) {
    message(">>> Fitting individual varying-effects model ...")
    mod_veff_weibull <- generate_STb_model(
        dl,
        est_acqTime = TRUE,
        intrinsic_rate = "weibull", # keep constant baseline; frailty is the new lever
        veff_params = c("lambda_0"), # try c("lambda_0", "s") for both rates
        veff_type = "id"
    )
    fit_veff_weibull <- do.call(fit_STb, c(list(dl, mod_veff_weibull), SAMPLE_ARGS))
    STb_save(fit_veff_weibull, output_dir = FIT_DIR, name = "fit_veff_weibull_014")
} else {
    message(">>> Varying-effects fit already exists, skipping refit: ", veff_weibull_path)
}

# ===========================================================================
# 2. PLOT: load all three fits and compare
# ===========================================================================

get_plotdata <- function(event_data, model_fit) {
    if (!is.null(event_data) & !is.null(model_fit)) {
        ed <- event_data %>%
            group_by(trial) %>%
            mutate(n_trial = n())
        plot_data_obs <- ed %>%
            filter(time <= t_end) %>%
            group_by(trial) %>%
            arrange(time, .by_group = TRUE) %>%
            mutate(cum_prop = row_number() / n_trial, type = "observed") %>%
            select(trial, time, cum_prop, type) %>%
            ungroup()
        if (!(0 %in% plot_data_obs$time)) {
            plot_data_obs <- bind_rows(
                plot_data_obs,
                plot_data_obs %>% distinct(trial) %>% mutate(time = 0, cum_prop = 0, type = "observed")
            ) %>% arrange(trial, time)
        }
        draws_df <- posterior::as_draws_df(model_fit$draws(variables = "acquisition_time", inc_warmup = FALSE))
        ppc_long <- draws_df %>%
            select(starts_with("acquisition_time[")) %>%
            pivot_longer(
                cols = everything(),
                names_to = c("trial", "ind"),
                names_pattern = "acquisition_time\\[(\\d+),(\\d+)\\]",
                values_to = "time"
            ) %>%
            mutate(
                trial = as.integer(trial), ind = as.integer(ind),
                draw = rep(1:(nrow(draws_df)),
                    each = length(unique(.$trial)) * length(unique(.$ind))
                )
            )
        sample_idx <- sample(c(1:max(ppc_long$draw)), 250)
        ppc_long <- ppc_long %>% filter(draw %in% sample_idx)
        ppc_long <- ppc_long %>%
            group_by(draw, trial) %>%
            mutate(n_trial = n())
        ppc_long <- ppc_long %>% filter(time > -1)
        plot_data_ppc <- ppc_long %>%
            group_by(draw, trial, time) %>%
            summarise(n = n(), n_trial = first(n_trial), .groups = "drop") %>%
            group_by(draw, trial) %>%
            arrange(time) %>%
            mutate(cum_prop = cumsum(n) / n_trial)
        plot_data_ppc <- bind_rows(
            plot_data_ppc,
            plot_data_ppc %>% distinct(trial, draw) %>% mutate(time = 0, cum_prop = 0, type = "ppc")
        ) %>% arrange(trial, time)
        return(list("obs" = plot_data_obs, "pred" = plot_data_ppc))
    } else {
        NULL
    }
}

get_curveplots <- function(plot_data, cid) {
    if (!is.null(plot_data)) {
        ggplot(mapping = aes(x = time, y = cum_prop)) +
            geom_line(data = plot_data$pred, aes(group = interaction(draw, trial)), alpha = 0.1) +
            geom_line(data = plot_data$obs, linewidth = 1) +
            labs(x = "Time", y = "Cumulative proportion informed", title = cid) +
            theme_minimal()
    } else {
        return(NULL)
    }
}
# ---------------------------------------------------------------------------

set.seed(1)

# The three fits to compare, in order
model_files <- c(
    "constant baseline" = file.path(FIT_DIR, "fit_social_014.rds"),
    "Weibull hazard" = weibull_path,
    "varying effect on lambda_0" = veff_path,
    "varying effect on s" = veff_path_s,
    "Weibull + varying effect on lambda_0" = veff_weibull_path
)

fits <- purrr::map(model_files, readRDS)
pds <- purrr::map(fits, ~ get_plotdata(events[[i]], .x))

# Individual panels
panels <- purrr::imap(pds, ~ get_curveplots(.x, paste0(cid, " — ", .y)))

combined <- panels[[1]] + panels[[2]] + panels[[3]] + panels[[4]] + panels[[5]] +
    patchwork::plot_layout(ncol = 1) +
    patchwork::plot_annotation(
        title = paste0("carc ", cid, ": PPC diffusion curves by hazard model"),
        subtitle = "black = observed; grey = posterior-predictive draws"
    )

ggsave(file.path(PLOT_DIR, "014_PPC_comparison.png"),
    combined,
    width = 7, height = 16, dpi = 120, bg = "white"
)

# Also save each panel on its own, in case the combined layout is unwieldy
purrr::iwalk(panels, ~ ggsave(
    file.path(PLOT_DIR, paste0("curve_014_", gsub("[^a-z]+", "_", tolower(.y)), ".png")),
    .x,
    width = 7, height = 5, dpi = 120
))

# --- quick numeric summary: predicted asymptote vs observed ----------------
obs_asym <- max(pds[[1]]$obs$cum_prop)
band <- purrr::imap_dfr(pds, function(pd, nm) {
    a <- pd$pred %>%
        group_by(draw) %>%
        summarise(asym = max(cum_prop), .groups = "drop")
    tibble(
        model = nm,
        asym_med = median(a$asym),
        asym_lo = quantile(a$asym, 0.05),
        asym_hi = quantile(a$asym, 0.95)
    )
})
cat(sprintf("\nObserved final proportion informed: %.3f\n", obs_asym))
print(band)

# --- shape / heterogeneity parameters, if present --------------------------
for (nm in names(fits)) {
    # gamma = Weibull shape; sigma_id = SD of the per-individual (frailty) random effect
    v <- tryCatch(fits[[nm]]$draws(variables = c("gamma", "sigma_id")),
        error = function(e) NULL
    )
    if (!is.null(v) && length(v)) {
        cat("\n== ", nm, " ==\n")
        print(posterior::summarise_draws(v))
    }
}

message("\nDone. Plots written to: ", normalizePath(PLOT_DIR))
