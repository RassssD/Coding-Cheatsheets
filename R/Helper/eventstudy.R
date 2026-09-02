
#===============================================================================
# Rasmus Duret
# event_study(): flexible event-study / DD / DDD estimator, wrapping fixest::feols.
# Supports multiple outcomes, arbitrary controls, optional unit/time fixed effects,
# endpoint binning of distant leads/lags, one categorical heterogeneity variable,
# and a treatment-control "contrast" (1 var = DD, 2 vars = DDD), all in one call.
# plot_event_study(): quick ggplot of the coefficient table event_study() returns.
#
# Depends on: data.table, fixest, ggplot2 (all called via `::`, nothing attached).
#===============================================================================


#===============================================================================
# 1 FUNCTION: event_study
#===============================================================================

# Args:
#   data          data.frame/data.table (panel or cross-section)
#   outcome       chr scalar or vector - outcome column name(s); one model per outcome
#   event_time    chr - column holding relative event time (already centered by caller)
#   ref_period    omitted/reference event-time value (default -1)
#   bin_k         optional: collapse leads/lags beyond +/-bin_k into endpoint bins
#   controls      chr vector of RHS terms (plain names or expressions, e.g. "log(x)")
#   unit_fe       chr - unit fixed-effect column name (NULL = none)
#   time_fe       chr - calendar time fixed-effect column name (NULL = none)
#   heterogeneity chr - one categorical column; event-time effects fully interacted with it
#   contrast      chr vector (length 1 or 2) - treatment/control var(s) for DD / DDD
#   cluster       chr - cluster variable; defaults to unit_fe if not given
#   ...           passed through to fixest::feols (weights, vcov, ssc, ...)
#
# Returns a list with class "event_study":
#   models  named list of fixest model objects, one per outcome
#   coefs   tidy data.table of event-time coefficients (one row per event_time x group x outcome)
#   meta    the resolved call inputs, for use by plot_event_study() or later re-runs

event_study <- function(data,
                         outcome,
                         event_time,
                         ref_period    = -1,
                         bin_k         = NULL,
                         controls      = NULL,
                         unit_fe       = NULL,
                         time_fe       = NULL,
                         heterogeneity = NULL,
                         contrast      = NULL,
                         cluster       = NULL,
                         ...) {

  stopifnot(is.data.frame(data))
  stopifnot(length(heterogeneity) <= 1L)
  if (length(contrast) > 2L) stop("`contrast` supports at most 2 variables (DD or DDD).")

  dt <- data.table::as.data.table(data)

  # --- validate inputs -----------------------------------------------------
  bare_controls <- controls[grepl("^[A-Za-z._][A-Za-z0-9._]*$", controls)]
  required_cols <- c(outcome, event_time, unit_fe, time_fe, heterogeneity, contrast, bare_controls)
  missing_cols  <- setdiff(required_cols, names(dt))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in data: ", paste(missing_cols, collapse = ", "))
  }

  # --- event-time construction (optional endpoint binning) ------------------
  et <- dt[[event_time]]
  if (!is.null(bin_k)) et <- pmin(pmax(et, -bin_k), bin_k)
  if (!(ref_period %in% et)) {
    stop("`ref_period` (", ref_period, ") not found among event-time values (after binning).")
  }
  dt[, .__et := et]

  # --- combine heterogeneity + contrast into one grouping factor ------------
  # fixest::i() only accepts a single interaction variable, so multiple grouping
  # vars are glued into one composite factor (the full Cartesian cross of levels).
  group_vars <- c(heterogeneity, contrast)
  grp_sep <- "@@"
  i_var <- NULL
  if (length(group_vars) == 1) {
    i_var <- group_vars
  } else if (length(group_vars) > 1) {
    dt[, .__grp := interaction(.SD, drop = TRUE, sep = grp_sep), .SDcols = group_vars]
    i_var <- ".__grp"
  }

  # --- build RHS -------------------------------------------------------------
  et_term <- if (is.null(i_var)) {
    sprintf("i(.__et, ref = %s)", ref_period)
  } else {
    sprintf("i(.__et, %s, ref = %s)", i_var, ref_period)
  }
  rhs <- paste(c(et_term, controls), collapse = " + ")

  # i_var's own level effects must be absorbed separately (as FE), otherwise the
  # reference-period baseline is forced equal across groups and the interaction
  # coefficients above no longer represent each group's own event-time path.
  fe_terms <- unique(c(unit_fe, time_fe, i_var))
  if (length(fe_terms) > 0) {
    rhs <- paste0(rhs, " | ", paste(fe_terms, collapse = " + "))
  }

  # informational: flag when i_var's FE is redundant with (nested in) unit_fe
  if (!is.null(i_var) && !is.null(unit_fe)) {
    grp_per_unit <- dt[, data.table::uniqueN(get(i_var)), by = c(unit_fe)]$V1
    if (max(grp_per_unit) == 1L) {
      message(sprintf(
        "Note: `%s` is constant within `%s` - its fixed effect is redundant with the unit FE (harmless).",
        paste(group_vars, collapse = " x "), unit_fe
      ))
    }
  }

  cluster_var <- if (!is.null(cluster)) cluster else unit_fe

  # --- estimate one model per outcome -----------------------------------------
  outcome <- as.character(outcome)
  models <- vector("list", length(outcome))
  names(models) <- outcome
  for (y in outcome) {
    form <- stats::as.formula(paste(y, "~", rhs))
    models[[y]] <- fixest::feols(form, data = dt, cluster = cluster_var, ...)
  }

  # --- tidy coefficients into one stacked table -------------------------------
  extract_coefs <- function(model, outcome_name) {
    ct <- fixest::coeftable(model)
    all_terms <- rownames(ct)
    keep  <- grepl("^\\.__et::", all_terms)
    terms <- all_terms[keep]
    ct    <- ct[keep, , drop = FALSE]

    ci <- tryCatch(stats::confint(model), error = function(e) NULL)
    if (!is.null(ci)) ci <- ci[match(terms, rownames(ci)), , drop = FALSE]

    body     <- sub("^\\.__et::", "", terms)               # "-2" or "-2:.__grp::North@@0"
    et_val   <- as.numeric(sub(":.*$", "", body))
    rest     <- sub("^[^:]*:?", "", body)                   # "" or ".__grp::North@@0"
    grp_label <- ifelse(rest == "", NA_character_, sub(".*::", "", rest))

    tstat_col <- grep("^(t|z) value$", colnames(ct), value = TRUE)[1]

    out <- data.table::data.table(
      outcome    = outcome_name,
      event_time = et_val,
      group      = grp_label,
      term       = terms,
      estimate   = ct[, "Estimate"],
      std.error  = ct[, "Std. Error"],
      statistic  = ct[, tstat_col],
      p.value    = ct[, "Pr(>|t|)"]
    )
    if (!is.null(ci)) out[, `:=`(conf.low = ci[, 1], conf.high = ci[, 2])]
    out
  }

  coef_dt <- data.table::rbindlist(lapply(outcome, function(y) extract_coefs(models[[y]], y)))
  if (anyNA(coef_dt$event_time)) {
    stop("Could not parse event-time coefficient names - inspect fixest::coeftable() directly; ",
         "the naming convention this parser expects may not match your fixest version.")
  }

  if (length(group_vars) > 1) {
    split_vals <- data.table::tstrsplit(coef_dt$group, grp_sep, fixed = TRUE)
    for (j in seq_along(group_vars)) coef_dt[[group_vars[j]]] <- split_vals[[j]]
    coef_dt[, group := NULL]
  } else if (length(group_vars) == 1) {
    data.table::setnames(coef_dt, "group", group_vars)
  } else {
    coef_dt[, group := NULL]
  }
  data.table::setorderv(coef_dt, c("outcome", group_vars, "event_time"))

  meta <- list(
    outcome = outcome, event_time = event_time, ref_period = ref_period, bin_k = bin_k,
    controls = controls, unit_fe = unit_fe, time_fe = time_fe,
    heterogeneity = heterogeneity, contrast = contrast, group_vars = group_vars,
    cluster = cluster_var, formula_rhs = rhs
  )

  structure(list(models = models, coefs = coef_dt, meta = meta), class = "event_study")
}


#===============================================================================
# 2 FUNCTION: plot_event_study
#===============================================================================

# Quick ggplot of an event_study() result.
#   - heterogeneity levels (if any) are overlaid as coloured lines on one panel,
#     never faceted apart.
#   - only one outcome is shown by default (the first one estimated); pass
#     outcome = c(...) explicitly to facet across several.
#   - if a `contrast` was used (DD/DDD), only the derived contrast effect is
#     plotted (e.g. treated - control, or the triple-difference for DDD) -
#     not each group's raw path - combined via the model's own vcov so the
#     SE/CI reflect the true covariance rather than a naive sum of variances.
# Args: es (event_study object), outcome (NULL = first outcome only, or a vector
#       of outcomes to facet over), groups (NULL = all heterogeneity levels, or
#       a subset of them to show), colours (optional vector of colours for the
#       groups, passed to scale_colour_manual - name it by group level for a
#       stable mapping), ncol (facet column count when >1 outcome).

plot_event_study <- function(es, outcome = NULL, groups = NULL, colours = NULL, ncol = NULL) {
  stopifnot(inherits(es, "event_study"))

  outcome <- if (is.null(outcome)) es$meta$outcome[1] else intersect(outcome, es$meta$outcome)
  if (length(outcome) == 0) {
    stop("`outcome` not found among the fitted outcomes: ", paste(es$meta$outcome, collapse = ", "))
  }

  heterogeneity <- es$meta$heterogeneity
  contrast      <- es$meta$contrast
  ref           <- es$meta$ref_period

  if (!is.null(groups) && is.null(heterogeneity)) {
    stop("`groups` was given but this event_study has no `heterogeneity` variable to filter on.")
  }

  plot_dt <- data.table::rbindlist(lapply(outcome, function(y) {
    dt_y <- es$coefs[es$coefs$outcome == y, ]
    out  <- if (is.null(contrast)) {
      .es_raw_series(dt_y, heterogeneity)
    } else {
      .es_contrast_series(dt_y, es$models[[y]], contrast, heterogeneity)
    }
    out[, outcome := y]
    out
  }))

  if (!is.null(groups)) {
    plot_dt <- plot_dt[plot_dt$group %in% groups, ]
    if (nrow(plot_dt) == 0) {
      stop("None of `groups` match the heterogeneity levels: ",
           paste(unique(es$coefs[[heterogeneity]]), collapse = ", "))
    }
  }

  key_cols <- c("outcome", if (!is.null(heterogeneity)) "group")
  ref_rows <- unique(plot_dt[, ..key_cols])
  ref_rows[, event_time := ref]
  ref_rows[, `:=`(estimate = 0, std.error = 0, conf.low = 0, conf.high = 0)]
  plot_dt <- data.table::rbindlist(list(plot_dt, ref_rows), use.names = TRUE, fill = TRUE)
  data.table::setorderv(plot_dt, c(key_cols, "event_time"))

  has_group <- !is.null(heterogeneity)
  dodge <- ggplot2::position_dodge(width = 0.3)
  p <- if (has_group) {
    ggplot2::ggplot(plot_dt, ggplot2::aes(event_time, estimate, colour = group))
  } else {
    ggplot2::ggplot(plot_dt, ggplot2::aes(event_time, estimate))
  }

  p <- p +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    ggplot2::geom_vline(xintercept = ref, linetype = "dotted", colour = "grey50") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = conf.low, ymax = conf.high), width = 0.2, position = dodge) +
    ggplot2::geom_line(position = dodge) + ggplot2::geom_point(position = dodge) +
    ggplot2::labs(
      x = "Event time", y = "Estimate", title = "Event study",
      subtitle = if (!is.null(contrast)) paste("Contrast effect:", paste(contrast, collapse = " x ")) else NULL,
      colour = heterogeneity
    ) +
    ggplot2::theme_minimal()

  if (has_group && !is.null(colours)) p <- p + ggplot2::scale_colour_manual(values = colours)
  if (length(outcome) > 1) p <- p + ggplot2::facet_wrap(~outcome, ncol = ncol)
  p
}


# --- internal helpers for plot_event_study ----------------------------------

# No contrast: nothing to derive, just standardize column names so heterogeneity
# (if any) becomes the generic "group" column plot_event_study() colours by.
.es_raw_series <- function(dt_y, heterogeneity) {
  out <- data.table::copy(dt_y)
  if (!is.null(heterogeneity)) {
    data.table::setnames(out, heterogeneity, "group")
  } else {
    out[, group := NA_character_]
  }
  out[, c("event_time", "group", "estimate", "std.error", "conf.low", "conf.high"), with = FALSE]
}

# Contrast given: collapse the 2^k contrast cells (k = 1 for DD, 2 for DDD) into
# one signed effect per event_time (and per heterogeneity level, if present).
# Combined via the model's vcov so Var(sum sign_i * beta_i) = sign' V sign is
# exact, not an independence-assuming sum of the individual variances.
.es_contrast_series <- function(dt_y, model, contrast, heterogeneity) {
  V     <- stats::vcov(model)
  tcrit <- stats::qt(0.975, fixest::degrees_freedom(model, type = "t"))

  # for each contrast var, the higher-sorted level is treated as "treatment" (+1)
  ref_levels <- lapply(contrast, function(cv) sort(unique(dt_y[[cv]])))
  lapply(seq_along(contrast), function(j) {
    if (length(ref_levels[[j]]) != 2) {
      stop("Contrast variable `", contrast[j], "` must have exactly 2 levels to plot a DD/DDD effect.")
    }
  })

  by_vars <- c("event_time", heterogeneity)
  combos  <- unique(dt_y[, ..by_vars])

  rows <- lapply(seq_len(nrow(combos)), function(i) {
    sub <- dt_y
    for (bv in by_vars) sub <- sub[sub[[bv]] == combos[[bv]][i], ]

    signs <- Reduce(`*`, lapply(seq_along(contrast), function(j) {
      ifelse(sub[[contrast[j]]] == ref_levels[[j]][2], 1, -1)
    }))

    est     <- sum(signs * sub$estimate)
    var_est <- as.numeric(t(signs) %*% V[sub$term, sub$term] %*% signs)
    se      <- sqrt(var_est)

    data.table::data.table(
      event_time = combos$event_time[i],
      group      = if (!is.null(heterogeneity)) combos[[heterogeneity]][i] else NA_character_,
      estimate   = est, std.error = se,
      conf.low   = est - tcrit * se, conf.high = est + tcrit * se
    )
  })
  data.table::rbindlist(rows)
}


#===============================================================================
# 3 EXAMPLES
#===============================================================================

# --- Example 1: plain event study -------------------------------------------
# Staggered adoption across units, one control, unit + time FE, endpoint binning.
# No heterogeneity/contrast - the simplest case.

set.seed(1)
N1 <- 200; T1 <- 12
ex1 <- data.table::CJ(unit = 1:N1, time = 1:T1)
adopt1 <- data.table::data.table(unit = 1:N1, adopt = sample(4:10, N1, replace = TRUE))
ex1 <- merge(ex1, adopt1, by = "unit")
ex1[, event_time := time - adopt]
ex1[, size       := rnorm(.N)]                          # time-varying control
ex1[, time_shock := rnorm(T1)[time]]
ex1[, truth      := ifelse(event_time >= 0, 2 + 0.3 * pmin(event_time, 5), 0)]
ex1[, y          := 5 + 0.8 * size + time_shock + truth + rnorm(.N, sd = 1)]

es1 <- event_study(
  data       = ex1,
  outcome    = "y",
  event_time = "event_time",
  ref_period = -1,
  bin_k      = 5,
  controls   = "size",
  unit_fe    = "unit",
  time_fe    = "time"
)
print(es1$coefs)
plot_event_study(es1)


# --- Example 2: DD x heterogeneity (~DDD via combination) --------------------
# Staggered per-unit adoption dates (every unit is "assigned" a date, but only
# treated units actually respond), treated vs. control (contrast), effect size
# differs by region (heterogeneity) -> composite .__grp has 4 cells. treated and
# region are both time-invariant per unit, so expect the redundant-FE message.
# (A single shared adoption date for everyone would make event_time a deterministic
# function of calendar time, colliding with time_fe - staggering avoids that.)

set.seed(2)
N2 <- 300; T2 <- 10
ex2 <- data.table::CJ(unit = 1:N2, time = 1:T2)
info2 <- data.table::data.table(
  unit    = 1:N2,
  adopt   = sample(4:8, N2, replace = TRUE),
  treated = sample(0:1, N2, replace = TRUE),
  region  = sample(c("North", "South"), N2, replace = TRUE)
)
ex2 <- merge(ex2, info2, by = "unit")
ex2[, event_time := time - adopt]
ex2[, unit_shock := rnorm(N2)[unit]]
ex2[, time_shock := rnorm(T2)[time]]
ex2[, effect := ifelse(event_time >= 0 & treated == 1,
                        ifelse(region == "North", 3, 1) * pmin(event_time + 1, 4), 0)]
ex2[, y := 4 + unit_shock + time_shock + effect + rnorm(.N, sd = 1)]

es2 <- event_study(
  data          = ex2,
  outcome       = "y",
  event_time    = "event_time",
  ref_period    = -1,
  bin_k         = 4,             # extreme event-time cells are sparse for some groups; bin them
  unit_fe       = "unit",
  time_fe       = "time",
  heterogeneity = "region",
  contrast      = "treated"
)
print(es2$coefs)
# plot: one panel, two coloured lines (North, South), each the region's own
# DD effect (treated - control) at each event time - not the 4 raw group paths.
plot_event_study(es2)
# custom colours, and/or show only one region:
plot_event_study(es2, colours = c(North = "#1b9e77", South = "#d95f02"))
plot_event_study(es2, groups = "North")


# --- Example 3: genuine two-variable DDD, multiple outcomes, no FE -----------
# "Regression-augmented time series" style: no unit/time FE at all, two contrast
# variables (policy_A, policy_B) crossed for a true triple-difference, two outcomes
# estimated at once, plus endpoint binning and a time-varying control.

set.seed(3)
N3 <- 250; T3 <- 8
ex3 <- data.table::CJ(unit = 1:N3, time = 1:T3)
info3 <- data.table::data.table(
  unit     = 1:N3,
  policy_A = sample(0:1, N3, replace = TRUE),
  policy_B = sample(0:1, N3, replace = TRUE)
)
ex3 <- merge(ex3, info3, by = "unit")
ex3[, event_time := time - 4]
ex3[, x1 := rnorm(.N)]
ex3[, ddd_effect := ifelse(event_time >= 0 & policy_A == 1 & policy_B == 1,
                           2.5 * (event_time + 1), 0)]
ex3[, y1 := 3 + 0.5 * x1 + ddd_effect + rnorm(.N, sd = 1.2)]
ex3[, y2 := 1 + 0.2 * x1 + 0.5 * ddd_effect + rnorm(.N, sd = 1)]

es3 <- event_study(
  data       = ex3,
  outcome    = c("y1", "y2"),
  event_time = "event_time",
  ref_period = -1,
  bin_k      = 3,
  controls   = "x1",
  unit_fe    = NULL,
  time_fe    = NULL,
  contrast   = c("policy_A", "policy_B")
)
print(es3$coefs)
plot_event_study(es3)                        # default: y1 only, single panel, DDD effect
plot_event_study(es3, outcome = c("y1", "y2"))  # explicit multi-outcome -> faceted
