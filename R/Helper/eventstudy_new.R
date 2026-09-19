library(data.table)
library(fixest)

# ==== run_es ==================================================================
# General-purpose event study around a unit-specific event (Kleven-style).
# Returns list(coef = tidy coefficient table, fits = fixest objects).

run_es <- function(
  data,
  outcomes,
  event_time    = "event_time",
  het           = NULL,
  controls      = NULL,
  fe            = NULL,
  base_period   = -1L,
  tau_window    = c(-5, 5),
  bin_endpoints = FALSE,
  winsorize     = NULL,
  weights       = NULL,
  vcov          = "hetero",
  cluster       = NULL,
  subset_fn     = NULL,
  loop_var      = NULL,
  loop_values   = NULL,
  loop_fn       = NULL
) {
  d <- copy(as.data.table(data))
  if (!is.null(subset_fn)) d <- subset_fn(d)

  single_loop <- is.null(loop_values)
  iter <- if (single_loop) list(NA) else loop_values
  if (is.null(loop_fn) && !is.null(loop_var))
    loop_fn <- function(dt, v) dt[get(loop_var) == v]

  out_list <- list(); fit_list <- list()

  for (lv in iter) {
    d_loop <- if (!single_loop && !is.null(loop_fn)) copy(loop_fn(d, lv)) else copy(d)

    if (bin_endpoints) {
      d_loop[, (event_time) := pmin(pmax(get(event_time), tau_window[1]), tau_window[2])]
    } else {
      d_loop <- d_loop[get(event_time) %between% tau_window]
    }

    for (out in outcomes) {
      d_o <- copy(d_loop)
      if (!is.null(winsorize)) {
        cap <- quantile(d_o[[out]], winsorize, na.rm = TRUE)
        d_o[, (out) := pmin(get(out), cap, na.rm = TRUE)]
      }

      es_term <- if (is.null(het))
        sprintf("i(%s, ref = %d)", event_time, base_period)
      else
        sprintf("factor(%s) + i(%s, factor(%s), ref = %d)", het, event_time, het, base_period)

      rhs <- paste(c(es_term, controls), collapse = " + ")
      f <- if (is.null(fe))
        as.formula(sprintf("%s ~ %s", out, rhs))
      else
        as.formula(sprintf("%s ~ %s | %s", out, rhs, fe))

      vc <- if (!is.null(cluster)) as.formula(paste0("~", cluster)) else vcov
      fit <- feols(f, data = d_o, vcov = vc, weights = weights, lean = TRUE)

      ct <- as.data.table(summary(fit)$coeftable, keep.rownames = "term")
      setnames(ct, c("Estimate", "Std. Error"), c("est", "se"))
      ct <- ct[grepl(paste0("^", event_time, "::"), term)]
      ct[, horizon := as.numeric(sub(paste0(event_time, "::(-?[0-9]+).*"), "\\1", term))]
      if (!is.null(het))
        ct[, het_level := sub(paste0(".*", het, "::([^:]+).*"), "\\1", term)]
      ct[, `:=`(ci_low = est - 1.96 * se, ci_high = est + 1.96 * se)]
      keep_cols <- c("horizon", if (!is.null(het)) "het_level", "est", "se", "ci_low", "ci_high")
      ct <- ct[, ..keep_cols]

      # Baseline rows
      if (!is.null(het)) {
        base_rows <- data.table(horizon = base_period, het_level = unique(ct$het_level),
                                est = 0, se = 0, ci_low = 0, ci_high = 0)
      } else {
        base_rows <- data.table(horizon = base_period, est = 0, se = 0, ci_low = 0, ci_high = 0)
      }
      ct <- rbind(ct, base_rows, use.names = TRUE)

      # Diagnostics
      by_cols <- c(event_time, if (!is.null(het)) het)
      diag <- d_o[, .(n_obs = .N, mean_y = mean(get(out), na.rm = TRUE)), by = by_cols]
      setnames(diag, by_cols, c("horizon", if (!is.null(het)) "het_level"))
      if (!is.null(het)) {
        diag[, het_level := as.character(het_level)]
        ct[, het_level := as.character(het_level)]
        ct <- merge(ct, diag[, .(horizon, het_level, n_obs)],
                    by = c("horizon", "het_level"), all.x = TRUE)
        ct <- merge(ct, diag[horizon == base_period, .(het_level, mean_base = mean_y)],
                    by = "het_level", all.x = TRUE)
      } else {
        ct <- merge(ct, diag[, .(horizon, n_obs)], by = "horizon", all.x = TRUE)
        ct[, mean_base := diag[horizon == base_period, mean_y]]
      }

      ct[, outcome := out]
      if (!single_loop) ct[, loop_value := lv]

      out_list[[length(out_list) + 1L]] <- ct
      fit_list[[paste(out, if (single_loop) "_" else lv, sep = "__")]] <- fit
    }
  }

  list(coef = rbindlist(out_list, fill = TRUE), fits = fit_list)
}

# Example:
# res <- run_es(
#   data        = dt,
#   outcomes    = c("inc_gross_labour", "inc_gross_total", "employed"),
#   het         = "q_npv",
#   controls    = "i(age)",
#   base_period = -1,
#   tau_window  = c(-5, 10),
#   loop_var    = "n_children_start",
#   loop_values = 1:4,
#   loop_fn     = function(dt, p) {
#     mothers <- dt[year == 2016 & n_children_start == p, unique(id_mother)]
#     dt[id_mother %in% mothers]
#   }
# )

# ==== tidy_es =================================================================
# Tidy an event-study model into a plotting/reporting frame.
#   m       : fixest model
#   name    : label for the model
#   pattern : regex selecting the interaction block (default: any i(year, X) term)
#   ref     : reference year, added back as an explicit 0 row

tidy_es <- function(m, name, pattern = "^year::", ref = 2016) {
  ct <- as.data.table(m$coeftable, keep.rownames = "term")
  setnames(ct, c("term", "estimate", "se", "tstat", "pval"))
  ct <- ct[grepl(pattern, term)]
  if (!nrow(ct)) stop("no terms matched: ", pattern)

  # year, and whatever the interacted variable/level is (NA when there is only one)
  ct[, year  := as.integer(sub("^year::(-?\\d+).*$", "\\1", term))]
  ct[, group := sub("^year::-?\\d+:?", "", term)]
  ct[group == "", group := NA_character_]

  dof <- degrees_freedom(m, type = "t")
  crit <- qt(0.975, dof)

  # explicit reference rows, one per group
  refs <- unique(ct[, .(group)])[, `:=`(term = NA_character_, year = ref,
    estimate = 0, se = NA_real_, tstat = NA_real_, pval = NA_real_)]

  out <- rbind(ct, refs, fill = TRUE)
  out[, `:=`(is_ref    = year == ref,
             conf_low  = estimate - crit * se,
             conf_high = estimate + crit * se,
             nobs = nobs(m), dof = dof, model = name)]
  setcolorder(out, c("model", "group", "year", "is_ref", "estimate", "se",
                     "conf_low", "conf_high", "tstat", "pval", "nobs", "dof", "term"))
  setorder(out, group, year)[]
}

# Example:
# es <- rbindlist(list(
#   tidy_es(m_dd1,  "DD1",  pattern = "par_2"),
#   tidy_es(m_dd2,  "DD2",  pattern = "bin_a1_f"),
#   tidy_es(m_ddd1, "DDD1", pattern = "par_2_hi")
# ))