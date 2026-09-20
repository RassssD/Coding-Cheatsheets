# =============================================================================
# ggplot theme sandbox
# -----------------------------------------------------------------------------
# Setup (one-off):
#   install.packages(c("shiny","ggplot2","dplyr","tibble","showtext","sysfonts",
#                       "colourpicker","colorspace","png"))
#
# Run:
#   shiny::runApp("app.R")   # from this file's folder
#
# First run needs internet to fetch Google Fonts via showtext. If a font
# fails to load it silently falls back to the system default — the app
# still works, plots just render in the fallback family.
# =============================================================================

library(shiny)
library(ggplot2)
library(dplyr)
library(tibble)
library(showtext)
library(sysfonts)
library(colourpicker)
library(colorspace)
library(png)

# ---- Fonts ------------------------------------------------------------------
# Shortlisted to the three fonts actually under consideration.
font_families <- c("Inter", "Fira Sans", "Lexend")

for (f in font_families) {
  if (!(f %in% sysfonts::font_families())) {
    try(sysfonts::font_add_google(f, f), silent = TRUE)
  }
}
showtext::showtext_auto()
showtext::showtext_opts(dpi = 96)

# ---- Palettes ---------------------------------------------------------------
palettes_qual <- list(
  "Okabe-Ito"  = c("#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7"),
  "Tol bright" = c("#4477AA","#EE6677","#228833","#CCBB44","#66CCEE","#AA3377","#BBBBBB"),
  "Tol muted"  = c("#CC6677","#332288","#DDCC77","#117733","#88CCEE","#882255","#44AA99"),
  "Wong"       = c("#000000","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00")
)
palettes_seq <- c("viridis", "magma", "plasma", "cividis", "mako")

# Names must match plot_registry's keys in server() — used to populate the
# "Print size" tab's plot picker before plot_registry itself exists.
plot_choices <- c("Time series", "Event study", "Binscatter", "Density",
                  "Grouped bar", "Stacked area", "Small multiples", "Forest plot")

# Page presets for the "Print size" tab. Physical mm, matching draw_acc.R's
# own unit. Presentation slide = the modern PowerPoint/Keynote/Google Slides
# 16:9 widescreen default (13.333 x 7.5in). Each also carries dummy "standard
# insert" content (a heading + body text/bullets, at font sizes typical for
# that format) so the chart renders next to something familiar to judge
# scale against, rather than alone on a blank page.
page_presets <- list(
  "A4 page" = list(
    w_mm = 210, h_mm = 297, note = "210 × 297mm", margin_mm = 25,
    title = "3. Results", title_pt = 14, body_pt = 11,
    body_lines = c(
      "Table 2 reports the main estimates. Figure 1 shows the corresponding",
      "event-study coefficients, with 95% confidence intervals shaded around",
      "each point estimate. Pre-trend coefficients are jointly insignificant",
      "(p = 0.41), consistent with the parallel-trends assumption."
    ),
    caption = "Figure 1. Event-study estimates, treatment at t = 0."
  ),
  "Presentation slide" = list(
    w_mm = 338.67, h_mm = 190.5, note = "16:9, 338.7 × 190.5mm", margin_mm = 15,
    title = "Employment effects", title_pt = 32, body_pt = 20,
    body_lines = c("Pre-trends are flat and jointly insignificant"),
    caption = "Source: author's calculations."
  ),
  "Mobile screen" = list(
    w_mm = 70, h_mm = 150, note = "≈70 × 150mm", margin_mm = 5,
    title = "Jobs report", title_pt = 15, body_pt = 10,
    body_lines = c("New data show manufacturing", "employment continued its", "decade-long decline."),
    caption = NULL
  )
)

# Static display box for the tab's plotOutput -- fit each page's true mm
# aspect ratio into a 440x650px budget so nothing renders oddly cropped or
# huge; the actual rendered content is always true-to-scale regardless.
fit_dims <- function(w_mm, h_mm, max_w = 440, max_h = 650) {
  scale <- min(max_w / w_mm, max_h / h_mm)
  list(w = round(w_mm * scale), h = round(h_mm * scale))
}
for (.nm in names(page_presets)) {
  d <- fit_dims(page_presets[[.nm]]$w_mm, page_presets[[.nm]]$h_mm)
  page_presets[[.nm]]$disp_w <- d$w
  page_presets[[.nm]]$disp_h <- d$h
}
rm(.nm)

# ---- Merge brand colours into base palette ---------------------------------
# Slot primary at position 1, secondary at 2. Drop the base colour nearest
# (in RGB space) to each, so the palette stays 7 long and colourblind-safe.
merged_palette <- function(base, primary, secondary) {
  brgb <- t(col2rgb(base))
  drop_nearest <- function(mat, target) {
    trgb <- as.numeric(col2rgb(target))
    d <- sqrt(rowSums((mat - matrix(trgb, nrow(mat), 3, byrow = TRUE))^2))
    which.min(d)
  }
  i1 <- drop_nearest(brgb, primary)
  brgb2 <- brgb[-i1, , drop = FALSE]
  base2 <- base[-i1]
  i2 <- drop_nearest(brgb2, secondary)
  base3 <- base2[-i2]
  c(primary, secondary, base3)
}

# ---- Colour ramps (tint/shade scales) ---------------------------------------
# n variants from light -> dark, generated in HCL space (colorspace::lighten/
# darken) rather than naive RGB blending, so steps stay perceptually even. n
# is forced odd so the colour you picked sits exactly at the centre of its
# own ramp.
make_ramp <- function(base_hex, n = 5, spread = 0.6) {
  if (n %% 2 == 0) n <- n + 1
  amounts <- seq(-spread, spread, length.out = n)
  vapply(amounts, function(a) {
    if (a < -1e-9)      colorspace::lighten(base_hex, -a, method = "relative")
    else if (a > 1e-9)  colorspace::darken(base_hex,   a, method = "relative")
    else                base_hex
  }, character(1))
}

# ---- Colourblind-safety check ------------------------------------------------
# Flags palette pairs that become hard to tell apart once simulated for a
# colour-vision deficiency: CIE76 dE in Lab space below `thresh`. A rough
# heuristic for "confusable at a glance in a chart", not a certified audit.
flag_confusable <- function(pal, thresh = 12) {
  if (length(pal) < 2) return(character(0))
  lab <- grDevices::convertColor(t(grDevices::col2rgb(pal)) / 255,
                                 from = "sRGB", to = "Lab")
  d <- as.matrix(stats::dist(lab))
  diag(d) <- Inf
  idx <- which(d < thresh, arr.ind = TRUE)
  idx <- idx[idx[, 1] < idx[, 2], , drop = FALSE]
  if (nrow(idx) == 0) return(character(0))
  sprintf("%s vs %s (ΔE ≈ %.1f)",
          toupper(pal[idx[, 1]]), toupper(pal[idx[, 2]]), d[idx])
}

# =============================================================================
# Simulated data — econ-flavoured
# =============================================================================
set.seed(42)

# 1. Country GDP index, 1980–2020, seven countries
countries7 <- c("USA","UK","Germany","Japan","France","Italy","Canada")
trends <- c(USA = 0.028, UK = 0.023, Germany = 0.020, Japan = 0.015,
            France = 0.021, Italy = 0.014, Canada = 0.025)
gdp_data <- expand.grid(year = 1980:2020, country = countries7,
                        stringsAsFactors = FALSE) |>
  as_tibble() |>
  group_by(country) |>
  arrange(year, .by_group = TRUE) |>
  mutate(shock  = ifelse(year == 2008, -0.03, 0) + ifelse(year == 2009, -0.02, 0),
         growth = trends[country] + shock + rnorm(n(), 0, 0.012),
         gdp_index = 100 * cumprod(1 + growth)) |>
  ungroup() |>
  select(year, country, gdp_index)

# 2. Mincer-flavoured cross-section
n_m <- 500
mincer <- tibble(
  schooling = pmax(6,  pmin(20, round(rnorm(n_m, 13, 3)))),
  exper     = pmax(0,  pmin(45, round(rnorm(n_m, 18, 10)))),
  log_wage  = 1.8 + 0.09 * schooling + 0.045 * exper - 0.0007 * exper^2 +
    rnorm(n_m, 0, 0.32)
)

# 3. Event study, three specifications
event_study <- expand.grid(period = -8:8,
                           spec = c("Baseline", "+ Controls", "Subgroup A"),
                           stringsAsFactors = FALSE) |>
  as_tibble() |>
  mutate(true = ifelse(period < 0, 0,
                       case_when(spec == "Baseline"   ~ 0.045 * period,
                                 spec == "+ Controls" ~ 0.040 * period,
                                 spec == "Subgroup A" ~ 0.070 * period)),
         estimate = true + rnorm(n(), 0, 0.012),
         se       = 0.020 + 0.006 * abs(period) / 8,
         lower    = estimate - 1.96 * se,
         upper    = estimate + 1.96 * se,
         estimate = ifelse(period == -1, 0, estimate),
         lower    = ifelse(period == -1, 0, lower),
         upper    = ifelse(period == -1, 0, upper))

# 4. Treated vs control outcome distribution
density_data <- bind_rows(
  tibble(group = "Control", y = rnorm(1500, 10.20, 0.55)),
  tibble(group = "Treated", y = rnorm(1500, 10.55, 0.60))
)

# 5. Sectoral employment shares, 3 economies
sectors <- c("Agriculture","Manufacturing","Services","Finance","Public")
sector_shares <- tribble(
  ~country,  ~sector,          ~share,
  "USA",     "Agriculture",     1.4,
  "USA",     "Manufacturing",   9.8,
  "USA",     "Services",       58.0,
  "USA",     "Finance",         7.5,
  "USA",     "Public",         15.3,
  "Germany", "Agriculture",     1.2,
  "Germany", "Manufacturing",  18.5,
  "Germany", "Services",       52.0,
  "Germany", "Finance",         3.8,
  "Germany", "Public",         14.5,
  "Japan",   "Agriculture",     3.4,
  "Japan",   "Manufacturing",  15.7,
  "Japan",   "Services",       56.0,
  "Japan",   "Finance",         2.5,
  "Japan",   "Public",         12.6
) |>
  mutate(sector = factor(sector, levels = sectors))

# 6. Regional small multiples
regions <- c("Northeast","Midwest","South","West","Mountain","Pacific")
region_trends <- c(Northeast = 0.020, Midwest = 0.016, South = 0.028,
                   West = 0.030, Mountain = 0.024, Pacific = 0.027)
region_data <- expand.grid(year = 2000:2020, region = regions,
                           stringsAsFactors = FALSE) |>
  as_tibble() |>
  group_by(region) |>
  arrange(year, .by_group = TRUE) |>
  mutate(shock  = ifelse(year == 2008, -0.025, 0) + ifelse(year == 2020, -0.035, 0),
         growth = region_trends[region] + shock + rnorm(n(), 0, 0.010),
         y      = 100 * cumprod(1 + growth)) |>
  ungroup() |>
  select(year, region, y)

# 7. Forest / coefficient plot
forest <- tibble(
  var = c("Age","Education","Female","Married","Urban",
          "Union","Public sector","Immigrant"),
  estimate = c(0.008, 0.087, -0.145, 0.062, 0.093, 0.121, 0.034, -0.078),
  se       = c(0.002, 0.006,  0.021, 0.019, 0.020, 0.024, 0.028,  0.031)
) |>
  mutate(var   = factor(var, levels = rev(var)),
         lower = estimate - 1.96 * se,
         upper = estimate + 1.96 * se)

# 8. Stacked area — sectoral composition of one economy
area_data <- expand.grid(year = 1980:2020, sector = sectors,
                         stringsAsFactors = FALSE) |>
  as_tibble() |>
  mutate(share = case_when(
    sector == "Agriculture"   ~ pmax(0.5, 4.5 - 0.08 * (year - 1980) + rnorm(n(), 0, 0.15)),
    sector == "Manufacturing" ~ pmax(6,  25   - 0.35 * (year - 1980) + rnorm(n(), 0, 0.40)),
    sector == "Services"      ~         45   + 0.20 * (year - 1980) + rnorm(n(), 0, 0.40),
    sector == "Finance"       ~          3   + 0.10 * (year - 1980) + rnorm(n(), 0, 0.20),
    sector == "Public"        ~         15   + 0.03 * (year - 1980) + rnorm(n(), 0, 0.30)
  )) |>
  group_by(year) |>
  mutate(share = 100 * share / sum(share)) |>
  ungroup() |>
  mutate(sector = factor(sector, levels = sectors))

# =============================================================================
# Theme builder
# =============================================================================
theme_sandbox <- function(base_size = 12,
                          base_family = "sans",
                          title_family = base_family,
                          title_ratio = 1.2,
                          grid = "both",
                          minor = TRUE,
                          axis_lines = TRUE,
                          legend_pos = "bottom",
                          legend_key_pt = 10,
                          scheme = list(bg = "white", fg = "grey20",
                                        mid = "grey30", soft = "grey40",
                                        grid = "#E5E5E5", minor = "#F0F0F0",
                                        strip = "grey95", axis = "grey30")) {

  s <- scheme

  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.title       = element_text(family = title_family, face = "bold",
                                      size = base_size * title_ratio,
                                      colour = s$fg,
                                      margin = margin(b = 6)),
      plot.subtitle    = element_text(family = base_family,
                                      size = base_size,
                                      colour = s$mid,
                                      margin = margin(b = 8)),
      plot.caption     = element_text(family = base_family,
                                      size = base_size * 0.8,
                                      colour = s$soft, hjust = 0,
                                      margin = margin(t = 8)),
      axis.title       = element_text(size = base_size * 0.95, colour = s$fg),
      axis.text        = element_text(size = base_size * 0.85, colour = s$mid),
      axis.line        = if (axis_lines) element_line(colour = s$axis, linewidth = 0.4) else element_blank(),
      axis.ticks       = if (axis_lines) element_line(colour = s$axis, linewidth = 0.4) else element_blank(),
      panel.background = element_rect(fill = s$bg, colour = NA),
      plot.background  = element_rect(fill = s$bg, colour = NA),
      panel.grid.major.x = if (grid == "both") element_line(colour = s$grid, linewidth = 0.3) else element_blank(),
      panel.grid.major.y = if (grid %in% c("both","horizontal")) element_line(colour = s$grid, linewidth = 0.3) else element_blank(),
      panel.grid.minor.x = if (minor && grid == "both") element_line(colour = s$minor, linewidth = 0.2) else element_blank(),
      panel.grid.minor.y = if (minor && grid %in% c("both","horizontal")) element_line(colour = s$minor, linewidth = 0.2) else element_blank(),
      legend.position  = legend_pos,
      legend.title     = element_text(size = base_size * 0.9, colour = s$fg),
      legend.text      = element_text(size = base_size * 0.85, colour = s$mid),
      legend.key.size  = grid::unit(legend_key_pt, "pt"),
      strip.text       = element_text(size = base_size * 0.9, face = "bold", colour = s$fg),
      strip.background = element_rect(fill = "white", colour = "black", linewidth = 0.4),
      plot.margin      = margin(10, 12, 8, 10)
    )
}

# ---- Theme presets ------------------------------------------------------------
# Each preset is a starting point, not a lock: picking one just pushes these
# values into the controls below via update*Input(); every control stays live
# and editable afterwards, and the "Code" tab reflects whatever the controls
# currently say regardless of which (if any) preset was last picked.
theme_presets <- list(
  # Baseline / default: casual use (social posts, blog posts, etc.) — mirrors
  # the sandbox's own default control values exactly, so "Default" and
  # "Custom" with nothing touched yet are the same theme.
  "Default" = list(
    dark = FALSE, base_family = "Inter", title_family = "(same as base)",
    base_size = 14, title_ratio = 1.2, grid = "horizontal", minor = FALSE,
    axis_lines = TRUE, panel_bg = "white", legend_pos = "bottom", legend_key = 14,
    line_width = 1.2, point_size = 2.0, alpha = 0.3
  ),
  # Academic article: print-column sized, minimal decoration (no minor grid),
  # restrained line/point weight and alpha so it holds up in greyscale print.
  "Academic article" = list(
    dark = FALSE, base_family = "Fira Sans", title_family = "(same as base)",
    base_size = 11, title_ratio = 1.1, grid = "horizontal", minor = FALSE,
    axis_lines = TRUE, panel_bg = "white", legend_pos = "bottom", legend_key = 8,
    line_width = 0.7, point_size = 1.3, alpha = 0.5
  ),
  # Academic presentation: same font and text sizing as Default (big enough
  # already, no need to go larger) — legible from the back of a room comes
  # from bolder lines/points and a cleaner (gridline-free) panel instead.
  # Legend stays at the bottom like every other preset.
  "Academic presentation" = list(
    dark = FALSE, base_family = "Inter", title_family = "(same as base)",
    base_size = 14, title_ratio = 1.2, grid = "none", minor = FALSE,
    axis_lines = TRUE, panel_bg = "white", legend_pos = "bottom", legend_key = 18,
    line_width = 1.6, point_size = 2.6, alpha = 0.6
  )
)

# =============================================================================
# UI
# =============================================================================
ui <- fluidPage(
  tags$head(
    # Load Google Fonts in the browser so the Typography preview renders
    # in the actual family (ggplot uses showtext independently)
    tags$link(rel = "stylesheet",
              href = paste0(
                "https://fonts.googleapis.com/css2",
                "?family=Inter:wght@400;600",
                "&family=Fira+Sans:wght@400;600",
                "&family=Lexend:wght@400;600",
                "&display=swap")),
    tags$style(HTML("
      .well { background-color: #FAFAFA; }
      h4 { margin-top: 6px; margin-bottom: 8px; font-size: 14px; }
      hr { margin: 10px 0; }
    "))),
  titlePanel("ggplot theme sandbox"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Preset"),
      selectInput("preset", NULL,
                  choices = c("Custom" = "", names(theme_presets)),
                  selected = "Default"),
      helpText(tags$small("Loads a starting point into the controls below — everything stays editable after.")),
      hr(),
      h4("Brand"),
      colourpicker::colourInput("primary", "Primary (position 1)",
                                value = "#1E53A4", allowTransparent = FALSE),
      colourpicker::colourInput("secondary", "Secondary (position 2)",
                                value = "#CAB06B", allowTransparent = FALSE),
      checkboxInput("dark", "Dark mode", value = FALSE),
      hr(),
      h4("Color system"),
      helpText(tags$small("Primary above, plus a contrast (opposition) colour and an optional third accent — each expanded into a light-to-dark ramp on the Color system tab. Single-series plots (binscatter, facets, forest plot) default to the primary colour.")),
      colourpicker::colourInput("contrast", "Contrast (opposition)",
                                value = "#E72922", allowTransparent = FALSE),
      checkboxInput("use_extra", "Add a third accent colour", value = TRUE),
      conditionalPanel(
        condition = "input.use_extra",
        colourpicker::colourInput("extra", "Extra accent",
                                  value = "#006637", allowTransparent = FALSE)
      ),
      sliderInput("ramp_n", "Variants per ramp",
                  min = 3, max = 9, value = 5, step = 2),
      sliderInput("ramp_spread", "Ramp spread (light ↔ dark)",
                  min = 0.2, max = 0.9, value = 0.6, step = 0.05),
      hr(),
      h4("Colour"),
      selectInput("pal_qual", "Base qualitative palette",
                  choices = names(palettes_qual), selected = "Okabe-Ito"),
      helpText(tags$small("Primary + secondary slot into positions 1–2; the two nearest base colours are dropped.")),
      radioButtons("n_series", "Series shown",
                   choices = c(2, 4, 7), selected = 4, inline = TRUE),
      selectInput("pal_seq", "Sequential (continuous fills)",
                  choices = palettes_seq, selected = "viridis"),
      radioButtons("seq_source", "Sequential fill source",
                   choices = c("Named palette" = "named", "Primary ramp" = "ramp"),
                   selected = "named", inline = TRUE),
      hr(),
      h4("Typography"),
      selectInput("base_family", "Base font",
                  choices = font_families, selected = "Inter"),
      selectInput("title_family", "Title font",
                  choices = c("(same as base)", font_families),
                  selected = "(same as base)"),
      sliderInput("base_size", "Base size",
                  min = 10, max = 20, value = 14, step = 1),
      sliderInput("title_ratio", "Title / axis text ratio",
                  min = 1, max = 1.6, value = 1.2, step = 0.05),
      hr(),
      h4("Grid & panel"),
      radioButtons("grid", "Gridlines",
                   choices = c("none","horizontal","both"),
                   selected = "horizontal", inline = TRUE),
      checkboxInput("minor", "Minor gridlines", value = FALSE),
      radioButtons("panel_bg", "Background",
                   choices = c("white","offwhite","transparent"),
                   selected = "white", inline = TRUE),
      checkboxInput("axis_lines", "Axis lines & ticks", value = TRUE),
      hr(),
      h4("Geom defaults"),
      sliderInput("line_width", "Line width",
                  min = 0.3, max = 2.5, value = 1.2, step = 0.1),
      sliderInput("point_size", "Point size",
                  min = 0.5, max = 4.0, value = 2.0, step = 0.1),
      sliderInput("alpha", "Alpha (fills / ribbons)",
                  min = 0.1, max = 1.0, value = 0.3, step = 0.05),
      hr(),
      h4("Legend"),
      radioButtons("legend_pos", "Position",
                   choices = c("top","right","bottom","none"),
                   selected = "bottom", inline = TRUE),
      sliderInput("legend_key", "Key size (pt)",
                  min = 6, max = 20, value = 14, step = 1),
      hr(),
      h4("Text on plots"),
      checkboxInput("show_title", "Titles", value = TRUE),
      checkboxInput("show_subtitle", "Subtitles", value = TRUE),
      checkboxInput("show_axis_labels", "Axis labels", value = TRUE),
      checkboxInput("show_caption", "Captions", value = TRUE)
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Plots",
                 br(),
                 fluidRow(column(6, plotOutput("p_ts",     height = "320px")),
                          column(6, plotOutput("p_es",     height = "320px"))),
                 fluidRow(column(6, plotOutput("p_bin",    height = "320px")),
                          column(6, plotOutput("p_dens",   height = "320px"))),
                 fluidRow(column(6, plotOutput("p_bar",    height = "320px")),
                          column(6, plotOutput("p_area",   height = "320px"))),
                 fluidRow(column(6, plotOutput("p_facet",  height = "340px")),
                          column(6, plotOutput("p_forest", height = "340px")))
        ),
        tabPanel("Color system",
                 br(),
                 p(em("Primary/contrast/extra, each auto-expanded into a light-to-dark ramp. Drives the sequential fill below when \"Primary ramp\" is selected in the sidebar, and the diverging scale emitted on the Code tab.")),
                 uiOutput("ramp_preview"),
                 hr(),
                 plotOutput("p_seq", height = "320px"),
                 hr(),
                 p(em("Full ramp, in use: one line per variant, faceted by which colour it came from.")),
                 plotOutput("p_ramp_facet", height = "320px"),
                 hr(),
                 p(em("Simplified ramp: just light / base / dark for primary and contrast, on one graph.")),
                 plotOutput("p_ramp3", height = "320px")
        ),
        tabPanel("Accessibility",
                 br(),
                 p(em("The 5-colour sector palette from the bar/area plots, simulated under common colour-vision deficiencies.")),
                 plotOutput("p_access", height = "300px"),
                 hr(),
                 uiOutput("access_warnings")
        ),
        tabPanel("Typography",
                 br(),
                 uiOutput("type_preview")),
        tabPanel("Brand preview",
                 br(),
                 uiOutput("brand_preview")),
        tabPanel("Code",
                 br(),
                 p(em("Paste this into ", code("themes.R"),
                      " and ", code("source()"), " at the top of every project.")),
                 verbatimTextOutput("code_output")),
        tabPanel("Print size",
                 br(),
                 p(em("Renders a plot from the ", code("Plots"), " tab at its literal export size (mm) — the way ",
                      code("draw_acc.R"), " renders true-to-scale — then drops that fixed image into a standard mock-up of the ",
                      "chosen format, scaled (up or down) and centred to fill the space the way it actually would once inserted: ",
                      "resized to fit a phone screen, filling most of a slide, centred on an A4 page. Realistic heading/body text ",
                      "renders alongside it at that format's typical size, so the chart's own text has something familiar to judge ",
                      "scale against.")),
                 fluidRow(
                   column(4, selectInput("size_plot", "Plot", choices = plot_choices)),
                   column(4, numericInput("size_x", "Export width (mm)", value = 120, min = 10, max = 400, step = 5)),
                   column(4, numericInput("size_y", "Export height (mm)", value = 80, min = 10, max = 400, step = 5))
                 ),
                 radioButtons("size_page", "Shown inserted into",
                              choices = names(page_presets), selected = "A4 page", inline = TRUE),
                 uiOutput("size_plot_box"),
                 uiOutput("size_notes")
        )
      )
    )
  )
)

# =============================================================================
# Server
# =============================================================================
server <- function(input, output, session) {

  # ---- Presets ---------------------------------------------------------------
  observeEvent(input$preset, {
    if (identical(input$preset, "") || !input$preset %in% names(theme_presets)) return()
    p <- theme_presets[[input$preset]]
    updateCheckboxInput(session, "dark", value = p$dark)
    updateSelectInput(session, "base_family", selected = p$base_family)
    updateSelectInput(session, "title_family", selected = p$title_family)
    updateSliderInput(session, "base_size", value = p$base_size)
    updateSliderInput(session, "title_ratio", value = p$title_ratio)
    updateRadioButtons(session, "grid", selected = p$grid)
    updateCheckboxInput(session, "minor", value = p$minor)
    updateCheckboxInput(session, "axis_lines", value = p$axis_lines)
    updateRadioButtons(session, "panel_bg", selected = p$panel_bg)
    updateRadioButtons(session, "legend_pos", selected = p$legend_pos)
    updateSliderInput(session, "legend_key", value = p$legend_key)
    updateSliderInput(session, "line_width", value = p$line_width)
    updateSliderInput(session, "point_size", value = p$point_size)
    updateSliderInput(session, "alpha", value = p$alpha)
  }, ignoreInit = TRUE)

  # ---- Reactive helpers ----------------------------------------------------
  scheme <- reactive({
    if (input$dark) {
      list(bg = "#0F1419", fg = "#E5E7EB", mid = "#9CA3AF", soft = "#6B7280",
           grid = "#2A2F36", minor = "#1E2229", strip = "#1F2937",
           axis = "#9CA3AF", sep = "#0F1419")
    } else {
      list(bg = switch(input$panel_bg, white = "white",
                       offwhite = "#FAFAFA", transparent = "transparent"),
           fg = "grey20", mid = "grey30", soft = "grey40",
           grid = "#E5E5E5", minor = "#F0F0F0", strip = "grey95",
           axis = "grey30", sep = "white")
    }
  })

  merged_pal <- reactive({
    merged_palette(palettes_qual[[input$pal_qual]], input$primary, input$secondary)
  })

  pal_current <- reactive({
    n <- as.integer(input$n_series)
    merged_pal()[seq_len(n)]
  })
  pal_full5 <- reactive({
    merged_pal()[seq_len(5)]
  })
  title_fam <- reactive({
    if (input$title_family == "(same as base)") input$base_family else input$title_family
  })

  # ---- Colour ramps ----------------------------------------------------------
  primary_ramp <- reactive({
    make_ramp(input$primary, n = input$ramp_n, spread = input$ramp_spread)
  })
  contrast_ramp <- reactive({
    make_ramp(input$contrast, n = input$ramp_n, spread = input$ramp_spread)
  })
  extra_ramp <- reactive({
    req(input$use_extra)
    make_ramp(input$extra, n = input$ramp_n, spread = input$ramp_spread)
  })

  th <- reactive({
    theme_sandbox(
      base_size     = input$base_size,
      base_family   = input$base_family,
      title_family  = title_fam(),
      title_ratio   = input$title_ratio,
      grid          = input$grid,
      minor         = input$minor,
      axis_lines    = input$axis_lines,
      legend_pos    = input$legend_pos,
      legend_key_pt = input$legend_key,
      scheme        = scheme()
    )
  })

  # Labels helper — respects text toggles
  labs_ <- function(title = NULL, subtitle = NULL,
                    x = NULL, y = NULL, caption = NULL) {
    labs(
      title    = if (input$show_title)       title    else NULL,
      subtitle = if (input$show_subtitle)    subtitle else NULL,
      x        = if (input$show_axis_labels) x        else NULL,
      y        = if (input$show_axis_labels) y        else NULL,
      caption  = if (input$show_caption)     caption  else NULL
    )
  }

  # Non-reactive scale factories (evaluated fresh each render)
  scale_col_d <- function() scale_colour_manual(values = pal_current(), name = NULL)
  scale_fil_d <- function() scale_fill_manual(values = pal_current(),   name = NULL)
  scale_fil_5 <- function() scale_fill_manual(values = pal_full5(),     name = NULL)

  # ---- 1-8. Plots tab, built as reusable functions --------------------------
  # Each plot is a plain function (not `renderPlot`) so the same ggplot object
  # can be reused by the "Print size" tab (renders it at an exact physical
  # size via draw_acc()) without duplicating the plotting code.
  build_ts <- function() {
    n <- as.integer(input$n_series)
    d <- gdp_data |> filter(country %in% countries7[seq_len(n)])
    ggplot(d, aes(year, gdp_index, colour = country)) +
      geom_line(linewidth = input$line_width) +
      geom_point(size = input$point_size * 0.6) +
      scale_col_d() +
      labs_(title    = "Real GDP index, selected countries",
            subtitle = "1980 = 100",
            x = "Year", y = "GDP index",
            caption  = "Source: Simulated data.") +
      th()
  }

  build_es <- function() {
    s <- scheme()
    dw <- position_dodge(width = 0.35)
    ggplot(event_study, aes(period, estimate, colour = spec, group = spec)) +
      geom_hline(yintercept = 0, linewidth = 0.3, colour = s$soft) +
      geom_vline(xintercept = -0.5, linetype = "dashed",
                 linewidth = 0.3, colour = s$soft) +
      geom_line(linewidth = input$line_width * 0.7, position = dw) +
      geom_pointrange(aes(ymin = lower, ymax = upper),
                      size = input$point_size * 0.6, fatten = 1,
                      linewidth = input$line_width * 0.6,
                      position = dw, key_glyph = "point") +
      scale_col_d() +
      labs_(title    = "Event study, three specifications",
            subtitle = "Coefficients with 95% CIs, treatment at t = 0",
            x = "Period relative to treatment", y = "Estimated effect",
            caption  = "Source: Simulated data.") +
      th()
  }

  build_bin <- function() {
    s <- scheme()
    bins <- mincer |>
      mutate(bin = ntile(schooling, 20)) |>
      group_by(bin) |>
      summarise(schooling = mean(schooling),
                log_wage  = mean(log_wage), .groups = "drop")
    ggplot(bins, aes(schooling, log_wage)) +
      geom_smooth(method = "lm", se = TRUE,
                  colour = input$primary, fill = input$primary,
                  alpha = input$alpha * 0.3,
                  linewidth = input$line_width) +
      geom_point(size = input$point_size * 0.6, colour = s$fg) +
      labs_(title    = "Log wages and schooling",
            subtitle = "Binscatter, 20 bins",
            x = "Years of schooling", y = "Log hourly wage",
            caption  = "Source: Simulated Mincer DGP.") +
      th()
  }

  build_dens <- function() {
    ggplot(density_data, aes(y, fill = group, colour = group)) +
      geom_density(alpha = input$alpha * 0.55,
                   linewidth = input$line_width * 0.7) +
      scale_col_d() + scale_fil_d() +
      labs_(title    = "Outcome distribution by group",
            subtitle = "Kernel densities, treated vs control",
            x = "Log earnings, post-period", y = "Density",
            caption  = "Source: Simulated data.") +
      th()
  }

  build_bar <- function() {
    s <- scheme()
    ggplot(sector_shares, aes(country, share, fill = sector)) +
      geom_col(position = position_dodge(0.75), width = 0.7,
               colour = s$sep, linewidth = 0.2) +
      scale_fil_5() +
      labs_(title    = "Employment shares by sector",
            subtitle = "Selected economies",
            x = NULL, y = "Share of employment (%)",
            caption  = "Source: Simulated data.") +
      th()
  }

  build_area <- function() {
    s <- scheme()
    ggplot(area_data, aes(year, share, fill = sector)) +
      geom_area(alpha = pmin(1, input$alpha + 0.25),
                colour = s$sep, linewidth = 0.15) +
      scale_fil_5() +
      labs_(title    = "Sectoral composition of employment",
            subtitle = "Single economy, 1980 – 2020",
            x = "Year", y = "Share of employment (%)",
            caption  = "Source: Simulated data.") +
      th()
  }

  build_facet <- function() {
    ggplot(region_data, aes(year, y)) +
      geom_line(colour = input$primary, linewidth = input$line_width) +
      geom_point(colour = input$primary, size = input$point_size * 0.6) +
      facet_wrap(~ region, ncol = 3) +
      labs_(title    = "Regional GDP, 2000 – 2020",
            subtitle = "Index, 2000 = 100",
            x = "Year", y = "GDP index",
            caption  = "Source: Simulated data.") +
      th()
  }

  build_forest <- function() {
    s <- scheme()
    ggplot(forest, aes(estimate, var)) +
      geom_vline(xintercept = 0, linewidth = 0.3, colour = s$soft) +
      geom_pointrange(aes(xmin = lower, xmax = upper),
                      colour = input$primary,
                      size      = input$point_size * 0.6, fatten = 1,
                      linewidth = input$line_width * 0.7) +
      labs_(title    = "Log-earnings regression, selected coefficients",
            subtitle = "Point estimates with 95% CIs",
            x = "Estimate", y = NULL,
            caption  = "Source: Simulated data.") +
      th()
  }

  # Registry driving both the Plots tab outputs below and the "Print size"
  # tab's plot picker.
  plot_registry <- list(
    "Time series"      = build_ts,
    "Event study"      = build_es,
    "Binscatter"       = build_bin,
    "Density"          = build_dens,
    "Grouped bar"      = build_bar,
    "Stacked area"     = build_area,
    "Small multiples"  = build_facet,
    "Forest plot"      = build_forest
  )

  output$p_ts     <- renderPlot({ build_ts() })
  output$p_es     <- renderPlot({ build_es() })
  output$p_bin    <- renderPlot({ build_bin() })
  output$p_dens   <- renderPlot({ build_dens() })
  output$p_bar    <- renderPlot({ build_bar() })
  output$p_area   <- renderPlot({ build_area() })
  output$p_facet  <- renderPlot({ build_facet() })
  output$p_forest <- renderPlot({ build_forest() })

  # ---- Print size: export at true size, then scale the image, in context ---
  # The page itself (background, heading, body text) is drawn directly in
  # literal physical mm/pt, draw_acc.R-style -- that only comes out true to
  # scale if the graphics device's own physical size equals the page's real
  # mm size, so unlike a normal Shiny plot, this fixes the device's pixel
  # dimensions explicitly (via renderPlot's width/height) to
  # page_mm / 25.4 * size_dpi at a fixed res, instead of letting Shiny size
  # it off the browser's arbitrary column width. The CHART itself is
  # handled differently -- see render_chart_raster()/compute_fit() below.
  # Only one page is ever shown at once (picked via input$size_page,
  # rebuilding the plotOutput's own container through renderUI so its CSS
  # height always matches that page's aspect) -- showing all three side by
  # side made Shiny's automatic image sizing fight the CSS aspect-ratio
  # hack that kept each one proportioned, producing overlap.
  size_dpi <- 96

  # Text-block height math shared between the actual render (page_mockup)
  # and the fit-scale estimate in size_notes below, so the two can't drift
  # out of sync with each other.
  text_metrics <- function(p) {
    title_h <- p$title_pt / 72 * 25.4 * 1.3
    line_h  <- p$body_pt  / 72 * 25.4 * 1.4
    list(title_h = title_h, line_h = line_h,
         total_h = title_h + 4 + length(p$body_lines) * line_h + 4)
  }

  # Renders the plot ONCE, as a fixed raster, at its literal export size
  # (x_mm x y_mm) -- baking in the same true-to-scale text/line proportions
  # draw_acc.R shows. That raster is then what gets scaled (never
  # re-rendered) into each page below, because that's what actually happens
  # when you drop a fixed-size export into a doc/slide/phone and it gets
  # resized to fit: everything in the image, text included, scales
  # together -- unlike re-rendering ggplot fresh at a new physical size,
  # where the theme's point-sized text would stay fixed and only the data
  # panel would resize.
  render_chart_raster <- function(plot_obj, x_mm, y_mm, dpi = 300) {
    tmp <- tempfile(fileext = ".png")
    on.exit(unlink(tmp))
    grDevices::png(tmp, width = round(x_mm / 25.4 * dpi), height = round(y_mm / 25.4 * dpi),
                    res = dpi, bg = "white")
    ggplot2::ggplot_build(plot_obj) |> ggplot2::ggplot_gtable() |> grid::grid.draw()
    grDevices::dev.off()
    png::readPNG(tmp)
  }

  # Where the raster ends up: scaled (up OR down, preserving aspect) to
  # fill the space left below the heading/body text, then centred there --
  # "resized to fit the screen", "centred, taking up most of the slide".
  compute_fit <- function(preset, x_mm, y_mm) {
    tm  <- text_metrics(preset)
    m   <- preset$margin_mm
    cap_h_mm <- if (!is.null(preset$caption)) (preset$body_pt * 0.8) / 72 * 25.4 * 1.6 else 0
    avail_w  <- preset$w_mm - 2 * m
    avail_h  <- (preset$h_mm - m - tm$total_h) - m - cap_h_mm
    scale    <- max(min(avail_w / x_mm, avail_h / y_mm), 0.02)
    list(avail_w = avail_w, avail_h = avail_h, cap_h_mm = cap_h_mm,
         scale = scale, disp_w = x_mm * scale, disp_h = y_mm * scale)
  }

  # A page isn't blank: draw a standard heading + body text/bullets at that
  # format's typical size first, so the chart's own text has something
  # familiar right next to it to judge scale against.
  page_mockup <- function(plot_obj, preset, x_mm, y_mm) {
    grid::grid.newpage()
    grid::rectGrob(gp = grid::gpar(fill = "white", col = "grey45", lwd = 1.2)) |> grid::grid.draw()

    m  <- preset$margin_mm
    tm <- text_metrics(preset)

    grid::grid.text(preset$title, x = grid::unit(m, "mm"), y = grid::unit(preset$h_mm - m, "mm"),
                     just = c("left", "top"),
                     gp = grid::gpar(fontsize = preset$title_pt, fontface = "bold", col = "grey15"))
    y_cursor <- preset$h_mm - m - tm$title_h - 4
    for (ln in preset$body_lines) {
      grid::grid.text(ln, x = grid::unit(m, "mm"), y = grid::unit(y_cursor, "mm"),
                       just = c("left", "top"),
                       gp = grid::gpar(fontsize = preset$body_pt, col = "grey35"))
      y_cursor <- y_cursor - tm$line_h
    }

    f  <- compute_fit(preset, x_mm, y_mm)
    cx <- m + f$avail_w / 2
    cy <- m + f$cap_h_mm + f$avail_h / 2
    img <- render_chart_raster(plot_obj, x_mm, y_mm)
    grid::grid.raster(img, x = grid::unit(cx, "mm"), y = grid::unit(cy, "mm"),
                       width = grid::unit(f$disp_w, "mm"), height = grid::unit(f$disp_h, "mm"))

    if (!is.null(preset$caption)) {
      grid::grid.text(preset$caption, x = grid::unit(m, "mm"), y = grid::unit(m + f$cap_h_mm * 0.25, "mm"),
                       just = c("left", "bottom"),
                       gp = grid::gpar(fontsize = preset$body_pt * 0.8, fontface = "italic", col = "grey45"))
    }
  }

  size_plot_obj <- reactive({
    req(input$size_plot)
    plot_registry[[input$size_plot]]()
  })

  # Rebuild the container each time the page choice changes, so its CSS
  # height (disp_h/disp_w -- see fit_dims()) always matches the newly
  # selected page's true aspect ratio.
  output$size_plot_box <- renderUI({
    req(input$size_page)
    p <- page_presets[[input$size_page]]
    tags$div(style = sprintf("max-width: %dpx; margin: 12px auto;", p$disp_w),
             plotOutput("p_size_page", width = "100%", height = sprintf("%dpx", p$disp_h)))
  })

  output$p_size_page <- renderPlot({
    req(input$size_page, input$size_x, input$size_y)
    p <- page_presets[[input$size_page]]
    page_mockup(size_plot_obj(), p, input$size_x, input$size_y)
  }, res = size_dpi,
     width  = function() round(page_presets[[input$size_page]]$w_mm / 25.4 * size_dpi),
     height = function() round(page_presets[[input$size_page]]$h_mm / 25.4 * size_dpi))

  # Scale note per page, computed outside the graphics device so it's
  # always legible. Shares compute_fit() with the actual render, so it
  # can't disagree with what's on screen.
  output$size_notes <- renderUI({
    x <- input$size_x; y <- input$size_y
    req(x, y)
    items <- lapply(names(page_presets), function(nm) {
      p   <- page_presets[[nm]]
      f   <- compute_fit(p, x, y)
      pct <- round(f$scale * 100)
      msg <- if (abs(f$scale - 1) < 0.01) {
        "shown at its native export size."
      } else if (f$scale < 1) {
        sprintf("scaled down to %d%% of its export size (%.0f × %.0fmm) to fit.", pct, f$disp_w, f$disp_h)
      } else {
        sprintf("scaled up to %d%% of its export size (%.0f × %.0fmm) to fill the space.", pct, f$disp_w, f$disp_h)
      }
      tags$li(tags$strong(nm), ": ", msg)
    })
    tags$div(style = "margin-top: 8px;", tags$ul(items))
  })

  # ---- 9. Sequential fill — colour-system ramp demo -----------------------
  # Same continuous-fill use case the (previously unwired) sequential-palette
  # selector implies — toggle between a named viridis-family palette and the
  # auto-generated primary ramp.
  output$p_seq <- renderPlot({
    s <- scheme()
    d <- mincer |>
      mutate(sch_bin = cut(schooling, breaks = 6),
             exp_bin = cut(exper,     breaks = 6)) |>
      group_by(sch_bin, exp_bin) |>
      summarise(log_wage = mean(log_wage), .groups = "drop")

    p <- ggplot(d, aes(sch_bin, exp_bin, fill = log_wage)) +
      geom_tile(colour = s$sep, linewidth = 0.3) +
      labs_(title    = "Log wages by schooling x experience",
            subtitle = if (input$seq_source == "ramp")
              "Fill: auto-generated primary ramp" else
              sprintf("Fill: %s", input$pal_seq),
            x = "Schooling (binned)", y = "Experience (binned)",
            caption  = "Source: Simulated Mincer DGP.") +
      th() +
      theme(axis.text.x = element_text(angle = 40, hjust = 1))

    if (input$seq_source == "ramp") {
      p + scale_fill_gradientn(colours = primary_ramp(), name = NULL)
    } else {
      p + scale_fill_gradientn(colours = viridisLite::viridis(256, option = input$pal_seq), name = NULL)
    }
  })

  # ---- 10. Ramp preview, full n_variants, faceted by source colour ----------
  output$p_ramp_facet <- renderPlot({
    years <- 1980:2020

    families <- list(Primary = primary_ramp(), Contrast = contrast_ramp())
    if (isTRUE(input$use_extra)) families$Extra <- extra_ramp()

    build_family <- function(ramp, label) {
      n   <- length(ramp)
      mid <- (n + 1) / 2
      bind_rows(lapply(seq_along(ramp), function(i) {
        g <- 0.015 + 0.006 * (i - mid) / max(1, mid - 1)
        tibble(ramp_type = label, variant = i, hex = ramp[i],
               year = years, y = 100 * (1 + g) ^ (years - min(years)))
      }))
    }

    d <- bind_rows(lapply(names(families), function(nm) build_family(families[[nm]], nm))) |>
      mutate(ramp_type = factor(ramp_type, levels = names(families)))

    ggplot(d, aes(year, y, colour = hex, group = interaction(ramp_type, variant))) +
      geom_line(linewidth = input$line_width) +
      scale_colour_identity() +
      facet_wrap(~ ramp_type, nrow = 1) +
      labs_(title    = "Ramp preview",
            subtitle = sprintf("%d variants per ramp, light -> dark", length(families[[1]])),
            x = "Year", y = "Index") +
      th()
  })

  # ---- 11. Ramp preview, 3 shades of primary + contrast, one graph ----------
  output$p_ramp3 <- renderPlot({
    years        <- 1980:2020
    shade_labels <- c("Light", "Base", "Dark")
    growth       <- c(0.030, 0.020, 0.010)
    p3 <- make_ramp(input$primary,  n = 3, spread = input$ramp_spread)
    c3 <- make_ramp(input$contrast, n = 3, spread = input$ramp_spread)

    fam <- function(hexes, label, offset) {
      bind_rows(lapply(seq_along(hexes), function(i) {
        tibble(series = paste(label, shade_labels[i]),
               hex    = hexes[i],
               year   = years,
               y      = offset * (1 + growth[i]) ^ (years - min(years)))
      }))
    }
    d <- bind_rows(fam(p3, "Primary", 100), fam(c3, "Contrast", 90))
    d$series <- factor(d$series, levels = unique(d$series))
    pal_named <- setNames(d$hex[match(levels(d$series), d$series)], levels(d$series))

    ggplot(d, aes(year, y, colour = series, group = series)) +
      geom_line(linewidth = input$line_width) +
      scale_colour_manual(values = pal_named, name = NULL) +
      labs_(title    = "Ramp preview, 3 shades",
            subtitle = "Primary & contrast: light / base / dark",
            x = "Year", y = "Index") +
      th()
  })

  # ---- Colour-system ramp swatches -----------------------------------------
  output$ramp_preview <- renderUI({
    ramp_swatch_row <- function(label, hexes) {
      swatches <- lapply(hexes, function(h) {
        tags$div(style = sprintf(
          "flex: 1; height: 56px; background: %s; display: flex; align-items: flex-end; justify-content: center;", h),
          tags$span(style = "font-size: 10px; font-family: monospace; color: #111; margin-bottom: 4px; background: rgba(255,255,255,0.7); padding: 0 3px;",
                    toupper(h))
        )
      })
      tags$div(
        tags$div(style = "font-weight: 600; font-size: 13px; margin: 12px 0 4px 0;", label),
        tags$div(style = "display: flex; border: 1px solid #ddd;", swatches)
      )
    }
    rows <- list(ramp_swatch_row("Primary", primary_ramp()),
                 ramp_swatch_row("Contrast", contrast_ramp()))
    if (isTRUE(input$use_extra)) {
      rows <- c(rows, list(ramp_swatch_row("Extra accent", extra_ramp())))
    }
    tags$div(style = "max-width: 900px;", rows)
  })

  # ---- Accessibility: CVD simulation of the 5-colour sector palette --------
  output$p_access <- renderPlot({
    pal <- pal_full5()
    sims <- list(
      "Normal"       = pal,
      "Deuteranopia" = colorspace::deutan(pal),
      "Protanopia"   = colorspace::protan(pal),
      "Tritanopia"   = colorspace::tritan(pal)
    )
    d <- bind_rows(lapply(names(sims), function(nm) {
      sector_shares |>
        mutate(vision   = nm,
               fill_hex = sims[[nm]][match(sector, sectors)])
    })) |>
      mutate(vision = factor(vision, levels = names(sims)))

    s <- scheme()
    ggplot(d, aes(country, share, fill = fill_hex)) +
      geom_col(position = position_dodge(0.75), width = 0.7,
               colour = s$sep, linewidth = 0.2) +
      scale_fill_identity() +
      facet_wrap(~ vision, nrow = 1) +
      labs_(title    = "Sector-share palette under colour-vision deficiency",
            subtitle = "Same data, same palette, three simulated CVD types",
            x = NULL, y = "Share of employment (%)") +
      th()
  })

  output$access_warnings <- renderUI({
    pal <- pal_full5()
    sims <- list(
      "Deuteranopia" = colorspace::deutan(pal),
      "Protanopia"   = colorspace::protan(pal),
      "Tritanopia"   = colorspace::tritan(pal)
    )
    items <- lapply(names(sims), function(nm) {
      warn <- flag_confusable(sims[[nm]])
      tags$li(
        tags$strong(nm), ": ",
        if (length(warn) > 0) paste(warn, collapse = "; ") else "no flagged pairs"
      )
    })
    tags$ul(items)
  })

  # ---- Typography preview -------------------------------------------------
  output$type_preview <- renderUI({
    s <- scheme()
    base <- input$base_family
    tit  <- title_fam()
    HTML(sprintf('
      <div style="font-family: %s; max-width: 720px; background: %s; color: %s; padding: 24px;">
        <h1 style="font-family: %s; font-weight: 700; font-size: %dpx;
                   margin: 8px 0 4px 0; color: %s;">Sample plot title</h1>
        <p style="color: %s; margin: 0 0 20px 0; font-size: %dpx;">
          A subtitle set in the base family.
        </p>
        <p style="line-height: 1.55; font-size: %dpx;">
          Body paragraph in %s. The numerals below matter: if they wobble
          in the column of the table, the font lacks tabular figures and
          your axis ticks will jitter. 1,234,567 &middot; $4.2M &middot;
          12.5%% &middot; &beta; = 0.087***
        </p>
        <table style="font-family: %s; border-collapse: collapse;
                      margin-top: 20px; font-size: %dpx;">
          <thead>
            <tr style="border-bottom: 1px solid %s;">
              <th style="padding: 6px 16px; text-align: left;">Variable</th>
              <th style="padding: 6px 16px; text-align: right;">Estimate</th>
              <th style="padding: 6px 16px; text-align: right;">SE</th>
            </tr>
          </thead>
          <tbody>
            <tr><td style="padding: 3px 16px;">Education</td>
                <td style="padding: 3px 16px; text-align: right; font-variant-numeric: tabular-nums;">0.087</td>
                <td style="padding: 3px 16px; text-align: right; font-variant-numeric: tabular-nums;">(0.006)</td></tr>
            <tr><td style="padding: 3px 16px;">Experience</td>
                <td style="padding: 3px 16px; text-align: right; font-variant-numeric: tabular-nums;">0.045</td>
                <td style="padding: 3px 16px; text-align: right; font-variant-numeric: tabular-nums;">(0.003)</td></tr>
            <tr><td style="padding: 3px 16px;">Female</td>
                <td style="padding: 3px 16px; text-align: right; font-variant-numeric: tabular-nums;">-0.145</td>
                <td style="padding: 3px 16px; text-align: right; font-variant-numeric: tabular-nums;">(0.021)</td></tr>
            <tr><td style="padding: 3px 16px;">Married</td>
                <td style="padding: 3px 16px; text-align: right; font-variant-numeric: tabular-nums;">0.062</td>
                <td style="padding: 3px 16px; text-align: right; font-variant-numeric: tabular-nums;">(0.019)</td></tr>
            <tr style="border-top: 1px solid %s;">
                <td style="padding: 3px 16px;">R&sup2;</td>
                <td style="padding: 3px 16px; text-align: right; font-variant-numeric: tabular-nums;">0.312</td><td></td></tr>
            <tr><td style="padding: 3px 16px;">N</td>
                <td style="padding: 3px 16px; text-align: right; font-variant-numeric: tabular-nums;">12,347</td><td></td></tr>
          </tbody>
        </table>
        <p style="margin-top: 28px; color: %s; font-size: %dpx;">
          Two-weight test: <span style="font-weight: 400;">regular weight</span>
          and <span style="font-weight: 600;">semibold weight</span>.
          Same family, different weight, is usually enough.
        </p>
      </div>
    ',
                 base, s$bg, s$fg,
                 tit, round(input$base_size * input$title_ratio * 1.6), s$fg,
                 s$mid, round(input$base_size * 1.1),
                 round(input$base_size * 1.0), base,
                 base, round(input$base_size * 0.95),
                 s$fg, s$fg,
                 s$mid, round(input$base_size * 0.95)))
  })

  # ---- Brand preview ------------------------------------------------------
  # Shows accent doing layout work: a header block, an underline rule,
  # a pull-quote rule, category tags, and a source-line rule.
  output$brand_preview <- renderUI({
    s <- scheme()
    base <- input$base_family
    tit  <- title_fam()
    p1 <- input$primary
    p2 <- input$secondary
    HTML(sprintf('
      <div style="font-family: %s; background: %s; color: %s;
                  padding: 32px 40px; max-width: 780px;
                  border: 1px solid %s;">

        <!-- Header: brand block + title -->
        <div style="display: flex; align-items: flex-start; gap: 16px; margin-bottom: 24px;">
          <div style="background: %s; color: white; width: 48px; height: 48px;
                      display: flex; align-items: center; justify-content: center;
                      font-family: %s; font-weight: 700; font-size: 22px;
                      flex-shrink: 0;">R</div>
          <div>
            <div style="font-family: %s; font-weight: 700; font-size: 26px; line-height: 1.15; color: %s;">
              The persistence of decline
            </div>
            <div style="color: %s; font-size: 15px; margin-top: 4px;">
              Manufacturing employment across four decades, and why the trend outlasted every intervention.
            </div>
          </div>
        </div>

        <!-- Accent underline rule -->
        <div style="border-top: 3px solid %s; width: 72px; margin: 0 0 24px 0;"></div>

        <!-- Body paragraph -->
        <p style="line-height: 1.65; font-size: 15px; margin: 0 0 20px 0;">
          Between 1980 and 2020, the share of employment in manufacturing fell
          by more than half across every advanced economy. The pattern is
          striking not for its magnitude — that has been well documented — but
          for its <em>uniformity</em>: countries with very different labour-market
          institutions, trade exposures, and industrial policies converged on
          similar trajectories.
        </p>

        <!-- Pull quote with accent rule -->
        <blockquote style="border-left: 3px solid %s; padding: 4px 16px;
                           color: %s; margin: 20px 0; font-style: italic;
                           font-size: 16px; line-height: 1.5;">
          The decline is not a story about any one country. It is a story about
          what the whole rich world stopped being able to do.
        </blockquote>

        <p style="line-height: 1.65; font-size: 15px; margin: 0 0 24px 0;">
          What follows is one attempt to disentangle the demand-side story
          (rising incomes shift consumption toward services) from the
          supply-side one (automation and trade).
        </p>

        <!-- Category tags in primary + secondary -->
        <div style="display: flex; gap: 8px; margin-bottom: 28px;">
          <span style="background: %s; color: white; padding: 4px 12px;
                       font-size: 11px; font-weight: 600; letter-spacing: 0.5px;
                       text-transform: uppercase;">Featured</span>
          <span style="background: %s; color: white; padding: 4px 12px;
                       font-size: 11px; font-weight: 600; letter-spacing: 0.5px;
                       text-transform: uppercase;">Data</span>
          <span style="border: 1px solid %s; color: %s; padding: 3px 12px;
                       font-size: 11px; font-weight: 600; letter-spacing: 0.5px;
                       text-transform: uppercase;">Long read</span>
        </div>

        <!-- Source line with accent rule -->
        <div style="border-top: 1px solid %s; padding-top: 12px;">
          <div style="display: flex; justify-content: space-between; align-items: baseline;">
            <span style="color: %s; font-size: 12px; letter-spacing: 0.3px;">
              <span style="color: %s; font-weight: 600;">SOURCE</span>
              &nbsp;&nbsp;Simulated data, drawn from the sandbox app.
            </span>
            <span style="color: %s; font-size: 12px;">rasmus &middot; 2026</span>
          </div>
        </div>
      </div>

      <div style="margin-top: 20px; color: #666; font-size: 12px; max-width: 780px;">
        <em>Uses of the accent above:</em> brand block (top-left), title
        underline rule, pull-quote left rule, category tags (primary +
        secondary + outlined), source-line label. If your primary reads
        wrong in any of these — too heavy as a block, too muddy as a rule,
        wrong tone next to secondary — that\'s the signal to iterate.
      </div>
    ',
                 base, s$bg, s$fg, s$grid,
                 p1, tit,
                 tit, s$fg,
                 s$mid,
                 p1,
                 p1, s$mid,
                 p1, p2,
                 p1, s$fg,
                 s$grid,
                 s$mid, p1,
                 s$soft))
  })

  # ---- Code generator -----------------------------------------------------
  output$code_output <- renderText({
    pal <- merged_pal()
    pal_str <- paste0('c("', paste(pal, collapse = '", "'), '")')
    ramp_str <- function(v) paste0('c("', paste(v, collapse = '", "'), '")')
    tit <- title_fam()
    second_font <- if (tit != input$base_family)
      sprintf('sysfonts::font_add_google("%s", "%s")\n', tit, tit) else ""
    extra_line <- if (isTRUE(input$use_extra))
      sprintf('extra_ramp    <- %s\n', ramp_str(extra_ramp())) else ""

    axis_line_call <- if (input$axis_lines)
      'ggplot2::element_line(colour = s$axis, linewidth = 0.4)' else
        'ggplot2::element_blank()'
    gx <- if (input$grid == "both")
      'ggplot2::element_line(colour = s$grid, linewidth = 0.3)' else
        'ggplot2::element_blank()'
    gy <- if (input$grid %in% c("both","horizontal"))
      'ggplot2::element_line(colour = s$grid, linewidth = 0.3)' else
        'ggplot2::element_blank()'
    gm <- if (input$minor && input$grid != "none")
      'ggplot2::element_line(colour = s$minor, linewidth = 0.2)' else
        'ggplot2::element_blank()'

    default_dark <- if (input$dark) "TRUE" else "FALSE"

    sprintf('# themes.R -- source() at the top of every project
library(ggplot2)
library(showtext)
sysfonts::font_add_google("%s", "%s")
%sshowtext::showtext_auto()

## ---- Brand colours ----
brand_primary   <- "%s"
brand_secondary <- "%s"

## ---- Qualitative palette (primary + secondary + colourblind-safe base) ----
pal_rasmus_qual <- %s

scale_colour_rasmus_d <- function(...) ggplot2::scale_colour_manual(values = pal_rasmus_qual, ...)
scale_fill_rasmus_d   <- function(...) ggplot2::scale_fill_manual(values   = pal_rasmus_qual, ...)
scale_colour_rasmus_c <- function(...) ggplot2::scale_colour_viridis_c(option = "%s", ...)
scale_fill_rasmus_c   <- function(...) ggplot2::scale_fill_viridis_c(option   = "%s", ...)

## ---- Colour ramps (light -> dark tint/shade scales, %d steps) ----
primary_ramp  <- %s
contrast_ramp <- %s
%sscale_fill_rasmus_seq   <- function(...) ggplot2::scale_fill_gradientn(colours = primary_ramp, ...)
scale_colour_rasmus_seq <- function(...) ggplot2::scale_colour_gradientn(colours = primary_ramp, ...)
scale_fill_rasmus_div   <- function(...) ggplot2::scale_fill_gradientn(colours = c(rev(contrast_ramp), "white", primary_ramp), ...)
scale_colour_rasmus_div <- function(...) ggplot2::scale_colour_gradientn(colours = c(rev(contrast_ramp), "white", primary_ramp), ...)

## ---- Theme (light + dark) ----
.scheme_light <- list(bg = "white",   fg = "grey20",   mid = "grey30",  soft = "grey40",
                      grid = "#E5E5E5", minor = "#F0F0F0", strip = "grey95",
                      axis = "grey30",  sep = "white")
.scheme_dark  <- list(bg = "#0F1419", fg = "#E5E7EB",  mid = "#9CA3AF", soft = "#6B7280",
                      grid = "#2A2F36", minor = "#1E2229", strip = "#1F2937",
                      axis = "#9CA3AF", sep = "#0F1419")

theme_rasmus <- function(base_size = %d, base_family = "%s", dark = %s) {
  s <- if (dark) .scheme_dark else .scheme_light
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.title         = ggplot2::element_text(family = "%s", face = "bold",
                                                 size = base_size * %.2f, colour = s$fg,
                                                 margin = ggplot2::margin(b = 6)),
      plot.subtitle      = ggplot2::element_text(size = base_size, colour = s$mid,
                                                 margin = ggplot2::margin(b = 8)),
      plot.caption       = ggplot2::element_text(size = base_size * 0.8, colour = s$soft,
                                                 hjust = 0, margin = ggplot2::margin(t = 8)),
      axis.title         = ggplot2::element_text(size = base_size * 0.95, colour = s$fg),
      axis.text          = ggplot2::element_text(size = base_size * 0.85, colour = s$mid),
      axis.line          = %s,
      axis.ticks         = %s,
      panel.background   = ggplot2::element_rect(fill = s$bg, colour = NA),
      plot.background    = ggplot2::element_rect(fill = s$bg, colour = NA),
      panel.grid.major.x = %s,
      panel.grid.major.y = %s,
      panel.grid.minor   = %s,
      legend.position    = "%s",
      legend.title       = ggplot2::element_text(size = base_size * 0.9, colour = s$fg),
      legend.text        = ggplot2::element_text(size = base_size * 0.85, colour = s$mid),
      legend.key.size    = grid::unit(%d, "pt"),
      strip.text         = ggplot2::element_text(size = base_size * 0.9, face = "bold", colour = s$fg),
      strip.background   = ggplot2::element_rect(fill = "white", colour = "black", linewidth = 0.4),
      plot.margin        = ggplot2::margin(10, 12, 8, 10)
    )
}

## ---- Geom defaults ----
ggplot2::update_geom_defaults("line",       list(linewidth = %.2f))
ggplot2::update_geom_defaults("point",      list(size = %.2f))
ggplot2::update_geom_defaults("pointrange", list(linewidth = %.2f))
ggplot2::update_geom_defaults("ribbon",     list(alpha = %.2f))

## ---- Use ----
# ggplot(d, aes(x, y, colour = g)) +
#   geom_line() + scale_colour_rasmus_d() + theme_rasmus()
#
# For dark mode:      theme_rasmus(dark = TRUE)
# Sequential fill:     ... + scale_fill_rasmus_seq()
# Diverging fill:      ... + scale_fill_rasmus_div()
',
    input$base_family, input$base_family,
    second_font,
    input$primary, input$secondary,
    pal_str,
    input$pal_seq, input$pal_seq,
    as.integer(input$ramp_n), ramp_str(primary_ramp()), ramp_str(contrast_ramp()), extra_line,
    as.integer(input$base_size), input$base_family, default_dark,
    tit, input$title_ratio,
    axis_line_call, axis_line_call,
    gx, gy, gm,
    input$legend_pos,
    as.integer(input$legend_key),
    input$line_width, input$point_size,
    input$line_width * 0.7, input$alpha * 0.4)
  })
}

shinyApp(ui, server)
