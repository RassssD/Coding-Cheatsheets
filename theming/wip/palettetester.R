# =============================================================================
# ggplot theme sandbox
# -----------------------------------------------------------------------------
# Setup (one-off):
#   install.packages(c("shiny","ggplot2","dplyr","tibble","showtext","sysfonts",
#                       "colourpicker","colorspace"))
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

# ---- Fonts ------------------------------------------------------------------
font_families <- c("Inter", "IBM Plex Sans", "Source Sans 3", "Fira Sans",
                   "Roboto", "EB Garamond", "Source Serif 4", "IBM Plex Serif")

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
accent_choices <- c("Red"    = "#D7301F",
                    "Blue"   = "#0570B0",
                    "Black"  = "#000000",
                    "Orange" = "#E6550D",
                    "Green"  = "#238B45")

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
      strip.background = element_rect(fill = s$strip, colour = NA),
      plot.margin      = margin(10, 12, 8, 10)
    )
}

# ---- Theme presets ------------------------------------------------------------
# Each preset is a starting point, not a lock: picking one just pushes these
# values into the controls below via update*Input(); every control stays live
# and editable afterwards, and the "Code" tab reflects whatever the controls
# currently say regardless of which (if any) preset was last picked.
theme_presets <- list(
  "Editorial" = list(
    dark = FALSE, base_family = "Source Serif 4", title_family = "IBM Plex Sans",
    base_size = 12, title_ratio = 1.25, grid = "horizontal", minor = FALSE,
    axis_lines = TRUE, panel_bg = "offwhite", legend_pos = "bottom", legend_key = 10,
    line_width = 0.9, point_size = 1.6, alpha = 0.6
  ),
  "Technical (dark)" = list(
    dark = TRUE, base_family = "IBM Plex Sans", title_family = "(same as base)",
    base_size = 12, title_ratio = 1.15, grid = "both", minor = TRUE,
    axis_lines = TRUE, panel_bg = "white", legend_pos = "right", legend_key = 10,
    line_width = 1.0, point_size = 1.8, alpha = 0.7
  ),
  "Presentation" = list(
    dark = FALSE, base_family = "Inter", title_family = "(same as base)",
    base_size = 15, title_ratio = 1.3, grid = "none", minor = FALSE,
    axis_lines = TRUE, panel_bg = "white", legend_pos = "top", legend_key = 14,
    line_width = 1.4, point_size = 2.4, alpha = 0.75
  ),
  "Academic / print" = list(
    dark = FALSE, base_family = "EB Garamond", title_family = "(same as base)",
    base_size = 11, title_ratio = 1.1, grid = "horizontal", minor = FALSE,
    axis_lines = TRUE, panel_bg = "white", legend_pos = "bottom", legend_key = 8,
    line_width = 0.7, point_size = 1.3, alpha = 0.5
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
                "&family=IBM+Plex+Sans:wght@400;600",
                "&family=Source+Sans+3:wght@400;600",
                "&family=Fira+Sans:wght@400;600",
                "&family=Roboto:wght@400;700",
                "&family=EB+Garamond:wght@400;600",
                "&family=Source+Serif+4:wght@400;600",
                "&family=IBM+Plex+Serif:wght@400;600",
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
                  selected = ""),
      helpText(tags$small("Loads a starting point into the controls below — everything stays editable after.")),
      hr(),
      h4("Brand"),
      colourpicker::colourInput("primary", "Primary (position 1)",
                                value = "#0E7C86", allowTransparent = FALSE),
      colourpicker::colourInput("secondary", "Secondary (position 2)",
                                value = "#C87533", allowTransparent = FALSE),
      checkboxInput("dark", "Dark mode", value = FALSE),
      hr(),
      h4("Color system"),
      helpText(tags$small("Primary above, plus a contrast (opposition) colour and an optional third accent — each expanded into a light-to-dark ramp on the Color system tab.")),
      colourpicker::colourInput("contrast", "Contrast (opposition)",
                                value = "#B5482A", allowTransparent = FALSE),
      checkboxInput("use_extra", "Add a third accent colour", value = FALSE),
      conditionalPanel(
        condition = "input.use_extra",
        colourpicker::colourInput("extra", "Extra accent",
                                  value = "#4C6B3A", allowTransparent = FALSE)
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
      selectInput("accent", "Emphasis colour (single-series plots)",
                  choices = accent_choices, selected = "#D7301F"),
      hr(),
      h4("Typography"),
      selectInput("base_family", "Base font",
                  choices = font_families, selected = "Inter"),
      selectInput("title_family", "Title font",
                  choices = c("(same as base)", font_families),
                  selected = "(same as base)"),
      sliderInput("base_size", "Base size",
                  min = 10, max = 14, value = 12, step = 1),
      sliderInput("title_ratio", "Title / axis text ratio",
                  min = 1, max = 1.6, value = 1.2, step = 0.05),
      hr(),
      h4("Grid & panel"),
      radioButtons("grid", "Gridlines",
                   choices = c("none","horizontal","both"),
                   selected = "both", inline = TRUE),
      checkboxInput("minor", "Minor gridlines", value = TRUE),
      radioButtons("panel_bg", "Background",
                   choices = c("white","offwhite","transparent"),
                   selected = "white", inline = TRUE),
      checkboxInput("axis_lines", "Axis lines & ticks", value = TRUE),
      hr(),
      h4("Geom defaults"),
      sliderInput("line_width", "Line width",
                  min = 0.3, max = 2.5, value = 0.9, step = 0.1),
      sliderInput("point_size", "Point size",
                  min = 0.5, max = 4.0, value = 1.6, step = 0.1),
      sliderInput("alpha", "Alpha (fills / ribbons)",
                  min = 0.1, max = 1.0, value = 0.65, step = 0.05),
      hr(),
      h4("Legend"),
      radioButtons("legend_pos", "Position",
                   choices = c("top","right","bottom","none"),
                   selected = "bottom", inline = TRUE),
      sliderInput("legend_key", "Key size (pt)",
                  min = 6, max = 20, value = 10, step = 1),
      hr(),
      h4("Text on plots"),
      checkboxInput("show_title", "Titles", value = TRUE),
      checkboxInput("show_subtitle", "Subtitles", value = FALSE),
      checkboxInput("show_axis_labels", "Axis labels", value = TRUE),
      checkboxInput("show_caption", "Captions", value = FALSE)
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
                 plotOutput("p_seq", height = "320px")
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
                 verbatimTextOutput("code_output"))
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

  # ---- 1. Time series ------------------------------------------------------
  output$p_ts <- renderPlot({
    n <- as.integer(input$n_series)
    d <- gdp_data |> filter(country %in% countries7[seq_len(n)])
    ggplot(d, aes(year, gdp_index, colour = country)) +
      geom_line(linewidth = input$line_width) +
      scale_col_d() +
      labs_(title    = "Real GDP index, selected countries",
            subtitle = "1980 = 100",
            x = "Year", y = "GDP index",
            caption  = "Source: Simulated data.") +
      th()
  })

  # ---- 2. Event study, three specs ----------------------------------------
  output$p_es <- renderPlot({
    s <- scheme()
    dw <- position_dodge(width = 0.35)
    ggplot(event_study, aes(period, estimate, colour = spec, group = spec)) +
      geom_hline(yintercept = 0, linewidth = 0.3, colour = s$soft) +
      geom_vline(xintercept = -0.5, linetype = "dashed",
                 linewidth = 0.3, colour = s$soft) +
      geom_line(linewidth = input$line_width * 0.7, position = dw) +
      geom_pointrange(aes(ymin = lower, ymax = upper),
                      size = input$point_size / 3,
                      linewidth = input$line_width * 0.6,
                      position = dw) +
      scale_col_d() +
      labs_(title    = "Event study, three specifications",
            subtitle = "Coefficients with 95% CIs, treatment at t = 0",
            x = "Period relative to treatment", y = "Estimated effect",
            caption  = "Source: Simulated data.") +
      th()
  })

  # ---- 3. Binscatter -------------------------------------------------------
  output$p_bin <- renderPlot({
    s <- scheme()
    bins <- mincer |>
      mutate(bin = ntile(schooling, 20)) |>
      group_by(bin) |>
      summarise(schooling = mean(schooling),
                log_wage  = mean(log_wage), .groups = "drop")
    ggplot(bins, aes(schooling, log_wage)) +
      geom_smooth(method = "lm", se = TRUE,
                  colour = input$accent, fill = input$accent,
                  alpha = input$alpha * 0.3,
                  linewidth = input$line_width) +
      geom_point(size = input$point_size * 1.4, colour = s$fg) +
      labs_(title    = "Log wages and schooling",
            subtitle = "Binscatter, 20 bins",
            x = "Years of schooling", y = "Log hourly wage",
            caption  = "Source: Simulated Mincer DGP.") +
      th()
  })

  # ---- 4. Density comparison ----------------------------------------------
  output$p_dens <- renderPlot({
    ggplot(density_data, aes(y, fill = group, colour = group)) +
      geom_density(alpha = input$alpha * 0.55,
                   linewidth = input$line_width * 0.7) +
      scale_col_d() + scale_fil_d() +
      labs_(title    = "Outcome distribution by group",
            subtitle = "Kernel densities, treated vs control",
            x = "Log earnings, post-period", y = "Density",
            caption  = "Source: Simulated data.") +
      th()
  })

  # ---- 5. Grouped bar — sectoral shares -----------------------------------
  output$p_bar <- renderPlot({
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
  })

  # ---- 6. Stacked area -----------------------------------------------------
  output$p_area <- renderPlot({
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
  })

  # ---- 7. Faceted small multiples -----------------------------------------
  output$p_facet <- renderPlot({
    ggplot(region_data, aes(year, y)) +
      geom_line(colour = input$accent, linewidth = input$line_width) +
      facet_wrap(~ region, ncol = 3) +
      labs_(title    = "Regional GDP, 2000 – 2020",
            subtitle = "Index, 2000 = 100",
            x = "Year", y = "GDP index",
            caption  = "Source: Simulated data.") +
      th()
  })

  # ---- 8. Forest / coefficient plot ---------------------------------------
  output$p_forest <- renderPlot({
    s <- scheme()
    ggplot(forest, aes(estimate, var)) +
      geom_vline(xintercept = 0, linewidth = 0.3, colour = s$soft) +
      geom_pointrange(aes(xmin = lower, xmax = upper),
                      colour = input$accent,
                      size      = input$point_size / 2.5,
                      linewidth = input$line_width * 0.7) +
      labs_(title    = "Log-earnings regression, selected coefficients",
            subtitle = "Point estimates with 95% CIs",
            x = "Estimate", y = NULL,
            caption  = "Source: Simulated data.") +
      th()
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
      strip.background   = ggplot2::element_rect(fill = s$strip, colour = NA),
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
