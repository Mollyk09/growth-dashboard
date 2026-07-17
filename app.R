library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(plotly)
library(here)

# ── Data ───────────────────────────────────────────────────────────────────────

acs_data <- readRDS(here("acs_child_indicators.rds"))

# ── Join in supplemental county data: urban/rural population split, MMR
# vaccination rate, and Child Opportunity Index scores ─────────────────────
other_data <- readRDS(here("all_other_data.rds")) |>
  select(
    GEOID, POPPCT_RUR, POP_URB, POP_RUR, POP_COU, mmr_rates,
    r_COI_nat, r_ED_nat, r_HE_nat, r_SE_nat,
    r_ED_EC_nat, r_ED_EL_nat, r_ED_ER_nat, r_ED_SP_nat,
    r_HE_EP_nat, r_HE_HR_nat, r_HE_SE_nat, r_HE_HE_nat,
    r_SE_EI_nat, r_SE_EO_nat, r_SE_ER_nat, r_SE_HQ_nat, r_SE_SR_nat, r_SE_WL_nat
  )

acs_data <- acs_data |> left_join(other_data, by = "GEOID")

# The national row has no counterpart in `other_data`, so its supplemental
# columns come back NA from the join above. Fill them in: a population-
# weighted average for share/score fields, a straight sum for raw counts.
coi_share_cols <- c("POPPCT_RUR", "mmr_rates")
coi_score_cols <- c(
  "r_COI_nat", "r_ED_nat", "r_HE_nat", "r_SE_nat",
  "r_ED_EC_nat", "r_ED_EL_nat", "r_ED_ER_nat", "r_ED_SP_nat",
  "r_HE_EP_nat", "r_HE_HR_nat", "r_HE_SE_nat", "r_HE_HE_nat",
  "r_SE_EI_nat", "r_SE_EO_nat", "r_SE_ER_nat", "r_SE_HQ_nat",
  "r_SE_SR_nat", "r_SE_WL_nat"
)
coi_count_cols <- c("POP_URB", "POP_RUR", "POP_COU")

is_county   <- acs_data$level == "county"
is_national <- acs_data$level == "national"
natl_weight <- acs_data$total_population[is_county]

weighted_mean_na <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

for (col in coi_share_cols) {
  acs_data[[col]][is_national] <- weighted_mean_na(acs_data[[col]][is_county], natl_weight)
}
for (col in coi_score_cols) {
  acs_data[[col]][is_national] <- as.integer(round(
    weighted_mean_na(acs_data[[col]][is_county], natl_weight)
  ))
}
for (col in coi_count_cols) {
  acs_data[[col]][is_national] <- sum(acs_data[[col]][is_county], na.rm = TRUE)
}

national   <- acs_data[acs_data$level == "national", ]
county_acs <- acs_data[acs_data$level == "county", ]

county_shapes <- readRDS(here("county_shapes.rds"))

counties_sf <- county_shapes |>
  left_join(county_acs |> select(-level), by = "GEOID") |>
  filter(!is.na(NAME))

# ── State FIPS lookup ──────────────────────────────────────────────────────────

state_fips_lookup <- data.frame(
  fips = c("01","02","04","05","06","08","09","10","11","12","13","15","16",
           "17","18","19","20","21","22","23","24","25","26","27","28","29",
           "30","31","32","33","34","35","36","37","38","39","40","41","42",
           "44","45","46","47","48","49","50","51","53","54","55","56"),
  state_name = c("Alabama","Alaska","Arizona","Arkansas","California","Colorado",
                 "Connecticut","Delaware","District of Columbia","Florida","Georgia",
                 "Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky",
                 "Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota",
                 "Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire",
                 "New Jersey","New Mexico","New York","North Carolina","North Dakota",
                 "Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island",
                 "South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont",
                 "Virginia","Washington","West Virginia","Wisconsin","Wyoming"),
  stringsAsFactors = FALSE
)

counties_sf <- counties_sf |>
  mutate(state_fips = substr(GEOID, 1, 2)) |>
  left_join(state_fips_lookup, by = c("state_fips" = "fips")) |>
  filter(!is.na(state_name))

# State boundary outlines (shown only once zoomed in past STATE_LINE_ZOOM)
# State boundary outlines (shown only once zoomed in past STATE_LINE_ZOOM)
state_shapes <- readRDS(here("state_shapes_raw.rds")) |>
  filter(STATEFP %in% state_fips_lookup$fips) |>
  select(GEOID, geometry)

# ── Brand palette ──────────────────────────────────────────────────────────────

CONTINUOUS_RAMP  <- colorRampPalette(c("#F4C542", "#F26A5B", "#cf2e2e"))(100)
BAR_COUNTY_COLOR <- "#005A9C"
BAR_NATL_COLOR   <- "#95a5a6"
SELECT_FILL      <- "#F4C542"
SELECT_BORDER    <- "#F26A5B"
DEFAULT_FILL     <- "#cccccc"
STATE_LINE_COLOR <- "#555555"

# Zoom level beyond which the map is basically inside a single state, so the
# faint state outline layer is no longer useful and gets hidden (national view
# is ~4; a single mid-sized state roughly fills the viewport around zoom 8)
STATE_LINE_MAX_ZOOM <- 8

# Plotly modebar: keep only "Download plot as PNG"
MODEBAR_REMOVE <- c("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d",
                    "autoScale2d", "resetScale2d", "hoverClosestCartesian",
                    "hoverCompareCartesian", "toggleSpikelines")

# Per-variable gradient for quintile choropleths: light tint → full brand color
# Each share variable gets its own single-hue gradient (5 shades, low → high)
var_gradient <- list(
  "children_under6_share"           = c("#ddeeff", "#005A9C"),
  "children_under6_poverty_share"   = c("#fde8e8", "#cf2e2e"),
  "children_under6_uninsured_share" = c("#fde8e5", "#F26A5B"),
  "children_pub_assist_share"       = c("#fef9e7", "#F4C542"),
  "children_of_color_u18_share"     = c("#e0f5f5", "#00A6A6"),
  "children_of_color_u5_share"      = c("#ecf7ec", "#7CBF6F"),
  "POPPCT_RUR"                      = c("#f4e9f7", "#8e44ad"),
  "mmr_rates"                       = c("#fbe6ee", "#c2185b")
)

get_quintile_colors <- function(var) {
  ends <- var_gradient[[var]]
  if (is.null(ends)) ends <- c("#ddeeff", "#005A9C")
  colorRampPalette(ends)(5)
}

# ── Variable metadata ──────────────────────────────────────────────────────────

var_meta <- data.frame(
  col = c(
    "children_under6_share", "children_under6_poverty_share",
    "children_under6_uninsured_share", "children_pub_assist_share",
    "children_of_color_u18_share", "children_of_color_u5_share",
    "total_population", "children_under6_count",
    "children_under6_poverty_count", "children_under6_uninsured_count",
    "children_pub_assist_count", "total_children_under18",
    "children_of_color_u18_count", "children_of_color_u5_count",
    "POPPCT_RUR", "mmr_rates", "POP_URB", "POP_RUR", "POP_COU",
    "r_COI_nat", "r_ED_nat", "r_HE_nat", "r_SE_nat",
    "r_ED_EC_nat", "r_ED_EL_nat", "r_ED_ER_nat", "r_ED_SP_nat",
    "r_HE_EP_nat", "r_HE_HR_nat", "r_HE_SE_nat", "r_HE_HE_nat",
    "r_SE_EI_nat", "r_SE_EO_nat", "r_SE_ER_nat", "r_SE_HQ_nat",
    "r_SE_SR_nat", "r_SE_WL_nat"
  ),
  label = c(
    "Children Under 6 (% of Pop.)", "Children Under 6 in Poverty (%)",
    "Children Under 6 Uninsured (%)", "Children Receiving Public Assist. / SNAP (%)",
    "Children of Color Under 18 (%)", "Children of Color Under 5 (%)",
    "Total Population", "Children Under 6 (Count)",
    "Children Under 6 in Poverty (Count)", "Children Under 6 Uninsured (Count)",
    "Children Receiving Public Assist. / SNAP (Count)", "Total Children Under 18",
    "Children of Color Under 18 (Count)", "Children of Color Under 5 (Count)",
    "Share of Population Living in a Rural Area", "MMR Vaccination Rates, 2024-25",
    "Urban Population (Count)", "Rural Population (Count)",
    "Total County Population, Urban/Rural File (Count)",
    "COI Score, Overall, Nationally Normed",
    "COI Score, Education, Nationally Normed",
    "COI Score, Health & Environment, Nationally Normed",
    "COI Score, Social & Economic, Nationally Normed",
    "COI Score, Education, Early Childhood Education, Nationally Normed",
    "COI Score, Education, Elementary Education, Nationally Normed",
    "COI Score, Education, Educational Resources, Nationally Normed",
    "COI Score, Education, Secondary & Post-Secondary Education, Nationally Normed",
    "COI Score, Health & Environment, Pollution, Nationally Normed",
    "COI Score, Health & Environment, Health Resources, Nationally Normed",
    "COI Score, Health & Environment, Safety-Related Resources, Nationally Normed",
    "COI Score, Health & Environment, Healthy Environments, Nationally Normed",
    "COI Score, Social & Economic, Socio-Economic Inequity, Nationally Normed",
    "COI Score, Social & Economic, Employment, Nationally Normed",
    "COI Score, Social & Economic, Economic Resources, Nationally Normed",
    "COI Score, Social & Economic, Housing Resources, Nationally Normed",
    "COI Score, Social & Economic, Social Resources, Nationally Normed",
    "COI Score, Social & Economic, Wealth, Nationally Normed"
  ),
  is_share = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
               FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
               TRUE, TRUE, FALSE, FALSE, FALSE,
               rep(FALSE, 18)),
  # Rural share, MMR rate, and the raw urban/rural population counts come
  # from the same supplemental file as the COI scores but aren't COI data
  # themselves, so they stay in "core" (Percentages/Counts) rather than the
  # "coi" group.
  group = c(rep("core", 14 + 5), rep("coi", 18)),
  # COI subdomain scores (e.g. r_ED_EC_nat) are excluded from the map dropdown
  # per request — only the overall + 3 domain scores show on the choropleth.
  is_subdomain = c(rep(FALSE, 14 + 5 + 4), rep(TRUE, 14)),
  stringsAsFactors = FALSE
)
# All COI score fields (overall/domain/subdomain) share the "r_" prefix —
# flagged so the choropleth can skip the generic ±2 SD outlier-capping rule
# for them (COI scores are already normed 0-100, unlike raw population counts).
var_meta$is_coi_score <- grepl("^r_", var_meta$col)

share_ch     <- setNames(var_meta$col[var_meta$group == "core" & var_meta$is_share],
                         var_meta$label[var_meta$group == "core" & var_meta$is_share])
count_ch     <- setNames(var_meta$col[var_meta$group == "core" & !var_meta$is_share],
                         var_meta$label[var_meta$group == "core" & !var_meta$is_share])
coi_map_ch   <- setNames(var_meta$col[var_meta$group == "coi" & !var_meta$is_subdomain],
                         var_meta$label[var_meta$group == "coi" & !var_meta$is_subdomain])
choro_var_ch <- list("Percentages" = share_ch, "Counts" = count_ch,
                     "Child Opportunity Index" = coi_map_ch)

# ── Info panel HTML builder ────────────────────────────────────────────────────

fmt_pct   <- function(x) ifelse(is.na(x), "N/A", sprintf("%.1f%%", x * 100))
fmt_num   <- function(x) ifelse(is.na(x), "N/A", formatC(as.integer(x), format = "d", big.mark = ","))
fmt_score <- function(x) ifelse(is.na(x), "N/A", sprintf("%.0f", x))

info_row <- function(label, val, shaded = FALSE) {
  bg <- if (shaded) "background:#f7f9fc;" else ""
  paste0("<tr style='", bg, "'><td style='padding:3px 7px;color:#444;font-size:12px'>", label,
         "</td><td align='right' style='padding:3px 7px;font-weight:600;font-size:12px'>",
         val, "</td></tr>")
}
info_section <- function(title) {
  paste0("<tr style='background:#fde8e8'><td colspan='2' style='padding:5px 7px 5px 14px;",
         "font-weight:700;color:#cf2e2e;font-size:11px;text-transform:uppercase;",
         "letter-spacing:.4px'>", title, "</td></tr>")
}
info_group <- function(title) {
  paste0("<tr><td colspan='2' style='padding:9px 7px 4px;",
         "font-weight:800;color:#2c3e50;font-size:12.5px;text-transform:uppercase;",
         "letter-spacing:.6px;border-bottom:2px solid #2c3e50'>", title, "</td></tr>")
}

# Domain-level rows inside the COI disclosure are themselves collapsible —
# clicking a domain (e.g. "Education") reveals its subdomain scores.
domain_details_open <- function(title, score) {
  paste0(
    "<details style='margin:1px 0;'><summary style='cursor:pointer;",
    "list-style:none;display:flex;justify-content:space-between;",
    "align-items:center;padding:3px 7px;font-size:12px;font-weight:600;",
    "color:#333;'><span><span class='disclosure-chevron'>&#9656;</span> ",
    title, "</span><span>", score, "</span></summary>",
    "<table style='width:100%;border-collapse:collapse'>"
  )
}
domain_details_close <- "</table></details>"

# `show_header` is set to FALSE when the caller (the multi-county picker in
# info_content) is already displaying the county's name/state as a clickable
# banner above the table, so the table itself doesn't repeat it.
build_info_html <- function(row_df, header_color = "#cf2e2e", show_header = TRUE) {
  r <- row_df
  header_html <- if (show_header) paste0(
    "<div style='background:", header_color, ";color:#fff;padding:8px 10px;",
    "font-size:13px;font-weight:700;border-radius:4px 4px 0 0;margin-bottom:6px'>",
    r$NAME, "</div>"
  ) else ""
  paste0(
    "<div style='font-family:Arial,sans-serif;line-height:1.5'>",
    header_html,
    "<table style='width:100%;border-collapse:collapse'>",
    
    info_group("Counts"),
    info_section("Population"),
    info_row("Total Population",           fmt_num(r$total_population), TRUE),
    info_section("Young Children (Under 6)"),
    info_row("Children Under 6",           fmt_num(r$children_under6_count)),
    info_row("In Poverty",                 fmt_num(r$children_under6_poverty_count), TRUE),
    info_row("Uninsured",                  fmt_num(r$children_under6_uninsured_count)),
    info_section("Public Assistance / SNAP"),
    info_row("Children in HH Receiving",   fmt_num(r$children_pub_assist_count), TRUE),
    info_section("Race & Ethnicity"),
    info_row("Total Children Under 18",    fmt_num(r$total_children_under18)),
    info_row("Children of Color (U18)",    fmt_num(r$children_of_color_u18_count), TRUE),
    info_row("Children of Color (U5)",     fmt_num(r$children_of_color_u5_count)),
    info_section("Urban/Rural Population"),
    info_row("Urban Population",             fmt_num(r$POP_URB), TRUE),
    info_row("Rural Population",             fmt_num(r$POP_RUR)),
    info_row("Total Pop. (Urban/Rural File)", fmt_num(r$POP_COU), TRUE),
    
    info_group("Shares (%)"),
    info_section("Young Children (Under 6)"),
    info_row("Share of Population",        fmt_pct(r$children_under6_share), TRUE),
    info_row("In Poverty",                 fmt_pct(r$children_under6_poverty_share)),
    info_row("Uninsured",                  fmt_pct(r$children_under6_uninsured_share), TRUE),
    info_section("Public Assistance / SNAP"),
    info_row("Children in HH Receiving",   fmt_pct(r$children_pub_assist_share)),
    info_section("Race & Ethnicity"),
    info_row("Children of Color (U18)",    fmt_pct(r$children_of_color_u18_share), TRUE),
    info_row("Children of Color (U5)",     fmt_pct(r$children_of_color_u5_share)),
    info_section("Urban/Rural & Health"),
    info_row("Share Living in Rural Area",   fmt_pct(r$POPPCT_RUR), TRUE),
    info_row("MMR Vaccination Rate, 2024-25", fmt_pct(r$mmr_rates)),
    "</table>",
    
    # Collapsed by default (click to expand) — a native <details>/<summary>
    # disclosure so the long COI score list doesn't clutter the panel. Each
    # domain row is itself a nested disclosure for its subdomain scores.
    "<details style='margin-top:6px;border-top:2px solid #2c3e50;'>",
    "<summary style='cursor:pointer;padding:9px 7px 4px;font-weight:800;",
    "color:#2c3e50;font-size:12.5px;text-transform:uppercase;",
    "letter-spacing:.6px;'><span class='disclosure-chevron'>&#9656;</span> ",
    "Child Opportunity Index</summary>",
    "<table style='width:100%;border-collapse:collapse'>",
    info_section("COI Scores (0–100, Nationally Normed)"),
    info_row("Overall", fmt_score(r$r_COI_nat), TRUE),
    "</table>",
    
    domain_details_open("Education", fmt_score(r$r_ED_nat)),
    info_row("Early Childhood Education",          fmt_score(r$r_ED_EC_nat), TRUE),
    info_row("Elementary Education",                fmt_score(r$r_ED_EL_nat)),
    info_row("Educational Resources",               fmt_score(r$r_ED_ER_nat), TRUE),
    info_row("Secondary & Post-Secondary Education", fmt_score(r$r_ED_SP_nat)),
    domain_details_close,
    
    domain_details_open("Health & Environment", fmt_score(r$r_HE_nat)),
    info_row("Pollution",                  fmt_score(r$r_HE_EP_nat), TRUE),
    info_row("Health Resources",           fmt_score(r$r_HE_HR_nat)),
    info_row("Safety-Related Resources",   fmt_score(r$r_HE_SE_nat), TRUE),
    info_row("Healthy Environments",       fmt_score(r$r_HE_HE_nat)),
    domain_details_close,
    
    domain_details_open("Social & Economic", fmt_score(r$r_SE_nat)),
    info_row("Socio-Economic Inequity", fmt_score(r$r_SE_EI_nat), TRUE),
    info_row("Employment",              fmt_score(r$r_SE_EO_nat)),
    info_row("Economic Resources",      fmt_score(r$r_SE_ER_nat), TRUE),
    info_row("Housing Resources",       fmt_score(r$r_SE_HQ_nat)),
    info_row("Social Resources",        fmt_score(r$r_SE_SR_nat), TRUE),
    info_row("Wealth",                  fmt_score(r$r_SE_WL_nat)),
    domain_details_close,
    
    "</details>",
    "</div>"
  )
}

national_info_html <- build_info_html(national, header_color = "#555555")

# Recomputes each share as sum(numerator count) / sum(denominator count) across
# the selected counties — NOT an average of the individual counties' shares —
# so a large county isn't underweighted relative to a small one. Scoped to the
# original acs_child_indicators variables plus the rural population share;
# COI scores and MMR rate aren't part of this aggregate.
build_aggregate_html <- function(df) {
  n <- nrow(df)
  
  tot_pop      <- sum(df$total_population, na.rm = TRUE)
  u6           <- sum(df$children_under6_count, na.rm = TRUE)
  u6_pov       <- sum(df$children_under6_poverty_count, na.rm = TRUE)
  u6_uninsured <- sum(df$children_under6_uninsured_count, na.rm = TRUE)
  pub_assist   <- sum(df$children_pub_assist_count, na.rm = TRUE)
  tot_u18      <- sum(df$total_children_under18, na.rm = TRUE)
  coc_u18      <- sum(df$children_of_color_u18_count, na.rm = TRUE)
  tot_u5       <- sum(df$total_children_u5, na.rm = TRUE)
  coc_u5       <- sum(df$children_of_color_u5_count, na.rm = TRUE)
  rur_pop      <- sum(df$POP_RUR, na.rm = TRUE)
  tot_pop_urf  <- sum(df$POP_COU, na.rm = TRUE)
  
  safe_div <- function(num, den) if (is.na(den) || den == 0) NA_real_ else num / den
  
  share_u6           <- safe_div(u6, tot_pop)
  share_u6_pov       <- safe_div(u6_pov, u6)
  share_u6_uninsured <- safe_div(u6_uninsured, u6)
  share_pub_assist   <- safe_div(pub_assist, tot_u18)
  share_coc_u18      <- safe_div(coc_u18, tot_u18)
  share_coc_u5       <- safe_div(coc_u5, tot_u5)
  share_rural        <- safe_div(rur_pop, tot_pop_urf)
  
  header_html <- paste0(
    "<div style='background:", BAR_COUNTY_COLOR, ";color:#fff;padding:8px 10px;",
    "font-size:13px;font-weight:700;border-radius:4px 4px 0 0;margin-bottom:6px'>",
    n, " Counties Selected — Aggregate</div>"
  )
  
  paste0(
    "<div style='font-family:Arial,sans-serif;line-height:1.5'>",
    header_html,
    "<table style='width:100%;border-collapse:collapse'>",
    
    info_group("Counts"),
    info_section("Population"),
    info_row("Total Population",           fmt_num(tot_pop), TRUE),
    info_section("Young Children (Under 6)"),
    info_row("Children Under 6",           fmt_num(u6)),
    info_row("In Poverty",                 fmt_num(u6_pov), TRUE),
    info_row("Uninsured",                  fmt_num(u6_uninsured)),
    info_section("Public Assistance / SNAP"),
    info_row("Children in HH Receiving",   fmt_num(pub_assist), TRUE),
    info_section("Race & Ethnicity"),
    info_row("Total Children Under 18",    fmt_num(tot_u18)),
    info_row("Children of Color (U18)",    fmt_num(coc_u18), TRUE),
    info_row("Total Children Under 5",     fmt_num(tot_u5)),
    info_row("Children of Color (U5)",     fmt_num(coc_u5), TRUE),
    info_section("Rural Population"),
    info_row("Rural Population",           fmt_num(rur_pop)),
    
    info_group("Shares (%)"),
    info_section("Young Children (Under 6)"),
    info_row("Share of Population",        fmt_pct(share_u6), TRUE),
    info_row("In Poverty",                 fmt_pct(share_u6_pov)),
    info_row("Uninsured",                  fmt_pct(share_u6_uninsured), TRUE),
    info_section("Public Assistance / SNAP"),
    info_row("Children in HH Receiving",   fmt_pct(share_pub_assist)),
    info_section("Race & Ethnicity"),
    info_row("Children of Color (U18)",    fmt_pct(share_coc_u18), TRUE),
    info_row("Children of Color (U5)",     fmt_pct(share_coc_u5)),
    info_section("Rural Population"),
    info_row("Share Living in Rural Area", fmt_pct(share_rural), TRUE),
    "</table></div>"
  )
}

# ── County dropdown ────────────────────────────────────────────────────────────

county_df <- counties_sf |>
  st_drop_geometry() |>
  select(GEOID, NAME, state_name) |>
  arrange(state_name, NAME)

county_choices_by_state <- lapply(
  split(county_df, county_df$state_name),
  function(x) setNames(x$GEOID, x$NAME)
)

# ── Map controls JavaScript ─────────────────────────────────────────────────────
# Injected into the Leaflet widget via htmlwidgets::onRender. Builds two
# on-map controls:
#   1. A combined mode-toggle + hint box (top-right): switches between
#      Multi-select and Single-select (sent to Shiny as input$select_mode),
#      and shows instructions that update to match the active mode.
#   2. The "Freehand Select" button (top-left); user holds mousedown and
#      drags to trace a line, and on mouseup the traced line's coordinates
#      are sent to Shiny via input$freehand_path — any county the line
#      passes through gets selected (no need to close the shape into a
#      polygon). Only enabled in Multi-select mode.

map_controls_js <- "
function(el, x) {
  var lf = HTMLWidgets.find('#' + el.id);
  if (!lf) return;
  var map = lf.getMap();

  var freehandActive = false;
  var drawing        = false;
  var points         = [];
  var drawnLayer     = null;
  var freehandBtn    = null;
  var hintBox        = null;
  var multiBtn       = null;
  var singleBtn      = null;
  var selectMode     = 'multi';

  var HINT_MULTI  = '<b>Click</b> a county to add/remove it &nbsp;|&nbsp; ' +
                     '<b>Freehand Select</b> drag a line to add every county it crosses &nbsp;|&nbsp; ' +
                     '<b>Click empty map</b> to clear all';
  var HINT_SINGLE = '<b>Click</b> a county to select it (replaces the current pick) &nbsp;|&nbsp; ' +
                     '<b>Click it again</b> to deselect &nbsp;|&nbsp; ' +
                     'Freehand Select is available in Multi-select only';

  function updateHint() {
    if (hintBox) hintBox.innerHTML = (selectMode === 'multi') ? HINT_MULTI : HINT_SINGLE;
  }

  function updateModeButtons() {
    if (!multiBtn || !singleBtn) return;
    multiBtn.style.background  = (selectMode === 'multi')  ? '#005A9C' : '#e8edf2';
    multiBtn.style.color       = (selectMode === 'multi')  ? '#fff'    : '#444';
    singleBtn.style.background = (selectMode === 'single') ? '#005A9C' : '#e8edf2';
    singleBtn.style.color      = (selectMode === 'single') ? '#fff'    : '#444';
  }

  function updateFreehandAvailability() {
    if (!freehandBtn) return;
    var enabled = (selectMode === 'multi');
    freehandBtn.disabled      = !enabled;
    freehandBtn.style.opacity = enabled ? '1' : '0.45';
    freehandBtn.style.cursor  = enabled ? 'pointer' : 'not-allowed';
    if (!enabled && freehandActive) {
      freehandActive = false;
      map.dragging.enable();
      el.style.cursor = '';
      freehandBtn.style.background = '#005A9C';
      freehandBtn.innerHTML = '✍️ Freehand Select';
      if (drawnLayer) { map.removeLayer(drawnLayer); drawnLayer = null; }
    }
  }

  function setMode(mode) {
    selectMode = mode;
    updateModeButtons();
    updateFreehandAvailability();
    updateHint();
    Shiny.setInputValue('select_mode', selectMode, { priority: 'event' });
  }

  var modeBtnStyle = 'flex:1;padding:3px 7px;border:none;border-radius:3px;' +
    'font-size:11px;cursor:pointer;white-space:nowrap;';

  var HintControl = L.Control.extend({
    options: { position: 'topright' },
    onAdd: function() {
      var container = L.DomUtil.create('div', 'map-hint');
      container.style.cssText = 'background:rgba(255,255,255,.94);padding:7px 9px;' +
        'border-radius:5px;font-size:11px;color:#555;box-shadow:0 1px 3px rgba(0,0,0,.2);' +
        'max-width:230px;';
      L.DomEvent.disableClickPropagation(container);

      var modeRow = L.DomUtil.create('div', '', container);
      modeRow.style.cssText = 'display:flex;gap:4px;margin-bottom:6px;';

      multiBtn = L.DomUtil.create('button', '', modeRow);
      multiBtn.innerHTML = 'Multi-select';
      multiBtn.style.cssText = modeBtnStyle;
      L.DomEvent.on(multiBtn, 'click', function() { setMode('multi'); });

      singleBtn = L.DomUtil.create('button', '', modeRow);
      singleBtn.innerHTML = 'Single-select';
      singleBtn.style.cssText = modeBtnStyle;
      L.DomEvent.on(singleBtn, 'click', function() { setMode('single'); });

      hintBox = L.DomUtil.create('div', '', container);

      updateModeButtons();
      updateHint();
      return container;
    }
  });
  new HintControl().addTo(map);

  var FreehandControl = L.Control.extend({
    options: { position: 'topleft' },
    onAdd: function() {
      freehandBtn = L.DomUtil.create('button');
      freehandBtn.innerHTML = '✍️ Freehand Select';
      freehandBtn.title = 'Hold and drag to draw a freehand selection (Multi-select only)';
      freehandBtn.style.cssText = 'padding:5px 10px;background:#005A9C;color:#fff;' +
        'border:none;cursor:pointer;font-size:11.5px;border-radius:4px;' +
        'white-space:nowrap;box-shadow:0 1px 3px rgba(0,0,0,.3);margin:5px;';
      L.DomEvent.disableClickPropagation(freehandBtn);
      L.DomEvent.on(freehandBtn, 'click', function() {
        if (selectMode !== 'multi') return;
        freehandActive = !freehandActive;
        if (freehandActive) {
          freehandBtn.style.background = '#F26A5B';
          freehandBtn.innerHTML = '✍️ Drawing… release to select';
          el.style.cursor = 'crosshair';
          map.dragging.disable();
        } else {
          resetBtn();
          if (drawnLayer) { map.removeLayer(drawnLayer); drawnLayer = null; }
        }
      });
      return freehandBtn;
    }
  });
  new FreehandControl().addTo(map);
  updateFreehandAvailability();

  function resetBtn() {
    freehandActive = false;
    map.dragging.enable();
    el.style.cursor = '';
    if (freehandBtn) {
      freehandBtn.style.background = '#005A9C';
      freehandBtn.innerHTML = '✍️ Freehand Select';
    }
  }

  map.on('mousedown', function(e) {
    if (!freehandActive || selectMode !== 'multi' || e.originalEvent.button !== 0) return;
    drawing = true;
    points  = [[e.latlng.lat, e.latlng.lng]];
    if (drawnLayer) { map.removeLayer(drawnLayer); }
    drawnLayer = L.polyline([[e.latlng.lat, e.latlng.lng]],
      { color: '#005A9C', weight: 2, opacity: 0.85 }).addTo(map);
    e.originalEvent.preventDefault();
  });

  map.on('mousemove', function(e) {
    if (!drawing) return;
    points.push([e.latlng.lat, e.latlng.lng]);
    drawnLayer.addLatLng([e.latlng.lat, e.latlng.lng]);
  });

  map.on('mouseup', function(e) {
    if (!drawing) return;
    drawing = false;

    if (points.length >= 2) {
      var coords = points.map(function(p) { return [p[1], p[0]]; });
      Shiny.setInputValue('freehand_path',
        { coords: coords, nonce: Math.random() }, { priority: 'event' });

      setTimeout(function() {
        if (drawnLayer) { map.removeLayer(drawnLayer); drawnLayer = null; }
      }, 1200);
    } else {
      if (drawnLayer) { map.removeLayer(drawnLayer); drawnLayer = null; }
    }
    resetBtn();
  });

  map.on('click', function() {
    if (drawnLayer) { map.removeLayer(drawnLayer); drawnLayer = null; }
  });

  Shiny.setInputValue('select_mode', selectMode, { priority: 'event' });
}
"

# ── CSS ────────────────────────────────────────────────────────────────────────

app_css <- "
  html, body { height: 100%; overflow: hidden; margin: 0; padding: 0;
                font-family: Arial, sans-serif; }
  #app-header { height: 46px; background: #cf2e2e; color: #fff;
                display: flex; align-items: center; padding: 0 14px; flex-shrink: 0; }
  #app-header h4 { margin: 0; font-size: 17px; font-weight: 700; }
  #app-header span { font-size: 11px; opacity: .8; margin-left: 12px; }
  #app-body { display: flex; height: calc(100vh - 46px); overflow: hidden; }

  #sidebar { width: 210px; min-width: 210px; overflow-y: auto;
             border-right: 1px solid #dde3ec; padding: 10px 10px 20px;
             background: #f8fafc; }
  .sec-head { color: #cf2e2e; font-weight: 700; font-size: 11px;
              text-transform: uppercase; letter-spacing: .5px;
              border-bottom: 2px solid #fce8e8; padding-bottom: 3px;
              margin: 14px 0 7px; }
  .sec-head:first-child { margin-top: 0; }
  .form-group { margin-bottom: 6px; }
  .form-control { font-size: 12px; }
  .btn { font-size: 12px; }
  .btn-primary { background-color: #005A9C !important; border-color: #005A9C !important; }
  .btn-primary:hover { background-color: #004880 !important; }
  .badge-cty { display: inline-block; background: #005A9C; color: #fff;
               border-radius: 10px; padding: 1px 7px; font-size: 11px; margin: 2px 1px; }

  #center-col { flex: 1; display: flex; flex-direction: column;
                overflow: hidden; padding: 6px; gap: 6px; min-width: 0; }
  #map-wrap { flex: 0 0 57%; overflow: hidden; border-radius: 5px;
              box-shadow: 0 1px 4px rgba(0,0,0,.1); }
  #map-wrap .leaflet-container { height: 100% !important; border-radius: 5px; }
  #chart-wrap { flex: 1; overflow: hidden; background: #fff; border-radius: 5px;
                box-shadow: 0 1px 4px rgba(0,0,0,.08); padding: 8px 10px 4px; min-height: 0; }
  .chart-title { color: #cf2e2e; font-weight: 700; font-size: 12px;
                 text-transform: uppercase; letter-spacing: .4px;
                 border-bottom: 2px solid #fce8e8; padding-bottom: 3px; margin-bottom: 4px; }

  #info-col { width: 250px; min-width: 250px; overflow-y: auto;
              border-left: 1px solid #dde3ec; padding: 10px; background: #fff; }
  .info-head { color: #cf2e2e; font-weight: 700; font-size: 11px;
               text-transform: uppercase; letter-spacing: .5px;
               border-bottom: 2px solid #fce8e8; padding-bottom: 3px; margin-bottom: 8px; }
  .map-hint { background: rgba(255,255,255,.92); padding: 5px 9px; border-radius: 4px;
              font-size: 11px; color: #555; box-shadow: 0 1px 3px rgba(0,0,0,.2); }

  #info-col summary { list-style: none; }
  #info-col summary::-webkit-details-marker { display: none; }
  #info-col .disclosure-chevron { display: inline-block; transition: transform .15s ease; font-size: 9px; }
  #info-col details[open] > summary .disclosure-chevron { transform: rotate(90deg); }
"

# ── UI ─────────────────────────────────────────────────────────────────────────

ui <- fillPage(
  padding = 0,
  tags$head(tags$style(HTML(app_css))),
  
  div(id = "app-header",
      tags$h4("Child Well-Being County Explorer"),
      tags$span("Census Data Estimates | County-Level Child Indicators")
  ),
  
  div(id = "app-body",
      
      div(id = "sidebar",
          div(class = "sec-head", "Navigate"),
          selectInput("zoom_state", NULL,
                      choices  = c("— Select a State —" = "",
                                   setNames(state_fips_lookup$state_name,
                                            state_fips_lookup$state_name)),
                      selected = ""),
          conditionalPanel(
            condition = "input.zoom_state != ''",
            selectInput("zoom_county", NULL, choices = c("— Whole State —" = ""))
          ),
          actionButton("btn_zoom", "Zoom to Selection",
                       class = "btn-primary btn-sm", width = "100%"),
          
          div(class = "sec-head", "Choropleth"),
          selectInput("choro_var", NULL,
                      choices  = c("None" = "", choro_var_ch),
                      selected = ""),
          
          div(class = "sec-head", "Add Counties"),
          p(style = "font-size:11px;color:#777;margin-bottom:5px;",
            "Search here, or use the selection controls on the map."),
          selectInput("sel_counties", NULL,
                      choices  = county_choices_by_state,
                      multiple = TRUE, selectize = TRUE),
          actionButton("btn_clear", "Clear All",
                       class = "btn-sm btn-default", width = "100%"),
          br(), br(),
          
          div(class = "sec-head", "Chart Variable"),
          selectInput("chart_var", NULL,
                      choices  = choro_var_ch,
                      selected = "children_under6_poverty_share"),
          # Subdomain drill-down only makes sense for the 3 COI domain scores —
          # "Overall" and the core ACS indicators have no subdomains.
          conditionalPanel(
            condition = "input.chart_var == 'r_ED_nat' || input.chart_var == 'r_HE_nat' || input.chart_var == 'r_SE_nat'",
            selectInput("chart_var_sub", "Subdomain (optional)",
                        choices = c("— Domain Overall —" = ""))
          ),
          
          div(class = "sec-head", "Selected"),
          uiOutput("county_badges")
      ),
      
      div(id = "center-col",
          div(id = "map-wrap",
              leafletOutput("map", height = "100%")
          ),
          div(id = "chart-wrap",
              uiOutput("chart_title"),
              plotlyOutput("bar_chart", height = "88%")
          )
      ),
      
      div(id = "info-col",
          uiOutput("info_view_toggle"),
          div(class = "info-head", uiOutput("info_label")),
          uiOutput("info_content")
      )
  )
)

# ── Server ─────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {
  
  selected_geo         <- reactiveVal(character(0))
  focused_geo          <- reactiveVal(NULL)
  sync_in_flight       <- reactiveVal(FALSE)
  last_was_shape_click <- reactiveVal(FALSE)
  last_was_freehand    <- reactiveVal(FALSE)
  info_view            <- reactiveVal("detail")  # "detail" | "aggregate"
  
  # ── Base map ──────────────────────────────────────────────────────────────────
  output$map <- renderLeaflet({
    leaflet(counties_sf, options = leafletOptions(preferCanvas = TRUE)) |>
      addProviderTiles("CartoDB.Positron") |>
      setView(lng = -96, lat = 38.5, zoom = 4) |>
      addPolygons(
        layerId      = ~GEOID,
        group        = "counties",
        fillColor    = DEFAULT_FILL,
        fillOpacity  = 0.45,
        color        = "#ffffff",
        weight       = 0.7,
        smoothFactor = 0.4,
        label        = ~NAME,
        highlightOptions = highlightOptions(
          weight = 2.5, color = SELECT_BORDER,
          fillOpacity = 0.75, bringToFront = FALSE
        )
      ) |>
      addPolygons(
        data = state_shapes, group = "state_lines",
        fill = FALSE, color = STATE_LINE_COLOR, weight = 1.1, opacity = 0.5,
        options = pathOptions(interactive = FALSE)
      ) |>
      addScaleBar(position = "bottomleft") |>
      htmlwidgets::onRender(map_controls_js)
  })
  
  # ── State outlines: visible until zoomed in past STATE_LINE_MAX_ZOOM ───────────
  # (i.e. until roughly only a single state fills the viewport)
  state_lines_shown <- reactiveVal(TRUE)
  observeEvent(input$map_zoom, {
    zoom <- input$map_zoom
    if (is.null(zoom)) return()
    proxy       <- leafletProxy("map", session)
    should_show <- zoom <= STATE_LINE_MAX_ZOOM
    if (should_show && !state_lines_shown()) {
      proxy |>
        addPolygons(
          data = state_shapes, group = "state_lines",
          fill = FALSE, color = STATE_LINE_COLOR, weight = 1.1, opacity = 0.5,
          options = pathOptions(interactive = FALSE)
        )
      state_lines_shown(TRUE)
    } else if (!should_show && state_lines_shown()) {
      proxy |> clearGroup("state_lines")
      state_lines_shown(FALSE)
    }
  })
  
  # ── Choropleth ─────────────────────────────────────────────────────────────────
  observeEvent(input$choro_var, {
    var   <- input$choro_var
    proxy <- leafletProxy("map", session)
    
    if (is.null(var) || var == "") {
      proxy |>
        addPolygons(
          data = counties_sf, layerId = ~GEOID, group = "counties",
          fillColor = DEFAULT_FILL, fillOpacity = 0.45,
          color = "#ffffff", weight = 0.7, smoothFactor = 0.4,
          label = ~NAME,
          highlightOptions = highlightOptions(
            weight = 2.5, color = SELECT_BORDER,
            fillOpacity = 0.75, bringToFront = FALSE
          )
        ) |>
        removeControl("choro_legend")
      return()
    }
    
    is_shr <- var_meta$is_share[var_meta$col == var]
    lbl    <- var_meta$label[var_meta$col == var]
    vals   <- counties_sf[[var]]
    
    if (is_shr) {
      vals_clean <- vals[!is.na(vals)]
      q_breaks   <- quantile(vals_clean, probs = seq(0, 1, 0.2))
      q_colors   <- get_quintile_colors(var)   # 5 gradient shades for this variable
      pal        <- colorBin(
        palette  = q_colors,
        domain   = vals,
        bins     = q_breaks,
        na.color = "#cccccc"
      )
      q_labels <- paste0(
        sprintf("%.1f", q_breaks[-length(q_breaks)] * 100), "% – ",
        sprintf("%.1f", q_breaks[-1] * 100), "%"
      )
      leg_title <- paste0("<b>", lbl, "</b>")
      
      proxy |>
        addPolygons(
          data = counties_sf, layerId = ~GEOID, group = "counties",
          fillColor = ~pal(vals), fillOpacity = 0.78,
          color = "#ffffff", weight = 0.7, smoothFactor = 0.4,
          label = ~NAME,
          highlightOptions = highlightOptions(
            weight = 2.5, color = "#2c3e50",
            fillOpacity = 0.9, bringToFront = FALSE
          )
        ) |>
        removeControl("choro_legend") |>
        addLegend(
          layerId  = "choro_legend",
          colors   = q_colors,
          labels   = q_labels,
          title    = leg_title,
          position = "bottomright",
          opacity  = 0.9
        )
      
    } else {
      is_coi_score <- var_meta$is_coi_score[var_meta$col == var]
      vals_clean   <- vals[!is.na(vals)]
      
      if (is_coi_score) {
        # COI scores are already normed 0-100, so the generic ±2 SD outlier
        # cap (built for uncapped population counts) doesn't apply here —
        # use the raw min/max instead.
        lo          <- min(vals_clean)
        hi          <- max(vals_clean)
        vals_capped <- vals
        leg_title   <- paste0("<b>", lbl, "</b>")
      } else {
        mu          <- mean(vals_clean)
        sig         <- sd(vals_clean)
        lo          <- max(0, mu - 2 * sig)
        hi          <- mu + 2 * sig
        vals_capped <- pmin(pmax(vals, lo), hi)
        leg_title   <- paste0("<b>", lbl, "</b>",
                              if (max(vals_clean, na.rm = TRUE) > hi)
                                "<br><small style='font-weight:400'>top values capped at +2 SD</small>"
                              else "")
      }
      pal <- colorNumeric(palette = CONTINUOUS_RAMP, domain = c(lo, hi),
                          na.color = "#cccccc")
      proxy |>
        addPolygons(
          data = counties_sf, layerId = ~GEOID, group = "counties",
          fillColor = pal(vals_capped), fillOpacity = 0.78,
          color = "#ffffff", weight = 0.7, smoothFactor = 0.4,
          label = ~NAME,
          highlightOptions = highlightOptions(
            weight = 2.5, color = "#2c3e50",
            fillOpacity = 0.9, bringToFront = FALSE
          )
        ) |>
        removeControl("choro_legend") |>
        addLegend(
          layerId   = "choro_legend",
          pal       = pal,
          values    = vals_capped[!is.na(vals_capped)],
          title     = leg_title,
          position  = "bottomright",
          labFormat = labelFormat(big.mark = ","),
          opacity   = 0.9
        )
    }
  })
  
  # ── County click: info panel + toggle selection ────────────────────────────────
  observeEvent(input$map_shape_click, {
    last_was_shape_click(TRUE)
    geoid <- input$map_shape_click$id
    if (is.null(geoid)) return()
    geoid <- sub("^sel_", "", geoid)  # click may have landed on the "selections" overlay
    cur <- selected_geo()
    if (identical(input$select_mode, "single")) {
      # Clicking the only selected county again deselects it; clicking any
      # other county replaces the selection instead of adding to it.
      new_geo <- if (length(cur) == 1 && cur == geoid) character(0) else geoid
    } else {
      new_geo <- if (geoid %in% cur) setdiff(cur, geoid) else c(cur, geoid)
    }
    selected_geo(new_geo)
    # If this click just deselected the county, fall back to focusing the
    # last remaining selection (or nothing) instead of the deselected one.
    focused_geo(if (geoid %in% new_geo) geoid else if (length(new_geo) > 0) tail(new_geo, 1) else NULL)
    sync_in_flight(TRUE)
    updateSelectInput(session, "sel_counties", selected = new_geo)
    sync_in_flight(FALSE)
  })
  
  # ── Background click: deselect (debounced so shape click fires first) ──────────
  map_click_r       <- reactive(input$map_click)
  map_click_delayed <- debounce(map_click_r, 300)
  
  observeEvent(map_click_delayed(), {
    if (last_was_shape_click()) { last_was_shape_click(FALSE); return() }
    if (last_was_freehand())    { last_was_freehand(FALSE);    return() }
    focused_geo(NULL)
    selected_geo(character(0))
    sync_in_flight(TRUE)
    updateSelectInput(session, "sel_counties", selected = character(0))
    sync_in_flight(FALSE)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  # ── Freehand path received: select every county the line crosses ──────────────
  # (the map-side control only allows freehand drawing in Multi-select mode)
  observeEvent(input$freehand_path, {
    last_was_freehand(TRUE)
    coords_raw <- input$freehand_path$coords
    tryCatch({
      mat        <- do.call(rbind, lapply(coords_raw, function(p) c(p[[1]], p[[2]])))
      drawn_line <- st_sfc(st_linestring(mat), crs = 4326)
      new_geoids <- counties_sf$GEOID[lengths(st_intersects(counties_sf, drawn_line)) > 0]
      
      if (length(new_geoids) > 0) {
        new_geo <- unique(c(selected_geo(), new_geoids))
        selected_geo(new_geo)
        focused_geo(tail(new_geoids, 1))
        sync_in_flight(TRUE)
        updateSelectInput(session, "sel_counties", selected = new_geo)
        sync_in_flight(FALSE)
      }
    }, error = function(e) message("Freehand select error: ", e$message))
  })
  
  # ── Dropdown → selected_geo ───────────────────────────────────────────────────
  observeEvent(input$sel_counties, {
    if (sync_in_flight()) return()
    new_val <- if (is.null(input$sel_counties)) character(0) else input$sel_counties
    if (identical(input$select_mode, "single") && length(new_val) > 1) {
      new_val <- tail(new_val, 1)
      sync_in_flight(TRUE)
      updateSelectInput(session, "sel_counties", selected = new_val)
      sync_in_flight(FALSE)
    }
    if (!setequal(selected_geo(), new_val)) {
      added <- setdiff(new_val, selected_geo())
      selected_geo(new_val)
      if (length(added) > 0) {
        focused_geo(tail(added, 1))
      } else if (length(new_val) == 0) {
        focused_geo(NULL)
      } else if (is.null(focused_geo()) || !(focused_geo() %in% new_val)) {
        focused_geo(tail(new_val, 1))
      }
    }
  }, ignoreNULL = FALSE)
  
  # ── Switching to single-select trims any existing multi-selection ─────────────
  observeEvent(input$select_mode, {
    if (identical(input$select_mode, "single") && length(selected_geo()) > 1) {
      trimmed <- tail(selected_geo(), 1)
      selected_geo(trimmed)
      focused_geo(trimmed)
      sync_in_flight(TRUE)
      updateSelectInput(session, "sel_counties", selected = trimmed)
      sync_in_flight(FALSE)
    }
  })
  
  # ── Clear all ──────────────────────────────────────────────────────────────────
  observeEvent(input$btn_clear, {
    selected_geo(character(0))
    focused_geo(NULL)
    sync_in_flight(TRUE)
    updateSelectInput(session, "sel_counties", selected = character(0))
    sync_in_flight(FALSE)
  })
  
  # ── Selection highlight + auto-zoom ───────────────────────────────────────────
  observe({
    cur   <- selected_geo()
    proxy <- leafletProxy("map", session)
    proxy |> clearGroup("selections")
    if (length(cur) == 0) return()
    sel_sf <- counties_sf[counties_sf$GEOID %in% cur, ]
    proxy |>
      addPolygons(
        # layerId is prefixed ("sel_") so this overlay is a distinct layer
        # from the base "counties" polygon of the same GEOID rather than
        # replacing it — leaflet's layerId is unique per shape *category*
        # across all groups, so reusing the bare GEOID here would swap out
        # the base gray/choropleth fill entirely, leaving a blank hole once
        # this overlay is later removed via clearGroup("selections").
        data = sel_sf, group = "selections", layerId = ~paste0("sel_", GEOID),
        fillColor = SELECT_FILL, fillOpacity = 0.45,
        color = SELECT_BORDER, weight = 2.5, smoothFactor = 0.4,
        label = ~NAME,
        highlightOptions = highlightOptions(
          weight = 2.5, color = "#2c3e50", fillOpacity = 0.75, bringToFront = FALSE
        )
      )
    bb <- st_bbox(sel_sf)
    proxy |> flyToBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
  })
  
  # ── Navigate ──────────────────────────────────────────────────────────────────
  observeEvent(input$zoom_state, {
    if (is.null(input$zoom_state) || input$zoom_state == "") {
      updateSelectInput(session, "zoom_county",
                        choices = c("— Whole State —" = ""), selected = "")
      return()
    }
    st_cty <- counties_sf |> st_drop_geometry() |>
      filter(state_name == input$zoom_state) |> arrange(NAME)
    updateSelectInput(session, "zoom_county",
                      choices  = c("— Whole State —" = "",
                                   setNames(st_cty$GEOID, st_cty$NAME)),
                      selected = "")
  })
  
  observeEvent(input$btn_zoom, {
    req(!is.null(input$zoom_state) && input$zoom_state != "")
    target_sf <- if (!is.null(input$zoom_county) && input$zoom_county != "") {
      counties_sf[counties_sf$GEOID == input$zoom_county, ]
    } else {
      counties_sf[counties_sf$state_name == input$zoom_state, ]
    }
    bb <- st_bbox(target_sf)
    leafletProxy("map", session) |>
      flyToBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
  })
  
  # ── Badges ────────────────────────────────────────────────────────────────────
  output$county_badges <- renderUI({
    cur <- selected_geo()
    if (length(cur) == 0)
      return(p(style = "color:#aaa;font-size:11px;font-style:italic;", "None"))
    nms <- counties_sf$NAME[counties_sf$GEOID %in% cur]
    tags$div(lapply(nms, function(n) span(class = "badge-cty", n)))
  })
  
  # ── Info panel: clicking a name in the multi-county picker re-focuses it ───────
  observeEvent(input$info_pick_county, {
    if (input$info_pick_county %in% selected_geo()) focused_geo(input$info_pick_county)
  })
  
  # ── Info panel: County Detail vs. Selected Counties Aggregate toggle ───────────
  observeEvent(input$view_detail_btn,    { info_view("detail") })
  observeEvent(input$view_aggregate_btn, {
    if (length(selected_geo()) >= 2) info_view("aggregate")
  })
  # Aggregate view stops making sense once fewer than 2 counties remain selected.
  observe({
    if (info_view() == "aggregate" && length(selected_geo()) < 2) info_view("detail")
  })
  
  output$info_view_toggle <- renderUI({
    n             <- length(selected_geo())
    can_aggregate <- n >= 2
    mode          <- info_view()
    tags$div(
      style = "display:flex;gap:4px;margin-bottom:8px;",
      actionButton("view_detail_btn", "County Detail",
                   class = if (mode == "detail") "btn-sm btn-primary" else "btn-sm btn-default",
                   style = "flex:1;font-size:11px;padding:4px 6px;"),
      actionButton("view_aggregate_btn",
                   paste0("Aggregate", if (n > 0) paste0(" (", n, ")") else ""),
                   class = if (mode == "aggregate") "btn-sm btn-primary" else "btn-sm btn-default",
                   style = paste0("flex:1;font-size:11px;padding:4px 6px;",
                                  if (!can_aggregate) "opacity:.45;cursor:not-allowed;" else ""))
    )
  })
  
  # ── Info panel ─────────────────────────────────────────────────────────────────
  output$info_label <- renderUI({
    cur <- selected_geo()
    if (info_view() == "aggregate" && length(cur) >= 2) {
      paste0(length(cur), " Counties — Aggregate")
    } else if (length(cur) == 0) "National" else "County Detail"
  })
  
  output$info_content <- renderUI({
    cur <- selected_geo()
    
    if (info_view() == "aggregate" && length(cur) >= 2) {
      agg_df <- counties_sf |> st_drop_geometry() |> filter(GEOID %in% cur)
      return(HTML(build_aggregate_html(agg_df)))
    }
    
    if (length(cur) == 0) return(HTML(national_info_html))
    
    # Fall back to the most-recently-selected county if nothing (or a since-
    # deselected county) is focused.
    foc <- focused_geo()
    if (is.null(foc) || !(foc %in% cur)) foc <- tail(cur, 1)
    
    row_df <- counties_sf |> st_drop_geometry() |> filter(GEOID == foc)
    if (nrow(row_df) == 0) return(HTML(national_info_html))
    
    # With multiple counties selected, show a clickable name/state banner per
    # county (styled like the single-county header) so any one of them can be
    # picked to view its data variables below.
    picker <- if (length(cur) > 1) {
      sel_rows <- counties_sf |> st_drop_geometry() |> filter(GEOID %in% cur)
      sel_rows <- sel_rows[match(cur, sel_rows$GEOID), ]
      tags$div(
        style = "margin-bottom:8px;",
        lapply(seq_len(nrow(sel_rows)), function(i) {
          r      <- sel_rows[i, ]
          active <- identical(r$GEOID, foc)
          tags$div(
            r$NAME,
            onclick = sprintf(
              "Shiny.setInputValue('info_pick_county', '%s', {priority:'event'})", r$GEOID
            ),
            style = paste0(
              "cursor:pointer;padding:7px 10px;font-size:12.5px;font-weight:700;",
              "color:#fff;border-radius:4px;margin-bottom:4px;",
              if (active) "background:#cf2e2e;" else "background:#aab2bd;"
            )
          )
        })
      )
    } else NULL
    
    tagList(picker, HTML(build_info_html(row_df, show_header = length(cur) == 1)))
  })
  
  # ── COI subdomain drill-down: repopulate the Subdomain dropdown to match
  # whichever domain is selected in Chart Variable ───────────────────────────
  coi_subdomain_choices <- list(
    r_ED_nat = c(
      "Early Childhood Education"          = "r_ED_EC_nat",
      "Elementary Education"                = "r_ED_EL_nat",
      "Educational Resources"                = "r_ED_ER_nat",
      "Secondary & Post-Secondary Education" = "r_ED_SP_nat"
    ),
    r_HE_nat = c(
      "Pollution"                = "r_HE_EP_nat",
      "Health Resources"         = "r_HE_HR_nat",
      "Safety-Related Resources" = "r_HE_SE_nat",
      "Healthy Environments"     = "r_HE_HE_nat"
    ),
    r_SE_nat = c(
      "Socio-Economic Inequity" = "r_SE_EI_nat",
      "Employment"              = "r_SE_EO_nat",
      "Economic Resources"      = "r_SE_ER_nat",
      "Housing Resources"       = "r_SE_HQ_nat",
      "Social Resources"        = "r_SE_SR_nat",
      "Wealth"                  = "r_SE_WL_nat"
    )
  )
  
  observeEvent(input$chart_var, {
    subs <- coi_subdomain_choices[[input$chart_var]]
    updateSelectInput(session, "chart_var_sub",
                      choices  = c("— Domain Overall —" = "", subs),
                      selected = "")
  })
  
  # Falls back to the domain-level score whenever no subdomain is picked
  # (or the current chart_var isn't a domain with subdomains at all).
  effective_chart_var <- reactive({
    sub <- input$chart_var_sub
    if (!is.null(sub) && sub != "" && input$chart_var %in% names(coi_subdomain_choices)) {
      sub
    } else {
      input$chart_var
    }
  })
  
  # ── Chart title ────────────────────────────────────────────────────────────────
  output$chart_title <- renderUI({
    lbl <- var_meta$label[var_meta$col == effective_chart_var()]
    div(class = "chart-title", paste0("County Comparison — ", lbl))
  })
  
  # ── Bar chart ──────────────────────────────────────────────────────────────────
  output$bar_chart <- renderPlotly({
    cur    <- selected_geo()
    var    <- effective_chart_var()
    lbl    <- var_meta$label[var_meta$col == var]
    is_shr <- var_meta$is_share[var_meta$col == var]
    
    if (length(cur) == 0) {
      return(plot_ly() |> layout(
        annotations = list(list(
          text = "Select counties on the map or from the dropdown.",
          x = 0.5, y = 0.5, xref = "paper", yref = "paper",
          showarrow = FALSE, font = list(size = 13, color = "#aaa")
        )),
        xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
        plot_bgcolor = "#fff", paper_bgcolor = "#fff"
      ) |> config(displaylogo = FALSE, modeBarButtonsToRemove = MODEBAR_REMOVE))
    }
    
    sel_df <- counties_sf |>
      st_drop_geometry() |>
      filter(GEOID %in% cur) |>
      select(GEOID, NAME, val = all_of(var)) |>
      mutate(type = "county") |>
      arrange(val)
    
    # National reference bar only makes sense for share variables — for raw
    # counts the U.S. total dwarfs any county, squashing the actual county
    # bars down to invisible slivers.
    if (is_shr) {
      nat_row <- data.frame(GEOID = NA_character_,
                            NAME  = "United States (National)",
                            val   = national[[var]],
                            type  = "national",
                            stringsAsFactors = FALSE)
      plot_df <- bind_rows(nat_row, sel_df)
    } else {
      plot_df <- sel_df
    }
    
    plot_df$NAME <- factor(plot_df$NAME, levels = plot_df$NAME)
    bar_colors   <- ifelse(plot_df$type == "national", BAR_NATL_COLOR, BAR_COUNTY_COLOR)
    
    if (is_shr) {
      plot_df$dv  <- plot_df$val * 100
      plot_df$lab <- sprintf("%.1f%%", plot_df$dv)
      tick_sfx    <- "%"
      tick_fmt    <- ".1f"
    } else {
      plot_df$dv  <- plot_df$val
      plot_df$lab <- formatC(as.integer(plot_df$dv), format = "d", big.mark = ",")
      tick_sfx    <- ""
      tick_fmt    <- ","
    }
    hover <- paste0("<b>", plot_df$NAME, "</b><br>", plot_df$lab)
    
    plot_ly(
      data = plot_df, x = ~dv, y = ~NAME, key = ~GEOID,
      type = "bar", orientation = "h", source = "bar_chart",
      marker       = list(color = bar_colors, line = list(color = "#fff", width = 0.5)),
      text         = ~lab, textposition = "outside", cliponaxis = FALSE,
      hovertext    = hover, hoverinfo = "text"
    ) |>
      layout(
        xaxis = list(title = "", ticksuffix = tick_sfx, tickformat = tick_fmt,
                     gridcolor  = "#eeeeee", zeroline = FALSE),
        yaxis  = list(title = "", automargin = TRUE),
        margin = list(l = 5, r = 45, t = 5, b = 25),
        plot_bgcolor  = "#ffffff", paper_bgcolor = "#ffffff",
        font = list(family = "Arial, sans-serif", size = 11),
        showlegend = FALSE
      ) |>
      event_register("plotly_hover") |>
      event_register("plotly_unhover") |>
      config(displaylogo = FALSE, modeBarButtonsToRemove = MODEBAR_REMOVE)
  })
  
  # ── Highlight the hovered chart bar's county on the map ────────────────────────
  observeEvent(event_data("plotly_hover", source = "bar_chart"), {
    geoid <- event_data("plotly_hover", source = "bar_chart")$key
    if (is.null(geoid) || is.na(geoid)) return()
    hover_sf <- counties_sf[counties_sf$GEOID == geoid, ]
    if (nrow(hover_sf) == 0) return()
    leafletProxy("map", session) |>
      clearGroup("chart_hover") |>
      addPolygons(
        data = hover_sf, group = "chart_hover",
        fillColor = SELECT_FILL, fillOpacity = 0.55,
        color = SELECT_BORDER, weight = 3, smoothFactor = 0.4,
        options = pathOptions(interactive = FALSE)
      )
  })
  
  observeEvent(event_data("plotly_unhover", source = "bar_chart"), {
    leafletProxy("map", session) |> clearGroup("chart_hover")
  })
}

# Auto-open a browser for local runs (RStudio Source, `Rscript
# shiny_app_ror_growth.R` from a terminal, etc.) — Shiny's default only
# auto-opens when interactive() is TRUE, which non-interactive launches
# (like `Rscript`) fail. But NOT when hosted (shinyapps.io/Connect set
# SHINY_PORT to tell the app which port to listen on) — forcing a browser
# open on a headless server errors out and breaks the deployed app.
local_run <- interactive() || identical(Sys.getenv("SHINY_PORT"), "")
runApp(shinyApp(ui, server), launch.browser = local_run)

