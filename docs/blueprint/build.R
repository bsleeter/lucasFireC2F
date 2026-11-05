# =============================================================================
# build.R - the lucasFireC2F blueprint: one schematic of the package's scenarios, stages and datasheets
# =============================================================================
# Writes three files from one drawing:
#   lucasfirec2f-blueprint.svg  print figure (light colours, fonts from Google Fonts with Arial/Consolas fallbacks)
#   lucasfirec2f-blueprint.png  the same at 2x, through headless Chrome or Edge when one is installed
#   index.html                  the same drawing, light/dark aware; click a stage for every datasheet it reads and writes
# Stage names, scripts and datasheet inputs and outputs come from package.xml; the layout, stage families, the national data groups
# (data-sources.R) and the one-line descriptions are set below. Rerun after changing package.xml; a stage or a written datasheet the
# layout does not place stops the build.
# Usage: Rscript docs/blueprint/build.R [out_dir]
# =============================================================================
suppressPackageStartupMessages({ library(xml2); library(jsonlite) })
here <- tryCatch(dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)))), error = function(e) "docs/blueprint")
pkg <- normalizePath(file.path(here, "..", ".."), winslash = "/")
args <- commandArgs(TRUE); out <- if (length(args)) args[1] else here
dir.create(out, showWarnings = FALSE, recursive = TRUE)
source(file.path(here, "style.R")); source(file.path(here, "data-sources.R"))

# ---- package.xml ------------------------------------------------------------
x <- read_xml(file.path(pkg, "package.xml"))
version <- xml_attr(x, "version")
ds <- xml_find_all(x, "/package/dataSheet")
dn <- setNames(xml_attr(ds, "displayName"), xml_attr(ds, "name"))
pretty <- function(s) if (s %in% names(dn)) unname(dn[[s]]) else gsub("([a-z])([A-Z])", "\\1 \\2", s)
tr <- xml_find_all(x, "/package/transformer[@programArguments]")
stage_id <- function(d) { k <- sub(" - .*", "", d); if (k == "Utility") "U" else k }
st <- lapply(tr, function(t) { sh <- xml_find_all(t, "dataSheet")
  list(id = stage_id(xml_attr(t, "displayName")), name = sub("^[^-]+ - ", "", xml_attr(t, "displayName")),
       script = xml_attr(t, "programArguments"), sheet = xml_attr(sh, "name"), type = xml_attr(sh, "type")) })
names(st) <- vapply(st, `[[`, "", "id")
order_ids <- names(fam)                                   # picker and chip order: by family, then pipeline order
if (length(miss <- setdiff(names(st), order_ids))) stop("stage(s) not in the blueprint layout: ", paste(miss, collapse = ", "))
if (length(miss <- setdiff(order_ids, names(st)))) stop("stage(s) in the layout but not in package.xml: ", paste(miss, collapse = ", "))
st <- st[order_ids]
by_order <- function(ids) order_ids[order_ids %in% ids]
writers <- function(sheets) by_order(unlist(lapply(st, function(s) if (any(s$sheet %in% sheets & s$type %in% c("Output", "Both"))) s$id)))
readers <- function(sheets) by_order(unlist(lapply(st, function(s) if (any(s$sheet %in% sheets & s$type %in% c("Input", "Both"))) s$id)))

where <- c(D = "Data Download, a scenario of its own, once per region", P1 = "Prep", P2 = "Prep", P3 = "Prep", P4 = "Prep", P5 = "Prep",
  C1 = "Calibrate", C2 = "Calibrate", C3 = "Calibrate", C5 = "Calibrate (after the seed run of stages 1 to 5)",
  `1` = "Run; also Calibrate, as the seed run", `2` = "Run; also Calibrate, as the seed run", `3` = "Run; also Calibrate, as the seed run (spread days uniform)",
  `4` = "Run; also Calibrate, as the seed run", M = "Merge (first in its pipeline, in place of stages 1 to 4)",
  `5` = "Run and Merge; also Calibrate, as the last stage of the seed run", `6` = "Run and Merge", `7` = "Run and Merge", `8` = "Run and Merge (optional)",
  C6 = "Run, after stage 8 (a seed run is not scored)", C7 = "Run and Merge (last stages)",
  R = "Run and Merge (optional, last), or alone in a Report scenario on any result", U = "A scenario of its own (optional tool; not part of a standard zone build)")
desc <- c(D = "Fetches the national datasets into one Data Sources Home folder, clipped to the fire zones plus a buffer, and records each in the home's manifest; five LANDFIRE 2020 zips are supplied by hand. A rerun fetches only what is missing, and no file in the home is ever replaced.",
  P1 = "Builds the zone's 150 m landscape on the national lattice from LANDFIRE and the zone data: fuel, canopy, elevation, fire-zone, weather-zone, fire-age and vegetation maps, and the zone boundary.",
  P2 = "Fits where large fires start, per cause (Human, Lightning): a GAM of FPA-FOD origins against background points, on ten 30 m covariates; writes the ignition probability maps.",
  P3 = "Extracts the daily weather of every weather zone on the grid from gridMET (weighted toward cells where MTBS fires burned), or reads a prebuilt zone-daily table.",
  P4 = "Builds the severity target stage 8 fits to: MTBS severity class shares by fuel group for the fires after the fuel vintage.",
  P5 = "Builds the burning-period wind ratio table from RAWS hourly records near the zone, which stages 6 and 8 use to turn daily-mean wind into the 20-ft wind the fire burned in.",
  C1 = "Imports the fire record: every FPA-FOD fire whose ignition point is in the zone, with date, size, cause and location.",
  C2 = "Builds the spread-day weather stream: FWI System indices per weather zone from the daily record, kept on days at or above the minimum FWI (optional delta change).",
  C3 = "Fits the fire population: FSim's large-fire threshold, fires per season (gamma), and their season and cause.",
  C5 = "Fits the spread-day distribution so that C2F-W's fire sizes match the recorded large-fire sizes, from the size kernel of the seed run.",
  `1` = "Turns the landscape and canopy maps into the grids C2F-W reads: recent-fire fuel rules, then each fuel type's engine fuel model.",
  `2` = "Samples the fires of every season (count, season, cause, ignition cell) and the shared pool of extra ignitions.",
  `3` = "Gives every sampled fire its spread days, burning hours, a run of weather-stream days and its dead and live fuel moisture.",
  `4` = "Grows every fire with Cell2Fire-World (Scott and Burgan), one call per fire in a window around its origin, in parallel.",
  M = "Combines finished zone runs into one landscape (mosaicked grids, pooled seasons), so that stages 5 to 8 make merged products without growing a fire again.",
  `5` = "Resamples fires below the minimum size from the extra pool, then writes burn probability and burn count maps.",
  `6` = "Recomputes how hard each burned cell burns (Rothermel surface fire, Van Wagner and Cruz crown fire): flame length, intensity, crown fire maps and summaries.",
  `7` = "Writes the WRC-style hazard and risk maps: high-intensity burn probability, conditional and expected risk to potential structures.",
  `8` = "Per-burn and per-fire detail without regrowing fires: burn effects table, fire perimeters, MTBS-style severity maps fitted to the severity target.",
  C6 = "Scores the run's kept fires against the record: fires and area per year by size class, and size quantiles, by cause and vegetation.",
  C7 = "Scores burn probability against where large MTBS fires burned (AUC), and against a benchmark map such as WRC on the same cells.",
  R = "Draws report-quality figures (inputs, burn probability, intensity, crown fire, risk, fire sizes, skill) and the tables behind Studio's Charts tab.",
  U = "Adds observed offshore-wind event days (Diablo, Santa Ana) to a weather stream. Currently a stub that reads and reports its inputs.")

# ---- drawing helpers --------------------------------------------------------
wrap2 <- function(s) { if (nchar(s) <= 19) return(s); sp <- gregexpr(" ", s)[[1]]; k <- sp[which.min(abs(sp - nchar(s) / 2))]
  c(substr(s, 1, k - 1), substr(s, k + 1, nchar(s))) }
short_name <- function(s) sub(" \\(optional\\)$", "", sub("^Offshore Wind ", "Offshore ", s))
BW <- 172; BH <- 64
block <- function(id, x, y) { s <- st[[id]]; nm <- wrap2(short_name(s$name))
  ny <- if (length(nm) == 1) y + 25 else c(y + 19, y + 33)
  paste0(sprintf('<g class="stage f-%s" data-stage="%s" tabindex="0" role="button" aria-label="Stage %s, %s">', fam[[id]], id, id, esc(s$name)),
    sprintf('<rect class="sb" x="%d" y="%d" width="%d" height="%d" rx="5"/>', x, y, BW, BH),
    sprintf('<rect class="badge" x="%d" y="%d" width="24" height="20" rx="3"/>', x + 9, y + 10), TX(x + 21, y + 24, id, "badge-t", "middle"),
    paste(mapply(function(t, yy) TX(x + 41, yy, t, "sn"), nm, ny), collapse = ""), TX(x + 9, y + 54, sub("^src/", "", s$script), "ss"), "</g>") }
chip <- function(id, x, y) sprintf('<g class="chip f-%s" data-stage="%s"><rect x="%s" y="%s" width="18" height="14" rx="3"/>%s</g>',
  fam[[id]], id, x, y, TX(x + 9, y + 10.5, id, "chip-t", "middle"))
chips <- function(ids, xr, y) paste(vapply(seq_along(ids), function(i) chip(ids[i], xr - (length(ids) - i + 1) * 21 + 3, y), ""), collapse = "")
engine <- function(xr, y, label = "C2F-W") { w <- 9 + 6.2 * nchar(label)
  paste0(sprintf('<g class="engine"><rect x="%s" y="%d" width="%s" height="18" rx="9"/>', xr - w, y, w), TX(xr - w / 2, y + 12.5, label, "engine-t", "middle"), "</g>") }
slot <- function(i) 284 + (i - 1) * (BW + 17)              # the five block columns of the middle region
run_arrows <- function(xs, y) for (k in seq_along(xs)[-1]) add(PATH(sprintf("M%d,%d H%d", xs[k - 1] + BW + 1, y + BH / 2, xs[k] - 1)))
pipeline <- function(xr, y, s) add(TX(xr, y, "PIPELINE", "eyebrow", "end"), TX(xr, y + 18, s, "mono", "end"))
W <- 1600; DY <- 162; H <- 1000 + DY   # DY: the Data Download band above the drawing, which is shifted down by it
g <- character()
add <- function(...) g <<- c(g, ...)

# ---- header and legend ------------------------------------------------------
add(sprintf('<rect class="bg" width="%d" height="%d"/>', W, H),
    TX(24, 44, "lucasFireC2F blueprint", "h1"),
    TX(24, 66, sprintf("SyncroSim 3 package · version %s · %d stages in src/ · the scenarios they run in and the datasheets they pass on", version, length(st)), "sub"))
add(fam_legend(38))
add(legend(list(list(kind = "edge", label = "run order in the pipeline"), list(kind = "dep", label = "scenario dependency")), 62))

# ---- Data Download (a band across the top) ------------------------------------
add(box(24, 104, 1552, 142, "Scenario · once per region", "Data Download"))
add(block("D", slot(1), 162))
add(TX(slot(2), 180, "Fetches the national datasets below into one folder, Data Sources Home, clipped to the fire zones + 60 km. Datasets kept whole are shared by every region.", "txt"),
    TX(slot(2), 197, "Five LANDFIRE 2020 zips sit behind a CAPTCHA and are put in the home's supplied folder by hand; the other datasets need no login (LANDFIRE jobs need an email).", "txt"),
    TX(slot(2), 214, "With every Data Sources path blank, the stages read the home and pick the smallest region holding their zone. A path set by hand wins over the home.", "txt"))
add(PATH("M132,246 V264", "dep"), TX(140, 258, "fills Data Sources Home", "lbl"))
add(sprintf('<g transform="translate(0,%d)">', DY))
# ---- national data (left) ---------------------------------------------------
grp <- unique(vapply(D, `[[`, "", 1))
add(box(24, 104, 216, 830, "Data Sources sheet", "National data"))
y <- 172
for (i in seq_along(grp)) { dd <- Filter(function(d) d[[1]] == grp[i], D)
  ids <- by_order(unique(unlist(lapply(dd, function(d) vapply(d[[5]], `[[`, "", "stage")))))
  if (i > 1) add(sprintf('<line class="rule" x1="40" y1="%d" x2="224" y2="%d"/>', y - 15, y - 15))
  add(TX(40, y, grp[i], "gt"))
  if (length(ids) > 5) { y <- y + 8; add(chips(ids, 224, y)); y <- y + 14 } else add(chips(ids, 224, y - 11))
  for (d in dd) { y <- y + 15; add(TX(40, y, d[[3]], "item")) }
  y <- y + 31 }
add(TX(40, y - 12, "Prebuilt rows are optional shortcuts;", "lbl"), TX(40, y + 1, "left blank, the stage builds them", "lbl"))
if (y > 920) stop("the data column runs past its box (y = ", y, ")")

# ---- Prep -------------------------------------------------------------------
add(box(268, 104, 960, 142, "Scenario · one per fire zone", "Prep"))
pipeline(1212, 126, "P1 › P2 › P3 › P4 › P5")
for (i in 1:5) add(block(paste0("P", i), slot(i), 162))
run_arrows(slot(1:5), 162)

# ---- Calibrate ---------------------------------------------------------------
add(PATH("M420,246 V280", "dep"), TX(428, 268, "Prep result", "lbl"))
add(box(268, 282, 960, 142, "Scenario · on the Prep result", "Calibrate"))
pipeline(1212, 304, "C1 › C2 › C3 › 1–5 (seed run) › C5")
add(block("C1", slot(1), 340), block("C2", slot(2), 340), block("C3", slot(3), 340), block("C5", slot(5), 340))
add(sprintf('<g class="pill f-grow"><rect x="%d" y="340" width="%d" height="%d" rx="5"/>%s%s</g>', slot(4), BW, BH,
            TX(slot(4) + BW / 2, 368, "Stages 1–5: seed run", "pill-t", "middle"), TX(slot(4) + BW / 2, 384, "spread days uniform 1–25", "lbl", "middle")))
run_arrows(slot(1:5), 340)

# ---- Run --------------------------------------------------------------------
add(PATH("M268,175 H254 V560 H266", "dep"))
add(PATH("M740,424 V458", "dep"), TX(748, 446, "Calibrate result (first dependency)", "lbl"))
add(box(268, 460, 960, 236, "Scenario · on the Calibrate and Prep results", "Run"))
pipeline(1212, 482, "1 › 2 › 3 › 4 › 5 › 6 › 7 › 8 › C6 › C7 › R")
for (i in 1:4) add(block(as.character(i), slot(i), 518))
add(engine(slot(4) + BW - 8, 518 + 42))
for (i in 5:8) add(block(as.character(i), slot(i - 4), 614))
run_arrows(slot(1:4), 518); run_arrows(slot(1:4), 614)
add(PATH(sprintf("M%d,582 V598 H%d V612", slot(4) + BW / 2, slot(1) + BW / 2)))
add(TX(slot(5) + 4, 540, "Fire seasons, random seed", "txt"), TX(slot(5) + 4, 555, "and options in Run Control;", "txt"),
    TX(slot(5) + 4, 570, "seed run off.", "txt"), TX(slot(5) + 4, 640, "8 is optional; then the", "txt"),
    TX(slot(5) + 4, 655, "scores and figures below.", "txt"))

# ---- Merge, scores and figures, utility --------------------------------------
add(PATH("M380,696 V722", "dep"), TX(388, 714, "zone Run results", "lbl"))
add(box(268, 724, 260, 210, "Scenario · on several Run results", "Merge"), block("M", 284, 782),
    TX(284, 868, "Pools finished zone runs; then", "txt"), TX(284, 883, "5–8, C7 and R on the merged", "txt"), TX(284, 898, "tables.", "txt"),
    TX(284, 920, "M › 5 › 6 › 7 › 8 › C7 › R", "mono"))
add(PATH("M760,696 V722"), TX(768, 714, "last in the pipelines", "lbl"))
add(box(552, 724, 410, 210, "Stages · end of Run and Merge", "Scores and figures"),
    block("C6", 568, 782), block("C7", 757, 782), block("R", 568, 856),
    TX(757, 872, "C6 and C7 end a Run;", "txt"), TX(757, 887, "C7 and R end a Merge.", "txt"),
    TX(757, 907, "R alone reports on any result.", "txt"))
add(box(986, 724, 242, 210, "Scenario · optional tool", "Utility"), block("U", 1021, 782),
    TX(1002, 872, "Not used in a standard zone", "txt"), TX(1002, 887, "build; a stub in this version.", "txt"))

# ---- datasheets passed on (right) -------------------------------------------
sheets <- list(
  "Data Download" = list("Download Results" = "OutputDataDownload", "Data Sources Used" = "OutputDataSources"),
  "Zone inputs (Prep)" = list("Landscape and Canopy Maps" = c("LandscapeRasters", "CanopyRasters"), "Zone Boundary" = "OutputZoneBoundary",
    "Ignition Probability Maps, fit" = c("IgnitionGrid", "OutputIgnitionFit"), "Daily Weather" = "DailyWeather", "Severity Target" = "SeverityTarget",
    "Wind Ratios, RAWS Stations" = c("WindRatio", "RAWSStation")),
  "Calibration" = list("Historical Fires" = "HistoricalFire", "Spread-Day Weather Stream" = "WeatherStream",
    "Ignition Count and Distribution" = c("IgnitionCount", "IgnitionDistribution"), "Fire Resampling (minimum size)" = "ResampleOption",
    "Spread-Day Distribution" = "SpreadDays", "Calibration fit and summary" = c("OutputCalibrationFit", "OutputCalibrationSummary")),
  "Fire tables" = list("Engine Landscape" = "OutputEngineLandscape", "Fire List" = "OutputFireList", "Fire Weather" = "OutputFireWeather",
    "Burn Table, Fire Statistics" = c("OutputBurnTable", "OutputFireStatistic")),
  "Maps and reports" = list("Burn Probability, Burn Count" = c("OutputBurnProbability", "OutputBurnCount"),
    "Fire Effects maps and summary" = c("OutputFireEffects", "OutputEffectsSummary"), "Hazard and Risk maps" = "OutputRisk",
    "Per-burn effects, perimeters" = c("OutputBurnEffects", "OutputFirePerimeter"), "Severity maps and fit" = c("OutputSeverityMap", "OutputSeverityFit", "SeverityClass"),
    "Scores against the record" = "OutputScore", "Validation and selection" = c("OutputValidation", "OutputSelection"),
    "Report figures and charts" = c("OutputReportFigure", "OutputChartSizeClass", "OutputChartSpreadDays", "OutputChartValidation")),
  "Options carried by Merge" = list("Effects, risk, weather, detail, output" = c("EffectsOption", "WeatherOption", "RiskOption", "FireDetailOption", "OutputOption")))
written <- unique(unlist(lapply(st, function(s) s$sheet[s$type %in% c("Output", "Both")])))
if (length(miss <- setdiff(written, unlist(sheets)))) stop("datasheet(s) written but not placed in the blueprint: ", paste(miss, collapse = ", "))
row <- function(label, sh, y) { w <- writers(sh); if (!length(w)) stop("no stage writes ", paste(sh, collapse = ", "))
  add(sprintf('<g class="row" data-sheet="%s" data-stages="%s">', paste(sh, collapse = " "), paste(w, collapse = " ")),
      sprintf('<rect class="rowbg" x="1264" y="%d" width="304" height="18" rx="3"/>', y - 13), TX(1272, y, label, "rowt"), chips(w, 1560, y - 11), "</g>") }
add(box(1256, 104, 320, 830, "Written by the stages", "Datasheets passed on"))
y <- 172
for (gname in names(sheets)) { add(TX(1272, y, toupper(gname), "ghead")); y <- y + 18
  for (lab in names(sheets[[gname]])) { row(lab, sheets[[gname]][[lab]], y); y <- y + 19 }
  y <- y + 14 }
add(TX(1272, y + 4, "Chips: the stages that write (or update)", "lbl"), TX(1272, y + 17, "each sheet. Select a stage for its readers.", "lbl"))
if (y > 925) stop("the datasheet column runs past its box (y = ", y, ")")

add("</g>")
add(TX(24, 962 + DY, "Solid arrows: the order stages run in a scenario's pipeline. Dashed: a scenario dependency (the dependency supplies its datasheets; a scenario's own rows win). Chips: the stages that read a data group, or write a sheet.", "foot"),
    TX(24, 978 + DY, sprintf("Run settings (fire seasons, seed) are in Run Control. Generated by docs/blueprint/build.R from package.xml (lucasFireC2F %s), %s.", version, format(Sys.Date())), "foot"))
body <- paste(c(defs, g), collapse = "\n")
head <- svg_head(W, H, "lucasFireC2F blueprint: scenarios, stages and datasheets")
writeLines(standalone_svg(head, body), file.path(out, "lucasfirec2f-blueprint.svg"), useBytes = TRUE)

# ---- interactive page -------------------------------------------------------
sheet_list <- function(s, types) { k <- s$type %in% types
  lapply(s$sheet[k], function(sh) list(sheet = sh, label = pretty(sh), from = setdiff(writers(sh), s$id), to = setdiff(readers(sh), s$id))) }
stages_json <- lapply(st, function(s) list(id = s$id, name = s$name, script = s$script, family = fam[[s$id]], where = where[[s$id]], desc = desc[[s$id]],
  reads = sheet_list(s, "Input"), writes = sheet_list(s, "Output"), both = sheet_list(s, "Both")))
json <- gsub("</", "<\\/", toJSON(list(order = order_ids, stages = stages_json), auto_unbox = TRUE), fixed = TRUE)
tpl <- paste(readLines(file.path(here, "template.html"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
writeLines(fill_page(tpl, paste0(head, "\n", body, "\n</svg>"), json, version, length(st), "package"), file.path(out, "index.html"), useBytes = TRUE)

render_png(file.path(out, "lucasfirec2f-blueprint.svg"), file.path(out, "lucasfirec2f-blueprint.png"), W, H)
cat("svg:", file.path(out, "lucasfirec2f-blueprint.svg"), "\nhtml:", file.path(out, "index.html"), "\n")
