# =============================================================================
# build-data.R - the lucasFireC2F data blueprint: every national dataset, the stages that read it, what for, and what switches it
# =============================================================================
# Companion to build.R (stages and datasheets). Writes lucasfirec2f-data-blueprint.svg (print), lucasfirec2f-data-blueprint.png (2x)
# and data.html (light/dark; click a dataset or a stage). The dataset list and display names come from package.xml's Data Sources
# sheet; the groups, short labels, vintages and what each stage does with each dataset are in data-sources.R. The build stops when
# package.xml has a Data Sources column the drawing does not place, or when a stage script and data-sources.R disagree about which
# columns it reads.
# Usage: Rscript docs/blueprint/build-data.R [out_dir]
# =============================================================================
suppressPackageStartupMessages({ library(xml2); library(jsonlite) })
here <- tryCatch(dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)))), error = function(e) "docs/blueprint")
pkg <- normalizePath(file.path(here, "..", ".."), winslash = "/")
args <- commandArgs(TRUE); out <- if (length(args)) args[1] else here
dir.create(out, showWarnings = FALSE, recursive = TRUE)
source(file.path(here, "style.R")); source(file.path(here, "data-sources.R"))

x <- read_xml(file.path(pkg, "package.xml")); version <- xml_attr(x, "version")
cols <- xml_find_all(x, "/package/dataSheet[@name='DataSource']/column")
ds_display <- setNames(xml_attr(cols, "displayName"), xml_attr(cols, "name"))
ds_display <- ds_display[!names(ds_display) %in% c("DataSourceId", "ProjectId")]
if (length(m <- setdiff(names(ds_display), names(D)))) stop("Data Sources column(s) not in the data blueprint: ", paste(m, collapse = ", "))
if (length(m <- setdiff(names(D), names(ds_display)))) stop("not a Data Sources column in package.xml: ", paste(m, collapse = ", "))

# ---- the stages that read national data -------------------------------------
S <- list(
  P1 = list("Prepare Zone Landscape", "The zone's 150 m fuel, canopy, elevation, zone, fire-age and vegetation maps, and its boundary", "Zone Preparation Options (zone, buffer, vintages)"),
  P2 = list("Fit Ignition Surfaces", "Ignition probability maps per cause, from a GAM on ten 30 m covariates", "Zone Preparation Options (ignition fire size, years, buffer)"),
  P3 = list("Extract Zone Weather", "Daily weather of each weather zone on the grid", "Data Sources (gridMET build, or a prebuilt table)"),
  P4 = list("MTBS Severity Target", "Severity class shares by fuel group, the target stage 8 fits to", "Zone Preparation Options (severity years)"),
  P5 = list("Burning-Period Wind (RAWS)", "Burning-period wind ratio table for stages 6 and 8", "Wind Table Options (months, years, hours, distance)"),
  C1 = list("Import Fire Record", "The zone's recorded fires (Historical Fires): date, size, cause, location", "Fire Record Import; Fire Record Sources"),
  C7 = list("Validate Burn Probability", "AUC of burn probability against large MTBS fires, and against the benchmark", "Validation Options; Validation Period"),
  R = list("Report Figures", "Report figures and the tables behind the Charts tab", "Run Control; the results of the stages that ran"))
stage_script <- function(id) { f <- Sys.glob(file.path(pkg, "src", sprintf("%s-*.R", tolower(id)))); stopifnot(length(f) == 1); f }
for (id in names(S)) { txt <- paste(readLines(stage_script(id), warn = FALSE), collapse = "\n")
  reads <- names(ds_display)[vapply(names(ds_display), function(c) grepl(paste0("\\b", c, "\\b"), txt, perl = TRUE), TRUE)]
  mine <- names(D)[vapply(D, function(d) any(vapply(d[[5]], function(z) z$stage == id, TRUE)), TRUE)]
  if (length(m <- setdiff(reads, mine))) stop(sprintf("stage %s reads %s but the data blueprint has no use for it", id, paste(m, collapse = ", ")))
  if (length(m <- setdiff(mine, reads))) stop(sprintf("the data blueprint says stage %s reads %s, but its script does not mention it", id, paste(m, collapse = ", "))) }
others <- setdiff(sub("-.*", "", basename(Sys.glob(file.path(pkg, "src", "*-*.R")))), c(tolower(names(S)), "u"))
for (o in others) { txt <- paste(readLines(Sys.glob(file.path(pkg, "src", paste0(o, "-*.R")))[1], warn = FALSE), collapse = "\n")
  if (any(vapply(names(ds_display), function(c) grepl(paste0("\\b", c, "\\b"), txt, perl = TRUE), TRUE))) stop("src/", o, "-*.R reads Data Sources but has no stage box") }

# ---- drawing ----------------------------------------------------------------
W <- 1600; H <- 1000
g <- character(); add <- function(...) g <<- c(g, ...)
add(sprintf('<rect class="bg" width="%d" height="%d"/>', W, H), TX(24, 44, "lucasFireC2F data blueprint", "h1"),
    TX(24, 66, sprintf("The national datasets lucasFireC2F reads, the stages that read them, what for, and the option that switches each one · version %s", version), "sub"))
add(fam_legend(38))
add(legend(list(list(alt = FALSE, label = "read with the default settings"), list(alt = TRUE, label = "read only when a shortcut row is filled")), 62))

add(box(24, 104, 600, 830, "Data Sources sheet · project scope", "National datasets"), TX(312, 146, "VINTAGE · RESOLUTION", "ghead"))
y <- 172; sy <- c(); grp <- ""
for (d in D) {
  if (d[[1]] != grp) { if (nzchar(grp)) y <- y + 12; grp <- d[[1]]; add(TX(40, y, toupper(grp), "ghead")); y <- y + 21 }
  add(sprintf('<g class="ds" data-ds="%s" tabindex="0" role="button" aria-label="%s"><rect class="dsbg" x="32" y="%d" width="584" height="22" rx="3"/>', d[[2]], esc(d[[3]]), y - 15),
      TX(40, y, d[[3]], "dsn"), TX(312, y, d[[4]], "dsv"), "</g>")
  sy[d[[2]]] <- y - 4; y <- y + 23 }
if (y > 934) stop("the dataset column runs past its box (y = ", y, ")")

BX <- 1000; BWD <- 576; BHT <- 72
by <- setNames(104 + (seq_along(S) - 1) * ((830 - BHT) / (length(S) - 1)), names(S))
uses <- do.call(rbind, lapply(D, function(d) do.call(rbind, lapply(d[[5]], function(z)
  data.frame(ds = d[[2]], stage = z$stage, alt = z$alt, sy = sy[[d[[2]]]], stringsAsFactors = FALSE)))))
uses$ty <- NA_real_
for (id in names(S)) { k <- which(uses$stage == id); k <- k[order(uses$sy[k])]
  uses$ty[k] <- by[[id]] + 6 + (seq_along(k) - 0.5) * (BHT - 12) / length(k) }
for (i in seq_len(nrow(uses))) { r <- uses[i, ]
  add(sprintf('<path class="wire f-%s%s" data-ds="%s" data-stage="%s" d="M616,%s C808,%s 808,%.1f %d,%.1f"/>', fam[[r$stage]], if (r$alt) " alt" else "",
              r$ds, r$stage, r$sy, r$sy, r$ty, BX - 2, r$ty)) }
for (n in names(sy)) add(sprintf('<circle class="sport" cx="616" cy="%s" r="3"/>', sy[[n]]))
for (id in names(S)) { s <- S[[id]]; yy <- round(by[[id]])
  add(sprintf('<g class="stage f-%s" data-stage="%s" tabindex="0" role="button" aria-label="Stage %s, %s">', fam[[id]], id, id, esc(s[[1]])),
      sprintf('<rect class="sb" x="%d" y="%d" width="%d" height="%d" rx="5"/>', BX, yy, BWD, BHT),
      sprintf('<rect class="badge" x="%d" y="%d" width="24" height="20" rx="3"/>', BX + 10, yy + 10), TX(BX + 22, yy + 24, id, "badge-t", "middle"),
      TX(BX + 44, yy + 25, s[[1]], "sn2"), TX(BX + 12, yy + 47, s[[2]], "mk"), TX(BX + 12, yy + 62, paste("Set by:", s[[3]]), "ch"), "</g>")
  for (ty in uses$ty[uses$stage == id]) add(sprintf('<circle class="port f-%s" cx="%d" cy="%.1f" r="2.4"/>', fam[[id]], BX, ty)) }

add(TX(24, 962, "Wires run from a dataset to each stage that reads it, coloured by the stage. Vintages are those the user guide lists (section 2.1). Every dataset is national, so one project's rows serve any pyrome in CONUS.", "foot"),
    TX(24, 978, sprintf("The other stages read only datasheets. Generated by docs/blueprint/build-data.R from package.xml (lucasFireC2F %s), %s.", version, format(Sys.Date())), "foot"))
body <- paste(c(defs, g), collapse = "\n")
head <- svg_head(W, H, "lucasFireC2F data blueprint: national datasets and the stages that read them")
svg_file <- file.path(out, "lucasfirec2f-data-blueprint.svg")
writeLines(standalone_svg(head, body), svg_file, useBytes = TRUE)

# ---- interactive page -------------------------------------------------------
dsj <- lapply(D, function(d) list(col = d[[2]], label = d[[3]], group = d[[1]], vintage = d[[4]], display = unname(ds_display[[d[[2]]]]),
  uses = lapply(d[[5]], function(z) list(stage = z$stage, role = z$role, note = if (is.na(z$note)) "" else z$note, alt = z$alt))))
stj <- lapply(names(S), function(id) { s <- S[[id]]
  list(id = id, label = id, name = s[[1]], family = fam[[id]], makes = s[[2]], set = s[[3]],
       reads = unname(lapply(Filter(function(d) any(vapply(d[[5]], function(z) z$stage == id, TRUE)), D), function(d) {
         z <- Filter(function(z) z$stage == id, d[[5]])[[1]]
         list(col = d[[2]], label = d[[3]], role = z$role, note = if (is.na(z$note)) "" else z$note, alt = z$alt) }))) })
names(stj) <- names(S)
json <- gsub("</", "<\\/", toJSON(list(datasets = unname(dsj), stages = stj), auto_unbox = TRUE), fixed = TRUE)
tpl <- paste(readLines(file.path(here, "template-data.html"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
writeLines(fill_page(tpl, paste0(head, "\n", body, "\n</svg>"), json, version, length(D), "data"), file.path(out, "data.html"), useBytes = TRUE)

render_png(svg_file, file.path(out, "lucasfirec2f-data-blueprint.png"), W, H)
cat("svg:", svg_file, "\nhtml:", file.path(out, "data.html"), "\n")
