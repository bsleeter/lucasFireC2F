# Builds docs/user-guide/lucasFireC2F-User-Guide.md for the repo from a Markdown export of the user guide (Claude Doc).
# The export marks each image as "&#91;image: <alt>\]"; this replaces them, in order, with links to the local figure files.
# Usage: Rscript make_guide_md.R <exported guide.md>
G <- "F:/lucasFireC2F/docs/user-guide"
x <- readLines(commandArgs(TRUE)[1], warn = FALSE, encoding = "UTF-8")
f <- sort(list.files(file.path(G, "figures"), "[.]png$")); f <- f[!startsWith(f, "01-02b")]
seq1 <- f[!grepl("^0[78]-", f)]                                   # chapters 1-6, in guide order
ch7 <- c("07-01-copy-calibrate.png", "07-03-calibrate-2011-summary.png", "07-02-calibration-period-2011.png",
         "07-04-run-2011-summary.png", "07-05-remove-dependency.png", "07-06-new-dependency.png")
fig <- c("_inputs", "_burn_probability", "_fire_intensity", "_crown_fire", "_hazard_risk", "_fire_sizes", "_validation")
map <- c(seq1, paste0("report/Fig", 1:7, fig, ".png"), ch7, "compare/Compare_burn_probability.png", "compare/Compare_fire_sizes.png",
         sort(f[startsWith(f, "08-")]), "merge/report/Fig2_burn_probability.png", "merge/report/Fig5_hazard_risk.png",
         "merge/Merge_full.png", "merge/Merge_boundary.png")
k <- grep("^&#91;image: ", x); cat(length(k), "image placeholders,", length(map), "figure files\n"); stopifnot(length(k) == length(map))
miss <- map[!file.exists(file.path(G, "figures", map))]; if (length(miss)) stop("missing: ", paste(miss, collapse = ", "))
alt <- gsub("[][\\\\]", "", sub("^&#91;image: ", "", x[k]))
x[k] <- sprintf("![%s](figures/%s)", alt, map)
x <- sub("^Sep 30, 2026 .*$", "For lucasFireC2F 2.4.0. Exported 16 October 2026 from the working copy of the guide.", x)
writeLines(x, file.path(G, "lucasFireC2F-User-Guide.md"), useBytes = TRUE)
for (i in c(1, 5, 26, 64, 71, 72, 73, 86)) cat(x[k[i]], "\n")

# the HTML guide: the same markdown rendered in the page (dev/report/guide_template.html) with a fixed contents rail.
# lucasFireC2F-User-Guide.html opens from the repo folder (figures/ beside it); a second copy without the document wrapper,
# for publishing as a claude.ai Artifact (which adds its own), goes to the folder given as the second argument, if any.
tpl <- paste(readLines("F:/lucasFireC2F/dev/report/guide_template.html", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
md <- gsub("</script", "<\\/script", paste(x, collapse = "\n"), ignore.case = TRUE)
tp <- strsplit(tpl, "<!--GUIDE_MARKDOWN-->", fixed = TRUE)[[1]]; stopifnot(length(tp) == 2)
page <- paste0(tp[1], md, tp[2])   # pasted, not sub(): the markdown's backslashes (Windows paths) must stay as they are
writeLines(c("<!doctype html>", "<html lang=\"en\">", "<head><meta charset=\"utf-8\"><meta name=\"viewport\" content=\"width=device-width, initial-scale=1, viewport-fit=cover\">",
             "<style>body{margin:0}img{max-width:100%}[hidden]{display:none!important}</style></head>", "<body>", page, "</body>", "</html>"),
           file.path(G, "lucasFireC2F-User-Guide.html"), useBytes = TRUE)
if (length(commandArgs(TRUE)) >= 2) writeLines(page, file.path(commandArgs(TRUE)[2], "lucasFireC2F-User-Guide.html"), useBytes = TRUE)
cat("wrote", file.path(G, "lucasFireC2F-User-Guide.html"), "\n")
