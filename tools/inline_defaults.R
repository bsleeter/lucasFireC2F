# =============================================================================
# inline_defaults.R - copies defaults/*.csv into package.xml as <record> rows
# =============================================================================
# Each datasheet that ships defaults carries a marker <!--@records Name--> (first run) or a block
# <!--@records Name--> ... <!--@end Name--> (later runs), which is replaced by one <record> per CSV row.
# Columns that reference another datasheet take that datasheet's display name (e.g. FuelType "GR2").
# Run from the repo root after tools/make_defaults.R:  Rscript tools/inline_defaults.R
# =============================================================================
x <- paste(readLines("package.xml", warn = FALSE), collapse = "\n")
esc <- function(s) { s <- gsub("&", "&amp;", s, fixed = TRUE); s <- gsub("<", "&lt;", s, fixed = TRUE)
  s <- gsub(">", "&gt;", s, fixed = TRUE); gsub("\"", "&quot;", s, fixed = TRUE) }
for (f in list.files("defaults", "\\.csv$")) {
  nm <- sub("\\.csv$", "", f)
  d <- read.csv(file.path("defaults", f), check.names = FALSE, na.strings = "", colClasses = "character")
  for (j in seq_along(d)) { v <- d[[j]]; v[v %in% "TRUE"] <- "-1"; v[v %in% "FALSE"] <- "0"; d[[j]] <- v }
  rec <- vapply(seq_len(nrow(d)), function(i) { k <- !is.na(unlist(d[i, ]))
    sprintf("\t\t<record columns=\"%s\" values=\"%s\"/>", paste(names(d)[k], collapse = "|"), esc(paste(unlist(d[i, ])[k], collapse = "|"))) }, "")
  block <- paste0("<!--@records ", nm, "-->\n", paste(rec, collapse = "\n"), "\n\t\t<!--@end ", nm, "-->")
  pat <- sprintf("(?s)<!--@records %s-->(.*<!--@end %s-->)?", nm, nm)
  if (!grepl(pat, x, perl = TRUE)) { cat("no marker for", nm, "- skipped\n"); next }
  x <- sub(pat, block, x, perl = TRUE)
  cat(sprintf("%-18s %3d records\n", nm, nrow(d)))
}
writeLines(x, "package.xml")
