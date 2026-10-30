# =============================================================================
# style.R - what the blueprint drawings share: text and path helpers, colour tokens, the SVG classes, and the PNG export
# =============================================================================
# Sourced by build.R (stages and datasheets) and build-data.R (national data). One set of tokens and classes, so both drawings look
# alike in print, light and dark. Adapted from the lucasCore blueprint (bsleeter/lucasCore docs/blueprint/style.R).
# =============================================================================
esc <- function(s) gsub(">", "&gt;", gsub("<", "&lt;", gsub("&", "&amp;", s, fixed = TRUE), fixed = TRUE), fixed = TRUE)
TX <- function(x, y, s, cls, anchor = NULL) sprintf('<text x="%s" y="%s" class="%s"%s>%s</text>', x, y, cls,
  if (is.null(anchor)) "" else sprintf(' text-anchor="%s"', anchor), esc(s))
PATH <- function(d, cls = "edge", arrow = TRUE) sprintf('<path d="%s" class="%s"%s/>', d, cls, if (arrow) ' marker-end="url(#ah)"' else "")
box <- function(x, y, w, h, eyebrow, name = NULL) paste0(sprintf('<rect class="box" x="%d" y="%d" width="%d" height="%d" rx="7"/>', x, y, w, h),
  TX(x + 16, y + 22, toupper(eyebrow), "eyebrow"), if (!is.null(name)) TX(x + 16, y + 42, name, "sname"))
defs <- '<defs><marker id="ah" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" orient="auto-start-reverse"><path d="M0,0 L10,5 L0,10 z" class="ah"/></marker></defs>'
# stage families (both drawings colour a stage, its chips and its wires by these): zone preparation, calibration, fire growth
# (and the merge that stands in for it), burn probability / effects / risk, scoring and figures
fam <- c(P1 = "prep", P2 = "prep", P3 = "prep", P4 = "prep", P5 = "prep", U = "prep",
         C1 = "cal", C2 = "cal", C3 = "cal", C5 = "cal",
         `1` = "grow", `2` = "grow", `3` = "grow", `4` = "grow", M = "grow",
         `5` = "fx", `6` = "fx", `7` = "fx", `8` = "fx",
         C6 = "eval", C7 = "eval", R = "eval")
fam_labels <- c(prep = "Zone preparation", cal = "Calibration", grow = "Fire growth and merge", fx = "Burn probability, effects, risk", eval = "Scores and figures")

rules <- "
.bg{fill:var(--bg)} .box{fill:var(--surface);stroke:var(--line);stroke-width:1}
.h1{font:700 22px var(--sans);fill:var(--ink)} .sub{font:400 12px var(--sans);fill:var(--muted)}
.eyebrow{font:500 9.5px var(--mono);letter-spacing:.08em;fill:var(--muted)} .sname{font:700 14px var(--sans);fill:var(--ink)}
.ghead{font:600 9px var(--mono);letter-spacing:.08em;fill:var(--muted)} .gt{font:600 12px var(--sans);fill:var(--ink)}
.item{font:400 10px var(--sans);fill:var(--ink2)} .txt{font:400 11px var(--sans);fill:var(--ink2)}
.mono{font:400 10px var(--mono);fill:var(--muted)} .lbl{font:400 9.5px var(--mono);fill:var(--muted)}
.foot{font:400 10.5px var(--sans);fill:var(--muted)} .lgt{font:400 11px var(--sans);fill:var(--ink2)}
.f-prep{--c:var(--prep);--t:var(--prep-t)} .f-cal{--c:var(--cal);--t:var(--cal-t)} .f-grow{--c:var(--grow);--t:var(--grow-t)}
.f-fx{--c:var(--fx);--t:var(--fx-t)} .f-eval{--c:var(--eval);--t:var(--eval-t)}
.lg{fill:var(--c)} .stage .sb{fill:var(--t);stroke:var(--c);stroke-width:1.2} .stage.ghost .sb{stroke-dasharray:4 3}
.badge{fill:var(--c)} .badge-t{font:600 11px var(--mono);fill:var(--on)} .sn{font:600 11.5px var(--sans);fill:var(--ink)}
.ss{font:400 9px var(--mono);fill:var(--muted)} .chip rect{fill:var(--c)} .chip-t{font:600 8.5px var(--mono);fill:var(--on)}
.pill rect{fill:var(--t);stroke:var(--c);stroke-width:1;stroke-dasharray:4 3} .pill-t{font:600 11px var(--sans);fill:var(--ink2)}
.edge{fill:none;stroke:var(--edge);stroke-width:1.3} .dep{fill:none;stroke:var(--edge);stroke-width:1.2;stroke-dasharray:5 4}
.ah{fill:var(--edge)} .rule{stroke:var(--line);stroke-width:1}
.engine rect{fill:var(--ink)} .engine-t{font:600 9.5px var(--mono);fill:var(--surface)}
.rowbg{fill:transparent} .rowt{font:400 10.5px var(--sans);fill:var(--ink2)}
.dsbg{fill:transparent} .dsn{font:600 11.5px var(--sans);fill:var(--ink)} .dsv{font:400 9.5px var(--mono);fill:var(--muted)}
.wire{fill:none;stroke:var(--c);stroke-width:1.3;opacity:.75} .wire.alt{stroke-dasharray:4 3}
.sport{fill:var(--edge)} .port{fill:var(--c)}
.sn2{font:600 13px var(--sans);fill:var(--ink)} .mk{font:400 11px var(--sans);fill:var(--ink2)} .ch{font:400 10px var(--sans);fill:var(--muted)}
"
light <- "--bg:#f5f5f3;--surface:#ffffff;--line:#d6d5d0;--ink:#1f2124;--ink2:#3a3c40;--muted:#5f6166;--edge:#45474c;--on:#ffffff;
--prep:#56666e;--prep-t:#eceff0;--cal:#2b679a;--cal-t:#e3edf6;--grow:#b4441c;--grow-t:#fbe9e1;--fx:#2d7650;--fx-t:#e4f1e9;--eval:#6a5293;--eval-t:#eee9f5;"
dark <- "--bg:#141517;--surface:#1c1d20;--line:#36373c;--ink:#e6e5e1;--ink2:#c8c7c2;--muted:#9a9a95;--edge:#b3b2ad;--on:#141517;
--prep:#9eb0b8;--prep-t:#1f2a2f;--cal:#79b3e3;--cal-t:#15253a;--grow:#f07a52;--grow-t:#3a2219;--fx:#6dc59b;--fx-t:#15291f;--eval:#b9a0e6;--eval-t:#251f33;"
fonts <- "--sans:'IBM Plex Sans',Arial,Helvetica,sans-serif;--mono:'IBM Plex Mono',Consolas,'Courier New',monospace;"
gf <- "https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@400;500;600&family=IBM+Plex+Sans:wght@400;600;700&display=swap"

svg_head <- function(W, H, title) paste0(sprintf('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 %d %d" width="%d" height="%d" role="img" aria-labelledby="bp-title">', W, H, W, H),
  "\n<title id=\"bp-title\">", esc(title), "</title>")
# the print figure: light tokens inside the SVG (CDATA, since the font URL has &)
standalone_svg <- function(head, body) paste0(head, "\n<style><![CDATA[@import url('", gf, "');\nsvg{", fonts, light, "}", rules, "]]></style>\n", body, "\n</svg>\n")
fill <- function(t, k, v) paste(strsplit(t, k, fixed = TRUE)[[1]], collapse = v)
# legend: family swatches, or line samples (kind = an edge class, or a wire with alt = TRUE/FALSE), right-aligned at x = 1576
legend <- function(items, y) { w <- vapply(items, function(it) 6.2 * nchar(it$label) + 30, 0); x0 <- 1576 - sum(w) + 12
  unlist(lapply(seq_along(items), function(i) { xx <- x0 + sum(w[seq_len(i - 1)]); it <- items[[i]]
    c(if (!is.null(it$fam)) sprintf('<rect class="lg f-%s" x="%s" y="%s" width="14" height="12" rx="2"/>', it$fam, xx, y - 10)
      else if (!is.null(it$alt)) sprintf('<path d="M%s,%s h18" class="wire f-prep%s" style="opacity:1"/>', xx, y - 4, if (it$alt) " alt" else "")
      else PATH(sprintf("M%s,%s h16", xx, y - 4), it$kind), TX(xx + 22, y, it$label, "lgt")) })) }
fam_legend <- function(y) legend(lapply(names(fam_labels), function(f) list(fam = f, label = fam_labels[[f]])), y)
# the pages are standalone documents served by GitHub Pages from docs/ (docs/.nojekyll: served as is). The nav uses the Pages URLs,
# so it also works from a copy of the page somewhere else
SITE <- "https://bsleeter.github.io/lucasFireC2F/"
PAGES <- paste0(SITE, "blueprint/")
REPO <- "https://github.com/bsleeter/lucasFireC2F"
page_head <- '<!doctype html>\n<html lang="en">\n<meta charset="utf-8">\n<meta name="viewport" content="width=device-width, initial-scale=1, viewport-fit=cover">\n'
# the three public pages: the User Guide (dev/report/make_guide_md.R) and the two blueprints
nav <- function(current) { l <- c(guide = "User Guide", package = "Package blueprint", data = "Data blueprint")
  href <- c(guide = paste0(SITE, "user-guide/lucasFireC2F-User-Guide.html"), package = PAGES, data = paste0(PAGES, "data.html"))
  a <- vapply(names(l), function(k) sprintf('<a href="%s"%s>%s</a>', href[[k]], if (k == current) ' aria-current="page"' else "", l[[k]]), "")
  paste0('<nav class="links" aria-label="lucasFireC2F documentation">', paste(a, collapse = ""), sprintf('<a href="%s">Source on GitHub</a>', REPO), '</nav>') }
fill_page <- function(tpl, svg_inline, json, version, n, current) {
  p <- fill(fill(fill(fill(fill(fill(tpl, "{{RULES}}", rules), "{{LIGHT}}", light), "{{DARK}}", dark), "{{FONTS}}", fonts), "{{SVG}}", svg_inline), "{{DATA}}", json)
  paste0(page_head, fill(fill(fill(p, "{{VERSION}}", version), "{{N}}", as.character(n)), "{{NAV}}", nav(current))) }
render_png <- function(svg_file, png_file, W, H) {
  browser <- Filter(file.exists, c("C:/Program Files/Google/Chrome/Application/chrome.exe", "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe"))
  if (!length(browser)) return(cat("png: no Chrome or Edge found; open the SVG in a browser to export one\n"))
  png <- normalizePath(png_file, winslash = "\\", mustWork = FALSE)
  system2(browser[1], c("--headless=new", "--disable-gpu", "--hide-scrollbars", "--force-device-scale-factor=2", "--virtual-time-budget=4000",
                        sprintf("--window-size=%d,%d", W, H), shQuote(paste0("--screenshot=", png)),
                        paste0("file:///", normalizePath(svg_file, winslash = "/"))), stdout = FALSE, stderr = FALSE)
  cat(if (file.exists(png)) paste("png:", png) else "png: headless browser wrote nothing", "\n") }
