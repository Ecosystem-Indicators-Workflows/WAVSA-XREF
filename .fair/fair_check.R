#!/usr/bin/env Rscript
# EIW FAIR check
#
# Scores this repository against the ARDC FAIR Software Checklist (v0.2) and
# the EIW targets (FAIR4EIW Appendix D), then:
#   - writes fair/fair-scores.png and fair/fair-score.json
#   - rewrites the README block between <!-- FAIR:START --> and <!-- FAIR:END -->
#   - writes the same summary to the GitHub Actions job summary, if available
#
# Run from the repository root: Rscript .fair/fair_check.R
# Settings that cannot be detected live in .fair/fair-checklist.yml.
#
# Scoring follows the ARDC checklist source (pages/v0.2/store.ts): a question
# scores the points of the selected answer, and a principle scores
# 100 * sum(points) / sum(max points). In v0.2 an answer's points equal its
# position (0, 1, 2, 3), so the scores double as the badge-URL answer indices.

suppressPackageStartupMessages({
  library(ggplot2)
  library(yaml)
  library(jsonlite)
})

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || identical(a, "")) b else a

cfg <- if (file.exists(".fair/fair-checklist.yml")) read_yaml(".fair/fair-checklist.yml") else list()
out_dir <- "fair"
dir.create(out_dir, showWarnings = FALSE)

# ---- Evidence ---------------------------------------------------------------

git <- function(...) {
  tryCatch(suppressWarnings(system2("git", c(...), stdout = TRUE, stderr = FALSE)),
           error = function(e) character())
}

# Tracked files only: that is what someone cloning the repository receives.
files <- git("ls-files")
if (!length(files)) files <- list.files(".", recursive = TRUE)
files <- files[!grepl("^(fair|\\.fair|renv/library)/", files)]
ext <- tolower(tools::file_ext(files))
root_files <- files[dirname(files) == "."]
has_root <- function(...) any(tolower(root_files) %in% tolower(c(...)))

tags <- git("tag", "--list")
commit <- substr(Sys.getenv("GITHUB_SHA") %||% (git("rev-parse", "HEAD")[1] %||% "unknown"), 1, 7)
repo_name <- Sys.getenv("GITHUB_REPOSITORY") %||% (cfg$name %||% basename(getwd()))

# Visibility comes from the Actions event payload; locally from the settings file.
vis <- tolower(Sys.getenv("REPO_PRIVATE"))
is_public <- if (vis %in% c("true", "false")) vis == "false" else (cfg$public %||% NA)

readme_path <- root_files[tolower(root_files) %in% c("readme.md", "readme.qmd", "readme.rmd")][1]
if (is.na(readme_path)) readme_path <- "README.md"
readme <- if (file.exists(readme_path)) readLines(readme_path, warn = FALSE) else character()
# Ignore this script's own block so generated links never count as evidence.
readme_txt <- gsub("(?s)<!-- FAIR:START -->.*?<!-- FAIR:END -->", "",
                   paste(readme, collapse = "\n"), perl = TRUE)

cff <- if (has_root("CITATION.cff")) read_yaml("CITATION.cff") else NULL
codemeta <- if (has_root("codemeta.json")) fromJSON("codemeta.json", simplifyVector = FALSE) else NULL

cff_doi <- cff$doi %||% {
  ids <- Filter(function(x) identical(tolower(x$type %||% ""), "doi"), cff$identifiers %||% list())
  if (length(ids)) ids[[1]]$value else NULL
}
refs <- cff$references %||% list()
linked <- function(r) !is.null(r$doi) || !is.null(r$url) || !is.null(r$`repository-code`)
ref_of <- function(types) Filter(function(r) tolower(r$type %||% "") %in% types && linked(r), refs)
software_refs <- ref_of("software")
data_refs <- ref_of(c("data", "dataset", "database"))

lic_file <- root_files[grepl("^(licen[cs]e|copying)(\\.md|\\.txt)?$", tolower(root_files))][1]
lic_txt <- if (!is.na(lic_file)) paste(head(readLines(lic_file, warn = FALSE), 40), collapse = " ") else ""
lic_id <- if (is.na(lic_file)) NA else if (grepl("Apache License", lic_txt)) "Apache-2.0" else
  if (grepl("MIT License|Permission is hereby granted, free of charge", lic_txt)) "MIT" else
  if (grepl("BSD", lic_txt)) "BSD" else
  if (grepl("GNU GENERAL PUBLIC LICENSE", lic_txt, ignore.case = TRUE)) "GPL" else
  if (grepl("Creative Commons|CC BY|CC-BY", lic_txt)) "CC-BY" else "unknown"
spdx <- cff$license %||% codemeta$license

src_ext <- c("r", "py", "ipynb", "qmd", "rmd", "jl", "sh")
geo_ext <- c("tif", "tiff", "gpkg", "geojson", "nc", "shp", "fgb", "asc", "parquet")
open_ext <- c("csv", "tsv", "json", "txt", "xml")
prop_ext <- c("xlsx", "xls", "rds", "rdata", "rda", "sav", "mat", "mdb")
# Outputs are often git-ignored, so formats can also be declared in settings.
fmt <- unique(c(ext, tolower(unlist(cfg$formats))))

# ---- Rules: one per ARDC question -------------------------------------------
# Each returns the score plus short "has" (earned) / "needs" (lost) evidence.

res <- function(score, has = "", needs = "") list(score = score, has = has, needs = needs)

rules <- list(
  "f:q0" = function() {
    site <- cfg$website %||% (if (!is.null(cff$url) && !grepl("github\\.com", cff$url)) cff$url)
    if (!is.null(site)) res(3, paste("Web page:", site))
    else if (isTRUE(is_public)) res(2, "Hosted on GitHub (basic search indexing)",
                                    "No web page with SEO (Quarto site / WorkflowHub)")
    else if (!is.null(cff) || !is.null(codemeta)) res(1, "Machine-readable metadata file",
                                                      "Not public, so not indexed")
    else res(0, "", "Not public and no machine-readable metadata")
  },
  "f:q1" = function() {
    res(1, if (!is.null(cff_doi)) "DOI is globally unique" else "GitHub URL is globally unique")
  },
  "a:q0" = function() {
    if (isTRUE(is_public)) res(3, "Public repo: anyone can clone it")
    else if (isFALSE(is_public)) res(0, "", "Repo is private: others cannot download it")
    else res(0, "", "Visibility unknown: set public in fair-checklist.yml")
  },
  "a:q1" = function() {
    if (!is.null(cff_doi)) res(2, paste("DOI", cff_doi))
    else res(0, "", "No DOI: a GitHub URL can move or vanish")
  },
  "a:q2" = function() res(1, "Retrievable over HTTPS / git"),
  "a:q3" = function() res(1, if (isTRUE(is_public)) "Openly available" else "GitHub supports authentication"),
  "a:q4" = function() {
    if (!is.null(cff_doi)) res(3, "CITATION.cff includes the DOI")
    else if (!is.null(cff$`repository-code`) || !is.null(codemeta$codeRepository))
      res(2, "Metadata has the repository URL", "No DOI in CITATION.cff")
    else if (!is.null(cff$title) || any(grepl("^#\\s+\\S", readme)))
      res(1, "Metadata names the workflow", "No unique ID in metadata (add CITATION.cff with DOI)")
    else res(0, "", "No workflow name or ID in metadata (no CITATION.cff)")
  },
  "i:q0" = function() {
    g <- intersect(fmt, geo_ext); o <- intersect(fmt, open_ext); p <- intersect(fmt, prop_ext)
    if (length(g)) res(3, paste("Open domain formats:", paste(g, collapse = ", ")))
    else if (length(o)) res(2, paste("Open formats:", paste(o, collapse = ", ")),
                            "No open geospatial formats (GeoTIFF/COG, GeoPackage)")
    else if (length(p)) res(1, "Data files present",
                            paste("Proprietary formats only:", paste(p, collapse = ", ")))
    else res(0, "", "No data formats found (declare them under formats:)")
  },
  "i:q1" = function() {
    semver <- grep("^v?[0-9]+\\.[0-9]+\\.[0-9]+$", tags, value = TRUE)
    if (length(semver)) {
      latest <- semver[which.max(package_version(sub("^v", "", semver)))]
      res(2, paste("Semantic version tags, latest", latest))
    } else if (length(tags) || !is.null(cff$version)) {
      res(1, "Versions are identified", "Versions do not follow vX.Y.Z")
    } else res(0, "", "No git tags or version numbers")
  },
  "i:q2" = function() {
    pinned_py <- has_root("requirements.txt") &&
      any(grepl("==", readLines("requirements.txt", warn = FALSE)))
    if (has_root("renv.lock")) res(3, "renv.lock pins versions and sources")
    else if (pinned_py || has_root("environment.yml", "conda-lock.yml"))
      res(3, "Pinned Python environment file")
    else if (!is.null(codemeta$softwareRequirements))
      res(2, "codemeta.json lists dependencies", "Versions not pinned (add renv.lock)")
    else if (has_root("DESCRIPTION", "requirements.txt"))
      res(1, "Dependency names listed", "Versions not pinned (add renv.lock)")
    else res(0, "", "No dependency file (renv.lock / requirements.txt)")
  },
  "i:q3" = function() {
    if (length(software_refs)) res(2, sprintf("%d software reference(s) in CITATION.cff", length(software_refs)))
    else if (!is.null(codemeta$relatedLink)) res(1, "codemeta.json related links",
                                                  "Links not typed (add CITATION.cff references)")
    else res(0, "", "No links to related software (CITATION.cff references)")
  },
  "i:q4" = function() {
    data_url <- "doi\\.org|zenodo|gbif|ala\\.org\\.au|tern\\.org|data\\.gov|ecocommons|bccvl"
    if (length(data_refs)) res(2, sprintf("%d data reference(s) in CITATION.cff", length(data_refs)))
    else if (grepl(data_url, readme_txt, ignore.case = TRUE))
      res(1, "Data URL in README", "No data references in CITATION.cff (add input DOIs)")
    else res(0, "", "No links to related data")
  },
  "r:q0" = function() {
    ok <- c(
      "README run steps" = grepl("\\b(run|install|usage|getting started)\\b", readme_txt, ignore.case = TRUE),
      "pinned dependencies" = has_root("renv.lock", "requirements.txt", "environment.yml", "DESCRIPTION"),
      "input data" = length(data_refs) > 0 || isTRUE(cfg$data_included) || any(grepl("^data/", files))
    )
    if (all(ok)) res(1, "Run steps, dependencies and data available")
    else res(0, if (any(ok)) paste("Has", paste(names(ok)[ok], collapse = ", ")) else "",
             paste("Missing:", paste(names(ok)[!ok], collapse = ", ")))
  },
  "r:q1" = function() {
    src <- any(ext %in% src_ext)
    doc <- nchar(readme_txt) > 300 || any(grepl("^docs?/", files))
    pkg <- has_root("DESCRIPTION", "pyproject.toml", "setup.py")
    if (src && doc && pkg) res(3, "Source, docs and installable package")
    else if (src && doc) res(2, "Source code + README")
    else if (src) res(1, "Source code available", "README too short to document the workflow")
    else res(0, "", "No source files found")
  },
  "r:q2" = function() {
    if (is.na(lic_id)) res(0, "", "No LICENSE file")
    else if (lic_id == "unknown") res(1, "LICENSE file present", "Licence not widely used / not recognised")
    else if (!is.null(spdx)) res(3, sprintf("%s LICENSE + SPDX id in metadata", lic_id))
    else res(2, sprintf("%s LICENSE file", lic_id), "No SPDX licence id in CITATION.cff / codemeta")
  }
)

# ---- Score table ------------------------------------------------------------

questions <- data.frame(
  id = names(rules),
  label = c("Indexed by search engines", "Globally unique identifier",
            "Identifier gives a copy", "Persistent identifier (DOI)",
            "Open retrieval protocol", "Open or authenticated access",
            "Identifier in metadata",
            "Open, domain-common formats", "Versioning scheme",
            "Dependencies specified", "Links to related software",
            "Links to related data",
            "Executable", "Source + documentation", "Licence"),
  max    = c(3, 1, 3, 2, 1, 1, 3, 3, 2, 3, 2, 2, 1, 3, 3),
  target = c(3, 1, 3, 2, 1, 1, 3, 3, 2, 3, 2, 2, 1, 2, 3)
)
for (id in names(cfg$targets)) questions$target[questions$id == id] <- cfg$targets[[id]]

results <- lapply(questions$id, function(id) {
  r <- rules[[id]]()
  o <- cfg$overrides[[id]]
  if (!is.null(o)) r <- modifyList(r, o)
  r
})
questions$current <- vapply(results, `[[`, numeric(1), "score")
questions$has <- vapply(results, `[[`, "", "has")
questions$needs <- vapply(results, `[[`, "", "needs")
questions$met <- questions$current >= questions$target
# Workflow-specific wording for a gap replaces the generic text.
for (id in names(cfg$hints)) {
  i <- questions$id == id
  if (!questions$met[i]) questions$needs[i] <- cfg$hints[[id]]
}
questions$needs[questions$met & questions$current == questions$max] <- ""

principles <- c(f = "Findable", a = "Accessible", i = "Interoperable", r = "Reusable")
questions$principle <- factor(principles[substr(questions$id, 1, 1)], levels = principles)

agg <- do.call(rbind, lapply(split(questions, questions$principle), function(d) data.frame(
  principle = as.character(d$principle[1]),
  current = sum(d$current), target = sum(d$target), max = sum(d$max))))
agg$pct <- round(100 * agg$current / agg$max)

total <- sum(questions$current); total_max <- sum(questions$max)
n_met <- sum(questions$met); all_met <- n_met == nrow(questions)

ardc_url <- paste0("https://fairsoftwarechecklist.net/v0.2?", paste(
  vapply(names(principles), function(p) paste0(p, "=", paste(
    questions$current[substr(questions$id, 1, 1) == p], collapse = "")), ""),
  collapse = "&"))
colour <- if (all_met) "brightgreen" else if (total / total_max >= 0.66) "yellow" else "orange"
shield <- sprintf("https://img.shields.io/badge/EIW%%20FAIR-%d%%2F%d-%s", total, total_max, colour)
today <- format(Sys.Date(), "%Y-%m-%d")

# ---- Plot -------------------------------------------------------------------

q <- questions
q$y <- factor(paste0(q$label, "  (", q$id, ")"), levels = rev(paste0(q$label, "  (", q$id, ")")))

p <- ggplot(q, aes(y = y)) +
  geom_col(aes(x = max), fill = "#f1efe8", width = 0.7) +
  geom_col(aes(x = current, fill = met), width = 0.7) +
  geom_errorbar(aes(xmin = target, xmax = target), width = 0.9, orientation = "y",
                colour = "#0b0b0b", linewidth = 0.8) +
  geom_text(aes(x = max + 0.08, label = paste0(current, " / ", target)),
            hjust = 0, size = 3, colour = "#52514e") +
  geom_text(aes(x = 3.7, label = has), hjust = 0, size = 2.7, colour = "#0f6e56") +
  geom_text(aes(x = 6.0, label = needs), hjust = 0, size = 2.7, colour = "#a32d2d") +
  scale_fill_manual(values = c(`TRUE` = "#1baf7a", `FALSE` = "#888780"),
                    labels = c(`TRUE` = "At EIW target", `FALSE` = "Below EIW target"),
                    name = NULL) +
  scale_x_continuous(breaks = 0:3, limits = c(0, 8.6), expand = expansion(mult = 0),
                     sec.axis = dup_axis(breaks = c(0, 3.7, 6.0), name = NULL,
                                         labels = c("Score vs target", "Why points were earned",
                                                    "Why points were lost"))) +
  facet_grid(principle ~ ., scales = "free_y", space = "free_y") +
  labs(title = repo_name,
       subtitle = sprintf("%s  |  %d / %d points, %d / 15 questions at EIW target",
                          paste(sprintf("%s %d%%", agg$principle, agg$pct), collapse = " · "),
                          total, total_max, n_met),
       x = "Score (0 = lowest answer)", y = NULL,
       caption = sprintf("Bar = current score | black tick = EIW target | light bar = maximum | ARDC FAIR Software Checklist v0.2 | commit %s, %s",
                         commit, today)) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "top",
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text.y = element_text(angle = 0, face = "bold", hjust = 0),
        axis.text.x.top = element_text(face = "bold", hjust = 0, colour = "#0b0b0b"),
        axis.title.x = element_text(hjust = 0.12),
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(colour = "#52514e", hjust = 0))

ggsave(file.path(out_dir, "fair-scores.png"), p, width = 13, height = 8.4, dpi = 200, bg = "white")

# ---- JSON -------------------------------------------------------------------

write_json(list(
  repository = repo_name, commit = commit, date = today,
  checklist = "ARDC FAIR Software Checklist v0.2", ardc_url = ardc_url,
  total = total, total_max = total_max, questions_met = n_met, all_met = all_met,
  principles = agg,
  questions = questions[, c("id", "label", "current", "target", "max", "met", "has", "needs")]
), file.path(out_dir, "fair-score.json"), auto_unbox = TRUE, pretty = TRUE)

# ---- README block and job summary -------------------------------------------

gaps <- questions[!questions$met, ]
summary_md <- c(
  "## FAIR assessment",
  "",
  sprintf("[![FAIR checklist badge](https://fairsoftwarechecklist.net/badge.svg)](%s) ![EIW FAIR score](%s)",
          ardc_url, shield),
  "",
  sprintf("**%d / %d points · %d / 15 questions at EIW target** · %s",
          total, total_max, n_met,
          if (all_met) "ready for the FAIR badge" else "not yet at EIW minimum"),
  "",
  "| Principle | Score | EIW target |",
  "|---|---|---|",
  sprintf("| %s | %d / %d (%d%%) | %d / %d |", agg$principle, agg$current, agg$max, agg$pct,
          agg$target, agg$max),
  "",
  if (nrow(gaps)) c(
    sprintf("<details><summary>Gaps to close (%d)</summary>", nrow(gaps)),
    "",
    sprintf("- **%s %s** (%d / %d): %s", gaps$id, gaps$label, gaps$current, gaps$target, gaps$needs),
    "",
    "</details>",
    ""
  )
)
readme_block <- c(
  "<!-- FAIR:START -->",
  summary_md,
  "![FAIR scores per question](fair/fair-scores.png)",
  "",
  sprintf("<sub>Updated %s from commit `%s` by `.fair/fair_check.R`. Edit `.fair/fair-checklist.yml`, not this block.</sub>",
          today, commit),
  "<!-- FAIR:END -->"
)

s <- grep("<!-- FAIR:START -->", readme, fixed = TRUE)
e <- grep("<!-- FAIR:END -->", readme, fixed = TRUE)
readme <- if (length(s) && length(e) && e[1] > s[1]) {
  c(readme[seq_len(s[1] - 1)], readme_block, readme[-seq_len(e[1])])
} else {
  c(readme, "", readme_block)
}
writeLines(readme, readme_path)

step_summary <- Sys.getenv("GITHUB_STEP_SUMMARY")
if (nzchar(step_summary)) write(summary_md, step_summary, append = TRUE)

cat(sprintf("FAIR: %d/%d points, %d/15 at target\n", total, total_max, n_met))