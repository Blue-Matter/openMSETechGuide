html_files <- list.files("docs", pattern = "\\.html$", recursive = TRUE, full.names = TRUE)

for (f in html_files) {
  txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
  txt <- gsub("(<span>)(\\d+)&nbsp;\\s*[^<]+(</span>)", "\\1Chapter \\2\\3", txt, perl = TRUE)
  writeLines(txt, f)
}