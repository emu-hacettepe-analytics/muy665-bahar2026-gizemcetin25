# Gerekli R paketlerini kontrol eder ve eksik olanlari kurar.

gerekli_paketler <- c(
  "readr",
  "dplyr",
  "tidyr",
  "stringr",
  "ggplot2",
  "knitr"
)

eksik_paketler <- gerekli_paketler[!gerekli_paketler %in% rownames(installed.packages())]

if (length(eksik_paketler) > 0) {
  install.packages(eksik_paketler, repos = "https://cloud.r-project.org")
} else {
  message("Gerekli paketlerin tamami kurulu.")
}

