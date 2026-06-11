# Adim 3 - Kesifsel veri analizi ve ozet tablolar

proje_klasoru <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
data_klasoru <- file.path(proje_klasoru, "data")
tablo_klasoru <- file.path(proje_klasoru, "outputs", "tables")
dir.create(tablo_klasoru, showWarnings = FALSE, recursive = TRUE)

gerekli_paketler <- c("readr", "dplyr", "tidyr")
eksik_paketler <- gerekli_paketler[!gerekli_paketler %in% rownames(installed.packages())]

if (length(eksik_paketler) > 0) {
  stop(
    "Eksik paket var. RStudio Console'da sunu calistirin: install.packages(c(",
    paste(sprintf('\"%s\"', eksik_paketler), collapse = ", "),
    "))"
  )
}

library(readr)
library(dplyr)
library(tidyr)

temiz_dosya <- file.path(data_klasoru, "03_m4_temiz_veri.csv")

if (!file.exists(temiz_dosya)) {
  stop("Once 02_veri_okuma_duzenleme.R scriptini calistirin.")
}

veri <- read_csv(temiz_dosya, show_col_types = FALSE)

degisken_ozeti <- veri |>
  summarise(
    gozlem_sayisi = n(),
    sapma_ortalama = mean(Sapma),
    sapma_min = min(Sapma),
    sapma_max = max(Sapma),
    toparlanma_ortalama = mean(Toparlanma),
    toparlanma_min = min(Toparlanma),
    toparlanma_max = max(Toparlanma),
    skor_ortalama = mean(Skor),
    skor_min = min(Skor),
    skor_max = max(Skor),
    enerji_ortalama = mean(Enerji),
    enerji_min = min(Enerji),
    enerji_max = max(Enerji)
  )

faktor_ozetleri <- veri |>
  pivot_longer(
    cols = c(A, B, C, D),
    names_to = "faktor",
    values_to = "seviye"
  ) |>
  group_by(faktor, seviye) |>
  summarise(
    sapma_ort = mean(Sapma),
    toparlanma_ort = mean(Toparlanma),
    skor_ort = mean(Skor),
    enerji_ort = mean(Enerji),
    .groups = "drop"
  )

kombinasyon_ozetleri <- veri |>
  group_by(A, B, C, D, kombinasyon) |>
  summarise(
    sapma_ort = mean(Sapma),
    toparlanma_ort = mean(Toparlanma),
    skor_ort = mean(Skor),
    enerji_ort = mean(Enerji),
    sapma_ss = sd(Sapma),
    toparlanma_ss = sd(Toparlanma),
    skor_ss = sd(Skor),
    enerji_ss = sd(Enerji),
    .groups = "drop"
  )

write_csv(degisken_ozeti, file.path(tablo_klasoru, "01_degisken_ozeti.csv"))
write_csv(faktor_ozetleri, file.path(tablo_klasoru, "02_faktor_ozetleri.csv"))
write_csv(kombinasyon_ozetleri, file.path(tablo_klasoru, "03_kombinasyon_ozetleri.csv"))

cat("Adim 3 tamamlandi.\n")
cat("Ozet tablolar outputs/tables klasorune yazildi.\n")


