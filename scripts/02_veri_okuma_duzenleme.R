# Adim 2 - Veri okuma, temizleme ve degisken tiplerini duzenleme

proje_klasoru <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
data_klasoru <- file.path(proje_klasoru, "data")

gerekli_paketler <- c("readr", "dplyr", "tidyr", "stringr")
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
library(stringr)

ham_dosya <- file.path(data_klasoru, "02_m4_sentetik_ham_veri.csv")

if (!file.exists(ham_dosya)) {
  stop("Once 01_sentetik_veri_uret.R scriptini calistirin.")
}

veri <- read_csv(ham_dosya, show_col_types = FALSE)

veri_temiz <- veri |>
  mutate(
    A = factor(A, levels = c("Dusuk", "Yuksek")),
    B = factor(B, levels = c("Kisa", "Uzun")),
    C = factor(C, levels = c("Dusuk", "Yuksek")),
    D = factor(D, levels = c("Dusuk", "Yuksek")),
    Rep = factor(Rep),
    kombinasyon = str_c("A=", A, " | B=", B, " | C=", C, " | D=", D)
  )

veri_uzun <- veri_temiz |>
  pivot_longer(
    cols = c(Sapma, Toparlanma, Skor, Enerji),
    names_to = "yanit",
    values_to = "deger"
  )

write_csv(veri_temiz, file.path(data_klasoru, "03_m4_temiz_veri.csv"))
write_csv(veri_uzun, file.path(data_klasoru, "04_m4_uzun_veri.csv"))

cat("Adim 2 tamamlandi.\n")
cat("Gozlem sayisi:", nrow(veri_temiz), "\n")
cat("Degisken sayisi:", ncol(veri_temiz), "\n")
cat("Eksik deger sayisi:", sum(is.na(veri_temiz)), "\n")


