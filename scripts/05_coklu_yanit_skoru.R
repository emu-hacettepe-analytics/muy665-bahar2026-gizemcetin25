# Adım 5 - Çoklu yanıt skoru ve karar tablosu

proje_klasoru <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
tablo_klasoru <- file.path(proje_klasoru, "outputs", "tables")
grafik_klasoru <- file.path(proje_klasoru, "outputs", "figures")

gerekli_paketler <- c("readr", "dplyr", "ggplot2")
eksik_paketler <- gerekli_paketler[!gerekli_paketler %in% rownames(installed.packages())]

if (length(eksik_paketler) > 0) {
  stop(
    "Eksik paket var. RStudio Console'da şunu çalıştırın: install.packages(c(",
    paste(sprintf('\"%s\"', eksik_paketler), collapse = ", "),
    "))"
  )
}

library(readr)
library(dplyr)
library(ggplot2)

renk_koyu <- "#1F3D3A"
renk_yesil <- "#2A9D8F"
renk_mint <- "#8FD6C8"
renk_acik <- "#D8F3EC"

kombinasyon_dosya <- file.path(tablo_klasoru, "03_kombinasyon_ozetleri.csv")

if (!file.exists(kombinasyon_dosya)) {
  stop("Önce 03_eda_ozet_tablolar.R scriptini çalıştırın.")
}

kombinasyon_ozetleri <- read_csv(kombinasyon_dosya, show_col_types = FALSE)

normalize_artir <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

normalize_azalt <- function(x) {
  (max(x) - x) / (max(x) - min(x))
}

karar_tablosu <- kombinasyon_ozetleri |>
  mutate(
    sapma_hedefe_yakinlik = 1 - abs(sapma_ort - 3.60) / max(abs(sapma_ort - 3.60)),
    toparlanma_puani = normalize_azalt(toparlanma_ort),
    skor_puani = normalize_artir(skor_ort),
    enerji_puani = normalize_azalt(enerji_ort),
    kalite_indeksi = 100 * (
      0.30 * sapma_hedefe_yakinlik +
      0.25 * toparlanma_puani +
      0.30 * skor_puani +
      0.15 * enerji_puani
    ),
    karar_sinifi = case_when(
      kalite_indeksi >= 75 ~ "Güçlü aday",
      kalite_indeksi >= 55 ~ "İzlenebilir aday",
      TRUE ~ "Zayıf aday"
    ),
    kombinasyon_kisa = paste0(
      "A:", recode(A, "Dusuk" = "Düşük", "Yuksek" = "Yüksek"),
      " | B:", recode(B, "Kisa" = "Kısa", "Uzun" = "Uzun"),
      " | C:", recode(C, "Dusuk" = "Düşük", "Yuksek" = "Yüksek"),
      " | D:", recode(D, "Dusuk" = "Düşük", "Yuksek" = "Yüksek")
    )
  ) |>
  arrange(desc(kalite_indeksi))

write_csv(karar_tablosu, file.path(tablo_klasoru, "04_coklu_yanit_karar_tablosu.csv"))

g6 <- ggplot(
  karar_tablosu,
  aes(x = reorder(kombinasyon_kisa, kalite_indeksi), y = kalite_indeksi, fill = karar_sinifi)
) +
  geom_col(width = 0.72) +
  scale_fill_manual(values = c(
    "Güçlü aday" = renk_yesil,
    "İzlenebilir aday" = renk_mint,
    "Zayıf aday" = "#5B7C78"
  )) +
  coord_flip() +
  labs(
    title = "Aday kombinasyonların kalite indeksi",
    subtitle = "Sapma, toparlanma, gerçekçilik skoru ve enerji yükü birlikte değerlendirilmiştir.",
    x = "Faktör kombinasyonu",
    y = "Kalite indeksi",
    fill = "Karar sınıfı"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15, color = renk_koyu),
    plot.subtitle = element_text(size = 11, color = "#4D5B5A"),
    axis.title = element_text(face = "bold", color = renk_koyu),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

ggsave(file.path(grafik_klasoru, "grafik_06_coklu_yanit_skoru.png"), g6, width = 10, height = 6, dpi = 300)

cat("Adım 5 tamamlandı.\n")
cat("En iyi üç aday:\n")
print(head(karar_tablosu, 3))
