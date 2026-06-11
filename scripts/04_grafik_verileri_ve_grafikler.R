# Adım 4 - Grafik verileri ve ggplot2 grafikleri

proje_klasoru <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
data_klasoru <- file.path(proje_klasoru, "data")
tablo_klasoru <- file.path(proje_klasoru, "outputs", "tables")
grafik_klasoru <- file.path(proje_klasoru, "outputs", "figures")
dir.create(grafik_klasoru, showWarnings = FALSE, recursive = TRUE)

gerekli_paketler <- c("readr", "dplyr", "tidyr", "ggplot2", "stringr", "scales")
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
library(tidyr)
library(ggplot2)
library(stringr)
library(scales)

renk_koyu <- "#1F3D3A"
renk_yesil <- "#2A9D8F"
renk_mint <- "#8FD6C8"
renk_acik <- "#D8F3EC"
renkler_standart <- c(renk_koyu, renk_yesil, renk_mint, "#5B7C78")

temiz_dosya <- file.path(data_klasoru, "03_m4_temiz_veri.csv")
uzun_dosya <- file.path(data_klasoru, "04_m4_uzun_veri.csv")
kombinasyon_dosya <- file.path(tablo_klasoru, "03_kombinasyon_ozetleri.csv")

if (!file.exists(temiz_dosya) || !file.exists(uzun_dosya) || !file.exists(kombinasyon_dosya)) {
  stop("Önce 02_veri_okuma_duzenleme.R ve 03_eda_ozet_tablolar.R scriptlerini çalıştırın.")
}

veri <- read_csv(temiz_dosya, show_col_types = FALSE) |>
  mutate(
    A = recode(A, "Dusuk" = "Düşük", "Yuksek" = "Yüksek"),
    B = recode(B, "Kisa" = "Kısa", "Uzun" = "Uzun"),
    C = recode(C, "Dusuk" = "Düşük", "Yuksek" = "Yüksek"),
    D = recode(D, "Dusuk" = "Düşük", "Yuksek" = "Yüksek")
  )

veri_uzun <- read_csv(uzun_dosya, show_col_types = FALSE) |>
  mutate(
    D = recode(D, "Dusuk" = "Düşük", "Yuksek" = "Yüksek"),
    yanit = recode(
      yanit,
      "Toparlanma" = "Toparlanma Süresi",
      "Sapma" = "Sapma",
      "Skor" = "Gerçekçilik Skoru",
      "Enerji" = "Enerji Yükü"
    )
  )

kombinasyon_ozetleri <- read_csv(kombinasyon_dosya, show_col_types = FALSE) |>
  mutate(
    kombinasyon_kisa = str_c(
      "A:", recode(A, "Dusuk" = "Düşük", "Yuksek" = "Yüksek"),
      " | B:", recode(B, "Kisa" = "Kısa", "Uzun" = "Uzun"),
      " | C:", recode(C, "Dusuk" = "Düşük", "Yuksek" = "Yüksek"),
      " | D:", recode(D, "Dusuk" = "Düşük", "Yuksek" = "Yüksek")
    )
  )

tema_m4 <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15, color = renk_koyu),
    plot.subtitle = element_text(size = 11, color = "#4D5B5A"),
    axis.title = element_text(face = "bold", color = renk_koyu),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", color = renk_koyu)
  )

grafik1_veri <- kombinasyon_ozetleri |>
  arrange(desc(skor_ort)) |>
  mutate(kombinasyon_kisa = reorder(kombinasyon_kisa, skor_ort))

g1 <- ggplot(grafik1_veri, aes(x = kombinasyon_kisa, y = skor_ort, fill = skor_ort)) +
  geom_col(width = 0.72) +
  scale_fill_gradient(low = renk_mint, high = renk_koyu, guide = "none") +
  coord_flip() +
  labs(
    title = "Faktör kombinasyonlarına göre ortalama gerçekçilik skoru",
    subtitle = "Yüksek skor, kullanıcının geri tepme hissini daha gerçekçi algıladığı senaryoları gösterir.",
    x = "Faktör kombinasyonu",
    y = "Ortalama gerçekçilik skoru"
  ) +
  tema_m4

ggsave(file.path(grafik_klasoru, "grafik_01_gercekcilik_skoru.png"), g1, width = 9, height = 6, dpi = 300)

g2 <- ggplot(veri, aes(x = Sapma, y = Toparlanma, color = Skor, shape = A)) +
  geom_point(size = 3.4, alpha = 0.88) +
  scale_color_gradient(low = renk_mint, high = renk_koyu) +
  labs(
    title = "Sapma ve toparlanma süresi ilişkisi",
    subtitle = "İdeal ayar, yeterli sapma üretirken toparlanma süresini kontrol altında tutmalıdır.",
    x = "Sapma",
    y = "Toparlanma süresi",
    color = "Gerçekçilik skoru",
    shape = "Sürüş seviyesi"
  ) +
  tema_m4

ggsave(file.path(grafik_klasoru, "grafik_02_sapma_toparlanma.png"), g2, width = 8, height = 5, dpi = 300)

g3 <- ggplot(veri_uzun |> filter(yanit == "Toparlanma Süresi"), aes(x = D, y = deger, fill = D)) +
  geom_boxplot(alpha = 0.88, width = 0.55, color = renk_koyu) +
  scale_fill_manual(values = c("Düşük" = renk_mint, "Yüksek" = renk_yesil)) +
  labs(
    title = "Sönümleme seviyesine göre toparlanma süresi",
    subtitle = "Kutu grafiği, sönümlemenin toparlanma davranışını nasıl değiştirdiğini gösterir.",
    x = "Sönümleme seviyesi",
    y = "Toparlanma süresi"
  ) +
  tema_m4 +
  theme(legend.position = "none")

ggsave(file.path(grafik_klasoru, "grafik_03_toparlanma_boxplot.png"), g3, width = 7, height = 5, dpi = 300)

g4 <- ggplot(veri, aes(x = Enerji, y = Skor, color = B)) +
  geom_point(size = 3.3, alpha = 0.9) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
  scale_color_manual(values = c("Kısa" = renk_yesil, "Uzun" = renk_koyu)) +
  labs(
    title = "Enerji yükü ve gerçekçilik skoru ilişkisi",
    subtitle = "Daha gerçekçi his veren ayarların enerji yükü açısından da izlenmesi gerekir.",
    x = "Enerji yükü",
    y = "Gerçekçilik skoru",
    color = "Darbe süresi"
  ) +
  tema_m4

ggsave(file.path(grafik_klasoru, "grafik_04_enerji_skor.png"), g4, width = 8, height = 5, dpi = 300)

ana_etki_veri <- read_csv(file.path(tablo_klasoru, "02_faktor_ozetleri.csv"), show_col_types = FALSE) |>
  mutate(
    faktor = recode(
      faktor,
      "A" = "Aktüatör sürüşü",
      "B" = "Darbe süresi",
      "C" = "Hareketli kütle",
      "D" = "Sönümleme"
    ),
    seviye = recode(seviye, "Dusuk" = "Düşük", "Yuksek" = "Yüksek", "Kisa" = "Kısa", "Uzun" = "Uzun")
  )

g5 <- ggplot(ana_etki_veri, aes(x = seviye, y = skor_ort, group = faktor, color = faktor)) +
  geom_point(size = 3.4) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = renkler_standart) +
  facet_wrap(~ faktor, scales = "free_x") +
  labs(
    title = "Faktör seviyelerine göre ortalama gerçekçilik skoru",
    subtitle = "Her faktörün düşük/yüksek ya da kısa/uzun seviyeleri ayrı ayrı karşılaştırılmıştır.",
    x = "Seviye",
    y = "Ortalama skor",
    color = "Faktör"
  ) +
  tema_m4 +
  theme(legend.position = "none")

ggsave(file.path(grafik_klasoru, "grafik_05_faktor_skor.png"), g5, width = 8, height = 5, dpi = 300)

cat("Adım 4 tamamlandı.\n")
cat("Grafikler outputs/figures klasörüne yazıldı.\n")
