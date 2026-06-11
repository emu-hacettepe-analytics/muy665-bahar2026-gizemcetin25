# Adim 1 - Sentetik deney verisini uret

proje_klasoru <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

data_klasoru <- file.path(proje_klasoru, "data")
dir.create(data_klasoru, showWarnings = FALSE, recursive = TRUE)

# Bu veri gercek test olcumu degildir.
# Amac, ders kapsamindaki R veri analitigi akisini uygulamak icin
# yeniden uretilebilir bir sentetik deney verisi olusturmaktir.
set.seed(665)

tasarim <- expand.grid(
  A = c("Dusuk", "Yuksek"),
  B = c("Kisa", "Uzun"),
  C = c("Dusuk", "Yuksek"),
  D = c("Dusuk", "Yuksek"),
  stringsAsFactors = FALSE
)

tasarim$Std <- seq_len(nrow(tasarim))

tasarim_iki_tekrar <- tasarim[rep(seq_len(nrow(tasarim)), each = 2), ]
tasarim_iki_tekrar$Rep <- rep(1:2, times = nrow(tasarim))
tasarim_iki_tekrar$Run <- seq_len(nrow(tasarim_iki_tekrar))

# Faktor seviyelerini sayisal etki kodlarina ceviriyoruz.
a <- ifelse(tasarim_iki_tekrar$A == "Yuksek", 1, 0)
b <- ifelse(tasarim_iki_tekrar$B == "Uzun", 1, 0)
c <- ifelse(tasarim_iki_tekrar$C == "Yuksek", 1, 0)
d <- ifelse(tasarim_iki_tekrar$D == "Yuksek", 1, 0)

# Sentetik yanit degiskenleri:
# Sapma: geri tepme hissi icin yeterli ama asiri olmayan sapma beklenir.
# Toparlanma: dusuk olmasi tercih edilir.
# Skor: yuksek olmasi tercih edilir.
# Enerji: cok yuksek olmamasi tercih edilir.
tasarim_iki_tekrar$Sapma <- 1.55 + 1.75 * a + 0.55 * b + 0.45 * c -
  0.35 * d + 0.35 * a * c + 0.25 * a * b - 0.20 * c * d +
  rnorm(nrow(tasarim_iki_tekrar), mean = 0, sd = 0.06)

tasarim_iki_tekrar$Toparlanma <- 112 + 30 * a + 42 * b + 38 * c -
  24 * d + 24 * b * c + 14 * a * b - 10 * a * d +
  rnorm(nrow(tasarim_iki_tekrar), mean = 0, sd = 4)

tasarim_iki_tekrar$Skor <- 7.25 + 1.00 * a - 0.35 * b + 0.45 * c +
  0.35 * d + 0.35 * a * (1 - b) - 0.55 * b * c +
  rnorm(nrow(tasarim_iki_tekrar), mean = 0, sd = 0.10)

tasarim_iki_tekrar$Enerji <- 5.70 + 3.05 * a + 2.25 * b + 2.05 * c +
  0.85 * d + 0.55 * a * c +
  rnorm(nrow(tasarim_iki_tekrar), mean = 0, sd = 0.10)

tasarim_iki_tekrar$Sapma <- round(tasarim_iki_tekrar$Sapma, 2)
tasarim_iki_tekrar$Toparlanma <- round(tasarim_iki_tekrar$Toparlanma, 0)
tasarim_iki_tekrar$Skor <- round(pmin(pmax(tasarim_iki_tekrar$Skor, 1), 10), 2)
tasarim_iki_tekrar$Enerji <- round(tasarim_iki_tekrar$Enerji, 2)

veri_ham <- tasarim_iki_tekrar[, c(
  "Run", "Std", "Rep", "A", "B", "C", "D",
  "Sapma", "Toparlanma", "Skor", "Enerji"
)]

write.csv(
  tasarim,
  file.path(data_klasoru, "01_m4_faktor_tasarimi.csv"),
  row.names = FALSE
)

write.csv(
  veri_ham,
  file.path(data_klasoru, "02_m4_sentetik_ham_veri.csv"),
  row.names = FALSE
)

cat("Adim 1 tamamlandi.\n")
cat("Olusan dosyalar:\n")
print(list.files(data_klasoru, pattern = "^0[12]_"))


