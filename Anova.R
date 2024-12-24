#NAMA: NI KADEK DWI JESIKA SARI
#NIM: 2415091005
#PRODI/KELAS: S1 SISTEM INFORMASI / 1DPS

# Library yang dibutuhkan
library(car)
library(lmtest)
library(ggplot2)
library(patchwork)

# 1. Membaca Data
data_wisatawan <- readRDS("data_wisatawan_bali.rds")
data_wisatawan$Jenis_Wisatawan <- as.factor(data_wisatawan$Jenis_Wisatawan)

# Membuat model linear
model_lm <- lm(Pengeluaran ~ Jenis_Wisatawan, data = data_wisatawan)
summary(model_lm)

# 2. Uji Asumsi
## a. Uji Normalitas Residual
residuals <- residuals(model_lm)
shapiro_test <- shapiro.test(residuals)
p_value_shapiro <- shapiro_test$p.value
print(shapiro_test)

## b. Uji Homogenitas Varians
levene_test <- leveneTest(Pengeluaran ~ Jenis_Wisatawan, data = data_wisatawan)
p_value_levene <- levene_test$`Pr(>F)`[1]
print(levene_test)

## c. Uji Independensi Residual
dw_test <- dwtest(model_lm)
dw_statistic <- dw_test$statistic
p_value_dw <- dw_test$p.value
print(dw_test)

# 3. Analisis
## ANOVA
model_aov <- aov(Pengeluaran ~ Jenis_Wisatawan, data = data_wisatawan) 
anova_result <- summary(model_aov)
p_value_anova <- summary(model_aov)[[1]][["Pr(>F)"]][1] 
print(anova_result)

## Tukey HSD (Jika hasil ANOVA signifikan)
if (p_value_anova < 0.05) {
  tukey_result <- TukeyHSD(model_aov)
  print(tukey_result)
  tukey_result_df <- as.data.frame(tukey_result$Jenis_Wisatawan)
  tukey_result_df$comparison <- rownames(tukey_result_df)
} else {
  cat("\nHasil ANOVA tidak signifikan, uji Tukey HSD tidak diperlukan.\n")
}

# 4. Visualisasi
## a. Boxplot
boxplot_plot <- ggplot(data_wisatawan, aes(x = Jenis_Wisatawan, y = Pengeluaran, fill = Jenis_Wisatawan)) +
  geom_boxplot() +
  labs(title = "Pengeluaran Berdasarkan Jenis Wisatawan", x = "Jenis Wisatawan", y = "Pengeluaran (IDR)") +
  theme_minimal()
print(boxplot_plot) 

# b. Tukey Plot
tukey_plot <- ggplot(tukey_result_df, aes(x = reorder(comparison, diff), y = diff)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black", alpha = 0.7) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2, color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(
    title = "Visualisasi Tukey HSD",
    x = "Comparison",
    y = "Mean Difference"
  ) +
  theme_minimal() +
  coord_flip()

print(tukey_plot)

## c. Residuals vs Fitted
rvf_plot <- ggplot(data.frame(fitted = fitted(model_lm), residuals = residuals), aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals") +
  theme_minimal()
print(rvf_plot)

## d. QQ Plot
qq_plot <- ggplot(data.frame(sample = residuals), aes(sample = sample)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "QQ Plot of Residuals") +
  theme_minimal()
print(qq_plot) 

## e. Histogram Residual
hist_plot <-ggplot(data.frame(residuals = residuals), aes(x = residuals)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Residuals", x = "Residuals", y = "Frequency") +
  theme_minimal()
print(hist_plot) 

#gabungan semua plot
print(boxplot_plot + rvf_plot + qq_plot / hist_plot + tukey_plot)

cat("INTERPRETASI ANALISIS PENGELUARAN BERDASARKAN JENIS WISATAWAN\n")

# Interpretasi Model Linear (Ringkasan)
p_value_f <- pf(summary_model$fstatistic[1], summary_model$fstatistic[2], summary_model$fstatistic[3], lower.tail = FALSE)
cat("--- Interpretasi Model Regresi (Uji F) ---\n")
cat(sprintf("Nilai p untuk uji F: %e\n", p_value_f))
if (p_value_f < 0.05) {
  cat("Kesimpulan: Model regresi signifikan secara statistik. Terdapat bukti yang cukup untuk menyimpulkan bahwa setidaknya satu jenis wisatawan memengaruhi pengeluaran secara signifikan.\n\n")
} else {
  cat("Kesimpulan: Model regresi tidak signifikan secara statistik. Tidak ada cukup bukti untuk menyimpulkan bahwa jenis wisatawan secara keseluruhan memengaruhi pengeluaran secara signifikan.\n\n")
}
cat(sprintf("R-squared: %.4f\n\n", summary_model$r.squared))


# Interpretasi Uji Asumsi
cat("--- Interpretasi Uji Asumsi ---\n")
# a. Normalitas Residual
p_value_shapiro <- shapiro.test(residuals)$p.value
cat("Uji Shapiro-Wilk untuk Normalitas Residual:\n")
cat(sprintf("  Nilai p: %e\n", p_value_shapiro))
if (p_value_shapiro > 0.05) {
  cat("  Kesimpulan: Residual berdistribusi normal.\n")
} else {
  cat("  Kesimpulan: Residual tidak berdistribusi normal.\n")
}

# b. Homogenitas Varians
p_value_levene <- leveneTest(Pengeluaran ~ Jenis_Wisatawan, data = data_wisatawan)$`Pr(>F)`[1]
cat("\nUji Levene untuk Homogenitas Varians:\n")
cat(sprintf("  Nilai p: %e\n", p_value_levene))
if (p_value_levene > 0.05) {
  cat("  Kesimpulan: Varians antar kelompok homogen.\n")
} else {
  cat("  Kesimpulan: Varians antar kelompok tidak homogen.\n")
}

# c. Independensi Residual
p_value_dw <- dwtest(model_lm)$p.value
dw_statistic <- dwtest(model_lm)$statistic
cat("\nUji Durbin-Watson untuk Independensi Residual:\n")
cat(sprintf("  Nilai p: %e, Statistik Durbin-Watson: %.2f\n", p_value_dw, dw_statistic))
if (p_value_dw > 0.05) {
  cat("  Kesimpulan: Tidak ada autokorelasi pada residual (residual independen).\n")
} else {
  cat("  Kesimpulan: Terdapat autokorelasi pada residual (residual tidak independen).\n")
}

cat("\n")

# Interpretasi ANOVA dan Tukey HSD
cat("--- Interpretasi ANOVA dan Tukey HSD ---\n")
p_value_anova <- summary(model_aov)[[1]][["Pr(>F)"]][1]
cat("Hasil ANOVA:\n")
cat(sprintf("  Nilai p: %e\n", p_value_anova))
if (p_value_anova < 0.05) {
  cat("  Kesimpulan: Terdapat perbedaan rata-rata pengeluaran yang signifikan antara setidaknya dua jenis wisatawan.\n")
  tukey_result <- TukeyHSD(model_aov)
  print(tukey_result)
  tukey_result_df <- as.data.frame(tukey_result$Jenis_Wisatawan)
  tukey_result_df$comparison <- rownames(tukey_result_df)
  
  cat("\nHasil Uji Tukey HSD (Perbandingan Berpasangan):\n")
  for (i in 1:nrow(tukey_result_df)){
    cat(sprintf("Perbandingan %s: \n",tukey_result_df$comparison[i]))
    cat(sprintf("   Selisih Rata-rata: %.2f\n", tukey_result_df$diff[i]))
    cat(sprintf("   p adj: %e\n", tukey_result_df$`p adj`[i]))
    if (tukey_result_df$`p adj`[i] < 0.05) {
      cat("   Kesimpulan: Perbedaan signifikan\n\n")
    } else {
      cat("   Kesimpulan: Tidak ada perbedaan signifikan\n\n")
    }
  }
} else {
  cat("  Kesimpulan: Tidak ada perbedaan rata-rata pengeluaran yang signifikan antar jenis wisatawan.\n")
}
cat("\n")

# Interpretasi Visualisasi
# Interpretasi Boxplot
cat("Boxplot digunakan untuk menampilkan distribusi data berdasarkan lima angka ringkasan (minimum, kuartil pertama, median, kuartil ketiga, dan maksimum).")
cat("\nFungsinya adalah untuk:\n")
cat("- Mengidentifikasi outlier dalam data.\n")
cat("- Menganalisis pola distribusi data secara visual.\n")
cat("- Membandingkan distribusi antar kelompok data.\n\n")

# Interpretasi Tukey Plot (hasil post-hoc Tukey)
cat("Tukey Plot digunakan untuk menyajikan hasil analisis post-hoc Tukey yang membandingkan rata-rata antar kelompok.")
cat("\nFungsinya adalah untuk:\n")
cat("- Menentukan apakah terdapat perbedaan signifikan antara rata-rata kelompok.\n")
cat("- Menyediakan interval kepercayaan untuk perbedaan rata-rata tersebut.\n\n")

# Interpretasi Residual vs Fitted Plot (RVF Plot)
cat("Residual vs Fitted (RVF) Plot digunakan untuk mengevaluasi asumsi dalam model regresi, khususnya terkait:\n")
cat("- Homoskedastisitas (kesamaan varian residual).\n")
cat("- Linearitas hubungan antara variabel bebas dan terikat.\n")
cat("\nFungsinya adalah untuk memvalidasi bahwa model regresi yang digunakan sesuai dengan asumsi statistik.\n\n")

# Interpretasi Q-Q Plot
cat("Q-Q Plot digunakan untuk memeriksa apakah residual dari model regresi mengikuti distribusi normal.")
cat("\nFungsinya adalah untuk:\n")
cat("- Menilai normalitas residual secara visual.\n")
cat("- Membantu memutuskan apakah asumsi normalitas dalam model regresi terpenuhi.\n\n")

# Interpretasi Histogram
cat("Histogram digunakan untuk menampilkan distribusi frekuensi dari suatu variabel.")
cat("\nFungsinya adalah untuk:\n")
cat("- Memahami pola distribusi data, seperti distribusi normal, skewed (miring), atau multimodal.\n")
cat("- Memberikan gambaran visual mengenai penyebaran nilai dalam dataset.\n")

