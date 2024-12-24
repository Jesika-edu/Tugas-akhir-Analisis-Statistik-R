# ID Wisatawan
ID_Wisatawan <- c(
  "wisatawan_1", "wisatawan_2", "wisatawan_3", "wisatawan_4", "wisatawan_5",
  "wisatawan_6", "wisatawan_7", "wisatawan_8", "wisatawan_9", "wisatawan_10",
  "wisatawan_11", "wisatawan_12", "wisatawan_13", "wisatawan_14", "wisatawan_15",
  "wisatawan_16", "wisatawan_17", "wisatawan_18", "wisatawan_19", "wisatawan_20",
  "wisatawan_21", "wisatawan_22", "wisatawan_23", "wisatawan_24", "wisatawan_25",
  "wisatawan_26", "wisatawan_27", "wisatawan_28", "wisatawan_29", "wisatawan_30",
  "wisatawan_31", "wisatawan_32", "wisatawan_33", "wisatawan_34", "wisatawan_35"
)

# Jenis Wisatawan
Jenis_Wisatawan <- c(
  "Internasional", "Internasional", "Internasional", "Internasional", "Internasional",
  "Internasional", "Internasional", "Internasional", "Internasional", "Internasional",
  "Internasional", "Internasional", "Nasional", "Nasional", "Nasional",
  "Nasional", "Nasional", "Nasional", "Nasional", "Nasional",
  "Nasional", "Nasional", "Nasional", "Nasional", "Lokal",
  "Lokal", "Lokal", "Lokal", "Lokal", "Lokal",
  "Lokal", "Lokal", "Lokal", "Lokal", "Lokal"
)

Pengeluaran <- c(
  4758485.74, 4545692.82, 4389623.70, 5575744.04, 5704722.14,
  4823504.50, 5847762.19, 4281561.64, 5251312.89, 4455194.08,
  4748221.94, 5248813.67, 4103469.85, 3539624.79, 4029038.60,
  3271901.58, 3240770.60, 3375992.67, 3770042.01, 3422294.27,
  3549069.82, 3999698.53, 4534314.35, 3382869.53, 2621374.12,
  2863235.59, 2559969.03, 2286912.36, 2251335.84, 2637819.15,
  2374469.27, 2394577.39, 2106416.74, 2242145.69, 2717390.14
)

# Membuat data frame
data_wisatawan <- data.frame(ID_Wisatawan, Jenis_Wisatawan, Pengeluaran)

# Menyimpan data frame ke file RDS
saveRDS(data_wisatawan, file = "data_wisatawan_bali.rds")


