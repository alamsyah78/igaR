#' Extract Institutional Grammar Components
#'
#' Fungsi ini digunakan untuk mengekstrak komponen **Institutional Grammar (IG)** dari token teks
#' berdasarkan pola regex yang telah didefinisikan.
#'
#' @param tokens Vektor karakter yang berisi token teks yang akan dianalisis.
#'
#' @return Sebuah list dengan 17 elemen, masing-masing mewakili kategori IG 2.0.
#'
#' @examples
#' tokens <- c("Gubernur", "harus", "menetapkan", "Rencana", "dalam waktu 6 bulan", "berdasarkan Undang-Undang")
#' result <- extract_ig_components(tokens)
#' print(result)
#'
#' @export
extract_ig_components <- function(tokens) {
  # Daftar istilah untuk setiap kategori IG
  attribute_terms <- c("gubernur", "bupati/walikota", "menteri", "presiden",
                       "pemerintah daerah", "direktur", "kepala desa",
                       "warga negara", "penduduk", "penyelenggara pelayanan publik",
                       "institusi penyelenggara negara", "korporasi",
                       "lembaga independen", "badan hukum lain",
                       "atasan satuan kerja penyelenggara",
                       "pimpinan satuan kerja",
                       "organisasi penyelenggara pelayanan publik",
                       "pelaksana pelayanan publik",
                       "pejabat", "pegawai", "petugas", "setiap orang",
                       "masyarakat", "kelompok", "badan usaha")
  deontic_terms <- c("harus", "dapat", "tidak boleh", "wajib")
  object_terms <- c("Rencana Perlindungan dan Pengelolaan Ekosistem Gambut", "Peta Ekosistem", "Program Nasional", "Kebijakan Lingkungan")
  activation_condition_terms <- c("terlebih dahulu harus mendapat", "paling lambat \\d+ bulan sejak", "sesuai dengan", "berdasarkan")
  condition_terms <- c("jika", "apabila", "sebelum", "setelah", "ketika", "selama")
  direct_object_terms <- c("dokumen", "laporan", "data", "surat")
  indirect_object_terms <- c("masyarakat", "pihak ketiga", "lembaga")
  execution_constraint_terms <- c("tidak lebih dari", "hanya jika", "tidak boleh melanggar")
  temporal_constraint_terms <- c("dalam waktu \\d+ hari", "setiap bulan", "paling lambat \\d+ bulan")
  spatial_constraint_terms <- c("di kawasan", "di lokasi", "di seluruh wilayah")
  normative_constraint_terms <- c("berdasarkan Undang-Undang", "mengacu pada peraturan", "sesuai dengan hukum")
  sanctions_terms <- c("denda maksimal", "hukuman pidana", "sanksi administratif")
  authority_terms <- c("Menteri", "Pemerintah Daerah", "Lembaga Penegak Hukum")
  monitoring_mechanism_terms <- c("audit tahunan", "laporan bulanan", "pemantauan lapangan")
  evaluation_mechanism_terms <- c("evaluasi triwulanan", "penilaian dampak", "review berkala")
  information_terms <- c("data lingkungan", "laporan publik", "informasi teknis")

  # Definisikan pola regex untuk Aim
  aim_pattern <- paste0(
    "\\b(",
    "men[aiueo][a-z]+",      # Kata kerja diawali 'men'
    "|meng[aiueo][a-z]+",    # Kata kerja diawali 'meng'
    "|mem[aiueo][a-z]+",     # Kata kerja diawali 'mem'
    "|mel[aiueo][a-z]+",     # Kata kerja diawali 'mel'
    ")\\b"
  )

  # Pola Regex untuk masing-masing komponen IG
  patterns <- list(
    Attribute = paste0("\\b(", paste(attribute_terms, collapse = "|"), ")\\b"),
    Deontic = paste0("\\b(", paste(deontic_terms, collapse = "|"), ")\\b"),
    Aim = aim_pattern,  # Gunakan regex untuk Aim
    Object = paste0("\\b(", paste(object_terms, collapse = "|"), ")\\b"),
    ActivationCondition = paste0("\\b(", paste(activation_condition_terms, collapse = "|"), ")\\b"),
    Condition = paste0("\\b(", paste(condition_terms, collapse = "|"), ")\\b"),
    DirectObject = paste0("\\b(", paste(direct_object_terms, collapse = "|"), ")\\b"),
    IndirectObject = paste0("\\b(", paste(indirect_object_terms, collapse = "|"), ")\\b"),
    ExecutionConstraint = paste0("\\b(", paste(execution_constraint_terms, collapse = "|"), ")\\b"),
    TemporalConstraint = paste0("\\b(", paste(temporal_constraint_terms, collapse = "|"), ")\\b"),
    SpatialConstraint = paste0("\\b(", paste(spatial_constraint_terms, collapse = "|"), ")\\b"),
    NormativeConstraint = paste0("\\b(", paste(normative_constraint_terms, collapse = "|"), ")\\b"),
    Sanctions = paste0("\\b(", paste(sanctions_terms, collapse = "|"), ")\\b"),
    Authority = paste0("\\b(", paste(authority_terms, collapse = "|"), ")\\b"),
    MonitoringMechanism = paste0("\\b(", paste(monitoring_mechanism_terms, collapse = "|"), ")\\b"),
    EvaluationMechanism = paste0("\\b(", paste(evaluation_mechanism_terms, collapse = "|"), ")\\b"),
    Information = paste0("\\b(", paste(information_terms, collapse = "|"), ")\\b")
  )

  # Inisialisasi hasil
  ig_components <- lapply(patterns, function(pattern) tokens[grepl(pattern, tokens)])

  # Menetapkan nama untuk elemen dalam list hasil
  names(ig_components) <- names(patterns)

  return(ig_components)
}
