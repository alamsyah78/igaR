#' Deteksi Pola Kalimat Berdasarkan IG 2.0 (Fleksibel)
#'
#' Fungsi ini mendeteksi pola kalimat berdasarkan struktur IG 2.0.
#' Kalimat tetap dicatat meskipun tidak semua elemen ditemukan.
#'
#' @param text String teks regulasi yang ingin dianalisis.
#' @param patterns List yang berisi pola regex untuk setiap komponen IG (Attribute, Deontic, Aim, Object).
#' @return Data frame berisi pola yang ditemukan, komponen yang cocok, dan kalimat terkait.
#'
#' @examples
#' text <- "Presiden harus menetapkan Rencana Pembangunan Nasional. Gubernur dapat mengelola program daerah."
#' patterns <- list(
#'   Attribute = "\\b(Presiden|Gubernur|Menteri)\\b",
#'   Deontic = "\\b(harus|dapat|wajib|tidak boleh)\\b",
#'   Aim = "\\b(menetapkan|mengelola|menyusun)\\b",
#'   Object = "\\b(Rencana Pembangunan Nasional|program daerah)\\b"
#' )
#' detect_sentence_patterns(text, patterns)
#'
#' @export
detect_sentence_patterns <- function(text, patterns) {
  # Periksa input
  if (!is.character(text) || length(text) != 1) {
    stop("Teks harus berupa string tunggal.")
  }
  if (!is.list(patterns) || !all(c("Attribute", "Deontic", "Aim", "Object") %in% names(patterns))) {
    stop("Patterns harus berupa list dengan komponen: Attribute, Deontic, Aim, Object.")
  }

  # Pecah teks menjadi kalimat
  sentences <- unlist(strsplit(text, "(?<=[.!?])\\s+", perl = TRUE))

  # Inisialisasi hasil
  results <- data.frame(
    Attribute = character(),
    Deontic = character(),
    Aim = character(),
    Object = character(),
    Sentence = character(),
    stringsAsFactors = FALSE
  )

  # Proses setiap kalimat
  for (sentence in sentences) {
    # Ekstrak komponen berdasarkan regex
    attribute <- unlist(regmatches(sentence, gregexpr(patterns$Attribute, sentence)))
    deontic <- unlist(regmatches(sentence, gregexpr(patterns$Deontic, sentence)))
    aim <- unlist(regmatches(sentence, gregexpr(patterns$Aim, sentence)))
    object <- unlist(regmatches(sentence, gregexpr(patterns$Object, sentence)))

    # Tambahkan hasil, meskipun hanya sebagian elemen ditemukan
    results <- rbind(results, data.frame(
      Attribute = if (length(attribute) > 0) paste(attribute, collapse = ", ") else NA,
      Deontic = if (length(deontic) > 0) paste(deontic, collapse = ", ") else NA,
      Aim = if (length(aim) > 0) paste(aim, collapse = ", ") else NA,
      Object = if (length(object) > 0) paste(object, collapse = ", ") else NA,
      Sentence = sentence,
      stringsAsFactors = FALSE
    ))
  }

  # Kembalikan hasil
  return(results)
}
