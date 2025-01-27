#' Ekstraksi Konteks Komponen
#'
#' Fungsi ini mengekstrak kalimat atau konteks di mana komponen tertentu ditemukan dalam teks regulasi.
#'
#' @param text String teks regulasi.
#' @param component Nama komponen yang akan dicari (misalnya, "Attribute", "Aim").
#' @param pattern Regex pola untuk mencocokkan komponen dalam teks.
#' @return Data frame berisi kalimat dan komponen yang ditemukan.
#'
#' @examples
#' text <- "Presiden harus menetapkan Rencana Pembangunan Nasional. Gubernur dapat mengelola program daerah. Menteri bertanggung jawab untuk menyusun kebijakan."
#' pattern <- "\\b(Presiden|Gubernur|Menteri)\\b" # Regex untuk Attribute
#' extract_context(text, component = "Attribute", pattern = pattern)
#'
#' @export
extract_context <- function(text, component, pattern) {
  # Periksa input
  if (!is.character(text) || length(text) != 1) {
    stop("Teks harus berupa string tunggal.")
  }
  if (!is.character(component) || length(component) != 1) {
    stop("Komponen harus berupa string tunggal.")
  }
  if (!is.character(pattern) || length(pattern) != 1) {
    stop("Pola regex harus berupa string tunggal.")
  }

  # Pecah teks menjadi kalimat
  sentences <- unlist(strsplit(text, "(?<=[.!?])\\s+", perl = TRUE))

  # Cari kalimat yang mengandung pola
  matching_sentences <- sentences[grepl(pattern, sentences)]

  # Jika tidak ada kalimat yang cocok, kembalikan pesan kosong
  if (length(matching_sentences) == 0) {
    return(data.frame(
      component = character(0),
      sentence = character(0),
      stringsAsFactors = FALSE
    ))
  }

  # Ekstrak komponen yang cocok
  matched_components <- regmatches(matching_sentences, gregexpr(pattern, matching_sentences))
  matched_components <- unlist(matched_components)

  # Buat data frame hasil
  result <- data.frame(
    component = matched_components,
    sentence = rep(matching_sentences, lengths(gregexpr(pattern, matching_sentences))),
    stringsAsFactors = FALSE
  )

  return(result)
}
