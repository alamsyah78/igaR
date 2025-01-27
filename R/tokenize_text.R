#' Tokenisasi Teks
#'
#' Fungsi ini digunakan untuk melakukan tokenisasi teks berbahasa Indonesia. Proses tokenisasi mencakup
#' normalisasi huruf kecil, penghapusan karakter tidak relevan, dan pemisahan teks menjadi token berdasarkan spasi.
#'
#' @param text String yang berisi teks yang akan ditokenisasi.
#' @return Vektor karakter yang berisi token yang dihasilkan dari teks input.
#'
#' @examples
#' text <- "Gubernur menetapkan kebijakan sesuai dengan aturan."
#' tokens <- tokenize_text(text)
#' print(tokens)
#'
#' @export
tokenize_text <- function(text) {
  # Ubah semua teks menjadi huruf kecil
  text <- tolower(text)

  # Hapus karakter yang tidak relevan seperti angka dan simbol khusus
  text <- gsub("[^a-z\\ ]", "", text)

  # Pecah teks menjadi token berdasarkan spasi
  tokens <- unlist(strsplit(text, "\\s+"))

  # Hapus token kosong (jika ada)
  tokens <- tokens[tokens != ""]

  return(tokens)
}

