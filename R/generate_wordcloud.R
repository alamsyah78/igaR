#' Word Cloud dengan Filter Frekuensi dan Stopwords
#'
#' Fungsi ini membuat visualisasi Word Cloud dengan opsi untuk memfilter kata berdasarkan frekuensi dan mengaktifkan stopwords.
#'
#' @param text String teks yang ingin divisualisasikan dalam Word Cloud.
#' @param min_freq Frekuensi minimum kata untuk dimasukkan dalam Word Cloud.
#' @param stopwords Boolean, jika TRUE maka stopwords (kata umum) akan dihapus.
#' @return Grafik Word Cloud menggunakan wordcloud2.
#'
#' @examples
#' text <- "Presiden menetapkan kebijakan nasional. Gubernur mengelola program daerah. Presiden menetapkan rencana."
#' generate_wordcloud(text, min_freq = 2, stopwords = TRUE)
#'
#' @export
generate_wordcloud <- function(text, min_freq = 1, stopwords = TRUE) {
  # Periksa apakah pustaka sudah terinstal
  if (!requireNamespace("tm", quietly = TRUE)) {
    stop("Paket 'tm' belum terinstal. Instal dengan `install.packages('tm')`.")
  }
  if (!requireNamespace("wordcloud2", quietly = TRUE)) {
    stop("Paket 'wordcloud2' belum terinstal. Instal dengan `install.packages('wordcloud2')`.")
  }

  # Daftar stopwords kustom (Bahasa Indonesia)
  stopwords_id <- c("dan", "di", "ke", "yang", "untuk", "dengan",
                    "dari", "atau", "pada", "yaitu", "adalah")

  # Buat Corpus
  corpus <- tm::VCorpus(tm::VectorSource(text))

  # Praproses Teks
  corpus <- tm::tm_map(corpus, tm::content_transformer(tolower))        # Ubah teks menjadi huruf kecil
  corpus <- tm::tm_map(corpus, tm::removePunctuation)                  # Hapus tanda baca
  corpus <- tm::tm_map(corpus, tm::removeNumbers)                      # Hapus angka
  if (stopwords) {
    corpus <- tm::tm_map(corpus, tm::removeWords, stopwords_id)        # Hapus stopwords (kustom)
  }

  # Buat Dokumen-Term Matriks
  dtm <- tm::TermDocumentMatrix(corpus)
  term_matrix <- as.matrix(dtm)
  term_freq <- sort(rowSums(term_matrix), decreasing = TRUE)

  # Buat Data Frame Frekuensi
  term_data <- data.frame(
    word = names(term_freq),
    freq = term_freq,
    stringsAsFactors = FALSE
  )

  # Filter Berdasarkan Frekuensi Minimum
  term_data <- term_data[term_data$freq >= min_freq, ]

  # Jika tidak ada kata setelah filter, hentikan
  if (nrow(term_data) == 0) {
    stop("Tidak ada kata yang memenuhi frekuensi minimum.")
  }

  # Buat Word Cloud
  wordcloud2::wordcloud2(term_data, size = 1)
}
