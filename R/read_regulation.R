#' Membaca File Teks Peraturan
#'
#' Fungsi ini digunakan untuk membaca file teks (.txt) yang berisi peraturan, membersihkannya dari baris kosong,
#' spasi berlebih, serta karakter khusus, dan mengubah teks menjadi huruf kecil.
#'
#' @param file_path String yang menunjukkan lokasi file teks (.txt) yang akan dibaca.
#'
#' @return Vektor karakter dari teks yang telah dibersihkan, di mana setiap elemen merepresentasikan satu baris.
#'
#' @details
#' Fungsi ini memastikan bahwa file yang diberikan:
#' - Berada pada lokasi yang benar.
#' - Memiliki ekstensi `.txt`.
#'
#' Proses pembersihan mencakup:
#' - Menghapus baris kosong.
#' - Menghilangkan spasi berlebih di awal dan akhir baris.
#' - Mengubah semua teks menjadi huruf kecil.
#' - Menghapus karakter khusus, tetapi tetap mempertahankan huruf, angka, spasi, dan tanda baca.
#'
#' @examples
#' # Contoh penggunaan:
#' # Misalkan file "regulation.txt" berada di direktori kerja
#' file_path <- "regulation.txt"
#' text <- read_regulation(file_path)
#' print(text)
#'
#' @export
read_regulation <- function(file_path) {
  # Memastikan file ada di lokasi yang ditentukan
  if (!file.exists(file_path)) {
    stop("File tidak ditemukan: ", file_path)
  }

  # Memastikan file memiliki ekstensi .txt
  if (!grepl("\\.txt$", file_path, ignore.case = TRUE)) {
    stop("Hanya file .txt yang didukung oleh fungsi ini.")
  }

  # Membaca isi file teks
  text <- readLines(file_path, warn = FALSE)

  # Pembersihan data
  # 1. Menghapus baris kosong
  text <- text[text != ""]

  # 2. Menghapus spasi berlebih di awal dan akhir baris
  text <- trimws(text)

  # 3. Mengubah semua teks menjadi huruf kecil
  text <- tolower(text)

  # 4. Menghapus karakter khusus atau simbol
  # Hanya huruf, angka, spasi, dan tanda baca yang dipertahankan
  text <- gsub("[^a-z0-9[:space:][:punct:]]", "", text)

  return(text)
}

