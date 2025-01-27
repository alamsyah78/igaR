#' Visualisasi Aim Tertentu
#'
#' Fungsi ini membuat jejaring hanya untuk token yang memiliki `Aim` tertentu.
#'
#' @param ig_data List hasil dari fungsi `extract_ig_components`.
#' @param aim Nama `Aim` yang ingin divisualisasikan (contoh: "menetapkan").
#' @return Grafik jejaring untuk `Aim` yang dipilih.
#'
#' @examples
#' tokens <- c("Gubernur", "Menteri", "menetapkan", "mengelola", "Rencana")
#' ig_data <- extract_ig_components(tokens)
#' visualize_specific_aim(ig_data, aim = "menetapkan")
#'
#' @export
visualize_specific_aim <- function(ig_data, aim) {
  # Periksa apakah igraph sudah terinstal
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Paket 'igraph' belum terinstal. Instal dengan `install.packages('igraph')`.")
  }

  # Ambil komponen Attribute dan Aim
  attribute_tokens <- ig_data$Attribute
  aim_tokens <- ig_data$Aim

  # Periksa jika tidak ada Aim dalam data
  if (length(aim_tokens) == 0) {
    stop("Tidak ada token `Aim` untuk divisualisasikan.")
  }

  # Periksa apakah Aim yang diminta ada dalam data
  if (!aim %in% aim_tokens) {
    stop(paste("Aim yang diminta (", aim, ") tidak ditemukan dalam data.", sep = ""))
  }

  # Buat daftar edge: hubungkan setiap Attribute ke Aim tertentu
  edges <- cbind(attribute_tokens, rep(aim, length(attribute_tokens)))
  edge_list <- as.matrix(edges)

  # Buat graph
  graph <- igraph::graph_from_edgelist(edge_list, directed = TRUE)

  # Visualisasi graph
  plot(
    graph,
    vertex.size = 15,
    vertex.color = c(rep("lightblue", length(attribute_tokens)), "lightgreen"),
    vertex.label.color = "black",
    vertex.label.cex = 0.8,
    edge.arrow.size = 0.5,
    main = paste("Jejaring untuk Aim:", aim)
  )
}
