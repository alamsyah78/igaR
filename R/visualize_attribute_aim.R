#' Jejaring antara Attribute dan Aim
#'
#' Fungsi ini membuat jejaring antara komponen `Attribute` dan `Aim` menggunakan pustaka `igraph`.
#'
#' @param ig_data List hasil dari fungsi `extract_ig_components`.
#' @return Grafik jejaring antara Attribute dan Aim.
#'
#' @examples
#' tokens <- c("Gubernur", "Menteri", "menetapkan", "mengelola", "Direktur")
#' ig_data <- extract_ig_components(tokens)
#' visualize_attribute_aim(ig_data)
#'
#' @export
visualize_attribute_aim <- function(ig_data) {
  # Periksa apakah igraph sudah terinstal
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Paket 'igraph' belum terinstal. Instal dengan `install.packages('igraph')`.")
  }

  # Ambil komponen Attribute dan Aim
  attribute_tokens <- ig_data$Attribute
  aim_tokens <- ig_data$Aim

  # Periksa jika tidak ada data di Attribute atau Aim
  if (length(attribute_tokens) == 0) {
    stop("Tidak ada token `Attribute` untuk divisualisasikan.")
  }
  if (length(aim_tokens) == 0) {
    stop("Tidak ada token `Aim` untuk divisualisasikan.")
  }

  # Buat daftar edge: hubungkan setiap Attribute ke setiap Aim
  edges <- expand.grid(Attribute = attribute_tokens, Aim = aim_tokens, stringsAsFactors = FALSE)
  edge_list <- as.matrix(edges)

  # Buat graph
  graph <- igraph::graph_from_edgelist(edge_list, directed = TRUE)

  # Visualisasi graph
  plot(
    graph,
    vertex.size = 15,
    vertex.color = c(rep("lightblue", length(attribute_tokens)), rep("lightgreen", length(aim_tokens))),
    vertex.label.color = "black",
    vertex.label.cex = 0.8,
    edge.arrow.size = 0.5,
    main = "Jejaring antara Attribute dan Aim"
  )
}
