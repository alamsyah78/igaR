#' Visualisasi Komponen Attribute
#'
#' Fungsi ini digunakan untuk memvisualisasikan token dalam komponen `Attribute` menggunakan pustaka `igraph`.
#'
#' @param ig_data List hasil dari fungsi `extract_ig_components`.
#' @return Plot jaringan `Attribute`.
#'
#' @examples
#' tokens <- c("Gubernur", "harus", "menetapkan", "Rencana", "Menteri", "Presiden")
#' ig_data <- extract_ig_components(tokens)
#' visualize_attribute(ig_data)
#'
#' @export
visualize_attribute <- function(ig_data) {
  # Periksa apakah igraph sudah terinstal
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Paket 'igraph' belum terinstal. Instal dengan `install.packages('igraph')`.")
  }

  # Ambil komponen Attribute
  attribute_tokens <- ig_data$Attribute

  # Periksa jika tidak ada token Attribute
  if (length(attribute_tokens) == 0) {
    stop("Tidak ada token `Attribute` untuk divisualisasikan.")
  }

  # Buat daftar edge dari Attribute (hubungkan kategori Attribute ke token)
  edges <- cbind(rep("Attribute", length(attribute_tokens)), attribute_tokens)

  # Buat graph
  graph <- igraph::graph_from_edgelist(edges, directed = FALSE)

  # Visualisasi graph
  plot(
    graph,
    vertex.size = 15,
    vertex.color = "lightblue",
    vertex.label.color = "black",
    vertex.label.cex = 0.8,
    edge.width = 1,
    main = "Visualisasi Komponen Attribute"
  )
}
