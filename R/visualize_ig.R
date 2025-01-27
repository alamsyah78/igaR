#' Visualisasi Institutional Grammar
#'
#' Fungsi ini digunakan untuk memvisualisasikan data IG menggunakan pustaka `igraph`.
#'
#' @param ig_data List hasil dari fungsi `extract_ig_components`, yang berisi kategori IG sebagai kunci
#'        dan token-token terkait sebagai elemen.
#' @return Plot jaringan IG.
#'
#' @examples
#' tokens <- c("Gubernur", "harus", "menetapkan", "Rencana", "dalam waktu 6 bulan", "berdasarkan Undang-Undang")
#' ig_data <- extract_ig_components(tokens)
#' visualize_ig(ig_data)
#'
#' @export
visualize_ig <- function(ig_data) {
  # Periksa apakah igraph sudah terinstal
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Paket 'igraph' belum terinstal. Instal dengan `install.packages('igraph')`.")
  }

  # Buat daftar edge dari data IG
  edges <- c()
  for (category in names(ig_data)) {
    tokens <- ig_data[[category]]
    if (length(tokens) > 0) {
      # Tambahkan edge dari kategori ke setiap token
      category_edges <- cbind(rep(category, length(tokens)), tokens)
      edges <- rbind(edges, category_edges)
    }
  }

  # Jika tidak ada edge, beri peringatan
  if (is.null(edges) || nrow(edges) == 0) {
    stop("Tidak ada data yang dapat divisualisasikan.")
  }

  # Buat graph dari edge
  graph <- igraph::graph_from_edgelist(edges, directed = TRUE)

  # Plot graph
  plot(
    graph,
    vertex.size = 15,
    vertex.color = "skyblue",
    vertex.label.color = "black",
    vertex.label.cex = 0.8,
    edge.arrow.size = 0.5,
    main = "Visualisasi Institutional Grammar"
  )
}
