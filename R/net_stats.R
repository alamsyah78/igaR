#' Statistik Jejaring untuk Komponen IG
#'
#' Fungsi ini menghitung statistik jejaring untuk kategori IG yang dipilih (misalnya, `Attribute`, `Deontic`, dll.).
#'
#' @param ig_data List hasil dari fungsi `extract_ig_components`.
#' @param component Nama kategori IG yang ingin dihitung statistik jejaringnya.
#' @return Statistik jejaring dalam bentuk list.
#'
#' @examples
#' tokens <- c("Gubernur", "Menteri", "Presiden", "Bupati", "Direktur", "harus", "menetapkan")
#' ig_data <- extract_ig_components(tokens)
#' stats <- net_stats(ig_data, component = "Attribute")
#' print(stats)
#'
#' @export
net_stats <- function(ig_data, component) {
  # Periksa apakah igraph sudah terinstal
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Paket 'igraph' belum terinstal. Instal dengan `install.packages('igraph')`.")
  }

  # Validasi komponen
  if (!component %in% names(ig_data)) {
    stop(paste("Komponen", component, "tidak ditemukan dalam ig_data."))
  }

  # Ambil token berdasarkan komponen yang dipilih
  tokens <- ig_data[[component]]

  # Periksa jika tidak ada token
  if (length(tokens) == 0) {
    stop(paste("Tidak ada token untuk komponen", component, "yang dapat dihitung statistiknya."))
  }

  # Buat daftar edge
  edges <- cbind(rep(component, length(tokens)), tokens)

  # Buat graph
  graph <- igraph::graph_from_edgelist(edges, directed = FALSE)

  # Hitung statistik jejaring
  stats <- list(
    component = component,                                           # Nama komponen
    num_nodes = igraph::vcount(graph),                              # Jumlah simpul
    num_edges = igraph::ecount(graph),                              # Jumlah tepi
    density = igraph::edge_density(graph),                          # Kepadatan jejaring
    degree = igraph::degree(graph),                                 # Derajat simpul
    diameter = igraph::diameter(graph, directed = FALSE),           # Diameter jejaring
    closeness = igraph::closeness(graph, normalized = TRUE),        # Closeness centrality
    betweenness = igraph::betweenness(graph, normalized = TRUE),    # Betweenness centrality
    connected_components = igraph::components(graph)$csize          # Ukuran komponen terhubung
  )

  return(stats)
}
