#' Visualisasi Jejaring Attribute dan Aim dengan Filter
#'
#' Fungsi ini membuat jejaring hanya untuk kombinasi `Attribute` dan `Aim` tertentu.
#'
#' @param ig_data List hasil dari fungsi `extract_ig_components`.
#' @param attribute Nama `Attribute` yang ingin divisualisasikan (opsional).
#' @param aim Nama `Aim` yang ingin divisualisasikan (opsional).
#' @return Grafik jejaring untuk kombinasi Attribute dan Aim yang dipilih.
#'
#' @examples
#' tokens <- c("Presiden", "Gubernur", "menetapkan", "melaksanakan", "Rencana")
#' ig_data <- extract_ig_components(tokens)
#' visualize_filtered_attribute_aim(ig_data, attribute = "Presiden", aim = "melaksanakan")
#'
#' @export
visualize_filtered_attribute_aim <- function(ig_data, attribute = NULL, aim = NULL) {
  # Periksa apakah igraph sudah terinstal
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Paket 'igraph' belum terinstal. Instal dengan `install.packages('igraph')`.")
  }

  # Ambil komponen Attribute dan Aim
  attribute_tokens <- ig_data$Attribute
  aim_tokens <- ig_data$Aim

  # Filter Attribute jika diberikan
  if (!is.null(attribute)) {
    if (!attribute %in% attribute_tokens) {
      stop(paste("Attribute yang diminta (", attribute, ") tidak ditemukan dalam data.", sep = ""))
    }
    attribute_tokens <- attribute_tokens[attribute_tokens == attribute]
  }

  # Filter Aim jika diberikan
  if (!is.null(aim)) {
    if (!aim %in% aim_tokens) {
      stop(paste("Aim yang diminta (", aim, ") tidak ditemukan dalam data.", sep = ""))
    }
    aim_tokens <- aim_tokens[aim_tokens == aim]
  }

  # Periksa jika tidak ada Attribute atau Aim setelah filter
  if (length(attribute_tokens) == 0) {
    stop("Tidak ada token `Attribute` yang sesuai dengan filter.")
  }
  if (length(aim_tokens) == 0) {
    stop("Tidak ada token `Aim` yang sesuai dengan filter.")
  }

  # Buat daftar edge: hubungkan Attribute ke Aim
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
    main = paste("Jejaring untuk Attribute dan Aim yang Difilter")
  )
}
