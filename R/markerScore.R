#' markerScore
#'
#' This function gives a probability for how specific a marker is for a organ/tissue.
#' @param counts Set counts equal to a file path of an document (such as excel file) containing a normalized count matrix with markers (ENSEMBL ID works best) on the y axis and organ/tissues as the header row.
#' @param gene Set gene equal to an id for a transcript marker (such as ENSEMBL ID)
#' @param weightsforTissue Set this equal to the output of the getWeights function.
#' @export
#'

markerScore <- function(counts,gene,weightsforTissue) {
  file_path <- counts
  data <- readxl::read_excel(file_path)
  data1 <- data.frame(data, row.names = TRUE)
  trans1 <- (t(data1[data$gene == gene, ]))
  score <- c()
  y <- (trans1/ sum(trans1))
  score <- c(score, sum(y * weightsforTissue))
  return(sum(score))
}

