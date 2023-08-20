
#' getWeights
#'
#' This function generates a list of weights that can be used in the markerScore function.
#' @param counts Set counts equal to a file path of an document (such as excel file) containing a normalized count matrix with markers on the y axis and organ/tissues as the header row. Before running the function, take a look at the provided packageWeights.xlsx file and note the organs/tissues listed here. Going back to your counts file, create a second row underneath your header row. in this row, for each element in the header row, type in an equivalent tissue from the list of tissues in the packageWeights file. For example, if cell B1 was "heart left ventricle" the closest approximation in the packageWeights file is "heart", so heart would go in cell B2. And so on. Import this modified counts file as the parameter for the function.
#' @param selectTissue choose a tissue from the list of tissues included in the packageWeights.xlsx file. Import as a string.
#' @keywords specificity
#' @export
#'
#'

getWeights <- function(counts, selectTissue) {



  file_path3 <- counts
  data0 <- (readxl::read_excel(file_path3))
  datan <- data.frame(t(data.frame(data0, row.names = TRUE)))
  datan$tissue

  assay <- datan$tissue[1:length(datan$tissue)]
  typeof(assay)




  trans2 <- ((weightfile2[weightfile1$tissue == selectTissue, ]))
  y = array(colnames(weightfile2))

  weights <- c()
  for (item in assay) {
    if (item %in% y) {
      addWeight = as.double(trans2[item])
      weights <- c(weights, addWeight)
    } else {
      weights <- c(weights, -1)
    }


  }
  print(weights)

}
