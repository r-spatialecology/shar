#' create_neighbourhood
#'
#' @description Create neighbourhood
#'
#' @param cells matrix with cell ids of focal cells.
#' @param matrix matrix in which cells are located.
#' @param directions Cells neighbour rule: 4 (rook's case), 8 (queen's case).
#'
#' @details
#' Get cell ids of all neighbouring cells. The neighbourhoood rule can be specified
#' and is either rook's case (4 neighbours) or queen's case (8 neighbours).
#'
#' @seealso
#' \code{\link{randomize_raster}}
#'
#' @return matrix
#'
#' @examples
#' mat <- matrix(1, nrow= 10, ncol = 10)
#' cell_id <- rbind(cbind(3,5), cbind(7,1))
#' create_neighbourhood(cell_id, mat)
#'
#' @aliases create_neighbourhood
#' @rdname create_neighbourhood

#' @export
create_neighbourhood <- function(cells, matrix, directions = 4) {

  if (directions == 4) {

    neighbours <- unique(rbind(cbind(cells[, 1] - 1, cells[, 2]),
                               cbind(cells[, 1] + 1, cells[, 2]),
                               cbind(cells[, 1], cells[, 2] - 1),
                               cbind(cells[, 1], cells[, 2] + 1)))
  }

  else if (directions == 8) {

    neighbours <- unique(rbind(cbind(cells[, 1] - 1, cells[, 2]),
                               cbind(cells[, 1] + 1, cells[, 2]),
                               cbind(cells[, 1], cells[, 2] - 1),
                               cbind(cells[, 1], cells[, 2] + 1),
                               cbind(cells[, 1] + 1, cells[, 2] + 1),
                               cbind(cells[, 1] - 1, cells[, 2] - 1),
                               cbind(cells[, 1] + 1, cells[, 2] - 1),
                               cbind(cells[, 1] - 1, cells[, 2] + 1)))
  }

  else {
    stop("'directions must be 'directions = 4' or 'directions = 8'.")
  }

  # remove all cases outside lower boundaries
  neighbours[neighbours == 0] <- NA

  # remove all cases outside higher boundaries
  neighbours[, 1][neighbours[, 1] > nrow(matrix)] <- NA

  neighbours[, 2][neighbours[, 2] > ncol(matrix)] <- NA

  neighbours <- neighbours[stats::complete.cases(neighbours), ]

  return(neighbours)
}
