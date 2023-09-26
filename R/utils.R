#' Util functions for this app
#'
#'     DO NOT REMOVE.
#' @noRd
#' @export
# a call creating input buttons:
shinyInput <- function(FUN, ids, ...) {
  inputs <- character(length(ids))
  for (id in ids) {
    inputs[id] <- as.character(FUN(id, ...))
  }
  inputs
}
