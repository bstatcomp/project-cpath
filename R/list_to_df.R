#' Transform list to a data frame.
#'
#' Transforms the input list to a data frame. Assumes the first three
#' elements of the list are N, P, and M.
#'
#' @param my_list The list to be converted to a data frame.
#'
#' @return A data frame.
#'
#' @examples
list_to_df <- function (my_list) {
  lnms    <- names(my_list)
  lnms    <- lnms[-(1:3)]
  df      <- matrix(data = NA, 
                    ncol = length(lnms), 
                    nrow = length(my_list[[lnms[1]]]))
  ind <- 1
  for (var in lnms) {
    df[ ,ind] <- my_list[[var]]
    ind       <- ind + 1
  }
  colnames(df) <- lnms
  return(data.frame(df))
}