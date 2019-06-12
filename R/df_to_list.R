#' Transform data frame to list.
#'
#' Transforms the input data frame to a list.
#'
#' @param df Input data frame.
#' @param covariates Vector of covariate names.
#'
#' @return A list suitable for transformation to csv.
#'
#' @examples
df_to_list <- function (df, covariates) {
  N    <- nrow(df)
  P    <- length(unique(df$IDp))
  M    <- length(unique(df$IDs))
  IDp  <- df$IDp
  IDs  <- df$IDs
  tmp_list <- nlist()
  for (i in 1:length(covariates)) {
    tmp <- covariates[i]
    assign(tmp, df[ ,tmp])
    tmp_list[[i]] <- df[ ,tmp]
  }
  names(tmp_list) <- covariates
  time <- df$time
  S    <- df$S
  out_list <- c(nlist(N, P, M, IDp, IDs), tmp_list)
  return(out_list)
  
}