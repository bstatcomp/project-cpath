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
  cov_nms <- all.vars(covariates)
  print(cov_nms)
  N       <- nrow(df)
  P       <- length(unique(df$IDp))
  M       <- length(unique(df$IDs))
  IDp     <- df$IDp
  IDs     <- df$IDs
  tmp_list <- nlist()
  for (i in 1:length(cov_nms)) {
    # tmp <- covariates[i]
    # assign(tmp, df[ ,tmp])
    tmp           <- cov_nms[i]
    tmp_list[[i]] <- df[ ,tmp]
  }
  names(tmp_list) <- cov_nms
  time <- df$time
  S    <- df$S
  out_list <- c(nlist(N, P, M, IDp, IDs), 
                tmp_list,
                nlist(time, S))
  return(out_list)
  
}