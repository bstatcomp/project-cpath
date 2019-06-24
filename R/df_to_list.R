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
df_to_list <- function (df,
                        IDp            = NULL,
                        IDs            = NULL,
                        times          = NULL,
                        S              = NULL,
                        S2             = NULL,
                        covx_nms       = NULL,
                        covy_nms       = NULL,
                        covz_nms       = NULL,
                        covariates) {
  # transform to long
  scores_vec <- c(S, S2)
  df_long    <- gather(df, ScoreId, Score, scores_vec)
  df_long$ScoreId[df_long$ScoreId == scores_vec[1]] <- 1
  if (length(scores_vec == 2)) {
    df_long$ScoreId[df_long$ScoreId == scores_vec[2]] <- 2
  }


  # create list
  cov_nms <- all.vars(covariates)
  print(cov_nms)
  N           <- nrow(df) # Or nrow(df2)? Or something else?
  P           <- length(unique(df_long[ ,IDp]))
  M           <- length(unique(df_long[ ,IDs]))
  IDp         <- df_long[ ,IDp]
  IDs         <- df_long[ ,IDs]
  times       <- df_long[ ,times]
  SId         <- df_long[ ,"ScoreId"]
  S           <- df_long[ ,"Score"]
  CovariatesX <- as.matrix(df_long[ ,covx_nms])
  CovariatesY <- as.matrix(df_long[ ,covy_nms])
  CovariatesZ <- as.matrix(df_long[ ,covz_nms])
  
  # old version, delete after new model
  tmp_list <- nlist()
  for (i in 1:length(cov_nms)) {
    tmp           <- cov_nms[i]
    tmp_list[[i]] <- df[ ,tmp]
  }
  names(tmp_list) <- cov_nms
  
  
  out_list <- nlist(N, P, M, IDp, IDs, 
                    CovariatesX, CovariatesY, CovariatesZ,
                    times, SId, S)
  
  # old version, delete after new model
  out_list_old <- c(nlist(N, P, M, IDp, IDs), 
                    tmp_list,
                    nlist(times, S))
  return(out_list)
}