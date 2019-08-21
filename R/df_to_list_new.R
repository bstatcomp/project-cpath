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
                        score          = NULL,
                        score2         = NULL,
                        covr_nms       = NULL,
                        covb_nms       = NULL,
                        covr2_nms      = NULL,
                        covb2_nms      = NULL,
                        m_r            = FALSE,
                        m_b            = FALSE,
                        m_r2           = FALSE,
                        m_b2           = FALSE,
                        is_pbo         = NULL,
                        is_pbo2        = NULL) {
  s1_ind <- df[ ,score]
  df1    <- df[!is.na(s1_ind), ]
  
  # CHECK FOR NAs!!!
  
  # browser()
  N1      <- nrow(df1)
  P1      <- length(unique(df1[ ,IDp]))
  M1      <- length(unique(df1[ ,IDs]))
  IDp1    <- df1[ ,IDp]
  IDs1    <- df1[ ,IDs]
  times1  <- df1[ ,times]
  S1      <- df1[ ,score]
  covsr1  <- as.matrix(df1[ ,covr_nms])
  covsb1  <- as.matrix(df1[ ,covb_nms])
  is_pbo1 <- df1[ ,is_pbo]
  
  out_list <- list(
    N                = N1,
    P                = P1,
    M                = M1,
    Q_r              = ncol(covsr1),
    Q_m              = ncol(covsb1),
    multiplicative_s = m_b,
    multiplicative_r = m_r,
    X_r              = covsr1,
    X_s              = covsb1,
    is_pbo           = is_pbo1,
    IDp              = IDp1,
    IDs              = IDs1,
    time             = times1,
    score            = S1
  )
  return(out_list)
}