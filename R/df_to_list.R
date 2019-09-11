#' Transform data frame to list.
#'
#' Transforms the input data frame to a list.
#'
#' @param df Input data frame.
#'
#' @return A list suitable as Stan input.
#'
#' @examples
df_to_list <- function (df, 
                        IDp            = NULL,
                        IDs            = NULL,
                        times          = NULL,
                        score          = NULL,
                        covr_nms       = NULL,
                        covb_nms       = NULL,
                        m_r            = NULL,
                        m_b            = NULL,
                        is_pbo         = NULL,
                        score2         = NULL,
                        covr2_nms      = NULL,
                        covb2_nms      = NULL,
                        m_r2           = NULL,
                        m_b2           = NULL,
                        is_pbo2        = NULL
                        ) {
  s1_ind <- df[ ,score]
  df1    <- df[!is.na(s1_ind), ]
  
  # Check for NAs and NULL
  df_vars <- df1[ ,c(IDp, IDs, times, score, covr_nms, covb_nms, is_pbo)]
  if(any(is.na(df_vars))) {
    stop("Missing values in the data frame for score 1.")
  }
  if(any(is.null(df_vars))) {
    stop("NULL values in the data frame for score 1.")
  }

  N1      <- nrow(df1)
  P1      <- length(unique(df1[ ,IDp]))
  M1      <- length(unique(df1[ ,IDs]))
  
  # re-index
  reindex <- function  (x) {
    new_inds <- x
    for (i in 1:length(x)) {
      new_inds[i] <- which(x[i] == unique(x))
    }
    return (new_inds)
  }
  # patients
  IDp1      <- df1[ ,IDp]
  original  <- IDp1
  IDp1      <- reindex(IDp1)
  reindexed <- IDp1
  IDp1_map  <- unique(data.frame(original, reindexed))
  
  # studies
  IDs1      <- df1[ ,IDs]
  original  <- IDs1
  IDs1      <- reindex(IDs1)
  reindexed <- IDs1
  IDs1_map  <- unique(data.frame(original, reindexed))

  map_list <- list(
    "patient1" = IDp1_map,
    "study1"   = IDs1_map
  )
  
  # other variables
  times1  <- df1[ ,times]
  S1      <- df1[ ,score]
  covsr1  <- as.matrix(df1[ ,covr_nms])
  covsb1  <- as.matrix(df1[ ,covb_nms])
  is_pbo1 <- df1[ ,is_pbo]

  # get placebo studies
  tmp     <- data.frame(IDs1, is_pbo1)
  tmp     <- unique(tmp)
  is_pbo1 <- tmp[order(tmp[ ,1]),2]

  
  out_list <- list(
    N                = N1,
    P                = P1,
    M                = M1,
    Q_r              = ncol(covsr1),
    Q_s              = ncol(covsb1),
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

  if (!is.null(score2)) {
    s2_ind <- df[ ,score2]
    df2    <- df[!is.na(s2_ind), ]
    
    df_vars <- df2[ ,c(IDp, IDs, times, score2, covr2_nms, covb2_nms, is_pbo2)]
    if(any(is.na(df_vars))) {
      stop("Missing values in the data frame for score 2.")
    }
    if(any(is.null(df_vars))) {
      stop("NULL values in the data frame for score 2.")
    }
    
    N2      <- nrow(df2)
    P2      <- length(unique(df2[ ,IDp]))
    M2      <- length(unique(df2[ ,IDs]))
    
    # reindex
    # patients
    IDp2      <- df2[ ,IDp]
    original  <- IDp2
    IDp2      <- reindex(IDp2)
    reindexed <- IDp2
    IDp2_map  <- unique(data.frame(original, reindexed))
    
    # studies
    IDs2      <- df2[ ,IDs]
    original  <- IDs2
    IDs2      <- reindex(IDs2)
    reindexed <- IDs2
    IDs2_map  <- unique(data.frame(original, reindexed))
    
    map_list <- list(
      "patient1" = IDp1_map,
      "study1"   = IDs1_map,
      "patient2" = IDp2_map,
      "study2"   = IDs2_map
    )
    
    times2  <- df2[ ,times]
    S2      <- df2[ ,score2]
    covsr2  <- as.matrix(df2[ ,covr2_nms])
    covsb2  <- as.matrix(df2[ ,covb2_nms])
    is_pbo2 <- df2[ ,is_pbo2]
    
    # get placebo studies
    tmp     <- data.frame(IDs2, is_pbo2)
    tmp     <- unique(tmp)
    is_pbo2 <- tmp[order(tmp[ ,1]),2]

    
    out_list <- list(
      N                 = N1,
      P                 = P1,
      M                 = M1,
      Q_r               = ncol(covsr1),
      Q_s               = ncol(covsb1),
      multiplicative_s  = m_b,
      multiplicative_r  = m_r,
      X_r               = covsr1,
      X_s               = covsb1,
      is_pbo            = is_pbo1,
      IDp               = IDp1,
      IDs               = IDs1,
      time              = times1,
      score             = S1,
      N2                = N2,
      P2                = P2,
      M2                = M2,
      Q_r2              = ncol(covsr2),
      Q_s2              = ncol(covsb2),
      multiplicative_s2 = m_b2,
      multiplicative_r2 = m_r2,
      X_r2              = covsr2,
      X_s2              = covsb2,
      is_pbo2           = is_pbo2,
      IDp2              = IDp2,
      IDs2              = IDs2,
      time2             = times2,
      score2            = S2
    )
  }
  tmp_out <- list(
    "data" = out_list,
    "maps" = map_list
  )
  return(tmp_out)
}