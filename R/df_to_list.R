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
                        CovariatesX    = NULL,
                        CovariatesY    = NULL,
                        CovariatesZ    = NULL,
                        SubjectIdVar   = NULL,
                        StudyIdVar     = NULL,
                        TimeVar        = NULL,
                        ScoreVar       = NULL,
                        SecondScoreVar = NULL,
                        covariates) {
  # assign names
  IDp   <- deparse(substitute(SubjectIdVar))
  IDs   <- deparse(substitute(StudyIdVar))
  times <- deparse(substitute(TimeVar))
  S     <- deparse(substitute(ScoreVar))
  if (IDp == "NULL") {
    IDp <- "SubjectId"
  } 
  if (IDs == "NULL") {
    IDs <- "StudyId"
  } 
  if (times == "NULL") {
    times <- "Time"
  } 
  if (S == "NULL") {
    S <- "Score"
  } 
  
  # check if in data frame
  cnames <- colnames(df)
  if (!(IDp %in% cnames)) {
    stop(paste0("No column with name ", IDp, " in the data frame."))
  }
  if (!(IDs %in% cnames)) {
    stop(paste0("No column with name ", IDs, " in the data frame."))
  }
  if (!(times %in% cnames)) {
    stop(paste0("No column with name ", times, " in the data frame."))
  }
  if (!(S %in% cnames)) {
    stop(paste0("No column with name ", S, " in the data frame."))
  }
  
  # check second score
  S2 <- deparse(substitute(SecondScoreVar))
  if (S2 != "NULL") {
    if (S2 == S) {
      stop("Second score has the same name as the first score.")
    }
    if (!(S2 %in% cnames)) {
      stop(paste0("No column with name ", S2, " in the data frame."))
    }
  }
  
  # covariates
  covx_nms <- all.vars(CovariatesX)
  covy_nms <- all.vars(CovariatesY)
  covz_nms <- all.vars(CovariatesZ)
  if (length(intersect(covx_nms, covy_nms)) > 0) {
    stop("Non-empty intersection between covariates X and Y.")
  }
  if (length(intersect(covx_nms, covz_nms)) > 0) {
    stop("Non-empty intersection between covariates X and Z.")
  }
  if (length(intersect(covy_nms, covz_nms)) > 0) {
    stop("Non-empty intersection between covariates Y and Z.")
  }
  other_names <- c(IDp, IDs, times, S)
  if (S2 != "NULL") {
    other_names <- c(other_names, S2)
  }
  if (length(intersect(covx_nms, other_names)) > 0) {
    stop("Non-empty intersection between covariates X and core attributes.")
  }
  if (length(intersect(covy_nms, other_names)) > 0) {
    stop("Non-empty intersection between covariates Y and core attributes.")
  }
  if (length(intersect(covz_nms, other_names)) > 0) {
    stop("Non-empty intersection between covariates Z and core attributes.")
  }
  
  
  # transform to long format!!!
  # TODO
  
  # other
  cov_nms <- all.vars(covariates)
  print(cov_nms)
  N       <- nrow(df)
  P       <- length(unique(df[ ,IDp]))
  M       <- length(unique(df[ ,IDs]))
  IDp     <- df[ ,IDp]
  IDs     <- df[ ,IDs]
  
  tmp_list <- nlist()
  for (i in 1:length(cov_nms)) {
    # tmp <- covariates[i]
    # assign(tmp, df[ ,tmp])
    tmp           <- cov_nms[i]
    tmp_list[[i]] <- df[ ,tmp]
  }
  names(tmp_list) <- cov_nms
  times <- df[ ,times]
  S     <- df[ ,S]
  
  
  
  
  
  out_list <- c(nlist(N, P, M, IDp, IDs), 
                tmp_list,
                nlist(times, S))
  return(out_list)
  
}