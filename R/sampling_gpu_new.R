#' Run a cmdstan model
#'
#' Runs a cmdstan model. All parameters with the number 2 in the name are
#' only used in case of 2 scores.
#'
#' @param df Input data frame.
#' @param mod_name character. The name of the model as saved in
#' "./bin/generalized_logistic_model/Win64/" or
#' "./bin/generalized_logistic_model/Linux/", without the extension.
#' @param SubjectIdVar Name of the column which holds the subject IDs.
#' Must be without quotation marks.
#' @param StudyIdVar Name of the column which holds the study IDs.
#' Must be without quotation marks.
#' @param TimeVar Name of the column which holds the times.
#' Must be without quotation marks.
#' @param ScoreVar Name of the column which holds the scores (responses).
#' Must be without quotation marks.
#' @param is_pbo binary vector. 1 for placebo studies, 0 for others.
#' @param CovariatesR formula. Formula of the form ~ A + B + ..., where
#' the letters represent the names of columns of the covariates for rate.
#' @param CovariatesB formula. Formula of the form ~ A + B + ..., where
#' the letters represent the names of columns of the covariates for baseline.
#' @param m_r binary. 0 if the covariates for rate are additive, 1 if they
#' are multiplicative.
#' @param m_b binary. 0 if the covariates for baseline are additive, 1 if they
#' are multiplicative.
#' @param ScoreVar2 Name of the column which holds the second scores
#' (responses) if applicable.
#' Must be without quotation marks.
#' @param is_pbo2 binary vector. 1 for placebo studies, 0 for others.
#' @param CovariatesR2 formula. Formula of the form ~ A + B + ..., where
#' the letters represent the names of columns of the covariates for rate.
#' @param CovariatesB2 formula. Formula of the form ~ A + B + ..., where
#' the letters represent the names of columns of the covariates for baseline.
#' @param m_r2 binary. 0 if the covariates for rate are additive, 1 if they
#' are multiplicative.
#' @param m_b2 binary. 0 if the covariates for baseline are additive, 1 if they
#' are multiplicative.
#' @param gpu_enabled binary. 1 to use the GPU model, 0 to use the CPU model.
#' @param init_list list. The list of initialization parameters for the model.
#' @param num_samples integer. The number of samples.
#' @param num_warmup integer. The number of samples in the warmup phase of the
#' sampling.
#' @param seed integer. Random seed for the model.
#' @param ... Other parameters for the sampler.
#'
#' @return A list with 4 elements. First is an object of stanfit. Second is the
#' data used in the modeling. Third are the maps from original subject and
#' study IDs to the IDs used in the model. Fourth are the predicted values for
#' each iteration.
#'
sampling_gpu_new    <- function (df,
                                 mod_name        = "glm",
                                 SubjectIdVar    = IDp,
                                 StudyIdVar      = IDs,
                                 TimeVar         = time,
                                 ScoreVar        = score,
                                 is_pbo          = placebo,
                                 CovariatesR     = NULL,
                                 CovariatesB     = NULL,
                                 m_r             = 0,
                                 m_b             = 0,
                                 ScoreVar2       = NULL,
                                 is_pbo2         = NULL,
                                 CovariatesR2    = NULL,
                                 CovariatesB2    = NULL,
                                 m_r2            = 0,
                                 m_b2            = 0,
                                 gpu_enabled     = 1,
                                 init_list       = NULL,
                                 num_samples     = 1000, # Stan parameters
                                 num_warmup      = 1000,
                                 save_warmup     = 0,
                                 thin            = 1,
                                 engaged         = 1,
                                 gamma           = 0.050000000000000003,
                                 delta           = 0.80000000000000004,
                                 kappa           = 0.75,
                                 t0              = 10,
                                 init_buffer     = 75,
                                 term_buffer     = 50,
                                 window          = 25,
                                 algorithm       = "hmc",
                                 engine          = "nuts",
                                 max_depth       = 10,
                                 metric          = "diag_e",
                                 metric_file     = "",
                                 stepsize        = 1 ,
                                 stepsize_jitter = 0,
                                 id              = 0,
                                 seed            = 1607674300,
                                 ...) {
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
      score1           = S1
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
      
      # Prepare mapping of patients for stan
      have_both <- base::intersect(map_list$patient1$original,
                                   map_list$patient2$original)
      pmap_stan <- data.frame("original" = have_both)
      pmap_stan <- merge(pmap_stan, map_list$patient1, by = "original")
      pmap_stan <- merge(pmap_stan, map_list$patient2, by = "original")
      pmap_stan <- pmap_stan[ ,c("reindexed.x", "reindexed.y")]
      
      pother1 <- map_list$patient1$reindexed[!(map_list$patient1$original %in% have_both)]
      pother2 <- map_list$patient2$reindexed[!(map_list$patient2$original %in% have_both)]
      
      # prepare mapping of studies for stan
      have_both <- base::intersect(map_list$study1$original,
                                   map_list$study2$original)
      smap_stan <- data.frame("original" = have_both)
      smap_stan <- merge(smap_stan, map_list$study1, by = "original")
      smap_stan <- merge(smap_stan, map_list$study2, by = "original")
      smap_stan <- smap_stan[ ,c("reindexed.x", "reindexed.y")]
      
      sother1 <- map_list$study1$reindexed[!(map_list$study1$original %in% have_both)]
      sother2 <- map_list$study2$reindexed[!(map_list$study2$original %in% have_both)]
      
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
        score1            = S1,
        
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
        score2            = S2,
        Pn              = nrow(pmap_stan),
        Sn              = nrow(smap_stan),
        patient_map     = pmap_stan,
        study_map       = smap_stan,
        other_patients1 = pother1,
        other_patients2 = pother2,
        other_studies1  = as.array(sother1),
        other_studies2  = as.array(sother2)
      )
    }
    tmp_out <- list(
      "data" = out_list,
      "maps" = map_list
    )
    return(tmp_out)
  }
  
  # assign names
  IDp        <- deparse(substitute(SubjectIdVar))
  IDs        <- deparse(substitute(StudyIdVar))
  times      <- deparse(substitute(TimeVar))
  score      <- deparse(substitute(ScoreVar))
  is_pbo     <- deparse(substitute(is_pbo))
  
  
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
  if (!(score %in% cnames)) {
    stop(paste0("No column with name ", score, " in the data frame."))
  }
  if (!(is_pbo %in% cnames)) {
    stop(paste0("No column with name ", is_pbo, " in the data frame."))
  }
  
  # covariates
  covr_nms    <- all.vars(CovariatesR)
  covb_nms    <- all.vars(CovariatesB)
  
  if (any(!(covr_nms %in% cnames))) {
    stop("No column with CovariatesR in the data frame.")
  }
  if (any(!(covb_nms %in% cnames))) {
    stop("No column with CovariatesB in the data frame.")
  }
  other_names <- c(IDp, IDs, times, score, is_pbo)
  
  score2 <- deparse(substitute(ScoreVar2))
  
  if (score2 != "NULL") {
    if (!(score2 %in% cnames)) {
      stop(paste0("No column with name ", score2, " in the data frame."))
    }
    is_pbo2      <- deparse(substitute(is_pbo2))
    if (!(is_pbo2 %in% cnames)) {
      stop(paste0("No column with name ", is_pbo2, " in the data frame."))
    }
    other_names  <- c(other_names, score2, is_pbo2)
    covr2_nms    <- all.vars(CovariatesR2)
    covb2_nms    <- all.vars(CovariatesB2)
    if (any(!(covr2_nms %in% cnames))) {
      stop("No column with CovariatesR2 in the data frame.")
    }
    if (any(!(covb2_nms %in% cnames))) {
      stop("No column with CovariatesB2 in the data frame.")
    }
    if (length(intersect(covr2_nms, other_names)) > 0) {
      stop("Non-empty intersection between rate2 covariates and core attributes.")
    }
    if (length(intersect(covb2_nms, other_names)) > 0) {
      stop("Non-empty intersection between baseline2 covariates and core attributes.")
    }
  }
  if (length(intersect(covr_nms, other_names)) > 0) {
    stop("Non-empty intersection between rate covariates and core attributes.")
  }
  if (length(intersect(covb_nms, other_names)) > 0) {
    stop("Non-empty intersection between baseline covariates and core attributes.")
  }
  
  # create temporary file names for data and init
  data_file <- paste0(tempdir(),
                      "/data",
                      paste(c(sample(c("a","b","c","d", 1:9), 4, TRUE)),
                            collapse = ""),
                      ".R")
  init_file <- paste0(tempdir(),
                      "/init",
                      paste(c(sample(c("a","b","c","d", 1:9), 4, TRUE)),
                            collapse = ""),
                      ".R")
  out_file  = paste0(tempdir(),
                     "/out",
                     paste(c(sample(c("a","b","c","d", 1:9), 4, TRUE)),
                           collapse = ""),
                     ".csv")
  
  if(score2 == "NULL") {
    data_list <- df_to_list(df,
                            IDp            = IDp,
                            IDs            = IDs,
                            times          = times,
                            score          = score,
                            covr_nms       = covr_nms,
                            covb_nms       = covb_nms,
                            m_r            = m_r,
                            m_b            = m_b,
                            is_pbo         = is_pbo)
  } else {
    # if (sum(!is.na(df[ ,score]) & !is.na(df[ ,score2])) == 0) {
    #   stop("Empty intersection between score 1 and score 2, use separate one-score models instead.")
    # }
    data_list <- df_to_list(df,
                            IDp            = IDp,
                            IDs            = IDs,
                            times          = times,
                            score          = score,
                            covr_nms       = covr_nms,
                            covb_nms       = covb_nms,
                            m_r            = m_r,
                            m_b            = m_b,
                            is_pbo         = is_pbo,
                            score2         = score2,
                            covr2_nms      = covr2_nms,
                            covb2_nms      = covb2_nms,
                            m_r2           = m_r2,
                            m_b2           = m_b2,
                            is_pbo2        = is_pbo2)
  }
  maps      <- data_list$maps
  data_list <- data_list$data
  tmp <<- data_list
  #data
  with(data_list, {
    stan_rdump(names(data_list), file = data_file)
  })
  
  # init
  with(init_list, {
    stan_rdump(names(init_list), file = init_file)
  })
  
  # here we have to check for OS and select the correct version into mod
  get_os <- function() {
    if (.Platform$OS.type == "windows") {
      "win"
    } else if (Sys.info()["sysname"] == "Darwin") {
      "mac"
    } else if (.Platform$OS.type == "unix") {
      "unix"
    } else {
      stop("Unknown OS")
    }
  }
  my_os <- get_os()
  
  if (my_os == "win") {
    if (score2 != "NULL") {
      if (gpu_enabled == 0) {
        mod <- system.file(paste0("bin/generalized_logistic_model/Win64/", mod_name, "_two_scores_delta_packed_CPU.exe"), package = "GLMCPath")
      } else {
        mod <- system.file(paste0("bin/generalized_logistic_model/Win64/", mod_name, "_two_scores_delta_packed_GPU.exe"), package = "GLMCPath")
      }
      
    } else {
      if (gpu_enabled == 0) {
        mod <- system.file(paste0("bin/generalized_logistic_model/Win64/", mod_name, "_one_score_delta_packed_CPU.exe"), package = "GLMCPath")
      } else {
        mod <- system.file(paste0("bin/generalized_logistic_model/Win64/", mod_name, "_one_score_delta_packed_GPU.exe"), package = "GLMCPath")
      }
      
    }
  }
  if (my_os == "unix") {
    if (score2 != "NULL") {
      if (gpu_enabled == 0) {
        mod <- system.file(paste0("bin/generalized_logistic_model/Linux/", mod_name, "_two_scores_delta_packed_CPU"), package = "GLMCPath")
      } else {
        mod <- system.file(paste0("bin/generalized_logistic_model/Linux/", mod_name, "_two_scores_delta_packed_GPU"), package = "GLMCPath")
      }
    } else {
      if (gpu_enabled == 0) {
        mod <- system.file(paste0("bin/generalized_logistic_model/Linux/", mod_name, "_one_score_delta_packed_CPU"), package = "GLMCPath")
      } else {
        mod <- system.file(paste0("bin/generalized_logistic_model/Linux/", mod_name, "_one_score_delta_packed_GPU"), package = "GLMCPath")
      }
    }
    # mod <- system.file("bin",paste0(mod_name),"Linux",paste0(mod_name), package = "GLMCPath")
  }
  if (my_os == "mac") {
    stop("macOS not supported.")
    # + link to page?
  }
  
  # create string
  model_call <- paste0(mod,
                       " sample",
                       " num_samples=", num_samples,
                       " num_warmup=", num_warmup,
                       " save_warmup=", save_warmup,
                       " thin=", thin,
                       " adapt",
                       " engaged=", engaged,
                       " gamma=", gamma,
                       " delta=", delta,
                       " kappa=", kappa,
                       " t0=", t0,
                       " init_buffer=", init_buffer,
                       " term_buffer=", term_buffer,
                       " window=", window,
                       " algorithm=", algorithm,
                       " engine=", engine,
                       " max_depth=", max_depth,
                       " metric=", metric,
                       " metric_file=", metric_file,
                       " stepsize=", stepsize,
                       " stepsize_jitter=", stepsize_jitter,
                       " data",
                       " file=", data_file)
  if (!is.null(init_list)) {
    model_call <- paste0(model_call,
                         " init=", init_file)
  }
  
  model_call <- paste0(model_call,
                       " random",
                       " seed=", seed,
                       " output file=", out_file)
  print(model_call)
  # run model + delete temporary files in case of error
  
  tryCatch({
    system(model_call)
  }, error   = function(e) {
    file.remove(data_file, init_file, out_file)
    stop(e)
  })
  
  
  # read saved csv and return the values + delete temporary files
  tryCatch({
    print(out_file)
    stan_out <- read_stan_csv(out_file)
  }, error   = function(e) {
    file.remove(data_file, init_file, out_file)
    stop(paste(e, "No output from the model. Check model call."))
  })
  
  
  if(score2 == "NULL") {
    ext <- rstan::extract(stan_out)
    pred_df1 <- data.frame(IDp = data_list$IDp,
                           IDs = data_list$IDs,
                           time = data_list$time,
                           scoreN = 1)
    pred_df1 <- cbind(pred_df1, t(ext$score1_pred))
    pred_df  <- pred_df1
    
    mdf1 <- df[ , colnames(df) != score2]
    
    # remove NAs
    mdf1 <- mdf1[!is.na(mdf1[ , score]), ]
    
    # rename columns
    colnames(mdf1)[colnames(mdf1) == score] <- "score"
    
    # cbind score name
    mdf1 <- cbind(mdf1, "scoreN" = 1)
    
    # rbind
    mdf <- mdf1
    
    mdf_sorted     <- mdf[order(mdf$scoreN, mdf[ , IDp], mdf[ , IDs], mdf[ , times]), ]
    pred_df_sorted <- pred_df[order(mdf$scoreN, mdf[ , IDp], mdf[ , IDs], mdf[ , times]), ]
    out_data       <- cbind(mdf_sorted, pred_df_sorted[ ,-(1:4)])
  } else {
    ext <- rstan::extract(stan_out)
    pred_df1 <- data.frame(IDp = data_list$IDp,
                           IDs = data_list$IDs,
                           time = data_list$time,
                           scoreN = 1)
    pred_df1 <- cbind(pred_df1, t(ext$score1_pred))
    
    pred_df2 <- data.frame(IDp = data_list$IDp2,
                           IDs = data_list$IDs2,
                           time = data_list$time2,
                           scoreN = 2)
    pred_df2 <- cbind(pred_df2, t(ext$score2_pred))
    pred_df  <- rbind(pred_df1, pred_df2)
    
    mdf1 <- df[ , colnames(df) != score2]
    mdf2 <- df[ , colnames(df) != score]
    
    # remove NAs
    mdf1 <- mdf1[!is.na(mdf1[ , score]), ]
    mdf2 <- mdf2[!is.na(mdf2[ , score2]), ]
    
    # rename columns
    colnames(mdf1)[colnames(mdf1) == score] <- "score"
    colnames(mdf2)[colnames(mdf2) == score2] <- "score"
    
    # cbind score name
    mdf1 <- cbind(mdf1, "scoreN" = 1)
    mdf2 <- cbind(mdf2, "scoreN" = 2)
    
    # rbind
    mdf <- rbind(mdf1, mdf2)
    
    mdf_sorted     <- mdf[order(mdf$scoreN, mdf[ , IDp], mdf[ , IDs], mdf[ , times]), ]
    pred_df_sorted <- pred_df[order(mdf$scoreN, mdf[ , IDp], mdf[ , IDs], mdf[ , times]), ]
    out_data       <- cbind(mdf_sorted, pred_df_sorted[ ,-(1:4)])
  }
  
  return(list("stan_model"   = stan_out,
              "data_used"    = data_list,
              "maps"         = maps,
              "pred_samples" = out_data))
}

