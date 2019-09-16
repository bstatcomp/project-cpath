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
#' @param gpu_enabled binary.
#' @param init_list list. The list of initialization parameters for the model.
#' @param num_samples integer. The number of samples.
#' @param num_warmup integer. The number of samples in the warmup phase of the
#' sampling.
#' @param seed integer. Random seed for the model.
#' @param ... Other parameters for the sampler.
#'
#' @return A list with 3 elements. First is an object of stanfit. Second is the
#' data used in the modeling. Third are the maps from original subject and
#' study IDs to the IDs used in the model.
#'
sampling_gpu        <- function (df,
                                 mod_name        = "generalized_logistic_model",
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
                                 metric_file     =  "",
                                 stepsize        = 1 ,
                                 stepsize_jitter = 0,
                                 id              = 0,
                                 seed            = 1607674300,
                                 ...) {

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
    data_list <- GLMCPath::df_to_list(df, 
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
    data_list <- GLMCPath::df_to_list(df, 
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
    mod <- system.file("bin",paste0(mod_name),"Win64",paste0(mod_name,".exe"), package = "GLMCPath")
    mod <- paste0("\"", mod, "\"")      
  }
  if (my_os == "unix") {
    mod <- system.file("bin",paste0(mod_name),"Linux",paste0(mod_name), package = "GLMCPath")
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

  # run model + delete temporary files in case of error
  tryCatch({
    system(model_call)
  }, warning = function(w){
    print(w)
  }, error   = function(e) {
    file.remove(data_file, init_file, out_file)
    stop(e)
  })
  

  # read saved csv and return the values + delete temporary files
  tryCatch({
    stan_out <- read_stan_csv(out_file)
  }, warning = function(w){
    print(w)
  }, error   = function(e) {
    file.remove(data_file, init_file, out_file)
    stop(paste(e, "No output from the model. Check model call."))
  })

  return(list("stan_model" = stan_out, 
              "data_used"  = data_list,
              "maps"       = maps))
}

