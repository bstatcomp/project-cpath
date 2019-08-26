sample_from_exe <- function (df,
                             SubjectIdVar    = "SubjectId",
                             StudyIdVar      = "StudyId",
                             TimeVar         = "Time",
                             ScoreVar        = "Score",
                             SecondScoreVar  = NULL,
                             CovariatesX     = NULL,
                             CovariatesY     = NULL,
                             CovariatesZ     = NULL,
                             covariates,
                             init_list       = NULL,
                             num_samples     = 1000,
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
  S          <- deparse(substitute(ScoreVar))
  scores_vec <- S
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
  } else {
    S2 <- NULL
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
  if (!is.null(S2)) {
    other_names <- c(other_names, S2)
    scores_vec  <- c(scores_vec, S2)
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
  
  
  # create temporary file names for data and init
  data_file <- paste0(tempdir(),
                      "\\data",
                      paste(c(sample(c("a","b","c","d", 1:9), 4, TRUE)), 
                            collapse = ""),
                      ".R")
  init_file <- paste0(tempdir(),
                      "\\init",
                      paste(c(sample(c("a","b","c","d", 1:9), 4, TRUE)), 
                            collapse = ""),
                      ".R")
  out_file  = paste0(tempdir(),
                     "\\out",
                     paste(c(sample(c("a","b","c","d", 1:9), 4, TRUE)), 
                           collapse = ""),
                     ".csv")
  
  
  # create a character vector for each variable in the list
  my_fun <- function (vec) {
    if (!is.null(dim(vec))) {
      n_row    <- dim(vec)[1]
      char_vec <- paste0(" <- matrix(c(", paste0(vec, collapse = " , "), "), nrow = ", n_row, ")")
    } else {
      if (length(vec) == 1) {
        char_vec <- paste0(" <- ", vec)
      } else {
        char_vec <- paste0(" <- c(", paste0(vec, collapse = " , "), ")")
      }
    }
    
    return (char_vec)
  }
  

  # data
  data_list <- df_to_list(df, 
                          IDp            = IDp,
                          IDs            = IDs,
                          times          = times,
                          S              = S,
                          S2             = S2,
                          covx_nms       = covx_nms,
                          covy_nms       = covy_nms,
                          covz_nms       = covz_nms,
                          covariates     = covariates)

  char_data <- sapply(data_list, my_fun)
  char_data <- paste0(names(char_data), char_data)
  char_data <- gsub('(.{1,90})(\\s|$)', '\\1\n', char_data)
  fileConn  <- file(data_file)
  writeLines(char_data,
             fileConn)
  close(fileConn)
  # init
  char_init <- sapply(init_list, my_fun)
  char_init <- paste0(names(char_init), char_init)
  char_init <- gsub('(.{1,90})(\\s|$)', '\\1\n', char_init)
  fileConn  <- file(init_file)
  writeLines(char_init,
             fileConn)
  close(fileConn)
  
  

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
    mod <- "./bin/generalized_logistic_model/Win64/generalized_logistic_model.exe"
  }
  if (my_os == "unix") {
    # TODO
  }
  if (my_os == "mac") {
    # link to page
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
  if (!is.null(init_file)) {
    model_call <- paste0(model_call, 
                         " init=", init_file)
  }
  model_call <- paste0(model_call,
                       " random",
                       " seed=", seed,
                       " output file=", out_file)
  
  # here comes try-catch + delete temporary files in case of error
  tryCatch({
    system(model_call)
  }, warning = function(w){
    print(w)
  }, error   = function(e) {
    file.remove(data_file, init_file, out_file) # without out_file?
    stop(e)
  })

  # read saved csv and return the values + delete temporary files
  tryCatch({
    stan_out <- read.delim(out_file, 
                           sep          = ",", 
                           comment.char = "#")
  }, warning = function(w){
    print(w)
  }, error   = function(e) {
    file.remove(data_file, init_file, out_file)
    stop(paste(e, "No output from the model. Check model call."))
  })
  
  file.remove(data_file, init_file, out_file)
  return(stan_out)
}
