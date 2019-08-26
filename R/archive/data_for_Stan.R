data_for_Stan <- function (df,
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
                                 is_pbo2         = placebo,
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
  other_names <- c(IDp, IDs, times, score, is_pbo)
  
  score2 <- deparse(substitute(ScoreVar2))
  # if second score
  if (score2 != "NULL") {
    if (!(score2 %in% cnames)) {
      stop(paste0("No column with name ", score2, " in the data frame."))
    }
    is_pbo2      <- deparse(substitute(is_pbo2))
    other_names  <- c(other_names, score2, is_pbo2)
    covr2_nms    <- all.vars(CovariatesR2)
    covb2_nms    <- all.vars(CovariatesB2)
    if (length(intersect(covr2_nms, other_names)) > 0) {
      stop("Non-empty intersection between rate covariates and core attributes.")
    }
    if (length(intersect(covb2_nms, other_names)) > 0) {
      stop("Non-empty intersection between baseline covariates and core attributes.")
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
      n_col    <- dim(vec)[2]
      if (n_col == 0) {
        char_vec <- paste0(" <- matrix(data = 0, nrow = ", n_row, ", ncol = 0)")
      } else {
        char_vec <- paste0(" <- matrix(c(", paste0(vec, collapse = " , "), "), nrow = ", n_row, ")")
      }
    } else {
      if (length(vec) == 1) {
        char_vec <- paste0(" <- ", vec)
      } else {
        char_vec <- paste0(" <- c(", paste0(vec, collapse = " , "), ")")
      }
    }
    
    return (char_vec)
  }
  
  # browser()
  # browser()
  
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
  
  #data
  char_data <- sapply(data_list, my_fun)
  char_data <- paste0(names(char_data), char_data)
  char_data <- gsub('(.{1,90})(\\s|$)', '\\1\n', char_data)
  fileConn  <- file(data_file)
  writeLines(char_data,
             fileConn)
  close(fileConn)
  
  
  
  
  
  # browser()
  # TRYING R DUMP
  with(data_list, {
    stan_rdump(names(data_list), file = data_file)
  })
  
  
  
  
  
  
  
  
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
    mod <- "./bin/generalized_logistic_model/Win64/new_model.exe"
    
    # CHANGE WINDOWS MODEL HERE
    
    write(gpu_enabled, file = "./bin/generalized_logistic_model/Win64/gpu_enabled.txt")
  }
  if (my_os == "unix") {
    
    # CHANGE LINUX MODEL HERE
    
    write(gpu_enabled, file = "./bin/generalized_logistic_model/Linux/gpu_enabled.txt")
    # TODO Linux executable
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
  if (!is.null(init_file)) {
    model_call <- paste0(model_call,
                         " init=", init_file)
  }
  
  model_call <- paste0(model_call,
                       " random",
                       " seed=", seed,
                       " output file=", out_file)
  # return(data_list)
  
  
  # browser()
  
  # # run model + delete temporary files in case of error
  # tryCatch({
  #   system(model_call)
  # }, warning = function(w){
  #   print(w)
  # }, error   = function(e) {
  #   file.remove(data_file, init_file, out_file)
  #   stop(e)
  # })
  # 
  # 
  # # read saved csv and return the values + delete temporary files
  # tryCatch({
  #   stan_out <- read.delim(out_file,
  #                          sep          = ",",
  #                          comment.char = "#")
  # }, warning = function(w){
  #   print(w)
  # }, error   = function(e) {
  #   file.remove(data_file, init_file, out_file)
  #   stop(paste(e, "No output from the model. Check model call."))
  # })
  
  # browser()
  #
  # file.remove(data_file, init_file, out_file)
  # return(stan_out)
  return(list("data_list"  = data_list,
              # "stan_model" = stan_out, 
              "maps"       = maps)) # temporary
}
