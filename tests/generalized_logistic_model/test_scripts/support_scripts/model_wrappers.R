model_glm_one_score <- function(dat_list, iter, warmup, seed) {
  return (process_stan_model("glm_one_score", dat_list, iter, warmup, seed))
}


process_stan_model <- function(model_name, dat_list, iter, warmup, seed) {
  library(rstan)
  # Compile model ----------------------------------------------------------------
  compiled_fn <- paste0("./temp/", model_name, ".compiled.rds")

  if (!file.exists(compiled_fn)) {
    model_fit   <- stan_model(model_code = readLines(paste0("./Stan/", model_name, ".stan")))
    saveRDS(model_fit, file = compiled_fn)
  } else {
    model_fit <- readRDS(compiled_fn)
  }

  dat_list$multiplicative_s <- 0
  dat_list$multiplicative_r <- 0
  output <- rstan::sampling(model_fit, data = dat_list$stan_data, 
                              iter = iter + warmup, warmup = warmup,
                              chains = 1, 
                              seed = seed)
  rstan::extract(output, permuted = F)
}

glm_one_score_delta_packed_CPU <- function(dat_list, iter, warmup, seed) {
  return (process_exe_model("glm_one_score_delta_packed_CPU", dat_list, iter, warmup, seed))
}

glm_one_score_delta_packed_GPU <- function(dat_list, iter, warmup, seed) {
  return (process_exe_model("glm_one_score_delta_packed_GPU", dat_list, iter, warmup, seed))
}


process_exe_model <- function(model_name, dat_list, iter, warmup, seed, GPU = 0) {
  library(rstan)

  exemod_name <- model_name
  df          <- dat_list$table_data
  
  if (grepl("CPU", model_name)) {
    gpue <- 0
  } else {
    gpue <- 1
  }

  exe_samps <- NULL
  tryCatch({
    exe_samps            <- sampling_gpu(df             = df,
                                         SubjectIdVar   = IDp,
                                         StudyIdVar     = IDs,
                                         TimeVar        = time,
                                         ScoreVar       = S1,
                                         is_pbo         = is_pbo,
                                         CovariatesR    = ~ COMED + AGE,
                                         CovariatesB    = ~ COMED + AGE,
                                         num_samples    = iter,
                                         num_warmup     = warmup,
                                         seed           = seed,
                                         gpu_enabled    = gpue)
  }, error   = function(e) {
    print("Model did not initialize. Try different seed.")
  })
  return(exe_samps)
}