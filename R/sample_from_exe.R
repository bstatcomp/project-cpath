sample_from_exe <- function (data_list,
                             init_list,
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
  # create temporary file names for data and init
  data_file <- paste0("./data/TEMP/data",
                      paste(c(sample(c("a","b","c","d", 1:9), 4, TRUE)), 
                            collapse = ""),
                      ".R")
  init_file <- paste0("./data/TEMP/init",
                      paste(c(sample(c("a","b","c","d", 1:9), 4, TRUE)), 
                            collapse = ""),
                      ".R")
  out_file  = paste0("./data/TEMP/out",
                     paste(c(sample(c("a","b","c","d", 1:9), 4, TRUE)), 
                           collapse = ""),
                     ".csv")
  
  
  # create a character vector for each variable in the list
  my_fun <- function (vec) {
    if (length(vec) == 1) {
      char_vec <- paste0(" <- ", vec)
    } else {
      char_vec <- paste0(" <- c(", paste0(vec, collapse = " , "), ")")
    }
    return (char_vec)
  }
  # data
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
  
  
  # run model
  mod <- "./bin/generalized_logistic_model/Win64/generalized_logistic_model.exe"
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
                       " file=", data_file,
                       " init=", init_file,
                       " random",
                       " seed=", seed,
                       " output file=", out_file)
  system(model_call)
  
  
  # read saved csv and return the values + delete temporary files
  stan_out <- read.delim(out_file, 
                         sep          = ",", 
                         comment.char = "#")
  file.remove(data_file, init_file, out_file)
  return(stan_out)
}
