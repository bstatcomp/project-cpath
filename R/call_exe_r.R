setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# 
# system("generalized_logistic_model.exe sample num_samples=10 num_warmup=10 data file=data.R init=init.R")
# system2("generalized_logistic_model.exe sample num_samples=10 num_warmup=10 data file=data.R init=init.R")
# 
# 
# tmp <- system("generalized_logistic_model.exe sample num_samples=10 num_warmup=10 data file=data.R init=init.R")


call_r_exe <- function (data_list,
                        out_file  = "./data/TEMP/glm_out.csv",
                        # N,
                        # P,
                        # M,
                        # IDp,
                        # IDs,
                        # SEX,
                        # AGE,
                        # COMED,
                        # APOE4,
                        # time,
                        # S,
                        nsamp     = 10,
                        nwarm     = 10,
                        ...) {
  # set paths
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  # MINUS ONE DIRECTORY
  
  
  # # create temporary files for data and init
  data_file <- "./data/TEMP/data.R"
  init_file <- "./data/TEMP/init.R"
  
  # create a character vector for each variable in the list
  char_vars <- c()
  ind <- 1
  vnames <- names(data_list)
  for (my_var in data_list) {
    if (length(my_var) == 1) {
      tmp_here <- paste(vnames[ind],
                        "<- ", 
                        paste(my_var, collapse = " , "))
      tmp_here <- gsub('(.{1,90})(\\s|$)', '\\1\n', tmp_here)
      
      char_vars <- c(char_vars,
                     tmp_here)
    } else {
      tmp_here <- paste(vnames[ind],
                        "<- c(", 
                        paste(my_var, collapse = " , "), 
                        ")")
      tmp_here <- gsub('(.{1,90})(\\s|$)', '\\1\n', tmp_here)
      # print(tmp_here)
      
      char_vars <- c(char_vars,
                     tmp_here)
    }

    ind <- ind + 1
  }
  # print(char_vars)
  # chv <<- char_vars
  
  
  

  fileConn  <- file("tmp2.R")
  writeLines(char_vars,
             fileConn)
  close(fileConn)
  
  #stop()
  # # this is temp, initialize data here
  # save(data_file, file = "./data/TEMP/data.R")
  # save(init_file, file = "./data/TEMP/init.R")
  data_file <- "./data/TEMP/data.R"
  init_file <- "./data/TEMP/init.R"
  
  
  data_file <- "tmp2.R"
  #data_file <- "data.R"
  
  
  # run model with temporary files
  model_call <- paste0("generalized_logistic_model.exe",
                       " sample",
                       " num_samples=", nsamp,
                       " num_warmup=", nwarm,
                       " data",
                       " file=", data_file,
                       " init=", init_file,
                       " output file=", out_file)
  system(model_call)
  

  # # delete temporary files
  # file.remove("./data/TEMP/data.R")
  # file.remove("./data/TEMP/init.R")

  
  # read saved csv and return the values + delete the csv
  stan_out <- read.delim("./data/TEMP/glm_out.csv", 
                         sep          = ",", 
                         comment.char = "#")
  # file.remove("./data/TEMP/glm_out.csv")
  return(stan_out)
}

a <- source("./data/TEMP/data.R")
in_data <- nlist(N,
                 P,
                 M,
                 IDp,
                 IDs,
                 AGE,
                 SEX,
                 COMED,
                 APOE4,
                 time,
                 S)
# 
# in_data <- list(X = c(1,5,3,2),
#                 Y = 4)
b <- call_r_exe(data_list = in_data)
