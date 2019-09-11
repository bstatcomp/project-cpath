## Comparison of RStan and cmdstan output
## TEST 2exes
testn <- 1

## WD --------------------------------------------------------------------------

## !!!
## If not using RStudio, change the working directory by hand to the root
## of the project folder, that contains the "tests" folder. 
## Use setwd("path_to_folder").
## If using RStudio, the below code will automatically set the path, relative
## to the path of this script.
my_path <- dirname(rstudioapi::getSourceEditorContext()$path)
my_path <- strsplit(my_path, split = "/")[[1]]
my_path <- paste0(my_path[-c((length(my_path) - 2):(length(my_path)))], 
                  collapse = "/")
setwd(my_path)

# libraries and sourcing -------------------------------------------------------
library(loo)
library(rstan)
library(mcmcse)
source("./R/sampling_gpu.R")
source("./R/df_to_list.R")
source("./R/compare_samples.R")

dir.create("./tests/generalized_logistic_model/saved_samples")
dir.create("./tests/generalized_logistic_model/compiled")

## data input ------------------------------------------------------------------
set.seed(1)
niter   <- 2000
in_data <- read.csv("./data/generalized_logistic_model/data_for_stan_model_cov_smallset2.csv")
is_pbo  <- rep(0, nrow(in_data))
is_pbo[in_data$IDs %in% c(13,14,15,16)]  <- 1
in_data <- cbind(in_data, "placebo" = is_pbo)


## cmdstan1 --------------------------------------------------------------------
file_name <- paste0("./tests/generalized_logistic_model/saved_samples/",
                    "test2exes_first",
                    testn,
                    "_Rstan_niter",
                    niter,
                    ".rds")
if (file.exists(file_name)) {
  exe_samps1 <- readRDS(file_name)
} else {
  set.seed(1)
  exe_samps1       <- sampling_gpu(df             = in_data,
                                   mod_name       = "new_model",
                                   SubjectIdVar   = IDp,
                                   StudyIdVar     = IDs,
                                   TimeVar        = TIME,
                                   ScoreVar       = ADAS,
                                   is_pbo         = placebo,
                                   CovariatesR    = ~ AGE + SEX,
                                   CovariatesB    = ~ COMED,
                                   num_samples    = niter / 2,
                                   num_warmup     = niter / 2,
                                   seed           = 1)
  saveRDS(exe_samps1, file = file_name)
}
ext_exe1 <- rstan::extract(exe_samps1$stan_model)


## cmdstan2 --------------------------------------------------------------------
file_name <- paste0("./tests/generalized_logistic_model/saved_samples/",
                    "test2exes_second",
                    testn,
                    "_Rstan_niter",
                    niter,
                    ".rds")
if (file.exists(file_name)) {
  exe_samps2 <- readRDS(file_name)
} else {
  set.seed(1)
  exe_samps2       <- sampling_gpu(df            = in_data,
                                   mod_name       = "new_model",
                                   SubjectIdVar   = IDp,
                                   StudyIdVar     = IDs,
                                   TimeVar        = TIME,
                                   ScoreVar       = ADAS,
                                   is_pbo         = placebo,
                                   CovariatesR    = ~ AGE + SEX,
                                   CovariatesB    = ~ COMED,
                                   num_samples    = niter / 2,
                                   num_warmup     = niter / 2,
                                   seed           = 1)
  saveRDS(exe_samps2, file = file_name)
}
ext_exe2 <- rstan::extract(exe_samps2$stan_model)


## Parameter distributions -----------------------------------------------------
cs <- compare_samples(do.call(cbind, ext_exe1), 
                      do.call(cbind, ext_exe2))
# View(cs)
summary(cs$is_same)
summary(cs$diff_at_third)


## ESS -------------------------------------------------------------------------
ess_df <- data.frame(apply(do.call(cbind, ext_exe1), 2, ess),
                     apply(do.call(cbind, ext_exe2), 2, ess))
# View(ess_df)


## Rhat ------------------------------------------------------------------------
rhat_df <- data.frame(summary(exe_samps1$stan_model)$summary[ ,"Rhat"],
                      summary(exe_samps2$stan_model)$summary[ ,"Rhat"])
# View(rhat_df)
summary(rhat_df)
