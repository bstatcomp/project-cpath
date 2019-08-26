# WD, libraries, and sourcing --------------------------------------------------
my_path <- dirname(rstudioapi::getSourceEditorContext()$path)
my_path <- strsplit(my_path, split = "/")[[1]]
my_path <- paste0(my_path[-c(length(my_path) - 1, length(my_path))], 
                  collapse = "/")
setwd(my_path)
library(loo)
library(rstan)
source("./R/sample_from_exe_new.R")
source("./R/df_to_list_new.R")

# functions --------------------------------------------------------------------
reindex <- function  (x) {
  new_inds <- x
  for (i in 1:length(x)) {
    new_inds[i] <- which(x[i] == unique(x))
  }
  return (new_inds)
}

# data input -------------------------------------------------------------------
in_data <- read.csv("./data/generalized_logistic_model/data_for_stan_model_cov_smallset2.csv")
is_pbo  <- rep(0, nrow(in_data))
is_pbo[in_data$IDs %in% c(13,14,15,16)]  <- 1
in_data <- cbind(in_data, "placebo" = is_pbo)

# subset
in_data <- in_data[in_data$IDs %in% c(1,12), ]
set.seed(1)
niter   <- 20000

# Compile model ----------------------------------------------------------------
model_name  <- "new_model"
compiled_fn <- paste0("./tests/generalized_logistic_model/compiled/", model_name, "_compiled.rds")

if (!file.exists(compiled_fn)) {
  model_fit <- stan_model(file = paste0("./tests/generalized_logistic_model/Stan/", model_name, ".stan"))
  saveRDS(model_fit, file = compiled_fn)
} else {
  model_fit <- readRDS(compiled_fn)
}


# test 1 -----------------------------------------------------------------------
M <- length(unique(in_data$IDs))
stan_data <- list(
  N                = nrow(in_data),
  P                = length(unique(in_data$IDp)),
  M                = length(unique(in_data$IDs)),
  Q_r              = 2,
  Q_s              = 1,
  multiplicative_r = 0,
  multiplicative_s = 0,
  X_r              = in_data[ ,colnames(in_data) %in% c("AGE", "SEX")],
  X_s              = as.matrix(in_data[ ,colnames(in_data) %in% c("COMED")]),
  is_pbo           = c(0,0),
  IDp              = reindex(in_data$IDp),
  IDs              = reindex(in_data$IDs),
  time             = in_data$TIME,
  score            = in_data$ADAS
)
file_name <- paste0("./tests/generalized_logistic_model/saved_samples/test1_Rstan_small_niter",
                    niter,
                    ".rds")
if (file.exists(file_name)) {
  RStan_samps <- readRDS(file_name)
} else {
  set.seed(1)
  RStan_samps <- sampling(model_fit, stan_data, chains = 1, iter = niter, seed = 1)
  saveRDS(RStan_samps, file = file_name)
}
ext <- rstan::extract(RStan_samps)

# exe
file_name <- paste0("./tests/generalized_logistic_model/saved_samples/test1_exe_small_niter",
                    niter,
                    ".rds")
if (file.exists(file_name)) {
  exe_samps <- readRDS(file_name)
} else {
  set.seed(1)
  exe_samps <- sample_from_exe_new(df             = in_data,
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
  saveRDS(exe_samps, file = file_name)
}

RStan_summ <- apply(do.call(cbind, ext), 2, median)
exe_summ   <- apply(exe_samps$stan_model, 2, median)
data.frame(RStan_summ[-length(RStan_summ)], exe_summ[-c(1:7)])



# test 2 -----------------------------------------------------------------------
M <- length(unique(in_data$IDs))
stan_data <- list(
  N                = nrow(in_data),
  P                = length(unique(in_data$IDp)),
  M                = length(unique(in_data$IDs)),
  Q_r              = 2,
  Q_s              = 1,
  multiplicative_r = 1,
  multiplicative_s = 1,
  X_r              = in_data[ ,colnames(in_data) %in% c("AGE", "SEX")],
  X_s              = as.matrix(in_data[ ,colnames(in_data) %in% c("COMED")]),
  is_pbo           = c(0,0),
  IDp              = reindex(in_data$IDp),
  IDs              = reindex(in_data$IDs),
  time             = in_data$TIME,
  score            = in_data$ADAS
)
file_name <- paste0("./tests/generalized_logistic_model/saved_samples/test2_Rstan_small_niter",
                    niter,
                    ".rds")
if (file.exists(file_name)) {
  RStan_samps <- readRDS(file_name)
} else {
  set.seed(1)
  RStan_samps <- sampling(model_fit, stan_data, chains = 1, iter = niter, seed = 1)
  saveRDS(RStan_samps, file = file_name)
}
ext <- rstan::extract(RStan_samps)

# exe
file_name <- paste0("./tests/generalized_logistic_model/saved_samples/test2_exe_small_niter",
                    niter,
                    ".rds")
if (file.exists(file_name)) {
  exe_samps <- readRDS(file_name)
} else {
  set.seed(1)
  exe_samps <- sample_from_exe_new(df             = in_data,
                                   SubjectIdVar   = IDp,
                                   StudyIdVar     = IDs,
                                   TimeVar        = TIME,
                                   ScoreVar       = ADAS,
                                   is_pbo         = placebo,
                                   CovariatesR    = ~ AGE + SEX,
                                   CovariatesB    = ~ COMED,
                                   m_r            = 1,
                                   m_s            = 1,
                                   num_samples    = niter / 2,
                                   num_warmup     = niter / 2,
                                   seed           = 1)
  saveRDS(exe_samps, file = file_name)
}

RStan_summ <- apply(do.call(cbind, ext), 2, median)
exe_summ   <- apply(exe_samps$stan_model, 2, median)
data.frame(RStan_summ[-length(RStan_summ)], exe_summ[-c(1:7)])


# test 3 -----------------------------------------------------------------------
M <- length(unique(in_data$IDs))
stan_data <- list(
  N                = nrow(in_data),
  P                = length(unique(in_data$IDp)),
  M                = length(unique(in_data$IDs)),
  Q_r              = 0,
  Q_s              = 0,
  multiplicative_r = 0,
  multiplicative_s = 0,
  X_r              = matrix(data = 0, nrow = nrow(in_data), ncol = 0),
  X_s              = matrix(data = 0, nrow = nrow(in_data), ncol = 0),
  is_pbo           = c(0,0),
  IDp              = reindex(in_data$IDp),
  IDs              = reindex(in_data$IDs),
  time             = in_data$TIME,
  score            = in_data$ADAS
)
file_name <- paste0("./tests/generalized_logistic_model/saved_samples/test3_Rstan_small_niter",
                    niter,
                    ".rds")
if (file.exists(file_name)) {
  RStan_samps <- readRDS(file_name)
} else {
  set.seed(1)
  RStan_samps <- sampling(model_fit, stan_data, chains = 1, iter = niter, seed = 1)
  saveRDS(RStan_samps, file = file_name)
}
ext <- rstan::extract(RStan_samps)

# exe
file_name <- paste0("./tests/generalized_logistic_model/saved_samples/test3_exe_small_niter",
                    niter,
                    ".rds")
if (file.exists(file_name)) {
  exe_samps <- readRDS(file_name)
} else {
  set.seed(1)
  exe_samps <- sample_from_exe_new(df             = in_data,
                                   SubjectIdVar   = IDp,
                                   StudyIdVar     = IDs,
                                   TimeVar        = TIME,
                                   ScoreVar       = ADAS,
                                   is_pbo         = placebo,
                                   num_samples    = niter / 2,
                                   num_warmup     = niter / 2,
                                   seed           = 1)
  saveRDS(exe_samps, file = file_name)
}

RStan_summ <- apply(do.call(cbind, ext), 2, median)
exe_summ   <- apply(exe_samps$stan_model, 2, median)
data.frame(RStan_summ[-length(RStan_summ)], exe_summ[-c(1:7)])

