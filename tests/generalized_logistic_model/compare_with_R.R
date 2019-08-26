# WD, libraries, and sourcing --------------------------------------------------
my_path <- dirname(rstudioapi::getSourceEditorContext()$path)
my_path <- strsplit(my_path, split = "/")[[1]]
my_path <- paste0(my_path[-c(length(my_path) - 1, length(my_path))], collapse = "/")
setwd(my_path)
library(loo)
library(rstan)
source("./data/generalized_logistic_model/init.R")
source("./R/sample_from_exe_new.R")
source("./R/df_to_list_new.R")

# data input -------------------------------------------------------------------
in_data <- readRDS("./data/generalized_logistic_model/data_df.rds")
set.seed(1)
is_pbo  <- rep(0, nrow(in_data))
is_pbo[in_data$IDs == 1]  <- 1
is_pbo2 <- rep(0, nrow(in_data))
is_pbo2[in_data$IDs == 2] <- 1
in_data <- cbind(in_data, "placebo" = is_pbo, "placebo2" = is_pbo2)
in_init <- nlist(theta_S0,
                 theta_r,
                 theta_SEX,
                 theta_AGE,
                 theta_APOE4_b,
                 theta_APOE4_r,
                 theta_COMED,
                 omega_pb,
                 omega_pr,
                 omega_sb,
                 omega_sr,
                 eta_pb,
                 eta_pr,
                 eta_sb,
                 eta_sr,
                 tau,
                 beta,
                 kel,
                 keq,
                 beta_bateman)

# Compile model ----------------------------------------------------------------

model_name  <- "new_model"
compiled_fn <- paste0("./tests/generalized_logistic_model/temp/", model_name, ".compiled.rds")

if (!file.exists(compiled_fn)) {
  model_fit   <- stan_model(file = paste0("./tests/generalized_logistic_model/Stan/", model_name, ".stan"))
  saveRDS(model_fit, file = compiled_fn)
} else {
  model_fit <- readRDS(compiled_fn)
}

# # RStan ------------------------------------------------------------------------
# b <- sample_from_exe_new(df             = in_data,
#                          SubjectIdVar   = IDp,
#                          StudyIdVar     = IDs,
#                          TimeVar        = time,
#                          ScoreVar       = S,
#                          is_pbo         = placebo,
#                          CovariatesR    = ~ AGE + SEX,
#                          CovariatesB    = ~ COMED,
#                          init_list      = in_init,
#                          num_samples    = 100,
#                          num_warmup     = 100)
# b2 <- list(
#   N                = nrow(in_data),
#   P                = length(unique(in_data$IDp)),
#   M                = length(unique(in_data$IDs)),
#   Q_r              = 2,
#   Q_s              = 1,
#   multiplicative_s = 0,
#   multiplicative_r = 0,
#   X_r              = as.matrix(in_data[ ,c("AGE", "SEX")]),
#   X_s              = as.matrix(in_data[ ,"COMED"]),
#   is_pbo           = c(1, rep(0, times = 14)),
#   IDp              = in_data$IDp,
#   IDs              = in_data$IDs,
#   time             = in_data$time,
#   score            = in_data$S
# )
# 
# RStan_samps  <- sampling(model_fit, b, chains = 1, iter = 20, seed = 1)
# RStan_samps2 <- sampling(model_fit, b2, chains = 1, iter = 20, seed = 1)


# Some study missing -----------------------------------------------------------
in_data2 <- in_data
in_data2[in_data2$IDs == 2, "S"] <- NA
b3 <- sample_from_exe_new(df             = in_data2,
                          SubjectIdVar   = IDp,
                          StudyIdVar     = IDs,
                          TimeVar        = time,
                          ScoreVar       = S,
                          is_pbo         = placebo,
                          CovariatesR    = ~ AGE + SEX,
                          CovariatesB    = ~ COMED,
                          init_list      = in_init,
                          num_samples    = 100,
                          num_warmup     = 100)
stan_data <- b3[[1]]
RStan_samps3 <- sampling(model_fit, stan_data, chains = 1, iter = 20, seed = 1)
# ext <- rstan::extract(RStan_samps3)
# dim(ext$eta_ps)
# ext$eta_ps
# ext$eta_ss
# plot(density(ext$eta_ss[ ,2]))
