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
source("./R/data_for_Stan.R")

# data input -------------------------------------------------------------------
in_data <- readRDS("./data/generalized_logistic_model/data_df.rds")
set.seed(1)
is_pbo  <- rep(0, nrow(in_data))
is_pbo[in_data$IDs == 1]  <- 1
is_pbo2 <- rep(0, nrow(in_data))
is_pbo2[in_data$IDs == 2] <- 1
in_data <- cbind(in_data, "placebo" = is_pbo, "placebo2" = is_pbo2)


# Compile model ----------------------------------------------------------------

model_name  <- "new_model"
compiled_fn <- paste0("./tests/generalized_logistic_model/temp/", model_name, ".compiled.rds")

if (!file.exists(compiled_fn)) {
  model_fit   <- stan_model(file = paste0("./tests/generalized_logistic_model/Stan/", model_name, ".stan"))
  saveRDS(model_fit, file = compiled_fn)
} else {
  model_fit <- readRDS(compiled_fn)
}

# ------------------------------------------------------------------------------
b <- data_for_Stan(df             = in_data,
                         SubjectIdVar   = IDp,
                         StudyIdVar     = IDs,
                         TimeVar        = time,
                         ScoreVar       = S,
                         is_pbo         = placebo,
                         CovariatesR    = ~ AGE + SEX,
                         CovariatesB    = ~ COMED,
                         init_list      = in_init,
                         num_samples    = 10,
                         num_warmup     = 10,
                         seed           = 1
                         )
RStan_samps3 <- sampling(model_fit, b$data_list, chains = 1, iter = 200, seed = 1)
# saveRDS(RStan_samps3, file = "./tests/generalized_logistic_model/RStan_stan_output.rds")
ext <- rstan::extract(RStan_samps3)


b2 <- sample_from_exe_new(df            = in_data,
                         SubjectIdVar   = IDp,
                         StudyIdVar     = IDs,
                         TimeVar        = time,
                         ScoreVar       = S,
                         is_pbo         = placebo,
                         CovariatesR    = ~ AGE + SEX,
                         CovariatesB    = ~ COMED,
                         # init_list      = in_init,
                         num_samples    = 1000,
                         num_warmup     = 1000,
                         seed           = 1
)
# saveRDS(b2, file = "./tests/generalized_logistic_model/exe_stan_output.rds")

plot(density(b2$stan_model$tau))
plot(density(ext$tau))

plot(density(b2$stan_model$beta))
plot(density(ext$beta))

RStan_summ <- apply(do.call(cbind, ext), 2, median)
exe_summ   <- apply(b2$stan_model, 2, median)
data.frame(RStan_summ[-length(RStan_summ)], exe_summ[-c(1:7)])
