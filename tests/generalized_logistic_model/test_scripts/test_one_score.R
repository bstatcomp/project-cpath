rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")
setwd("..")
setwd("..")

library(Rcpp)
library(digest)
library(mcmcse)
library(rstan)
library(dplyr)
library(tidyr)
library(ggplot2)
library(GLMCPath)
source("./tests/generalized_logistic_model/test_scripts/support_scripts/model_wrappers.R")

SKIP_MISSING <- F
fo           <- F

# Setup ------------------------------------------------------------------------
datasets <- list.files("./data/generalized_logistic_model/tests/one_score/", pattern = ".rds", full.names = T)


models   <- list(
  model1 = list(name = "one_score", func = "model_glm_one_score")
  , model6 = list(name = "glm_one_score_delta_packed_CPU", func = "glm_one_score_delta_packed_CPU")
  , model7 = list(name = "glm_one_score_delta_packed_GPU", func = "glm_one_score_delta_packed_GPU")
)



seeds    <- c(1, 20, 2535, 4467, 156)  # the number of these also determines the number of repetitions
iter     <- 200
warmup   <- 200

design <- expand.grid(modl = 1:length(models), data = 1:length(datasets), seed = 1:length(seeds))


test <- function(k) {
  print(k)
  model <- models[[design[k,1]]]
  dataset <- datasets[design[k,2]]
  seed <- seeds[design[k,3]]  
  
  run_string <- digest(list(model$name, dataset, seed, iter, warmup))
  dat <- readRDS(dataset)
  # Run model ------------------------------------------------------------------
  fn <- paste0("./tests/generalized_logistic_model/test_scripts/temp/", run_string,".rds")
  if (file.exists(fn) & (fo == FALSE)) {
    output <- readRDS(fn)
  } else {
    if (SKIP_MISSING & (fo == FALSE)) {
      res <- data.frame(k = k, run_string, model$name, dataset, seed, iter, warmup, 
                        initialized = NA,
                        Time = NA,
                        MedianESS = NA, 
                        Q005ESS = NA,
                        TotalCor = NA,
                        PPCorMu = NA,
                        PPCorMe = NA,
                        PPCor005 = NA,
                        par_tau1_mean = NA,
                        par_tau1_sd = NA,
                        par_study01_r_mean = NA,
                        par_study01_r_sd = NA,
                        par_study02_r_mean = NA,
                        par_study02_r_sd = NA,
                        par_study10_r_mean = NA,
                        par_study10_r_sd = NA,
                        par_beta_pbo1_mean = NA,
                        par_beta_pbo1_sd = NA)
      return (res)
    }
    
    t_start <- Sys.time()
    output <- get(model$func)(dat_list = dat, seed = seed, iter = iter, warmup = warmup)
    t_elaps <- Sys.time() - t_start
    
    saveRDS(list(model = output, runtime = t_elaps), fn)
    output <- list(model = output, runtime = t_elaps)
  }
  
  
  if (length(output$model) > 4) { # if we have the R model
    my_mat  <- output$model
    t_elaps <- output$runtime
    smp <- my_mat[,1,]
    
    # MedianESS <- median(ess(smp, method = "bartlett"), na.rm = T)
    # Q005ESS   <- quantile(ess(smp, method = "bartlett"), 0.05, na.rm = T)
    MedianESS <- 1 # temporary, until we fix the ess calculation
    Q005ESS   <- 1
    
    post_means  <- colMeans(smp[,grep(pattern = "score1_pred", colnames(smp), fix = T)])
    orig_data   <- dat$stan_data$score1
    
    df_smp  <- data.frame(smp)
    all_cor <- c()
    for (p in 1:dat$stan_data$P) {
      all_cor <- c(all_cor, cor(post_means[dat$stan_data$IDp == p], 
                                dat$stan_data$score1[dat$stan_data$IDp == p]))
    }
    
  } else {
    if (is.null(output$model$stan_model)) {
      res <- data.frame(k = k, run_string, model$name, dataset, seed, iter, warmup, 
                        initialized = F,
                        Time = as.numeric(output$runtime, units = "mins"),
                        MedianESS = NA, 
                        Q005ESS = NA,
                        TotalCor = NA,
                        PPCorMu = NA,
                        PPCorMe = NA,
                        PPCor005 = NA,
                        par_tau1_mean = NA,
                        par_tau1_sd = NA,
                        par_study01_r_mean = NA,
                        par_study01_r_sd = NA,
                        par_study02_r_mean = NA,
                        par_study02_r_sd = NA,
                        par_study10_r_mean = NA,
                        par_study10_r_sd = NA,
                        par_beta_pbo1_mean = NA,
                        par_beta_pbo1_sd = NA)
      return (res)
    }
    my_mat  <- rstan::extract(output$model$stan_model, permuted = F)
    t_elaps <- output$runtime
    smp     <- my_mat[,1,]
    psmp    <- output$model$pred_samples
    X       <- psmp[ ,-(1:10)]
    preds   <- apply(X, 1, mean)
    tmp_df  <- cbind(psmp[ ,c("IDp", "IDs", "time", "scoreN")], "pred" = preds)
    tmp_df$scoreN[tmp_df$scoreN == 1] <- "S1"
    
    dat_eval <- dat$table_data %>%
      select(IDp, IDs, S1, S2, time) %>%
      gather(key = scoreN, value = orig_val, S1)
    
    jdf <- left_join(dat_eval, tmp_df, by = c("IDp", "IDs", "time", "scoreN"))
    
    # MedianESS <- median(ess(smp, method = "bartlett"), na.rm = T)
    # Q005ESS   <- quantile(ess(smp, method = "bartlett"), 0.05, na.rm = T)
    MedianESS <- 1 # temporary, until we fix the ess calculation
    Q005ESS   <- 1
    
    df_smp  <- data.frame(smp)
    all_cor <- c()
    for (p in unique(jdf$IDp)) {
      for (sc in unique(jdf$scoreN)) {
        tmp_df <- jdf %>%
          filter(IDp == p, scoreN == sc, !is.na(orig_val))
        all_cor <- c(all_cor, cor(tmp_df$orig_val, tmp_df$pred))
      }
    }
    post_means <- jdf$pred[!is.na(jdf$pred)]
    orig_data  <- jdf$orig_val[!is.na(jdf$pred)]
    
  }
  
  # combine results
  res <- data.frame(k = k, run_string, model$name, dataset, seed, iter, warmup,
                    initialized = T,
                    Time = as.numeric(t_elaps, units = "mins"),
                    MedianESS, Q005ESS,
                    TotalCor = cor(post_means, orig_data),
                    PPCorMu = mean(all_cor),
                    PPCorMe = median(all_cor),
                    PPCor005 = quantile(all_cor, 0.05, na.rm = T),
                    par_tau1_mean = mean(df_smp$tau1),
                    par_tau1_sd = sd(df_smp$tau1),
                    par_study01_r_mean = mean(df_smp$eta_sr1.1.),
                    par_study01_r_sd = sd(df_smp$eta_sr1.1.),
                    par_study02_r_mean = mean(df_smp$eta_sr1.2.),
                    par_study02_r_sd = sd(df_smp$eta_sr1.2.),
                    par_study10_r_mean = mean(df_smp$eta_sr1.10.),
                    par_study10_r_sd = sd(df_smp$eta_sr1.10.),
                    par_beta_pbo1_mean = mean(df_smp$beta_pbo1),
                    par_beta_pbo1_sd = sd(df_smp$beta_pbo1))
  
  return (res)
}


## Run -------------------------------------------------------------------------
out_list <- list()
for (k in 1:nrow(design)) {
  out_list[[k]] <- test(k)
}
out_df <- do.call(rbind, out_list) %>%
  arrange(dataset, seed, model.name)
head(out_df)

