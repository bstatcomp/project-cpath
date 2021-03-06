% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sampling_gpu.R
\name{sampling_gpu}
\alias{sampling_gpu}
\title{Run a cmdstan model}
\usage{
sampling_gpu(
  df,
  mod_name = "glm",
  SubjectIdVar = IDp,
  StudyIdVar = IDs,
  TimeVar = time,
  ScoreVar = score,
  is_pbo = placebo,
  CovariatesR = NULL,
  CovariatesB = NULL,
  m_r = 0,
  m_b = 0,
  ScoreVar2 = NULL,
  is_pbo2 = NULL,
  CovariatesR2 = NULL,
  CovariatesB2 = NULL,
  m_r2 = 0,
  m_b2 = 0,
  gpu_enabled = 1,
  init_list = NULL,
  num_samples = 1000,
  num_warmup = 1000,
  save_warmup = 0,
  thin = 1,
  engaged = 1,
  gamma = 0.05,
  delta = 0.8,
  kappa = 0.75,
  t0 = 10,
  init_buffer = 75,
  term_buffer = 50,
  window = 25,
  algorithm = "hmc",
  engine = "nuts",
  max_depth = 10,
  metric = "diag_e",
  metric_file = "",
  stepsize = 1,
  stepsize_jitter = 0,
  id = 0,
  seed = 1607674300,
  ...
)
}
\arguments{
\item{df}{Input data frame.}

\item{mod_name}{character. The name of the model. For now the only possible
value is "glm".}

\item{SubjectIdVar}{Name of the column which holds the subject IDs.
Must be without quotation marks.}

\item{StudyIdVar}{Name of the column which holds the study IDs.
Must be without quotation marks.}

\item{TimeVar}{Name of the column which holds the times.
Must be without quotation marks.}

\item{ScoreVar}{Name of the column which holds the scores (responses).
Must be without quotation marks.}

\item{is_pbo}{Name of the placebo column for score 1. Must be without 
quotation marks.}

\item{CovariatesR}{formula. Formula of the form ~ A + B + ..., where
the letters represent the names of columns of the covariates for rate.}

\item{CovariatesB}{formula. Formula of the form ~ A + B + ..., where
the letters represent the names of columns of the covariates for baseline.}

\item{m_r}{binary. 0 if the covariates for rate are additive, 1 if they
are multiplicative.}

\item{m_b}{binary. 0 if the covariates for baseline are additive, 1 if they
are multiplicative.}

\item{ScoreVar2}{Name of the column which holds the second scores
(responses) if applicable.
Must be without quotation marks.}

\item{is_pbo2}{Name of the placebo column for score 2. Must be without 
quotation marks.}

\item{CovariatesR2}{formula. Formula of the form ~ A + B + ..., where
the letters represent the names of columns of the covariates for rate.}

\item{CovariatesB2}{formula. Formula of the form ~ A + B + ..., where
the letters represent the names of columns of the covariates for baseline.}

\item{m_r2}{binary. 0 if the covariates for rate are additive, 1 if they
are multiplicative.}

\item{m_b2}{binary. 0 if the covariates for baseline are additive, 1 if they
are multiplicative.}

\item{gpu_enabled}{binary. 1 to use the GPU model, 0 to use the CPU model.}

\item{init_list}{list. The list of initialization parameters for the model.}

\item{num_samples}{integer. The number of samples.}

\item{num_warmup}{integer. The number of samples in the warmup phase of the
sampling.}

\item{seed}{integer. Random seed for the model.}

\item{...}{Other parameters for the sampler.}
}
\value{
A list with 4 elements. 
\item{stan_model}{An object of stanfit. Fitted model.}
\item{data_used}{The exact data that stan model used.}
\item{maps}{The maps from original subject and study IDs to the IDs used in the model.}
\item{pred_samples}{The predicted values for each iteration.}
}
\description{
Runs a cmdstan model. All parameters with the number 2 in the name are
only used in case of 2 scores.
}
