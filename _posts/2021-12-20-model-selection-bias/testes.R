seq(2, 10)/seq(0.1, 0.9, by = 0.1)
lapply(seq(2, 10), function(x) x/seq(0.1, 0.9, by = 0.1))

future::plan(future::multisession)
progressr::with_progress({
  paginas <- 1:26
  p <- progressr::progressor(length(paginas))
  furrr::future_map(
    paginas,
    baixar_pagina,
    pasta = "wse",
    prog = p
  )
})

dir.create("sims_model_bias")

sim_bias_multi <- function(reps, p, n, SNR, b, corr) {
  
  
  Sigma <- matrix(corr, p, p)
  diag(Sigma) <- 1
  beta <- rep(b, p)
  names(beta) <- paste0("x", 1:p)
  b0 <- 1
  sigma_error <-  sqrt(as.numeric(crossprod(beta, Sigma %*% beta) / SNR))
  
  rsq <- NULL
  coefs <- tvals <- matrix(NA, nrow = reps, ncol = p)
  cover <- matrix(0, nrow = reps, ncol = p)
  colnames(coefs) <- paste0("x", 1:p)
  colnames(cover) <- paste0("x", 1:p)
  colnames(tvals) <- paste0("x", 1:p)
  
  for (i in seq(reps)) {
    
    X <-  MASS::mvrnorm(n = n, rep(0, p) , Sigma)
    y <- as.numeric(cbind(1, X) %*% c(b0, beta) + rnorm(n, 0, sigma_error))
    Xy <- as.data.frame( cbind(X, y))
    colnames(Xy) <- c(paste0("x", 1:p), "y")
    fit <- lm(y ~., data = Xy)
    sel <- step(fit, k = 2, trace = FALSE)
    s <- summary(sel)
    tval <- s$coefficients[,3][-1]
    tvals[i, names(tval)] <-  tval
    coefs[i, names(tval)] <- coef(sel)[-1]
    rsq[i] <- s$r.squared
    cis <- confint(sel)[-1,]
    if (length(cis) < 3) {
      cover[i,names(tval)] <- ifelse(cis[1] < beta[names(tval)] & cis[2] > beta[names(tval)], 1, 0)
    } else {
      cover[i,names(tval)] <- ifelse(cis[names(tval),1] < beta[names(tval)] & cis[names(tval),2] > beta[names(tval)], 1, 0)
    }
    
  }
  
  res <- list(coefs = coefs, tvals = tvals, cover = cover, bias = coefs - beta, mse = (coefs - beta)^2, rsq = rsq, corr = corr, p = p)
  
  saveRDS(res, file= paste0("sims_model_bias/sim_", p, "_", stringr::str_remove(corr, "\\.")))

}

set.seed(1991)
future::plan(future::multisession)
furrr::future_map(seq(2, 10), ~furrr::future_pmap(list(reps = 1000, p = .x, n = 100, SNR = 0.5, 1, corr = seq(0.1, 0.9, by = 0.1)),
                                sim_bias_multi))

furrr::future_pmap(list(reps = 1000, p = 9, n = 100, SNR = 0.5, 1, corr = seq(0.7, 0.9, by = 0.1)),
                   sim_bias_multi)

purrr::pmap(list(reps = 1000, p = 9, n = 100, SNR = 0.5, 1, corr = 0.9),
            sim_bias_multi)

sims <- lapply(paste0("sims_model_bias/", list.files("sims_model_bias")), readRDS)


sim_summary <- function(l) {
  
  df <- tibble::tibble(
    
    cor = l$corr,
    npred = l$p,
    predictor = colnames(l$cover),
    ratio = cor/npred,
    coverage = colMeans(l$cover),
    estimate = colMeans(l$coefs, na.rm = TRUE),
    bias = colMeans((l$coefs - 1), na.rm = TRUE),
    mse = colMeans((l$coefs - 1)^2, na.rm = TRUE),
    rsq = mean(l$rsq)
    
  )
  df
  
}


library(ggplot2)

simdf <- purrr::map_dfr(sims, sim_summary)

r2 <- simdf |> 
  dplyr::group_by(cor, npred) |> 
  dplyr::summarise(ratio = mean(ratio),
                   coverage = mean(coverage),
                   estimate = mean(estimate),
                   bias = mean(bias),
                   mse = mean(mse),
                   rsq = mean(rsq))
simdf |> 
  dplyr::group_by(cor, npred) |> 
  dplyr::summarise(ratio = mean(ratio),
                   coverage = mean(coverage),
                   estimate = mean(estimate),
                   bias = mean(bias),
                   mse = mean(mse)) |> 
  dplyr::filter(ratio > 0.15) |> 
  print(n = Inf)

ggplot(r2) +
  aes(x = ratio, y = rsq) +
  geom_point(shape = "circle", size = 1.5, colour = "#B22222") +
  labs(x = "Ratio", y = "R^2") +
  theme_minimal()
