#Bootstrap resampling to generate 95% quantile intervals across a statistic

boot_mean <- function(original_vector, resample_vector) {
  mean(original_vector[resample_vector])
}

boot_years <- function(x){
  temp_res <- boot(data = as.numeric(x), boot_mean, R = 1000)
  as.matrix(paste0(as.numeric(x) %>% mean() %>% round(digits = 2), " (",
        temp_res$t %>% quantile(0.025) %>% round(digits = 2), "-",
        temp_res$t %>% quantile(0.975) %>% round(digits = 2), ") "),
        byrow = TRUE, nrow = 20)
}

boot_stats <- function(x){
  apply(x, 1, boot_years)
}
