## Final project
## LI Chuyuan
## 01/01/2019

# Generate bootstrap resampled data in a data frame
#
# ARGUMENTS:
# n: the number of variates to sample
# x: a vector from which the variates will be sampled with replacement
#
# RETURN VALUE:
# A vector of bootstrap variates sampled from `x`
#
sample_bootstrap <- function(x, n) {
  sample(x, n, replace=TRUE)
}


# Function to generate N_SAMPLE times of difference in rt_SRC and rt_ORC for one region
## ARGUMENTS : 
##  - grouping_var is a region
##  - n: the number of variates to sample
##  - x: a vector from which the variates will be sampled with replacement
##  - statistic function sampling_bootstrap
##  - number of iteration N_SAMLPES
## RETURN:
##  - a data frame with two colomns: region, diff 
#
diff_from_sampling <- function(grouping_var, x, n, sample_bootstrap, N_SAMPLES){
  # create an empty dataframe to stock the bootstrap result
  df_concat <- tibble::tibble(region = character(), diff = character())
  for (i in 1:N_SAMPLES){
    bootstrap <- sample_bootstrap(x, n)
    result <- tibble::tibble(region = grouping_var, diff = bootstrap)
    df_concat <- rbind(df_concat, result)
  }
  return (df_concat)
}


# Function to generate N_SAMPLE times of difference in rt_SRC and rt_ORC in all regions
## ARGUMENTS : 
##  - a dataframe contains all the regions
##  - number of iteration N_SAMLPES
## RETURN:
##  - a data frame with two colomns: region, diff 
#
all_region_boots_sampling <- function(df, N_SAMPLES){
  return_val <- tibble::tibble(region = character(), diff = character())
  regions <- unique(df$region)

  for (r in regions) {
    grouping_var <- r
    sub_df <- df %>% dplyr::filter(region == grouping_var)
    x <- sub_df$diff_SRC_ORC
    n <- nrow(sub_df)
    return_val <- rbind(diff_from_sampling(r, x, n, sample_bootstrap, N_SAMPLES), return_val)
  }
  return (return_val)
}


# Function to record each decision of accept/reject H0 under different confident level
## ARGUMENTS : 
##  - a dataframe contains all the regions
##  - number of iteration N_SAMLPES
##  - a hypothesis mean: use for one sample t test
## RETURN:
##  - a data frame with three colomns: region, decision at conf.level 0.95, decision at conf.level 0.99
#
analysis_assessment <- function(df, N_SAMPLES, hypo_mean = 0){
  regions <- unique(df$region)
  decision_df <- tibble::tibble(region = character(), 
                                decision_05 = character(), decision_001 = character())
  
  for (r in regions) {
    grouping_var <- r
    sub_df <- df %>% dplyr::filter(region == grouping_var)
    x <- sub_df$diff_SRC_ORC
    n <- nrow(sub_df)
    
    for (i in 1:N_SAMPLES){
      bootstrap <- sample_bootstrap(x, n)
      pvalue <- t.test(bootstrap, mu = hypo_mean, conf.level = 0.95)$p.value
      deci_05 = ifelse(pvalue < 0.95, "reject H0", "accept H0")
      pvalue <- t.test(bootstrap, mu = hypo_mean, conf.level = 0.99)$p.value
      deci_001 = ifelse(pvalue < 0.99, "reject H0", "accept H0")
      result <- tibble::tibble(region = grouping_var, decision_05 = deci_05, decision_001 = deci_001)
      decision_df <- rbind(decision_df, result)
    }
  }
  return (decision_df)
}

