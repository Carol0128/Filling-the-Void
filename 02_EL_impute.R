library(ordinal)

# the datasets used here are loaded to the environment once the first 
# code block (preprocessing and eda) is run

#:::::::::::::IMPUTATION::::::::::::::::::::

# Step 1: NAP for under 15 years; 

# preliminary check if there are groups in df_missing not yet observed
rows_in_missing_only <- anti_join(df_missing, df_observed, 
                                  by = c("GBD3_RED4", "SEX", "REGIOJ", "AGEGRP"))
nrow(rows_in_missing_only)

## Probabilistic Imputation
prob_impute <- function(data){
  #STEP1: NAP for under 15 years
  data <- data %>% 
    mutate(CD_ISCED_CENSUS_IMP1 = ifelse(is.na(CD_ISCED_CENSUS) &
                                           AGEGRP %in% c("[0,5)", "[5,15)"), 
                                         "NAP", as.character(CD_ISCED_CENSUS)))
  
  # column for imputed values in Step 2
  data$CD_ISCED_CENSUS_IMP2 <- as.character(data$CD_ISCED_CENSUS_IMP1)
  
  
  #records for which Ed level is not missing after step 1
  df_observed_imp1 <- data %>% filter(!is.na(CD_ISCED_CENSUS_IMP1))
  #records for which Ed level is missing
  df_missing_imp1 <- data %>% filter(is.na(CD_ISCED_CENSUS_IMP1)) 
  
  # Calculating the probabilities
  ## 1. p(C | e, X)
  p_cod_given_EX <- df_observed_imp1 %>%
    group_by(GBD3_RED4, CD_ISCED_CENSUS_IMP1, AGEGRP, REGIOJ, SEX) %>%
    summarise(n_cex = n(), .groups = "drop") %>%
    group_by(CD_ISCED_CENSUS_IMP1, AGEGRP, REGIOJ, SEX) %>%
    mutate(m_cex = sum(n_cex), p_cod_eX = n_cex / sum(n_cex))
  
  ## 2. p(e | X)
  p_E_given_X <- df_observed_imp1 %>%
    group_by(CD_ISCED_CENSUS_IMP1, AGEGRP, REGIOJ, SEX) %>%
    summarise(n_ex = n(), .groups = "drop") %>%
    group_by(AGEGRP, REGIOJ, SEX) %>%
    mutate(m_ex = sum(n_ex), p_e_X = n_ex / sum(n_ex)) 
  
  ## 3. p(C | X)
  p_cod_given_X <- df_observed_imp1 %>%
    group_by(GBD3_RED4, AGEGRP, REGIOJ, SEX) %>%
    summarise(n_cx = n(), .groups = "drop") %>%
    group_by(AGEGRP, REGIOJ, SEX) %>%
    mutate(m_cx = sum(n_cx), p_cod_X = n_cx / sum(n_cx))
  
  ## I-1 Calculating the posterior p(E|C, X) from the 3 components
  posterior_df <- p_cod_given_EX %>%
    left_join(p_E_given_X, by = c("REGIOJ", "AGEGRP", "SEX", 
                                  "CD_ISCED_CENSUS_IMP1")) %>%
    left_join(p_cod_given_X, by = c("REGIOJ", "AGEGRP", "SEX", "GBD3_RED4")) %>%
    mutate(p_E_given_all = (p_cod_eX * p_e_X) / p_cod_X)
  
  
  # create a list of lists with each EL and it prob for every COD,X combo
  posterior_summary <- posterior_df %>% group_by(GBD3_RED4, AGEGRP, SEX, 
                                                 REGIOJ) %>%
    summarise(group_size = mean(n_cx), ed_prob_list =
                list(purrr::map2(as.character(CD_ISCED_CENSUS_IMP1),
                                 p_E_given_all, ~ list(ED = .x, p = .y))), .groups = "drop")
  
  ## II. Fallback probs when there are missing sets or sparse groups
  p_E_given_age <- df_observed_imp1 %>%
    group_by(CD_ISCED_CENSUS_IMP1, AGEGRP) %>% summarise(n = n(), .groups = "drop") %>%
    group_by(AGEGRP) %>% mutate(m = sum(n), p_E_age = n/sum(n)) %>%
    summarise(ed_prob_list = list(purrr::map2(as.character(CD_ISCED_CENSUS_IMP1),
                                              p_E_age, ~ list(ED = .x, p = .y))), .groups = "drop")
  
  #Imputation begins here
  for (i in 1:nrow(df_missing_imp1)){
    row <- df_missing_imp1[i,]
    
    #find matching group from the posterior summary table
    match_prob <- posterior_summary %>% filter(GBD3_RED4 == row$GBD3_RED4,
                                               REGIOJ == row$REGIOJ, AGEGRP == row$AGEGRP, SEX == row$SEX)
    stratum_size <- match_prob$group_size
    
    cat("Imputing row", i, "\n")
    # If match is found
    if (nrow(match_prob) > 0 && stratum_size > 9) {
      ed_prob_list <- match_prob$ed_prob_list[[1]]
      
      # Extracting Ed level and probs to simplify sampling step
      ed_levels <- sapply(ed_prob_list, function(x) x$ED)
      probs <- sapply(ed_prob_list, function(x) x$p)
      
      # Randomly sample Education level
      selected_ED <- sample(ed_levels, 1, prob = as.numeric(probs))
    }
    
    # If there is no match:
    else{
      match_fallback <- p_E_given_age %>% filter(AGEGRP == row$AGEGRP)
      
      ed_prob_list2 <- match_fallback$ed_prob_list[[1]]
      ed_levels2 <- sapply(ed_prob_list2, function(x) x$ED)
      probs2 <- sapply(ed_prob_list2, function(x) x$p)
      
      selected_ED <- sample(ed_levels2, 1, prob = probs2)
    }
    
    # impute the selected one in the initial dataset
    df_missing_imp1[i, "CD_ISCED_CENSUS_IMP1"] <- selected_ED
    
  }
  
  df_full <- rbind(df_observed_imp1, df_missing_imp1)
  
  #categorization for low, medium, high
  df_full <- df_full %>% mutate(CD_ISCED_CENSUS2_IMP2 = 
                                  ifelse(CD_ISCED_CENSUS_IMP2 == "NAP", "NAP",
                                         ifelse(CD_ISCED_CENSUS_IMP2 %in% c(0,1,2), "Low",
                                                ifelse(CD_ISCED_CENSUS_IMP2 %in% c(3,4), "Medium", "High"))))%>%
    mutate(CD_ISCED_CENSUS2_IMP2 = factor(CD_ISCED_CENSUS2_IMP2,
                                          levels = c("Low", "Medium", "High", "NAP"), ordered = T))
  
  
  return(df_full)  
  
}

# Implementation on the first dataframe
prob_imputed_df <- prob_impute(df)


## Model based Imputation
model_impute <- function(data){
  #STEP1: NAP for under 15 years
  data <- data %>% 
    mutate(CD_ISCED_CENSUS2_IMP1 = ifelse(is.na(CD_ISCED_CENSUS_2) &
                                            AGEGRP %in% c("[0,5)", "[5,15)"), 
                                          "NAP", as.character(CD_ISCED_CENSUS_2)))
  
  # column for imputed values in Step 2
  data$CD_ISCED_CENSUS2_IMP2 <- as.character(data$CD_ISCED_CENSUS2_IMP1)
  
  #split observed and missing subsets
  df_obs2 <- data %>% filter(!is.na(CD_ISCED_CENSUS2_IMP1))
  #records for which Ed level is missing
  df_miss2 <- data %>% filter(is.na(CD_ISCED_CENSUS2_IMP1))
  
  # dummies for the predictor variables; to make calculation of eta easy
  df_missing_copy <- df_miss2 %>% transmute(GBD3_RED4,
                                            SEXF = ifelse(SEX == "F",1,0),
                                            REGIOJFL = ifelse(REGIOJ == "FL", 1, 0),
                                            REGIOJWA = ifelse(REGIOJ == "WA", 1, 0),
                                            `AGEGRP[45,65)` = ifelse(AGEGRP == "[45,65)",1,0),
                                            `AGEGRP[65,85)` = ifelse(AGEGRP == "[65,85)",1,0),
                                            `AGEGRP[85,Inf)` = ifelse(AGEGRP == "[85,Inf)",1,0))
  
  model.fit <- clmm2(CD_ISCED_CENSUS_2 ~ REGIOJ + SEX, 
                     nominal = ~ AGEGRP, random = GBD3_RED4,
                     data = df_obs2, Hess = TRUE)
  
  m2 <- clmm(CD_ISCED_CENSUS_2 ~ (1 | GBD3_RED4), data = df_obs2)
  
  # Fixed effects for proportional variables 
  po_betas <- model.fit$beta
  
  X_po_cols <- names(po_betas)
  X_po_fixed <- as.matrix(df_missing_copy[, X_po_cols])
  
  eta_po_fixed <- X_po_fixed %*% po_betas
  
  #Matrix with alphas and beta-coefficients for the uncommon slopes
  theta_matrix <- model.fit$Theta
  
  #-- extract cumulative thresholds (alpha_j) as an array
  alpha <- matrix(theta_matrix["(Intercept)", ], nrow = 1)
  
  # effects for non proportional variables
  beta_age_low <- -1*as.vector(t(theta_matrix[-1, 1]))
  beta_age_medium <- -1*as.vector(t(theta_matrix[-1, 2]))
  
  X_npo_fixed <- as.matrix(df_missing_copy[, 5:7])
  
  eta_age_low <- X_npo_fixed %*% beta_age_low
  eta_age_medium <- X_npo_fixed %*% beta_age_medium
  
  
  # Random effects
  re <- model.fit$ranef
  re_df <- data.frame(GBD3_RED4 = rownames(ranef(m2)$GBD3_RED4), RE_Int = re)
  
  # left join re_df with our df_missing_copy
  df_missing_copy <- df_missing_copy %>%left_join(re_df, by = "GBD3_RED4")
  b_i <- df_missing_copy$RE_Int # random effects for COD in missing df
  
  # RHS linear predictors
  eta_low <- eta_po_fixed + eta_age_low + b_i
  eta_medium <- eta_po_fixed + eta_age_medium + b_i
  
  # Cumulative probabilities
  cumprobs_low <- sapply(alpha[1], function(x) plogis(x - eta_low))
  cumprobs_medium <- sapply(alpha[2], function(x) plogis(x - eta_medium))
  
  # probabilities for each category
  probs <- matrix(NA, nrow = length(eta_low), ncol = 3)
  colnames(probs) <- c("Low", "Medium", "High")
  probs[, 1] <- cumprobs_low
  probs[, 2] <- cumprobs_medium - cumprobs_low
  probs[, 3] <- 1 - cumprobs_medium
  
  return(list(missing = df_miss2, observed = df_obs2,
              probs = probs))
}

categories <- c("Low", "Medium", "High")


model_output <- model_impute(df)
model_probs <- model_output$probs
model_df_miss <- model_output$missing # df that will be imputed
model_df_obs <- model_output$observed # completely observed df, to be merged with imputed

#sampling 1 category per row using the probabilities
set.seed(9020)
el_sampled <- apply(model_probs, 1, function(p) {
  sample(categories, size = 1, prob = p) })
model_df_miss$CD_ISCED_CENSUS2_IMP2 <- el_sampled #impute
model_imputed_df <- rbind(model_df_obs, model_df_miss)


##:::::::::EVALUATION::::::::::::::::

prob_impute <- readRDS("prob_imputed_df.rds")
df1 <- prob_impute[[1]]

df1 %>% group_by(CD_ISCED_CENSUS2_IMP2) %>% summarise(n = n())
df1 %>% filter(R_ind == 0) %>% group_by(CD_ISCED_CENSUS2_IMP2) %>% summarise(n = n())

tbl_summary <- df1 %>%
  filter(!AGEGRP %in% c('[0,5)', '[5,15)')) %>%
  count(CD_ISCED_CENSUS_IMP1, AGEGRP, R_ind) %>%
  group_by(R_ind) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()


ggplot(tbl_summary, aes(x = CD_ISCED_CENSUS_IMP1, y = prop, fill = AGEGRP)) +
  geom_bar(stat = "identity", position = "dodge")+
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Education Level", y = "Proportion", fill = "Age Group") +
  facet_wrap(~R_ind, labeller = labeller(R_ind = c("1" = "Complete subset", 
                                                   "0" = "Imputed subset")))

model_impute <- readRDS("model_imputed_df.rds")
df2 <- model_impute[[1]]

df2 %>% group_by(CD_ISCED_CENSUS2_IMP2) %>% summarise(n = n())
df2 %>% filter(R_ind == 0) %>% group_by(CD_ISCED_CENSUS2_IMP2) %>% summarise(n = n())
