#libraries
library(lme4)
library(ordinal)

# the datasets used here are loaded to the environment once the first 
# code block (preprocessing and eda) is run

##:::::::::::Ordinal Logistic Regression::::::::::::::

pred_vars <- c("SEX", "AGEGRP", "REGIOJ")

# Each variable on its own
onevar_olr_models <- list()
onevar_olr_results <- list()

for (v in pred_vars) {
  formula <- as.formula(paste("CD_ISCED_CENSUS_2 ~", v))
  model <- clm(formula, data = df_observed)
  onevar_olr_models[[paste(v)]] <- model
  onevar_olr_results[[paste(v)]] <- summary(model)
}

# Pairs of variables
two_var_combos <- list("SEX + AGEGRP", "SEX + REGIOJ", "REGIOJ+AGEGRP")

twovar_olr_models <- list()
twovar_olr_results <- list()

for (v in two_var_combos) {
  formula <- as.formula(paste("CD_ISCED_CENSUS_2 ~ +", v))
  model <- clm(formula, data = df_observed)
  twovar_olr_models[[paste(v)]] <- model
  twovar_olr_results[[paste(v)]] <- summary(model)
}

# three variables
threevar_olr_model <- clm(CD_ISCED_CENSUS_2 ~ SEX + AGEGRP + REGIOJ, 
                          data = df_observed)
threevar_olr_result <- summary(threevar_olr_model)

## Models with GBD3

# random effects only model
gbd3_olr_model <- clmm(CD_ISCED_CENSUS_2 ~ (1|GBD3_RED4), data = df_observed)
gbd3_olr_result <- summary(gbd3_olr_model)

# GBD3 with one var
twovar_olr_re_model <- list()
twovar_olr_re_results <- list()

for (v in pred_vars) {
  formula <- as.formula(paste("CD_ISCED_CENSUS_2 ~ (1|GBD3_RED4) +", v))
  model <- clmm(formula, data = df_observed)
  twovar_olr_re_model[[paste(v)]] <- model
  twovar_olr_re_results[[paste(v)]] <- summary(model)
}

# GBD3 with two vars
threevar_olr_re_model <- list ()
threevar_olr_re_results <- list()

for (v in two_var_combos) {
  formula <- as.formula(paste("CD_ISCED_CENSUS_2 ~ (1|GBD3_RED4)+", v))
  model <- clmm(formula, data = df_observed)
  threevar_olr_re_model[[paste(v)]] <- model
  threevar_olr_re_results[[paste(v)]] <- summary(model)
}

# ALL variables
allvars_olr_model <- clmm(CD_ISCED_CENSUS_2 ~ SEX + AGEGRP + REGIOJ +
                            (1|GBD3_RED4), data = df_observed)
allvars_olr_results <- summary(allvars_olr_model)

##:::Check for PO assumption
#creating bin1 and bin2 variables
df_observed <- df_observed %>% mutate(
  bin1 = ifelse(CD_ISCED_CENSUS_2 == "Low", 0, 1),
  bin2 = ifelse(CD_ISCED_CENSUS_2 == "High", 1, 0))

# Each variable on its own
onevar_glm_models1 <- list() #fit for bin1
onevar_glm_results1 <- list()
onevar_glm_models2 <- list() #fit for bin2
onevar_glm_results2 <- list()

for (v in pred_vars) {
  formula1 <- as.formula(paste("bin1 ~", v))
  model1 <- glm(formula1, family = binomial, data = df_observed)
  onevar_glm_models1[[paste(v)]] <- model1
  onevar_glm_results1[[paste(v)]] <- summary(model1)
  
  #bin2
  formula2 <- as.formula(paste("bin2 ~", v))
  model2 <- glm(formula2, family = binomial, data = df_observed)
  onevar_glm_models2[[paste(v)]] <- model2
  onevar_glm_results2[[paste(v)]] <- summary(model2)
}

# Pairs of variables
twovar_glm_model1 <- list()
twovar_glm_result1 <- list()
twovar_glm_model2 <- list()
twovar_glm_result2 <- list()

for (v in two_var_combos) {
  formula1 <- as.formula(paste("bin1 ~ +", v))
  model1 <- glm(formula1, family = binomial, data = df_observed)
  twovar_glm_model1[[paste(v)]] <- model1
  twovar_glm_result1[[paste(v)]] <- summary(model1)
  
  # bin2
  formula2 <- as.formula(paste("bin2 ~ +", v))
  model2 <- glm(formula2, family = binomial, data = df_observed)
  twovar_glm_model2[[paste(v)]] <- model2
  twovar_glm_result2[[paste(v)]] <- summary(model2)
}

# three variables
threevar_glm_model1 <- glm(bin1 ~ SEX + AGEGRP + REGIOJ, family = binomial,
                           data = df_observed)
threevar_glm_result1 <- summary(threevar_glm_model1)

threevar_glm_model2 <- glm(bin2 ~ SEX + AGEGRP + REGIOJ, family = binomial,
                           data = df_observed)
threevar_glm_result2 <- summary(threevar_glm_model2)

## Models with GBD3

# random effects only model
gbd3_glm_model1 <- glmer(bin1 ~ (1|GBD3_RED4), 
                         family = binomial, data = df_observed)
gbd3_glm_result1 <- summary(gbd3_glm_model1)

gbd3_glm_model2 <- glmer(bin2 ~ (1|GBD3_RED4), 
                         family = binomial, data = df_observed)
gbd3_glm_result2 <- summary(gbd3_glm_model2)

# GBD3 with one var
twovar_glm_re_model1 <- list()
twovar_glm_re_result1 <- list()
twovar_glm_re_model2 <- list()
twovar_glm_re_result2 <- list()

for (v in pred_vars) {
  formula1 <- as.formula(paste("bin1 ~ (1|GBD3_RED4) +", v))
  model1 <- glmer(formula1, family = binomial, data = df_observed)
  twovar_glm_re_model1[[paste(v)]] <- model1
  twovar_glm_re_result1[[paste(v)]] <- summary(model1)
  
  #bin2
  formula2 <- as.formula(paste("bin2 ~ (1|GBD3_RED4) +", v))
  model2 <- glmer(formula2, family = binomial, data = df_observed)
  twovar_glm_re_model2[[paste(v)]] <- model2
  twovar_glm_re_result2[[paste(v)]] <- summary(model2)
}

# GBD3 with two vars
threevar_glm_re_model1 <- list ()
threevar_glm_re_result1 <- list()
threevar_glm_re_model2 <- list ()
threevar_glm_re_result2 <- list()

for (v in two_var_combos) {
  formula1 <- as.formula(paste("bin1 ~ (1|GBD3_RED4)+", v))
  model1 <- glmer(formula1, family = binomial, data = df_observed)
  threevar_glm_re_model1[[paste(v)]] <- model1
  threevar_glm_re_result1[[paste(v)]] <- summary(model1)
  
  #bin2
  formula2 <- as.formula(paste("bin2 ~ (1|GBD3_RED4)+", v))
  model2 <- glmer(formula2, family = binomial, data = df_observed)
  threevar_glm_re_model2[[paste(v)]] <- model2
  threevar_glm_re_result2[[paste(v)]] <- summary(model2)
  
}

# ALL variables
allvars_glm_model1 <- glmer(bin1 ~ SEX + AGEGRP + REGIOJ + (1|GBD3_RED4), 
                            family = binomial, data = df_observed)
allvars_glm_result1 <- summary(allvars_glm_model1)

allvars_glm_model2 <- glmer(bin2 ~ SEX + AGEGRP + REGIOJ + (1|GBD3_RED4), 
                            family = binomial, data = df_observed)
allvars_glm_result2 <- summary(allvars_glm_model2)


# partial po models for models whose PO assumption is violated

# 1. GBD3 + agegroup
#for starting values
age_only <- clm(CD_ISCED_CENSUS_2 ~ 1, data = df_observed) 
age_gbd3_model <- clmm2(CD_ISCED_CENSUS_2 ~ 1, nominal = ~ AGEGRP,
                        random = GBD3_RED4, data = df_observed,
                        start = c(age_only$alpha ,rep(0,6),log(0.3122)),
                        control = clmm2.control(method = "nlminb"),  Hess = TRUE)

# 2.1 Sex + Agegroup
# for starting values
sex_only <- clm(CD_ISCED_CENSUS_2 ~ SEX, data = df_observed)
sex_age_model <- clm2(CD_ISCED_CENSUS_2 ~ SEX, nominal = ~ AGEGRP,
                      data = df_observed, 
                      start = c(sex_only$alpha, sex_only$beta ,rep(0,6)),
                      Hess = TRUE)

# 2.2 Sex,age and gbd3
sex_gbd3_only <- clmm(CD_ISCED_CENSUS_2 ~ SEX + (1|GBD3_RED4),
                      data = df_observed)

sex_age_gbd3_model <-  clmm2(CD_ISCED_CENSUS_2 ~ SEX, nominal = ~ AGEGRP,
                             random = GBD3_RED4, data = df_observed, 
                             start = c(sex_gbd3_only$alpha, sex_gbd3_only$beta ,rep(0,6), log(0.2897)),
                             control = clmm2.control(method = "nlminb"), Hess = TRUE)


# 3.1 Age + Region
# for starting values
region_only <- clm(CD_ISCED_CENSUS_2 ~ REGIOJ, data = df_observed)
region_age_model <- clm2(CD_ISCED_CENSUS_2 ~ REGIOJ, nominal = ~ AGEGRP,
                         data = df_observed, 
                         start = c(region_only$alpha, region_only$beta ,rep(0,6)),
                         Hess = TRUE)


# 3.2  Age, Region, GBD3
region_gbd3 <- clmm(CD_ISCED_CENSUS_2 ~ REGIOJ + (1|GBD3_RED4), data = df_observed)

region_age_model <- clmm2(CD_ISCED_CENSUS_2 ~ REGIOJ, nominal = ~ AGEGRP,
                          random = GBD3_RED4, data = df_observed, 
                          start = c(region_gbd3$alpha, region_gbd3$beta ,rep(0,6),log(0.2015)),
                          Hess = TRUE)

# 4.1 Sex, age, region
sex_region <- clm(CD_ISCED_CENSUS_2 ~ REGIOJ + SEX, data = df_observed)

all_vars_model <- clm2(CD_ISCED_CENSUS_2 ~ REGIOJ + SEX, nominal = ~ AGEGRP,
                       data = df_observed, 
                       start = c(sex_region$alpha, sex_region$beta ,rep(0,6)),
                       Hess = TRUE)

# 4.2 All vars + gbd3
sex_region_gbd3 <- clmm(CD_ISCED_CENSUS_2 ~ REGIOJ + SEX + (1|GBD3_RED4),
                        data = df_observed)

all_vars_ppo_model <- clmm2(CD_ISCED_CENSUS_2 ~ REGIOJ + SEX, 
                            nominal = ~ AGEGRP, random = GBD3_RED4,
                            data = df_observed, Hess = TRUE)


m2 <- clmm(CD_ISCED_CENSUS_2 ~ REGIOJ + SEX +(1 | GBD3_RED4) ,
           nominal = ~ AGEGRP,
           data = df_observed, Hess = TRUE)

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

# Implementation on 10 dataframes
prob_imputed_df <- list()

for (i in 1:10) {
  df_i <- data_preprocess(rds_dfs, i)
  
  prob_imputed_df[[i]] <- prob_impute(df_i)
  cat("Imputation for dataframe", i, "complete\n")
}

saveRDS(prob_imputed_df, file = "prob_imputed_df.rds")


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

# Implementation on 10 dataframes
model_imputed_df <- list()

set.seed(9020)
for (i in 1:10) {
  df_i <- data_preprocess(rds_dfs, i)
  
  model_output <- model_impute(df_i)
  model_probs <- model_output$probs
  model_df_miss <- model_output$missing # df that will be imputed
  model_df_obs <- model_output$observed # full df, to be merged with imputed
  
  #sampling 1 category suing the probabilities
  el_sampled <- apply(model_probs, 1, function(p) {
    sample(categories, size = 1, prob = p) })
  
  model_df_miss$CD_ISCED_CENSUS2_IMP2 <- el_sampled #impute
  full_df_i <- rbind(model_df_obs, model_df_miss)
  
  model_imputed_df[[i]] <- full_df_i
  cat("Imputation for dataframe", i, "complete\n")
}

saveRDS(model_imputed_df, file = "model_imputed_df.rds")


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
