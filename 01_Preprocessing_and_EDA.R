#libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(treemap)

# Loading the dataset
filep <- "***" #file path to the rds file
rds_dfs <- readRDS(filep)


# function for basic preprocessing of the data
data_preprocess <- function(data, d_index){

  df <- data[[d_index]]
  #some data management:

  # (a)split agegrp from agegrpsex to have a consistent    age group

  df <- df %>% mutate(AGEGRP = str_sub(as.character(AGEGRPSEX), 1, -2))

  # remove whitespaces and convert column to factor
  df$AGEGRP <- as.factor(stringr::str_trim(df$AGEGRP))

  df$CD_ISCED_CENSUS[df$CD_ISCED_CENSUS == 9] <- NA

  df$REGIOJ <- as.factor(df$REGIOJ)


  #create a new column with Ed levels aggregated into 3 levels
  df <- df %>% mutate(CD_ISCED_CENSUS_2 = ifelse(is.na(CD_ISCED_CENSUS),
                                                 NA_character_, ifelse(CD_ISCED_CENSUS %in% c(0,1,2), "Low",
                                                                       ifelse(CD_ISCED_CENSUS %in% c(3,4), "Medium", "High"))),
                      R_ind = ifelse(is.na(CD_ISCED_CENSUS), 0, 1)) %>%
    mutate(R_ind = factor(R_ind, levels = c(1, 0))) %>%
    mutate(CD_ISCED_CENSUS_2 = factor(CD_ISCED_CENSUS_2,
                                      levels = c("Low", "Medium", "High"), ordered = T)) %>%
    mutate(CD_ISCED_CENSUS = factor(CD_ISCED_CENSUS))

  #arrange the dataframe by frequency of CODs
  categorized_df <- df %>% group_by(GBD3_RED4) %>% mutate(n = n()) %>%
    arrange(desc(n)) %>% ungroup() %>% mutate(GBD3_RED4 =
                                                factor(GBD3_RED4, levels = unique(GBD3_RED4[order(-n)]),
                                                       ordered = TRUE))

  return(categorized_df)
}

# selection of 1 dataset from the 100, and initial preprocessing
df <- data_preprocess(rds_dfs, 1)

#:::::::::::::::EDA::::::::::::
## Overall distribution
nrow(df)
ggplot(df, aes(x = CD_ISCED_CENSUS, y = after_stat(prop), group = 1)) +
  geom_bar(fill = "#4575b4") +
  scale_y_continuous(labels = scales::percent)+
  ylab("Proportion")+xlab("Education Level")

# count of the missing in 0-15
df %>% filter(AGEGRP %in% c("[0,5)", "[5,15)")) %>% count()


## PATTERNS OF MISSINGNESS
#''' This part is examined in three smaller parts:
#'''  I. pattern of missingness, relative to the whole dataset
#'''  II. differences in characteristics in the complete and missing subsets
#'''  III. relationships between the variables and each ed level -for observed

#remove agegroups under 15
df_subset <- df %>% filter(!AGEGRP %in% c("[0,5)", "[5,15)"))

df_observed <- df_subset %>% filter(R_ind == 1)
df_missing <- df_subset %>% filter(R_ind == 0)


# i. patterns of missingness

# age group only
df_subset %>% group_by(AGEGRP) %>%
  summarise(tot = n(), n_missing = sum(R_ind == 0), prop = n_missing / tot) %>%
  ggplot(aes(x = AGEGRP, y = prop)) +
  geom_col(fill = "#4575b4") +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Proportion Missing", x = "Age Group")


# sex only
df_subset %>% group_by(SEX) %>%
  summarise(tot = n(), n_missing = sum(R_ind == 0), prop = n_missing / tot)

# region only
df_subset %>% group_by(REGIOJ) %>%
  summarise(tot = n(), n_missing = sum(R_ind == 0), prop = n_missing / tot)%>%
  ggplot(aes(x = REGIOJ, y = prop)) +
  geom_col(fill = "#4575b4") +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Proportion Missing", x = "Region")


# age group, sex
df_subset %>% group_by(AGEGRP, SEX) %>%
  summarise(tot = n(), n_missing = sum(R_ind == 0), prop = n_missing / tot) %>%
  ggplot(aes(x = AGEGRP, y = prop, fill = SEX)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Proportion Missing", x = "Age Group")

# age group, sex, region
#--plot
df_subset %>% group_by(AGEGRP, SEX, REGIOJ) %>%
  summarise(tot = n(), n_missing = sum(R_ind == 0), prop = n_missing / tot) %>%
  ggplot(aes(x = AGEGRP, y = prop, fill = SEX)) +
  geom_col(position = "dodge") + facet_wrap(~REGIOJ)+
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Proportion Missing", x = "Age Group")

#--table
df_subset %>% group_by(AGEGRP, SEX, REGIOJ) %>%
  summarise(tot = n(), n_missing = sum(R_ind == 0), prop = n_missing / tot)

# COD
# -- level 1 for a higher level view
df_subset %>% group_by(GBD1_RED4) %>%
  summarise(tot = n(), n_missing = sum(R_ind == 0), prop = n_missing / tot)

# -- gbd1 and sex
df_subset %>% group_by(GBD1_RED4, SEX) %>%
  summarise(tot = n(), n_missing = sum(R_ind == 0), prop = n_missing / tot)%>%
  ggplot(aes(x = GBD1_RED4, y = prop, fill = SEX)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Proportion Missing", x = "GBD level 1")

# ---level 3
df_subset %>% group_by(GBD3_RED4) %>%  summarise(tot = n(),
                                                 n_missing = sum(R_ind == 0),  prop = n_missing / tot) %>%
  filter(prop > 0) %>% arrange(desc(tot)) %>% mutate(x_pos = row_number()) %>%
  ggplot(aes(x = x_pos, y = 0, fill = prop)) +
  geom_tile(width = 1, height = 1) +
  scale_fill_distiller(palette = "Blues", direction = 1, name = "Proportion Missing")+
  labs(x = "COD (ordered from most to least frequent)", y = NULL)+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        legend.position = "top")

# COD and agegroup
cod_age <- df_subset %>% group_by(GBD3_RED4, AGEGRP) %>% summarise(tot = n(),
                                                                   n_missing = sum(R_ind == 0), prop = n_missing / tot) %>%
  filter(prop > 0) %>% arrange(desc(tot))

write.csv(cod_age, "output_csv_files/cod_age_missingness.csv")

# COD, age and sex
cod_age_sex <- df_subset %>% group_by(GBD3_RED4, SEX, AGEGRP) %>%
  summarise(tot = n(), n_missing = sum(R_ind == 0), prop = n_missing / tot) %>%
  filter(prop > 0) %>% arrange(desc(tot))

write.csv(cod_age_sex, "output_csv_files/cod_age_sex_missingness.csv")

# COD, age, sex and region
cod_all <- df_subset %>% group_by(GBD3_RED4, SEX, AGEGRP, REGIOJ) %>%
  summarise(tot = n(), n_missing = sum(R_ind == 0), prop = n_missing / tot) %>%
  filter(prop > 0) %>% arrange(desc(tot))

write.csv(cod_all, "output_csv_files/cod_all_missingness.csv")


# ii. characteristics of observed vs missing

r_ind_labels <- c("0" = "Missing", "1" = "Observed")

# by COD only
tree_obs <- df_subset %>% filter(R_ind == 1) %>% group_by(GBD3_RED4) %>%
  summarise(tot = n()) %>% slice_max(order_by = tot, n = 10)

treemap(tree_obs, index = "GBD3_RED4", vSize = "tot", title = "",
        vColor = "GBD1_RED4", palette = "Paired", fontcolor.labels = "black",
        fontsize.labels = 7, border.col = "grey")

tree_miss <- df_subset %>% filter(R_ind == 0) %>% group_by(GBD3_RED4) %>%
  summarise(tot = n())%>% slice_max(order_by = tot, n = 10)

treemap(tree_miss, index = "GBD3_RED4", vSize = "tot", title = "",
        vColor = "GBD1_RED4", palette = "Paired", fontcolor.labels = "black",
        fontsize.labels = 7, border.col = "grey")



# by sex only
df_subset %>% group_by(SEX, R_ind) %>% summarise(tot = n(), .groups = "drop") %>%
  group_by(R_ind) %>% mutate(prop = tot/sum(tot)) %>% ungroup() %>%
  ggplot(aes(x = SEX, y = prop, fill = SEX)) +
  geom_col(position = "dodge") +
  facet_wrap(~R_ind, labeller = as_labeller(r_ind_labels))+
  scale_y_continuous(labels = scales::percent)+
  geom_text(aes(label = scales::percent(prop, accuracy = 0.1)), vjust =-0.2, size = 3)+
  labs(x = "Sex", y = "Proportion")

# by age group only
df_subset %>% group_by(AGEGRP, R_ind) %>% summarise(tot = n(), .groups = "drop") %>%
  group_by(R_ind) %>% mutate(prop = tot/sum(tot)) %>% ungroup() %>%
  ggplot(aes(x = AGEGRP, y = prop, fill = AGEGRP)) +
  geom_col(position = "dodge") +
  facet_wrap(~R_ind, labeller = as_labeller(r_ind_labels))+
  scale_y_continuous(labels = scales::percent)+
  labs(x = "Age group", y = "Proportion")

# region only
df_subset %>% group_by(REGIOJ, R_ind) %>% summarise(tot = n(), .groups = "drop") %>%
  group_by(R_ind) %>% mutate(prop = tot/sum(tot)) %>% ungroup() %>%
  ggplot(aes(x = REGIOJ, y = prop, fill = REGIOJ)) +
  geom_col(position = "dodge") +
  facet_wrap(~R_ind, labeller = as_labeller(r_ind_labels))+
  scale_y_continuous(labels = scales::percent)+
  labs(x = "Region", y = "Proportion")

# by cod only
df_subset %>% group_by(GBD3_RED4, R_ind) %>% summarise(tot = n(), .groups = "drop") %>%
  group_by(R_ind) %>% mutate(prop = tot/sum(tot), x_pos = row_number()) %>%
  ggplot(aes(x = x_pos, y = 0, fill = prop)) +
  geom_tile(width = 1, height = 1) +
  facet_wrap(~R_ind, labeller = as_labeller(r_ind_labels))+
  scale_fill_distiller(palette = "Blues", direction = 1, name = "Proportion")+
  labs(x = "COD (ordered from most to least frequent)", y = NULL)+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        legend.position = "top")

# trying gbd level 1
df_subset %>% group_by(GBD1_RED4, R_ind) %>% summarise(tot = n(), .groups = "drop") %>%
  group_by(R_ind) %>% mutate(prop = tot/sum(tot)) %>% ungroup() %>%
  ggplot(aes(x = GBD1_RED4, y = prop, fill = GBD1_RED4)) +
  geom_col(position = "dodge") +
  facet_wrap(~R_ind, labeller = as_labeller(r_ind_labels))+
  scale_y_continuous(labels = scales::percent)+
  labs(x = "GBD Level 1", y = "Proportion") #--no pattern observed


# age and sex
df_subset %>% group_by(AGEGRP, SEX, R_ind) %>% summarise(tot = n(), .groups = "drop") %>%
  group_by(R_ind) %>% mutate(prop = tot/sum(tot)) %>% ungroup() %>%
  ggplot(aes(x = AGEGRP, y = prop, fill = SEX)) +
  geom_col(position = "dodge") +
  facet_wrap(~R_ind, labeller = as_labeller(r_ind_labels))+
  scale_y_continuous(labels = scales::percent)+
  labs(x = "Age group", y = "Proportion")

# age, sex and region
df_subset %>% group_by(REGIOJ,AGEGRP, SEX, R_ind) %>% summarise(tot = n(), .groups = "drop") %>%
  group_by(R_ind, REGIOJ) %>% mutate(prop = tot/sum(tot)) %>% ungroup() %>%
  ggplot(aes(x = AGEGRP, y = prop, fill = SEX)) +
  geom_col(position = "dodge") +
  facet_grid(R_ind ~ REGIOJ, labeller = labeller(R_ind = r_ind_labels))+
  scale_y_continuous(labels = scales::percent)+
  labs(x = "Age group", y = "Proportion")


# iii. relationship between each variable and ed levels

## COD only
df_observed %>% count(GBD3_RED4, CD_ISCED_CENSUS) %>%
  group_by(GBD3_RED4) %>% mutate(proportion = n / sum(n)) %>%
  ggplot(aes(x = CD_ISCED_CENSUS, y = GBD3_RED4,
             fill = proportion)) +
  geom_tile() + scale_fill_viridis_c(option =  "plasma") +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.title.y = element_blank(), panel.grid = element_blank()) +
  labs(x = "Education Levels", fill = "Proportion") + ylab("GBD Level 3")


# sex only
summary_table_sex <- df_observed %>% group_by(CD_ISCED_CENSUS) %>%
  summarise(overall_prop = round((n() / nrow(df_observed)),6),
            Male = round((sum(SEX == "M") / sum(df_observed$SEX == "M")),6),
            Female = round((sum(SEX == "F") / sum(df_observed$SEX == "F")),6))

summary_table_sex %>% pivot_longer(cols = c(Male, Female),
                                   names_to = "SEX", values_to = "Proportion") %>%
  ggplot(aes(x = CD_ISCED_CENSUS, y = Proportion, fill = SEX)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::percent)+
  labs(x = "Education Levels", y = "Proportion")

# age group only
summary_table_agegrp <- df_observed %>% group_by(CD_ISCED_CENSUS) %>%
  summarise(overall_prop = round((n() / nrow(df_observed))*100,2),
            "[15,45)" = round((sum(AGEGRP == "[15,45)") / sum(df_observed$AGEGRP == "[15,45)")),6),
            "[45,65)" = round((sum(AGEGRP == "[45,65)") / sum(df_observed$AGEGRP == "[45,65)")),6),
            "[65,85)" = round((sum(AGEGRP == "[65,85)") / sum(df_observed$AGEGRP == "[65,85)")),6),
            "[85,Inf)" = round((sum(AGEGRP == "[85,Inf)") / sum(df_observed$AGEGRP == "[85,Inf)")),6))

summary_table_agegrp %>%
  pivot_longer(cols = c(`[15,45)`,`[45,65)`,`[65,85)`, `[85,Inf)`),
               names_to = "AGEGRP",
               values_to = "Proportion") %>%
  ggplot(aes(x = CD_ISCED_CENSUS, y = Proportion, fill = AGEGRP)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::percent)+
  labs(x = "Education Levels", y = "Proportion")

# region only (output: table)
summary_table_region <- df_observed %>% group_by(CD_ISCED_CENSUS) %>%
  summarise(overall_prop = round((n() / nrow(df_observed))*100,2),
            BR = round((sum(REGIOJ == "BR") / sum(df_observed$REGIOJ == "BR")),6),
            FL = round((sum(REGIOJ == "FL") / sum(df_observed$REGIOJ == "FL")),6),
            WA = round((sum(REGIOJ == "WA") / sum(df_observed$REGIOJ == "WA")),6))

# age groups and sex
ggplot(df_observed, aes(x = AGEGRP, fill = CD_ISCED_CENSUS)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent)+
  facet_wrap(~ SEX) +
  labs(x = "Age Group", y = "Proportion") +
  scale_fill_discrete(name = "Education Level")
