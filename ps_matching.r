library(dplyr)
library(tidyr)
library(arrow)
library(MatchIt)

#--- Read partitioned parquet
ds <- open_dataset("data/feat.parquet")
sc <- Scanner$create(ds)
at <- sc$ToTable()
df <- as.data.frame(at) %>%
  # reverse onehot encoding -> truprice
  pivot_longer(most_price_driven:price_neutral, names_to = "truprice", values_to = "flag") %>%
  # rename column with special char
  rename_with(make.names) %>%
  mutate(ty_brand_sales = sum(feat_brand_ty_h1_sum.sales., feat_brand_ty_h2_sum.sales.)) %>%
  select(-c(flag, , aisle_flag, ty_flag)) %>%
  # select(-c(flag, feat_brand_ty_h1_sum.sales.:feat_brand_ty_h2_sum.visits., aisle_flag, ty_flag))
  mutate(feat_ly_h1_sum.sales. = sum(pick((starts_with("feat_ly") & ends_with("h1_sum.sales."))))) %>%
  mutate(feat_ly_h2_sum.sales. = sum(pick((starts_with("feat_ly") & ends_with("h2_sum.sales."))))) %>%
  mutate(feat_ty_h1_sum.sales. = sum(pick((starts_with("feat_ty") & ends_with("h1_sum.sales."))))) %>%
  mutate(feat_ty_h2_sum.sales. = sum(pick((starts_with("feat_ty") & ends_with("h2_sum.sales."))))) %>%

  mutate(feat_cate_ly_h1_sum.sales. = sum(pick((starts_with("feat_cate_ly") & ends_with("h1_sum.sales."))))) %>%
  mutate(feat_cate_ly_h2_sum.sales. = sum(pick((starts_with("feat_cate_ly") & ends_with("h2_sum.sales."))))) %>%
  mutate(feat_cate_ty_h1_sum.sales. = sum(pick((starts_with("feat_cate_ty") & ends_with("h1_sum.sales."))))) %>%
  mutate(feat_cate_ty_h2_sum.sales. = sum(pick((starts_with("feat_cate_ty") & ends_with("h2_sum.sales."))))) %>%

  select(household_id, group:feat_cate_ty_h2_sum.sales., starts_with("feat_aisle") & ends_with("sum.visits."))


# create  formular from all column name
ps_formular <- df %>%
  select(-c(household_id, ty_brand_sales, store_flag)) %>%
  names() %>%
  reformulate(, response = "store_flag")

#---- Speed up MatchIt Option 1 : pre-run score
#step 1 : pre-run score
df$myfit <- fitted(glm(ps_formular, data = df, family = "binomial"))

#step 2 : trim data
trimmed_data <- df %>% select(household_id, myfit, store_flag)

#step 3
m_out <- matchit(store_flag ~ household_id, data = trimmed_data, method = "nearest", distance = trimmed_data$myfit)
matched_unique_ids_etc <- match.data(m_out, data = trimmed_data)
matched_unique_ids <- select(matched_unique_ids_etc, household_id)
matched_data <- matched_unique_ids %>% inner_join(df)

#---- Speed up MatchIt Option 2 : Split test, match all control (match with replacement)
# split control
df0 <- df[df$store_flag == 0, ]

# split treatment into 10 group
n <- 20
df1 <- df[df$store_flag == 1, ]
split_list <- split(df1, factor(sort(rank(row.names(df1)) %% n)))

psm <- matchit(ps_formular, data = rbind(df0, split_list[[1]]))

summary(psm)

# get paried of data after matched
psm_data <- match.data(psm)

# recheck if return completed match
psm_data %>%
  group_by(subclass) %>%
  summarise(gr = n()) %>%
  summarise(n())

# sample check each pair
psm_data[psm_data$subclass == 799, ]

# suppress e+ notation
options(scipen=999)

# Code to loop all the split list, run psm matching and append
# create new dataframes of each test split + combine with all control

combined <- list()
for (I in seq_along(split_list)) {
  df_to_match <- rbind(split_list[[I]], df0)
  psm <- matchit(store_flag ~ feat_cate_ly_sales + feat_ly_sales + feat_cate_ty_sales + feat_aisle_ty_visits + truprice,
                data = df_to_match)
  psm_data <- match.data(psm)
  combined[[I]] <- psm_data
}
saveRDS(combined, file = "data/combined.RData")

# Load list of matched data
combinded <- readRDS(file = "data/combined.RData")

combinded_salted <- list()
for (i in seq_along(combinded)) {
  li <- combinded[[i]]
  salted <- li %>%
    mutate(subclass_n = paste0(1, sprintf("%02d", i), subclass)) %>%
    select(-subclass) %>%
    rename(subclass = subclass_n)
  combinded_salted[[i]] <- salted
}

big_salted <- do.call(rbind, combinded_salted)
tail(big_salted)

write.csv(big_salted, file = "data/paired.csv")
