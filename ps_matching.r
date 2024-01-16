library(dplyr)
library(tidyr)
library(arrow)

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
  select(-c(flag, feat_brand_ty_h1_sum.sales.:feat_brand_ty_h2_sum.visits., aisle_flag, ty_flag))

# split control
df0 <- df[df$store_flag == 0, ]

# split treatment into 10 group
n <- 20
df1 <- df[df$store_flag == 1, ]
split_list <- split(df1, factor(sort(rank(row.names(df1)) %% n)))

# create ps formular from all column name
ps_formular <- df %>%
          select(-c(household_id, ty_brand_sales, store_flag)) %>%
          names() %>%
          reformulate(, response = "store_flag")

library(MatchIt)

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

#----Code to loop all the split list, run psm matching and append
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
