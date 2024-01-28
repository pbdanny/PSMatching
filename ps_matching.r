library(multidplyr)
library(dplyr)
library(tidyr)
library(arrow)
library(MatchIt)

cluster <- new_cluster(2)

#--- Read partitioned parquet
ds <- open_dataset("data/feat.parquet")
sc <- Scanner$create(ds)
at <- sc$ToTable()

df <- as.data.frame(at) %>%
  # reverse one-hot-encoding -> truprice, select truprice_flag == 1
  pivot_longer(most_price_driven:price_neutral, names_to = "truprice", values_to = "truprice_flag") %>%
  filter(truprice_flag == 1) %>%
  # many household_id have mulitiple trupice_flag, dedup by select the first
  group_by(household_id) %>%
  mutate(hh_rank = row_number()) %>%
  filter(hh_rank == 1) %>%
  # remove unused flag
  select(-c(truprice_flag, aisle_flag, ty_flag, hh_rank)) %>%
  # rename column with special char
  rename_with(make.names)

# distributed data to worker , partition by truprice
df_dist <- df %>%
  group_by(truprice) %>%
  partition(cluster) %>%
  mutate(ty_brand_sales = sum(feat_brand_ty_h1_sum.sales., feat_brand_ty_h2_sum.sales.)) %>%
  mutate(ly_brand_sales = sum(feat_brand_ly_h1_sum.sales., feat_brand_ly_h2_sum.sales.)) %>%
  mutate(brand_sales_growth = ty_brand_sales / ly_brand_sales - 1) %>%
  mutate(feat_ly_h1_sum.sales. = sum(pick((starts_with("feat_ly") & ends_with("h1_sum.sales."))))) %>%
  mutate(feat_ly_h2_sum.sales. = sum(pick((starts_with("feat_ly") & ends_with("h2_sum.sales."))))) %>%
  mutate(feat_ty_h1_sum.sales. = sum(pick((starts_with("feat_ty") & ends_with("h1_sum.sales."))))) %>%
  mutate(feat_ty_h2_sum.sales. = sum(pick((starts_with("feat_ty") & ends_with("h2_sum.sales."))))) %>%
  mutate(feat_cate_ly_h1_sum.sales. = sum(pick((starts_with("feat_cate_ly") & ends_with("h1_sum.sales."))))) %>%
  mutate(feat_cate_ly_h2_sum.sales. = sum(pick((starts_with("feat_cate_ly") & ends_with("h2_sum.sales."))))) %>%
  mutate(feat_cate_ty_h1_sum.sales. = sum(pick((starts_with("feat_cate_ty") & ends_with("h1_sum.sales."))))) %>%
  mutate(feat_cate_ty_h2_sum.sales. = sum(pick((starts_with("feat_cate_ty") & ends_with("h2_sum.sales."))))) %>%
  mutate(household_id = as.character(household_id)) %>%
  select(household_id, group:feat_cate_ty_h2_sum.sales., starts_with("feat_aisle") & ends_with("sum.visits."))

# collect from worker to local
df <- df_dist %>% collect()

# check uniqueness of household_id
length(df$household_id)
length(unique(as.data.frame(sc$ToTable())$household_id))

# Check covariate effect on coefficient of `store_flag`
logit_before1 <- glm(ty_brand_sales ~ store_flag, data = df)
summary(logit_before1)

logit_before2 <- glm(ty_brand_sales ~ store_flag + 
                                          ly_brand_sales + 
                                          feat_aisle_ty_h1_sum.visits. + feat_aisle_ty_h2_sum.visits. + 
                                          feat_cate_ty_h1_sum.sales. + feat_cate_ty_h2_sum.sales.
                                          , data = df)
summary(logit_before2)
# With unmatched data, coef. of `store flag` change direction, when more covariate variable put the regression formula

#---- PS matcing : test on tratified sample data
small_df <- df %>%
  group_by(store_flag) %>%
  slice_sample(prop = 0.005)

# create regression formula from all column name excluding
# hh_id, ty_brand_sales (targetb ariable), store_flag (teat - control flag)
ps_formular <- df %>%
  select(-c(household_id, ty_brand_sales, brand_sales_growth, store_flag)) %>%
  names() %>%
  reformulate(, response = "store_flag")

# Check unmatched balance - Able to skip, since summary matching have comparison
# for unmatched data as well
# m_out0 <- matchit(ps_formular, data = small_df, method = NULL, distance = "glm")
# summary(m_out0)

#  Use matching method = 'quick'
m_out1 <- matchit(ps_formular, data = small_df, method = "quick", distance = "glm", link = "logit")
matched_data <- match.data(m_out1)
summary(m_out1)
# Check distribution of PS score : treat should have same range as control
plot(m_out1, type = "jitter", interactive = FALSE)

# Check covariate balance : standarized mean difference before / after matched
plot(summary(m_out1))

# Check covriate balance : with density plot
plot(m_out1, type = "density", interactive = FALSE,
     which.xs = ~ly_brand_sales + feat_aisle_ty_h1_sum.visits. + feat_aisle_ty_h2_sum.visits.)

# Check covariate effect on coefficient of `store_flag`
logit_matched1 <- glm(ty_brand_sales ~ store_flag, data = matched_data)
summary(logit_matched1)

logit_matched2 <- glm(ty_brand_sales ~ store_flag + 
                                          ly_brand_sales + 
                                          feat_aisle_ty_h1_sum.visits. + feat_aisle_ty_h2_sum.visits. + 
                                          feat_cate_ty_h1_sum.sales. + feat_cate_ty_h2_sum.sales.
                                          , data = matched_data)
summary(logit_matched2)
# With matched data, the coef. of `store_flag` stable for any regression formula

# Estimate effect
t.test(ty_brand_sales ~ store_flag, data = matched_data)

#---- Estimate effect on full data set
# Can not `summary`, `plot` of full data set
ps_formular <- df %>%
  select(-c(household_id, ty_brand_sales, brand_sales_growth, store_flag)) %>%
  names() %>%
  reformulate(, response = "store_flag")

#  Use matching method = 'quick'
m_out2 <- matchit(ps_formular, data = df, method = "quick", distance = "glm", link = "logit")
matched_data2 <- match.data(m_out2)

summary(m_out2)

# Check covariate effect on coefficient of `store_flag`
logit_matched1 <- glm(ty_brand_sales ~ store_flag, data = matched_data2)
summary(logit_matched1)

logit_matched2 <- glm(ty_brand_sales ~ store_flag + 
                                          ly_brand_sales + 
                                          feat_aisle_ty_h1_sum.visits. + feat_aisle_ty_h2_sum.visits. + 
                                          feat_cate_ty_h1_sum.sales. + feat_cate_ty_h2_sum.sales.
                                          , data = matched_data2)
summary(logit_matched2)
# With matched data, the coef. of `store_flag` stable for any regression formula

# Estimate effect
t.test(ty_brand_sales ~ store_flag, data = matched_data2)



#---- Speed up MatchIt Option 1 : pre-run score
#step 1 : pre-run score
df$myfit <- fitted(glm(ps_formular, data = small_df, family = "binomial"))

#step 2 : trim data
trimmed_data <- df %>% select(household_id, myfit, store_flag)


#step 3
m_out <- matchit(store_flag ~ household_id, data = trimmed_data,
                 method = "quick", distance = trimmed_data$myfit, replace = TRUE)
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
options(scipen = 999)

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