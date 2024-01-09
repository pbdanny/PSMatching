library(dplyr)

df <- read.csv("data/feat.csv")
summary(df)

df %>%
  group_by(store_flag) %>%
  summarise(n())

# split control
df0 <- df[df$store_flag == 0, ]
nrow(df0)

# split treatment into 10 group
n <- 20
df1 <- df[df$store_flag == 1, ]
split_list <- split(df1, factor(sort(rank(row.names(df1)) %% n)))

# create new dataframes of each test split + combine with all control
for (I in seq_along(split_list)) {
  assign(paste0("df1_", I), rbind(split_list[[I]], df0))
}

# Count each split + control
df1_1 %>%
  group_by(store_flag) %>%
  summarise(n())

library(MatchIt)

psm <- matchit(store_flag ~ feat_cate_ly_sales + feat_ly_sales + feat_cate_ty_sales + feat_aisle_ty_visits + truprice,
               data = df1_1)

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
