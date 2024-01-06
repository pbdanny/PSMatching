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
