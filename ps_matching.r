library(dplyr)

df <- read.csv("feat.csv")
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

# create new dataframes from each splited dataframe
for (I in seq_along(split_list)) {
  assign(paste0("df1_", I), rbind(split_list[[I]], df0))
}

df1_1 %>%
  group_by(store_flag) %>%
  summarise(n())

library(MatchIt)

psm <- matchit(store_flag ~ expose_level +
               feat_brand_ty_sales + feat_cate_ty_sales + feat_aisle_ty_sales,
               data = df1_1)

summary(psm)