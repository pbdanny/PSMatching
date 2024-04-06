library(dplyr)
library(tidyr)
library(MatchIt)
library(marginaleffects)

# Load features, pivit, flag test/control----
test_store <- read.csv("./data/store_matching_before_rm_outlier.csv") %>%
  select(test_store_id) %>%
  rename(store_id = test_store_id) %>%
  mutate(ab_flag = 1)

# agg at store x period
period_feat <- read.csv("./data/features_byWk_byStore.csv") %>%
  filter(period_fis_wk %in% c("pre", "dur")) %>%
  group_by(period_fis_wk, store_id, level) %>%
  summarise(epos_sales = sum(epos_sales), epos_units = sum(epos_units), custs = sum(custs))

head(period_feat)

df_flag <- period_feat %>%
  pivot_wider(names_from = c(level, period_fis_wk), values_from = c(epos_sales, epos_units, custs), values_fill = 0.0) %>%
  left_join(test_store, by=join_by("store_id")) %>%
  replace_na(list(ab_flag = 0)) %>%
  mutate(epos_units_feat_growth = epos_units_feat_dur/epos_units_feat_pre - 1)

head(df_flag %>% select(c(store_id, ab_flag)))

df_flag %>% write.csv("./data/features_byWk_byStore_flag.csv", row.names=F)

# Load prep features----
df <- read.csv("./data/features_byWk_byStore_flag.csv")
head(df) %>% select(c(store_id, ab_flag))

# Regression check covariate impact on ab_flag
logit_before1 <- glm(epos_units_feat_growth ~ ab_flag + 
                                           epos_units_cate_exc_feat_dur +
                                           custs_cate_exc_feat_dur, 
                                           data = df)
summary(logit_before1)

logit_before2 <- glm(epos_units_feat_growth ~ ab_flag + 
                                           custs_feat_pre + 
                                           epos_units_cate_exc_feat_pre +
                                           custs_cate_exc_feat_pre + 
                                           epos_units_cate_exc_feat_dur +
                                           custs_cate_exc_feat_dur
                                           , data = df)
summary(logit_before2)
# With unmatched data, coef. of `ab flag` not significant

# PS matcing : test on tratified sample data----

# small_df <- df %>%
#   group_by(store_flag) %>%
#   slice_sample(prop = 0.005)

# create regression formula
ps_formula <- df %>%
  select(-c(store_id, ab_flag, epos_units_feat_growth, epos_sales_feat_dur, epos_units_feat_dur, custs_feat_dur)) %>%
  names() %>%
  reformulate(, response = "ab_flag")

# Check unmatched balance
pre_matching_balance <- matchit(ps_formular, data = df, method = NULL, distance = "glm")
summary(pre_matching_balance)

#  Use matching method = 'nearest', 1:1 matching with NN
nearest_onetoone <- matchit(ps_formula, data = df, method = "nearest", distance = "glm", link = "logit")
nearest_onetoone

# Standardized paired distance still high
summary(nearest_onetoone)

# Visual inspection, show difference in PS score
plot(nearest_onetoone, type = "jitter", interactive = FALSE)
plot(nearest_onetoone, type = "density", interactive = FALSE)
plot(summary(nearest_onetoone))

# Try matching method = "full"
full_matching <- matchit(ps_formula, data = df, method = "full", distance = "glm", link = "logit")
full_matching
# The test obs with high score, have higher weight of control to matched with; vise versa, for control with low propensity score
plot(full_matching, type = "jitter", interactive = FALSE)
summary(full_matching)
plot(summary(full_matching))

# Estimate effect----
matched_data <- match.data(full_matching)
head(matched_data)

# Estimate method 1) Difference in mean by t.test
t.test(epos_units_feat_growth ~ ab_flag, data = matched_data)
# show no difference in mean for test / control

# Estimate method 2) G-Computation
# Outcome model : regression target variable with covariates in formula
var_outcome_model <- colnames(full_matching$X)
# Recom to have interaction term
outcome_formula <- paste("epos_units_feat_growth ~ ab_flag * (", paste(var_ps, collapse = "+"), ")")
# MUST have weights
fit <- glm(outcome_formula, data = matched_data, weights = weights)
avg_comparisons(fit, variables = "ab_flag", 
                     vcov = ~subclass, 
                     newdata = subset(matched_data, ab_flag == 1), 
                     wts = "weights")
# Also g-computation yield no marginal effect on effect measure (epos_unit_growth)

# Estimand of each group
avg_predictions(fit, variables = "ab_flag",
                vcov = ~subclass,
                newdata = subset(matched_data, ab_flag == 1),
                wts = "weights")