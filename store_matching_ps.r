library(dplyr)
library(tidyr)
library(MatchIt)
library(marginaleffects)

# Load features, pivot, flag test/control----
test_store <- read.csv("./data/store_matching_before_rm_outlier.csv") %>%
  select(test_store_id) %>%
  rename(store_id = test_store_id) %>%
  mutate(ab_flag = 1)

# control store
reserved_store <- read.csv("data/reserved_store.csv") %>%
  mutate(ab_flag = 0)

test_reserved_store <- rbind(test_store, reserved_store)

# check dup test / control
test_control_store %>%
  group_by(store_id) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

test_control_store %>%
  group_by(ab_flag) %>%
  summarise(n = n())

# agg at store x period
period_feat <- read.csv("./data/features_byWk_byStore.csv") %>%
  filter(period_fis_wk %in% c("pre", "dur")) %>%
  group_by(period_fis_wk, store_id, level) %>%
  summarise(epos_sales = sum(epos_sales), epos_units = sum(epos_units), custs = sum(custs))

head(period_feat)

# Store performance with reserved
df_flag_reserved <- period_feat %>%
  pivot_wider(names_from = c(level, period_fis_wk), values_from = c(epos_sales, epos_units, custs), values_fill = 0.0) %>%
  inner_join(test_control_store, by = join_by("store_id")) %>%
  # replace_na(list(ab_flag = 0)) %>%
  mutate(epos_units_feat_growth = epos_units_feat_dur / epos_units_feat_pre - 1)

df_flag_reserved %>%
  group_by(ab_flag) %>%
  summarise(n = n()) %>%
  mutate(prop = prop.table(n))

# Store performanc the rest as control
df_flag <- period_feat %>%
  pivot_wider(names_from = c(level, period_fis_wk), values_from = c(epos_sales, epos_units, custs), values_fill = 0.0) %>%
  left_join(test_store, by = join_by("store_id")) %>%
  replace_na(list(ab_flag = 0)) %>%
  mutate(epos_units_feat_growth = epos_units_feat_dur/epos_units_feat_pre - 1)

df_flag %>%
  group_by(ab_flag) %>%
  summarise(n = n()) %>%
  mutate(prop = prop.table(n))

head(df_flag %>% select(c(store_id, ab_flag)))

df_flag_reserved %>% write.csv("./data/features_byWk_byStore_flag_use_reserved.csv", row.names = FALSE)
df_flag %>% write.csv("./data/features_byWk_byStore_flag.csv", row.names = FALSE)

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
outcome_formula <- paste("epos_units_feat_growth ~ ab_flag * (", paste(var_outcome_model, collapse = "+"), ")")
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

# Estimate effect direct from matched data----
# Direct calculation equal with t.test regression since t.test estimand mean of group
matched_data %>%
  group_by(ab_flag) %>%
  summarise(mean(epos_units_feat_growth))

matched_data %>%
  filter(subclass == 22) %>%
  group_by(ab_flag) %>%
  summarise(n = n(), tt_weight = sum(weights))

t.test(epos_units_feat_growth ~ ab_flag, data = matched_data)

# Compare matching between with reserved , rest
full_match_result <- function(df, df_name, ps_formula) {
  full_matching <- matchit(ps_formula, data = df, method = "full", distance = "glm", link = "logit")
  plot(summary(full_matching), main = df_name)
  matched_data <- match.data(full_matching)
  var_outcome_model <- colnames(full_matching$X)
  outcome_formula <- paste("epos_units_feat_growth ~ ab_flag * (", paste(var_outcome_model, collapse = "+"), ")")
  fit <- glm(outcome_formula, data = matched_data, weights = weights)
  print(paste("Marginal Effect :", df_name))
  print(avg_comparisons(fit, variables = "ab_flag",
                      vcov = ~subclass,
                      newdata = subset(matched_data, ab_flag == 1),
                      wts = "weights")
  )
  print(
  avg_predictions(fit, variables = "ab_flag",
                  vcov = ~subclass,
                  newdata = subset(matched_data, ab_flag == 1),
                  wts = "weights")
  )
  print("T.test")
  print(
    t.test(epos_units_feat_growth ~ ab_flag, data = matched_data)
  )

  print("Mean difference :")
  matched_data %>%
    group_by(ab_flag) %>%
    summarise(mean(epos_units_feat_growth))
}

# Use `reserved store`, less matching quality than `rest`
full_match_result(df_flag, "rest", ps_formula)
full_match_result(df_flag_reserved, "reserved", ps_formula)

# Result from `marginaleffect` difference from `t.test` or mean difference
full_match_result(df_flag, "rest", ps_formula)
