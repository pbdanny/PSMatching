import pandas as pd
import sys
import os

os.getcwd()
os.chdir("/home/danny/Downloads")
df = pd.read_parquet("feat.parquet").drop_duplicates()
df["truprice"] = df[
    [
        "most_price_driven",
        "most_price_insensitive",
        "price_driven",
        "price_insensitive",
        "price_neutral",
    ]
].idxmax(1)
# df.to_csv("feat_scales.csv")
# df.reset_index().to_feather("feat_scales.feather")

# EDA
df.head()
df.columns
df.groupby(["expose_level"]).agg(
    {
        "household_id": ["count"],
        "feat_aisle_ty_h1_sum(visits)": ["mean"],
        "feat_aisle_ty_h2_sum(visits)": ["mean"],
    }
)

# Exposure level, based on aisle visits
(
    df.assign(
        ty_visits=lambda x: x["feat_aisle_ty_h1_sum(visits)"]
        + x["feat_aisle_ty_h2_sum(visits)"]
    )
    .groupby(["store_flag", "expose_level"])
    .agg({"household_id": ["count"], "ty_visits": ["mean"]})
)

corr_mat = df[
    [
        "feat_ly_q1_h1_sum(sales)",
        "feat_ly_q1_h1_sum(visits)",
        "feat_ly_q2_h1_sum(sales)",
        "feat_ly_q2_h1_sum(visits)",
        "feat_ly_q3_h2_sum(sales)",
        "feat_ly_q3_h2_sum(visits)",
        "feat_ly_q4_h2_sum(sales)",
        "feat_ly_q4_h2_sum(visits)",
        "feat_ty_q1_h1_sum(sales)",
        "feat_ty_q1_h1_sum(visits)",
        "feat_ty_q2_h1_sum(sales)",
        "feat_ty_q2_h1_sum(visits)",
        "feat_ty_q3_h2_sum(sales)",
        "feat_ty_q3_h2_sum(visits)",
        "feat_ty_q4_h2_sum(sales)",
        "feat_ty_q4_h2_sum(visits)",
        "feat_cate_ly_q1_h1_sum(sales)",
        "feat_cate_ly_q1_h1_sum(visits)",
        "feat_cate_ly_q2_h1_sum(sales)",
        "feat_cate_ly_q2_h1_sum(visits)",
        "feat_cate_ly_q3_h2_sum(sales)",
        "feat_cate_ly_q3_h2_sum(visits)",
        "feat_cate_ly_q4_h2_sum(sales)",
        "feat_cate_ly_q4_h2_sum(visits)",
        "feat_cate_ty_q1_h1_sum(sales)",
        "feat_cate_ty_q1_h1_sum(visits)",
        "feat_cate_ty_q2_h1_sum(sales)",
        "feat_cate_ty_q2_h1_sum(visits)",
        "feat_cate_ty_q3_h2_sum(sales)",
        "feat_cate_ty_q3_h2_sum(visits)",
        "feat_cate_ty_q4_h2_sum(sales)",
        "feat_cate_ty_q4_h2_sum(visits)",
        "feat_brand_ly_h1_sum(sales)",
        "feat_brand_ly_h1_sum(visits)",
        "feat_brand_ly_h2_sum(sales)",
        "feat_brand_ly_h2_sum(visits)",
        "feat_brand_ty_h1_sum(sales)",
        "feat_brand_ty_h1_sum(visits)",
        "feat_brand_ty_h2_sum(sales)",
        "feat_brand_ty_h2_sum(visits)",
        "feat_aisle_ly_h1_sum(sales)",
        "feat_aisle_ly_h1_sum(visits)",
        "feat_aisle_ly_h2_sum(sales)",
        "feat_aisle_ly_h2_sum(visits)",
        "feat_aisle_ty_h1_sum(sales)",
        "feat_aisle_ty_h1_sum(visits)",
        "feat_aisle_ty_h2_sum(sales)",
        "feat_aisle_ty_h2_sum(visits)",
    ]
].corr()

from matplotlib import pyplot as plt

fig, ax = plt.subplots()
im = ax.imshow(corr_mat)

#---- Combine data Ty-Ly
# Feature
feat_ty_sales_col = [c for c in df.columns if (c.startswith("feat_ty")) & (c.endswith("sum(sales)"))]
feat_ty_visits_col = [c for c in df.columns if (c.startswith("feat_ty")) & (c.endswith("sum(visits)"))]
feat_ly_sales_col = [c for c in df.columns if (c.startswith("feat_ly")) & (c.endswith("sum(sales)"))]
feat_ly_visits_col = [c for c in df.columns if (c.startswith("feat_ly")) & (c.endswith("sum(visits)"))]

# Featured brand
feat_brand_ty_sales_col = [c for c in df.columns if (c.startswith("feat_brand_ty")) & (c.endswith("sum(sales)"))]
feat_brand_ty_visits_col = [c for c in df.columns if (c.startswith("feat_brand_ty")) & (c.endswith("sum(visits)"))]
feat_brand_ly_sales_col = [c for c in df.columns if (c.startswith("feat_brand_ly")) & (c.endswith("sum(sales)"))]
feat_brand_ly_visits_col = [c for c in df.columns if (c.startswith("feat_brand_ly")) & (c.endswith("sum(visits)"))]


# Featured cate
feat_cate_ty_sales_col = [c for c in df.columns if (c.startswith("feat_cate_ty")) & (c.endswith("sum(sales)"))]
feat_cate_ty_visits_col = [c for c in df.columns if (c.startswith("feat_cate_ty")) & (c.endswith("sum(visits)"))]
feat_cate_ly_sales_col = [c for c in df.columns if (c.startswith("feat_cate_ly")) & (c.endswith("sum(sales)"))]
feat_cate_ly_visits_col = [c for c in df.columns if (c.startswith("feat_cate_ly")) & (c.endswith("sum(visits)"))]

# Featured cate
feat_aisle_ty_sales_col = [c for c in df.columns if (c.startswith("feat_aisle_ty")) & (c.endswith("sum(sales)"))]
feat_aisle_ty_visits_col = [c for c in df.columns if (c.startswith("feat_aisle_ty")) & (c.endswith("sum(visits)"))]
feat_aisle_ly_sales_col = [c for c in df.columns if (c.startswith("feat_aisle_ly")) & (c.endswith("sum(sales)"))]
feat_aisle_ly_visits_col = [c for c in df.columns if (c.startswith("feat_aisle_ly")) & (c.endswith("sum(visits)"))]

df_agg = (df
.assign(feat_ty_sales=lambda x: x[feat_ty_sales_col].sum(axis=1))
.assign(feat_ly_sales=lambda x: x[feat_ly_sales_col].sum(axis=1))
.assign(feat_ty_visits=lambda x: x[feat_ty_visits_col].sum(axis=1))
.assign(feat_ly_visits=lambda x: x[feat_ly_visits_col].sum(axis=1))
.assign(feat_brand_ty_sales=lambda x: x[feat_brand_ty_sales_col].sum(axis=1))
.assign(feat_brand_ly_sales=lambda x: x[feat_brand_ly_sales_col].sum(axis=1))
.assign(feat_brand_ty_visits=lambda x: x[feat_brand_ty_visits_col].sum(axis=1))
.assign(feat_brand_ly_visits=lambda x: x[feat_brand_ly_visits_col].sum(axis=1))
.assign(feat_cate_ty_sales=lambda x: x[feat_cate_ty_sales_col].sum(axis=1))
.assign(feat_cate_ly_sales=lambda x: x[feat_cate_ly_sales_col].sum(axis=1))
.assign(feat_cate_ty_visits=lambda x: x[feat_cate_ty_visits_col].sum(axis=1))
.assign(feat_cate_ly_visits=lambda x: x[feat_cate_ly_visits_col].sum(axis=1))
.assign(feat_aisle_ty_sales=lambda x: x[feat_aisle_ty_sales_col].sum(axis=1))
.assign(feat_aisle_ly_sales=lambda x: x[feat_aisle_ly_sales_col].sum(axis=1))
.assign(feat_aisle_ty_visits=lambda x: x[feat_aisle_ty_visits_col].sum(axis=1))
.assign(feat_aisle_ly_visits=lambda x: x[feat_aisle_ly_visits_col].sum(axis=1))
)
new_column_names = [
    "feat_ty_sales",
    "feat_ly_sales",
    "feat_ty_visits",
    "feat_ly_visits",
    "feat_brand_ty_sales",
    "feat_brand_ly_sales",
    "feat_brand_ty_visits",
    "feat_brand_ly_visits",
    "feat_cate_ty_sales",
    "feat_cate_ly_sales",
    "feat_cate_ty_visits",
    "feat_cate_ly_visits",
    "feat_aisle_ty_sales",
    "feat_aisle_ly_sales",
    "feat_aisle_ty_visits",
    "feat_aisle_ly_visits"
]
export_col = ["household_id", 'group', 'expose_level', 'store_flag', 'aisle_flag', 'ty_flag', "truprice"] + new_column_names

df_selected = df_agg.loc[:,export_col]

df_selected.to_csv("feat.csv", index=False)