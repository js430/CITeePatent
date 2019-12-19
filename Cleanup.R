library(tidyverse)
library(tidyselect)

df_ownership <- read_csv("data/20191126_Patents_Ownership-Category.csv") %>% select(-`All Years`)

df_ownership_long <- pivot_longer(data = df_ownership,
                                  cols = `1963`:`2018`,
                                  names_to = 'year',
                                  values_to = "patent_count")


write_csv(df_ownership_long, "data/20191126_Patents_Ownership-Category_long.csv")

# ===========

df_org <- read_csv("data/20191126_Patents_Orgs_1000-or-More-Patents.csv")

df_org_long <- pivot_longer(data = df_org,
                                  cols = `1969`:`2018`,
                                  names_to = 'year',
                                  values_to = "patent_count")


write_csv(df_org_long, "data/20191126_Patents_Orgs_1000-or-More-Patents_long.csv")

# =================

df_hist <-  read_csv("data/20191126_Patents_Annual-Historical_NBER-Category.csv")

df_hist_inforce <- df_hist %>%
  select(year, ends_with("inforce"))

df_hist_inforce_long <- pivot_longer(data = df_hist_inforce, cols = -year,
                        names_to = "nber_subcat", values_to = "patent_count")
write_csv(df_hist_inforce_long, "data/20191126_Patents_Annual-Historical_NBER-Category_Inforce.csv")


df_hist_iss <- df_hist %>%
  select(year, ends_with("iss"))

df_hist_iss_long <- pivot_longer(data = df_hist_iss, cols = -year,
                                     names_to = "nber_subcat", values_to = "patent_count")

write_csv(df_hist_iss_long, "data/20191126_Patents_Annual-Historical_NBER-Category_Issued.csv")
