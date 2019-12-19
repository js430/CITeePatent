library(tidyverse)

df_orig <- read_csv("20170914_Patents_MSA09-All_Class-All_2000-2015.csv")

df_allmsa <- df_orig %>%
  filter(msa_code09 == 0)

df_allmsa %>%
  group_by(year) %>%
  summarise(total_patents = sum(num_patents)) %>%
  ggplot(aes(x = year, y = total_patents)) +
    geom_col()

df_allmsa %>%
  group_by(tech_cat) %>%
  summarise(total_patents = sum(num_patents)) %>%
  ggplot(aes(x = reorder(tech_cat, total_patents), y = total_patents)) +
    geom_col() +
    coord_flip()

df_allmsa %>%
  group_by(year, tech_cat) %>%
  summarise(total_patents = sum(num_patents)) %>%
  ggplot(aes(x = year, y = total_patents)) +
    geom_col() +
    facet_wrap(~ tech_cat)

df_allmsa %>%
  group_by(year, tech_subcat) %>%
  summarise(total_patents = sum(num_patents)) %>%
  top_n(15, total_patents) %>%
  ggplot(aes(x = year, y = total_patents)) +
    geom_col() +
    facet_wrap(~ tech_subcat)

df_allmsa %>%
  filter(tech_cat == "Computers & Communications") %>%
  group_by(year, tech_class_name) %>%
  summarise(total_patents = sum(num_patents)) %>%
  top_n(10, total_patents) %>%
  ggplot(aes(x = year, y = total_patents)) +
    geom_col() +
    facet_wrap(~tech_class_name)

df_msa <-  df_orig %>% filter(msa_code09 != 0)

df_msa %>%
  group_by(year, msa_name09) %>%
  summarise(total_patents = sum(num_patents)) %>%
  top_n(20, total_patents) %>%
  ggplot(aes(x = year, y = total_patents)) +
    geom_col() +
    facet_wrap(~ msa_name09)


df_msa %>%
  filter(tech_cat == "Computers & Communications") %>%
  group_by(year, msa_name09) %>%
  summarise(total_patents = sum(num_patents)) %>%
  top_n(10, total_patents) %>%
  ggplot(aes(x = year, y = total_patents)) +
    geom_col() +
    facet_wrap(~ msa_name09)


df_msa %>%
  filter(!tech_cat %in% c("Chemical", "Mechanical", "Others")) %>%
  group_by(year, tech_cat, msa_name09) %>%
  summarise(total_patents = sum(num_patents)) %>%
  top_n(10, total_patents) %>%
  ggplot(aes(x = year, y = total_patents)) +
    geom_col() +
    facet_grid(tech_cat ~ msa_name09)

# =============================================
# GCPD
# =============================================

df_gcpd <- read_csv("data/20190430_gcpd.csv")

df_gcpd2k <-
  df_gcpd %>%
  filter(year >= 2000, year <= 2015) %>%
  mutate(us_rest = if_else(country == "United States", "United States", "Rest of the World"))

df_gcpd2k %>%
  group_by(year) %>%
  summarise(total_patents = sum(patentcount)) %>%
  ggplot(aes(x = year, y = total_patents)) +
    geom_col()

df_gcpd2k %>%
  group_by(year, us_rest) %>%
  summarise(total_patents = sum(patentcount)) %>%
  ggplot(aes(x = year, y = total_patents)) +
    geom_col() +
    facet_wrap(~ us_rest)

df_gcpd2k %>%
  group_by(year, industry_code, us_rest) %>%
  summarise(total_patents = sum(patentcount, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = total_patents)) +
  geom_col() +
  facet_grid(industry_code ~ us_rest)


df_gcpd2k %>%
  filter(industry_code == 6) %>%
  group_by(year, us_rest) %>%
  summarise(total_patents = sum(patentcount, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = total_patents)) +
  geom_col() +
  facet_wrap( ~ us_rest)

# =============================================
# GCPD vs. MSA totals
# =============================================

df_msa %>%
  group_by(year) %>%
  summarise(sum(num_patents))

df_gcpd2k %>%
  filter(country == "United States") %>%
  group_by(year) %>%
  summarise(sum(patentcount, na.rm = TRUE))

df_gcpdaggr <- read_csv("data/20190507_gcpd_aggregate.csv")

df_gcpdaggr %>%
  filter(grant_year >= 2000, grant_year <= 2015) %>%
  group_by(grant_year) %>%
  summarise(total_patents = sum(patent_count, na.rm = TRUE)) %>%
  ggplot(aes(x = grant_year, y = total_patents)) +
  geom_col()
