library(tidyverse)

df <- read_csv("data/20191126_Patents_Ownership-Category_long.csv")

df_grant <- df %>% filter(year_type == "Grant year")
df_app <- df %>% filter(year_type == "Application year")

df_grant %>%
  filter(ownership_category == "U.S. CORPORATION") %>%
  ggplot(aes(x = year, y = patent_count)) +
  geom_col()

df_app %>%
  filter(ownership_category == "U.S. CORPORATION") %>%
  ggplot(aes(x = year, y = patent_count)) +
  geom_col()

df %>%
  filter(ownership_category == "U.S. CORPORATION") %>%
  ggplot(aes(x = year, y = patent_count, fill = year_type)) +
  geom_col() +
  facet_wrap(~ year_type)

df %>%
  filter(str_ends(ownership_category, "CORPORATION"),
         year > 1990) %>%
  ggplot(aes(x = year, y = patent_count, color = year_type)) +
  geom_line() +
  facet_wrap(~ ownership_category)

#  ---------------------------------------------
# Individual
# ---------------------------------------------

df %>%
  filter(ownership_category == "U.S. INDIVIDUAL", year > 1990) %>%
  ggplot(aes(x = year, y = patent_count, color = year_type)) +
  geom_line() + geom_point()

df %>%
  filter(ownership_category == "FOREIGN INDIVIDUAL", year > 1990) %>%
  ggplot(aes(x = year, y = patent_count, color = year_type)) +
  geom_line() + geom_point()

df %>%
  filter(str_ends(ownership_category, "INDIVIDUAL"),
         year > 1990) %>%
  ggplot(aes(x = year, y = patent_count, color = year_type)) +
  geom_line() + geom_point() +
  facet_wrap(~ ownership_category)

# =============================================
# Government
# =============================================

df %>%
  filter(str_ends(ownership_category, "GOVERNMENT"), year > 1970) %>%
  ggplot(aes(x = year, y = patent_count, color = year_type)) +
  geom_line() + geom_point() +
  facet_wrap(~ ownership_category)

# =============================================
# Compare all categories
# =============================================

df %>%
  filter(year > 1970, year_type == "Grant year") %>%
  ggplot(aes(x = year, y = patent_count)) +
  geom_col(aes(fill = ownership_category),
           position = "fill")

df %>%
  filter(year > 1970) %>%
  ggplot(aes(x = year, y = patent_count)) +
  geom_col(aes(fill = ownership_category), position = "fill") +
  facet_wrap(~ year_type) +
  theme(legend.position = "bottom")
