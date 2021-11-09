
##
## Script for plotting WGI-indicator Government effectiveness
##

library(tidyverse)
library(WDI)
library(countrycode)
library(here)
library(credentials)
library(gitcreds)

# Load and prepare data ---------------------------------------------------

# Searching for GE-indicator
WDIsearch('control of corruption')

# WGI-indicator Government effectiveness
wgi_raw <- WDI(
  indicator=c("GE.EST", "RL.EST", "RQ.EST", "CC.EST"),
  start=1996,end=2020)

# Including iso3c column from package countrycode
df_wgi <- wgi_raw %>%
  mutate(iso3c = countrycode(iso2c, origin = "iso2c", destination = "iso3c")) %>%
  select(-iso2c) %>%
  relocate(iso3c)

# Include column average for all four estimates
df_wgi$ALL.EST <- rowMeans(df_wgi[ , c("GE.EST","RL.EST", "RQ.EST", "CC.EST")])

# Repair unmatched iso2c-codes: AN and XK.

# Load income data
df_wb <- readxl::read_excel(here("data", "wb.xlsx")) %>%
  select(-country) %>%
  mutate(year = as.integer(year))

# Include income data in wgi dataset, merging by iso3c and year
df <- left_join(df_wgi, df_wb, by = c("iso3c"="code", "year"="year"))

# Some anti joins
anti <- anti_join(df_wgi, df_wb, by = c("iso3c"="code", "year"="year"))

# Line plot by income group ---------------------------------------------------------------

# Dataset for plot by income group averages
df_plot <- df %>%
  mutate(category = na_if(category, "..")) %>%
  filter(!is.na(category)) %>%
  filter(!is.na(ALL.EST)) %>%
  group_by(category, year) %>%
  summarise(
    GE.EST = mean(GE.EST),
    RL.EST = mean(RL.EST),
    RQ.EST = mean(RQ.EST),
    CC.EST = mean(CC.EST),
    ALL.EST = mean(ALL.EST)) %>%
  ungroup() %>%
  mutate(category = case_when(
    category == "H" ~ "Høyinntektsland",
    category == "UM" ~ "Høyere mellominntektsland",
    category == "LM" ~ "Lavere mellominntektsland",
    category == "L" ~ "Lavinntektsland")) %>%
  mutate(category = fct_reorder(category, ALL.EST, .desc = TRUE))

# Save dataset
writexl::write_xlsx(df_plot, here("output", "wdi_incomegroup.xlsx"))

# Line plot
p_wdi_income <- ggplot(df_plot, aes(x = year, y = ALL.EST, group = category, colour = category)) +
  geom_line(size = 1) +
  scale_y_continuous(limits = c(-2, 2)) +
  scale_colour_brewer(palette = "Set1",
                      name = NULL) +
  labs(title = "GE/RQ/RL/CC i ulike inntektsgrupper",
       subtitle = "Gjennomsnittsscore per gruppe, 1996-2019.",
       caption = "Kilde: World Governance Indicators (WGI), Verdensbanken. Scorevariasjon fra -2,5 til 2,5.",
       x = NULL,
       y = NULL) +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0))

# Save plot
ggsave(here("figs", "GE_RQ_RL_CC.png"), plot = p_wdi_income)

# Line plot by partner country --------------------------------------------

# Partner countries
partnerland_lang <- c(
  "Colombia",
  "Ethiopia",
  "Ghana",
  "Indonesia",
  "Myanmar",
  "Mozambique",
  "Malawi",
  "Nepal",
  "Tanzania",
  "Uganda")

# Dataset for plot
df_plot_partnerland <- df %>%
  mutate(category = na_if(category, "..")) %>%
  filter(country %in% partnerland_lang)

# Save dataset
writexl::write_xlsx(df_plot_partnerland, here("output", "wdi_partnercountry.xlsx"))

# Line plot
p_wdi_partnerland <- ggplot(df_plot_partnerland, aes(x = year, y = GE.EST, colour = country)) +
  geom_line(size = 1.2) +
  scale_y_continuous(limits = c(-2, 2)) +
  scale_colour_brewer(palette = "Set3",
                      name = NULL) +
  labs(title = "Government Effectiveness i partnerland for langsiktig utvikling",
       subtitle = "1996-2019.",
       caption = "Kilde: World Governance Indicators (WGI), Verdensbanken. Scorevariasjon fra -2,5 til 2,5.",
       x = NULL,
       y = NULL) +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0))

# Save plot
ggsave(here("figs", "p_wdi_partnerland.png"), plot = p_wdi_partnerland)


# Github ------------------------------------------------------------------
# credentials::set_github_pat(force_new = TRUE)
# gitcreds_get()

