
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
WDIsearch('rule of law')

# WGI-indicator Government effectiveness
wgi_raw <- WDI(
  indicator=c("GE.EST", "GE.STD.ERR",
              "RL.EST", "RL.STD.ERR",
              "RQ.EST", "RQ.STD.ERR",
              "CC.EST", "CC.STD.ERR"),
  start=1996,end=2020)

# Add upper and lower standard errors for each indicator
wgi_raw <- wgi_raw %>%
  mutate("GE.UPPER" = GE.EST + (0.5 * GE.STD.ERR)) %>%
  mutate("GE.LOWER" = GE.EST - (0.5 * GE.STD.ERR)) %>%
  mutate("RL.UPPER" = RL.EST + (0.5 * RL.STD.ERR)) %>%
  mutate("RL.LOWER" = RL.EST - (0.5 * RL.STD.ERR)) %>%
  mutate("RQ.UPPER" = RQ.EST + (0.5 * RQ.STD.ERR)) %>%
  mutate("RQ.LOWER" = RQ.EST - (0.5 * RQ.STD.ERR)) %>%
  mutate("CC.UPPER" = CC.EST + (0.5 * CC.STD.ERR)) %>%
  mutate("CC.LOWER" = CC.EST - (0.5 * CC.STD.ERR))

# Include column average for all four estimates
wgi_raw$ALL.EST <- rowMeans(wgi_raw[ , c("GE.EST","RL.EST", "RQ.EST", "CC.EST")])

# Include column average standard errors
wgi_raw$ALL.UPPER <- rowMeans(wgi_raw[ , c("GE.UPPER","RL.UPPER", "RQ.UPPER", "CC.UPPER")])

wgi_raw$ALL.LOWER <- rowMeans(wgi_raw[ , c("GE.LOWER","RL.LOWER", "RQ.LOWER", "CC.LOWER")])


# Including iso3c column from package countrycode
df_wgi <- wgi_raw %>%
  mutate(iso3c = countrycode(iso2c, origin = "iso2c", destination = "iso3c")) %>%
  select(-iso2c) %>%
  relocate(iso3c)

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
    GE.LOWER = mean(GE.LOWER),
    GE.UPPER = mean(GE.UPPER),    
    RL.EST = mean(RL.EST),
    RL.LOWER = mean(RL.LOWER),
    RL.UPPER = mean(RL.UPPER),     
    RQ.EST = mean(RQ.EST),
    RQ.LOWER = mean(RQ.LOWER),
    RQ.UPPER = mean(RQ.UPPER),     
    CC.EST = mean(CC.EST),
    CC.LOWER = mean(CC.LOWER),
    CC.UPPER = mean(CC.UPPER),     
    ALL.EST = mean(ALL.EST),
    ALL.LOWER = mean(ALL.LOWER),
    ALL.UPPER = mean(ALL.UPPER)) %>%
  ungroup() %>%
  mutate(category = case_when(
    category == "H" ~ "Høyinntektsland",
    category == "UM" ~ "Høyere mellominntektsland",
    category == "LM" ~ "Lavere mellominntektsland",
    category == "L" ~ "Lavinntektsland")) %>%
  mutate(category = fct_reorder(category, ALL.EST, .desc = TRUE))

# Save dataset
if(file.exists(here("output")) == FALSE) {
  dir.create(here("output"))
}

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
if(file.exists(here("figs")) == FALSE) {
  dir.create(here("figs"))
}

ggsave(here("figs", "GE_RQ_RL_CC.png"), plot = p_wdi_income)

# Line plot standard errors
p_wdi_income_err <- ggplot(df_plot, aes(x = year, y = ALL.EST, group = category,
                                    ymax = ALL.UPPER, ymin = ALL.LOWER)) +
  geom_line(aes(colour = category), size = 1) +
  geom_ribbon(aes(fill = category), alpha=0.3, show.legend = F) +
  scale_y_continuous(limits = c(-2, 2)) +
  scale_colour_brewer(palette = "Set1",
                      name = NULL) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "GE/RQ/RL/CC i ulike inntektsgrupper",
       subtitle = "Gjennomsnittsscore per gruppe, 1996-2019.",
       caption = "Kilde: World Governance Indicators (WGI), Verdensbanken. Scorevariasjon fra -2,5 til 2,5.",
       x = NULL,
       y = NULL) +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0))

# Save plot
if(file.exists(here("figs")) == FALSE) {
  dir.create(here("figs"))
}

ggsave(here("figs", "GE_RQ_RL_CC_err.png"), plot = p_wdi_income_err)

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
if(file.exists(here("output")) == FALSE) {
  dir.create(here("output"))
}

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
if(file.exists(here("figs")) == FALSE) {
  dir.create(here("figs"))
}

ggsave(here("figs", "p_wdi_partnerland.png"), plot = p_wdi_partnerland)


# Line plot error
p_wdi_partnerland_err <- ggplot(df_plot_partnerland,
                                aes(x = year, y = ALL.EST, ymax = ALL.UPPER, ymin = ALL.LOWER)) +
  geom_line(aes(colour = country), size = 1.2) +
  geom_ribbon(aes(fill = country), alpha=0.3, show.legend = F) +  
  scale_y_continuous(limits = c(-2, 2)) +
  scale_colour_brewer(palette = "Set3",
                      name = NULL) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "GE/RQ/RL/CC i partnerland for langsiktig utvikling",
       subtitle = "1996-2019.",
       caption = "Kilde: World Governance Indicators (WGI), Verdensbanken. Scorevariasjon fra -2,5 til 2,5.",
       x = NULL,
       y = NULL) +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0))

# Save plot
if(file.exists(here("figs")) == FALSE) {
  dir.create(here("figs"))
}

ggsave(here("figs", "p_wdi_partnerland_err.png"), plot = p_wdi_partnerland_err)


# Github ------------------------------------------------------------------
# credentials::set_github_pat(force_new = TRUE)
# gitcreds_get()

