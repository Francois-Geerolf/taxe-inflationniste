library(tidyverse)
library(readxl)

# PIB --------

temp <- tempfile()

curl::curl_download("https://www.insee.fr/fr/statistiques/fichier/8068582/T_1101_1103.xlsx",
                    temp)

T_1101 <- read_excel(temp, sheet = "T_1101 en niveau", skip = 3) %>%
  mutate(line = 1:n()) %>%
  rename(variable = ...1, Variable = ...2) %>%
  gather(year, value, -variable, -Variable, -line) %>%
  filter(!is.na(value)) %>%
  mutate(year = as.numeric(year))

PIB <- T_1101 %>%
  filter(variable == "B1GQ") %>%
  select(year, PIB = value)

unlink(temp)

# Deflateur du PIB ---------

temp <- tempfile()

curl::curl_download("https://www.insee.fr/fr/statistiques/fichier/8068582/T_1101_1103.xlsx",
                    temp)

T_1103 <- read_excel(temp, sheet = "T_1103 en évolution", skip = 3) %>%
  mutate(line = 1:n()) %>%
  rename(variable = ...1, Variable = ...2) %>%
  gather(year, value, -variable, -Variable, -line) %>%
  filter(!is.na(value)) %>%
  mutate(year = as.numeric(year))

deflateur <- T_1103 %>%
  filter(variable == "B1GQ") %>%
  select(year, deflateur = value)

unlink(temp)

# Croissance réelle du PIB ---------

temp <- tempfile()

curl::curl_download("https://www.insee.fr/fr/statistiques/fichier/8068582/T_1101_1103.xlsx",
                    temp)

T_1102 <- read_excel(temp, sheet = "T_1102 en évolution", skip = 3) %>%
  mutate(line = 1:n()) %>%
  rename(variable = ...1, Variable = ...2) %>%
  gather(year, value, -variable, -Variable, -line) %>%
  filter(!is.na(value)) %>%
  mutate(year = as.numeric(year))

croissance_reelle <- T_1102 %>%
  filter(variable == "B1GQ") %>%
  select(year, croissance_reelle = value)

unlink(temp)

# Dette en % du PIB --------

temp <- tempfile()

curl::curl_download("https://www.insee.fr/fr/statistiques/fichier/8068622/t_3101.xlsx",
                    temp)

t_3101 <- read_excel(temp, skip = 2) %>%
  mutate(line = 1:n()) %>%
  rename(Variable = ...1) %>%
  gather(year, value, -Variable, -line) %>%
  filter(!is.na(value)) %>%
  mutate(year = as.numeric(year))

dette_PIB <- t_3101 %>%
  filter(line == 10) %>%
  select(year, dette_PIB = value)

# Deficit en % du PIB --------

temp <- tempfile()

curl::curl_download("https://www.insee.fr/fr/statistiques/fichier/8068622/t_3106.xlsx",
                    temp)

t_3106 <- read_excel(temp, skip = 1) %>%
  mutate(line = 1:n()) %>%
  rename(Variable = ...1) %>%
  gather(year, value, -Variable, -line) %>%
  mutate(value = as.numeric(value)) %>%
  filter(!is.na(value)) %>%
  mutate(year = as.numeric(year))

deficit_PIB <- t_3106 %>%
  filter(line == 10) %>%
  select(year, deficit_PIB = value)

# Charge d'intérêt ----------

temp <- tempfile()

curl::curl_download("https://www.insee.fr/fr/statistiques/fichier/8068612/T_7301.xlsx",
                    temp)

T_7301 <- read_excel(temp, skip = 4, sheet = 2) %>%
  mutate(line = 1:n()) %>%
  rename(variable = ...1, Variable = ...2) %>%
  gather(year, value, -variable, -Variable, -line) %>%
  mutate(value = as.numeric(value)) %>%
  filter(!is.na(value)) %>%
  mutate(year = as.numeric(year))

charge_interets <- T_7301 %>%
  filter(line %in% c(56, 47)) %>%
  select(year, line, value) %>%
  spread(line, value) %>%
  transmute(year, charge_interets = `56` - `47`)

# Données ----------

figure2 <- PIB %>%
  full_join(deflateur, by = "year") %>%
  full_join(croissance_reelle, by = "year") %>%
  full_join(dette_PIB, by = "year") %>%
  full_join(deficit_PIB, by = "year") %>%
  full_join(charge_interets, by = "year") %>%
  mutate()

figure2 %>%
  transmute(date = as.Date(paste0(year, "-01-01")),
            `Charge d'intérêts (% du PIB)` = charge_interets/PIB,
            `Taxe inflationniste (% du PIB)` = dette_PIB*deflateur/10000,
            `Charge d'intérêts réelle (% du PIB)` = charge_interets/PIB-dette_PIB*deflateur/10000) %>%
  filter(date >= as.Date("1979-01-01")) %>%
  gather(variable, value, -date) %>%
  ggplot + geom_line(aes(x = date, y = value, color = variable))



figure2 %>%
  transmute(date = as.Date(paste0(year, "-01-01")),
            `Charge d'intérêts (% du PIB)` = charge_interets/PIB,
            `Charge d'intérêts réelle (% du PIB)` = charge_interets/PIB-dette_PIB*deflateur/10000) %>%
  filter(date >= as.Date("1979-01-01")) %>%
  gather(variable, value, -date) %>%
  ggplot + geom_line(aes(x = date, y = value, color = variable)) +
  theme_minimal() + xlab("") + ylab("") +
  theme(legend.title = element_blank(),
        legend.position = c(0.4, 0.5)) +
  scale_x_date(breaks = seq(1950, 2100, 5) %>% paste0("-01-01") %>% as.Date,
               labels = scales::date_format("%Y")) +
  scale_y_continuous(breaks = 0.01*seq(-100, 100, 1),
                     labels = scales::percent_format(accuracy = 1))


ggsave("figure2.png", width = 1.25*6, height = 1.25*3.375)
ggsave("figure2.pdf", width = 1.25*6, height = 1.25*3.375)