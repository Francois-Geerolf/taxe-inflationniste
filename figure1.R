library(tidyverse)

offline <- T

version <- "7722817"

temp <- tempfile()

curl::curl_download(paste0("https://www.insee.fr/fr/statistiques/fichier/", version, "/t_men_val.xls"),
              destfile = temp,
              quiet = F)

figure1 <- readxl::read_excel(temp, skip = 7, sheet = 2) %>%
  transmute(date = zoo::as.yearqtr(...1, format = "%YT%q"),
            `Intérêts et dividendes nets reçus` = D4,
            `Revenu disponible brut` = B6) %>%
  gather(variable, value, -date) %>%
  filter(!is.na(value)) %>%
  filter(date >= zoo::as.yearqtr("2021 Q4")) %>%
  group_by(variable) %>%
  arrange(date) %>%
  mutate(value = 100*value/value[1])

unlink(temp)

figure1 %>%
  ggplot + geom_line(aes(x = date, y = value, color = variable)) +
  theme_minimal() + xlab("") + ylab("Indice 100 = 2021T4") +
  zoo::scale_x_yearqtr(labels = scales::date_format("%YT%q"), n = 24) +
  scale_y_log10(breaks = seq(0, 1500, 5)) +
  theme(legend.position = c(0.3, 0.8),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  geom_label(data = . %>% 
              filter(date == max(date)),
            aes(x = date, y = value,  color = variable, label = round(value, 1)))

ggsave("figure1.png", width = 1.25*6, height = 1.25*3.375)
ggsave("figure1.pdf", width = 1.25*6, height = 1.25*3.375)
