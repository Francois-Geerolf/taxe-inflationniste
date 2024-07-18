
library(tidyverse)
load("figure2.rds")

donnees <- figure2 %>%
  filter(year >= 2022) %>%
  select(-year) %>%
  t() 

colnames(donnees) <- c("2022", "2023")

donnees

table1 <- tibble(`% ou Md€` = c("% du PIB", 
                                "Md€"),
                 `Dette 2022` = c(donnees["dette_PIB", "2022"]/100,
                                  donnees["PIB", "2022"]*donnees["dette_PIB", "2022"]/100),
                 `Dette 2023` = c(donnees["dette_PIB", "2023"]/100,
                                  donnees["PIB", "2023"]*donnees["dette_PIB", "2023"]/100),
                 `Déficit 2023` = c(-donnees["deficit_PIB", "2023"]/100,
                                    -donnees["PIB", "2023"]*donnees["deficit_PIB", "2023"]/100),
                 `Taxe inflationniste` = c(-donnees["taxe_inflationniste_PIB", "2023"]/100,
                                           -donnees["taxe_inflationniste", "2023"]),
                 `Effet croissance` = c(-donnees["croissance_reelle", "2023"]*donnees["dette_PIB", "2022"]/10000,
                                        -donnees["croissance_reelle", "2023"]*donnees["dette_PIB", "2022"]/10000*donnees["PIB", "2023"]),
                 `Charge d'intérêts` = c(donnees["charge_interets", "2023"]/donnees["PIB", "2023"],
                                         donnees["charge_interets", "2023"]),
                 `Charge d'intérêts réelle` = c(donnees["charge_interets", "2023"]/donnees["PIB", "2023"]-donnees["taxe_inflationniste_PIB", "2023"]/100,
                                                donnees["charge_interets", "2023"] - donnees["taxe_inflationniste", "2023"])) |>
  gt::gt() |>
  gt::fmt_percent(
    rows = 1,
    decimals = 1
  ) |>
  gt::fmt_number(
    rows = 2,
    decimals = 0
  ) |>
  gt::cols_align(
    align = "center",
    columns = everything()
  ) |>
  gt::cols_label(
    `% ou Md€` = gt::html(""),
    `Dette 2022` = gt::html("Dette<br>2022"),
    `Dette 2023` = gt::html("Dette<br>2023"),
    `Déficit 2023` = gt::html("Déficit<br>2023"),
    `Taxe inflationniste` = gt::html("Taxe<br>inflationniste"),
    `Effet croissance` = gt::html("Effet<br>croissance")
  )

table1

table1  |>
  gt::gtsave(filename = "table1.png")

table1  |>
  gt::gtsave(filename = "table1.pdf")

system("pdfcrop table1.pdf table1.pdf")
