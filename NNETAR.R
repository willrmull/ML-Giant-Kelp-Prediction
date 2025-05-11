library(fpp3)
library(zoo)
library(tsibbledata)

nino <- read.csv("http://raw.githubusercontent.com/willrmull/ML-Giant-Kelp-Prediction/refs/heads/main/Datasets/climind_quarterly.csv")
nino <- nino %>% mutate(date = paste(year, quarter))
nino$date <- as.yearqtr(nino$date, "%Y%q")
nino <- nino %>% select(-c(year, quarter))

nino_merged <- merge(nino, final_dataset, by=c("date"))

nino_merged <- nino_merged %>% filter(date < 2020)
nino_merged <- na.omit(d)

nino_merged$Site <- as.numeric(as.factor(nino_merged$Site))

nino_merged$date <- yearquarter(nino_merged$date)
nino_merged <- nino_merged %>% relocate(date, Site, kelp)
nino_merged <- as_tsibble(nino_merged, key = Site, index = date,  validate = TRUE, .drop = TRUE,)

#########Making Heirachical tsibble

nino_merged_reg <- nino_merged %>% filter(year(date) <= 2018)

#Xreg variables
xreg <- cbind(sst = nino_merged_reg[, "temp"],
              no3 = nino_merged_reg[, "no3"],
              waves = nino_merged_reg[, "waves"],
              date = nino_merged_reg[, "date"],
              NPGO = nino_merged_reg[, "NPGO"],
              MEI = nino_merged_reg[, "MEI"],
              PDO = nino_merged_reg[, "PDO"],
              date = nino_merged_reg[, "date"])

subfull <- d |>
  aggregate_key(Site, kelp = sum(kelp))

model <- subfull |>
  filter(year(date) <= 2018) |>
  model(NNETAR(kelp, 
               n_nodes = 5, 
               P = 4, 
               n_networks = 100, 
               scale_inputs = TRUE, 
               xreg = as.matrix(xreg)))

fit <- model %>% 
  filter(is_aggregated(Site)) %>%
  forecast(h = 4)

fit %>% autoplot(subfull) + labs(x = "Date (Year-Quarter)", y = "Kelp Biomass (kg)", title = "Kelp Biomass")

aggregate <- subfull %>% filter(is_aggregated(Site), year(date) > 2018)
fit %>% autoplot(aggregate) + labs(x = "Date (Year-Quarter)", y = "Kelp Biomass (kg)", title = "Kelp Biomass")


results <- fit %>% accuracy(subfull, by=c('date', '.model'))
