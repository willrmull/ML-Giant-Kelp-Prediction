library(fpp3)
library(zoo)
library(tsibbledata)

nino <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/Projects/Spring 2025/ML-Giant-Kelp-Predtiction/Datasets/Macrocystis pyrifera biomass and environmental drivers/climind_quarterly.csv")
nino <- nino %>% mutate(date = paste(year, quarter))
nino$date <- as.yearqtr(nino$date, "%Y%q")
nino <- nino %>% select(-c(year, quarter))

#import main dataset
df_final_sites <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/Projects/Spring 2025/ML-Giant-Kelp-Predtiction/df_final_sites.csv")

site_final <- df_final_sites %>% group_by(date, Site) %>%
  reframe(temp = mean(temp),
          kelp = sum(kelp),
          no3 = mean(no3), 
          waves = mean(waves)) %>%
  ungroup()

nino_merged <- merge(nino, site_final, by=c("date"))

d <- nino_merged %>% filter(date < 2020)
d <- na.omit(d)

d$Site <- as.numeric(as.factor(d$Site))

d$date <- yearquarter(d$date)
d <- d %>% relocate(date, Site, kelp)
d <- as_tsibble(d, key = Site, index = date,  validate = TRUE, .drop = TRUE,)

#########Making Heirachical tsibble

d_reg <- d %>% filter(year(date) <= 2018)

#Xreg variables
xreg <- cbind(sst = d_reg[, "temp"],
              no3 = d_reg[, "no3"],
              waves = d_reg[, "waves"],
              date = d_reg[, "date"],
              NPGO = d_reg[, "NPGO"],
              MEI = d_reg[, "MEI"],
              PDO = d_reg[, "PDO"],
              date = d_reg[, "date"])

subfull <- d |>
  aggregate_key(Site, kelp = sum(kelp))

fit <- subfull |>
  filter(year(date) <= 2017) |>
  model(NNETAR(kelp, n_nodes = 10, P = 4, n_networks = 100, scale_inputs = TRUE, xreg = as.matrix(xreg)))

fit <- subfull |>
  filter(year(date) <= 2017) |>
  model(NNETAR(kelp, n_nodes = 10, p = NULL, P = 4, period=4, xreg = as.matrix(xreg)))


fit %>% 
  filter(is_aggregated(Site)) %>%
  forecast(h = 4) %>%
  autoplot(subfull) %>%
  fitted()

fit_val <- fit %>% 
  filter(is_aggregated(Site)) %>%
  forecast(h = 4) 

#Calculate accuracy
result <- fit_val %>%
  accuracy(subfull, by=c("date", ".model"))

fitted(fitted)
fitted
