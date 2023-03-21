### Script for endemicChannel testing

library(incidence)
library(lubridate)
library(ggplot2)

setwd("C:/Users/juand/Documents/GitHub/epiCo")
load("ibague_historic.RData")

first_date <- epiCalendar(2007)[1]
last_date <- rev(epiCalendar(2013))[1] + 6

incidence_historic <- incidence(ibague_historic$FEC_NOT,
  first_date = first_date,
  last_date = last_date, interval = "1 epiweek"
)

observations <- incidence_historic[1:16]$counts

outlier_years <- c("2010", "2013")

endemic_chan <- endemicChannel(observations, incidence_historic,
  outlier_years = outlier_years, outliers_handling = "replaced_by_geom_mean",
  method = "geometric", plot = TRUE
)

library(tsibble)

ts <- tsibble(
  Week = incidence_historic$dates,
  Count = as.numeric(incidence_historic$counts),
  index = Week
)

autoplot(ts)

ts %>% gg_season(Count, period = "year") +
  theme(legend.position = "none") +
  labs(y = "Number of cases")

ts %>%
  ACF(Count, lag_max = 104) %>%
  autoplot()

dcmp <- ts |>
  model(stl = STL(Count))

components(dcmp) |>
  as_tsibble() |>
  autoplot(Count, colour = "gray") +
  geom_line(aes(y = trend), colour = "#D55E00")

components(dcmp) |> autoplot()

components(dcmp) |>
  as_tsibble() |>
  autoplot(Count, colour = "gray") +
  geom_line(aes(y = season_adjust), colour = "#0072B2")

ts |>
  model(
    classical_decomposition(Count, type = "additive")
  ) |>
  components() |>
  autoplot()


# Estimate parameters
fit <- ts |>
  model(multiplicative = ETS(Count ~ error("A") + trend("Ad") +
    season("M", period = 12)))
fc <- fit |>
  forecast(h = 52)

fc |>
  autoplot(ts) +
  geom_line(aes(y = .fitted),
    col = "#D55E00",
    data = augment(fit)
  )

# Auto Arima

fit <- ts |>
  model(
    auto = ARIMA(ts, stepwise = FALSE, approx = FALSE)
  )

fc <- fit |>
  forecast(h = 8)

fc |>
  autoplot() +
  geom_line(aes(y = .fitted),
    col = "#D55E00",
    data = augment(fit)
  )

library(forecast)

model <- auto.arima(ts)
model




ts |>
  model(
    STL(
      ts ~ trend(window = 7) +
        season(window = "periodic"),
      robust = TRUE
    )
  ) |>
  components() |>
  autoplot()

### CARLOS FEEL
ST <- ts(data = as.numeric(incidence_historic$counts), frequency = 26)
D1 <- stl(ST, s.window = 26, t.window = 52)
plot(D1)
SEAS <- D1$time.series[, 1]
TREND <- D1$time.series[, 2]
REM <- D1$time.series[, 3]
plot(ST)
lines((TREND + SEAS), col = "red")
lines(TREND, col = "blue")

D1$time.series


model <- auto.arima(ST, D = 1)



# WAVELETS
library(ggplot2)
# install.packages("WaveletComp")
library(WaveletComp)
DF <- data.frame(t = seq(1, length(ST), by = 1), Inc = ST) # Creamos dataframe
w.analisis <- analyze.wavelet(DF, "Inc", loess.span = 1, dt = 1 / 52, upperPeriod = 52 * 3, make.pval = T, n.sim = 100) # Corremos wavelet. JUGAR CON LOESS.SPAN
wt.image(w.analisis)

pronostico <- forecast(model, 1, level = 95) # Pronostico SARIMA Casos, se pone el numero de unidades de tiempo que se hara el pronostico con su nivel de confidencia. En estos modelos es normal que usar valores altos de tiempo hagan una prediccion plana pues se predice sobre predicciones.
plot(pronostico)


# Prueba

ST2 <- msts(as.numeric(incidence_historic$counts), seasonal.periods = c(52))

fc <- auto.arima(ST2, D = 1)

for_fc <- forecast(fc, h = 52 * 3)
plot(for_fc)
