### Script for endemicChannel testing

load("ibague_historic.RData")

first_date = epiCalendar(2007)[1]
last_date = rev(epiCalendar(2013))[1]+6

incidence_historic <- incidence(ibague_historic$FEC_NOT, first_date = first_date,
                                last_date = last_date, interval = "1 epiweek")

extra_weeks <- which(epiweek(incidence_historic$dates)==53)

dates_historic <- incidence::get_dates(incidence_historic)[-extra_weeks]
counts_historic <- incidence::get_counts(incidence_historic)[-extra_weeks]

historic <- as.data.frame(matrix(counts_historic, nrow = 7, byrow = TRUE))
colnames(historic) <- seq(1,52)
rownames(historic) <- seq(2007,2013)

GM <- as.numeric(apply(historic, MARGIN = 2, FUN = geomMean, method = "positive"))
CI <- as.numeric(apply(historic, MARGIN = 2, FUN = function(x) qt(p = c(0.975),df = 6)*sd(x)/sqrt(length(x))))
UL <- GM+CI
LL <- GM-CI

GM <- as.numeric(apply(historic, MARGIN = 2, FUN = mean))
Q <- as.data.frame(matrix(as.numeric(apply(historic, MARGIN = 2, FUN = quantile)), nrow = 5))