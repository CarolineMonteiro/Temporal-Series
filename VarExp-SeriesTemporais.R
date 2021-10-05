library(readxl)
library(dynlm)
library(lmtest)
library(forecast)

dados <- read_excel("Dados.xlsx")

# Dados mensais:

y_ts <- ts(dados$y,
           start = c(2013, 1),
           end = c(2020, 12),
           frequency = 12) 

x1_ts <- ts(dados$x1,
            start = c(2013, 1),
            end = c(2020, 12),
            frequency = 12)

x2_ts <- ts(dados$x2,
            start = c(2013, 1),
            end = c(2020, 12),
            frequency = 12)

x3_ts <- ts(dados$x3,
            start = c(2013, 1),
            end = c(2020, 12),
            frequency = 12)

dados_ts <- ts.union(y_ts, x1_ts, x2_ts, x3_ts)

mod <- dynlm(y_ts ~ x1_ts + x2_ts + x3_ts + L(y_ts))
coeftest(mod)
summary(mod)

subset <- window(dados_ts, c(2020, 11), c(2020, 12))
forecast <- coef(mod) %*% c(1, subset[2,2], subset[2,3], subset[2,4], subset[1,1])
