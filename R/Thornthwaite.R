
library(tidyverse) # arreglo de datos 
library(lubridate) # fechas
library(hutilscpp) 

data <- read_csv("data/Tpr_Prc_thornthwaite.csv", col_names = TRUE)

# obtener el índice de calor para el año
I <- data %>%   
mutate(
    I = (Tpr/5)^1.514 
         )  %>% 
  select(I) %>% 
  colSums(na.rm = TRUE)

# exponente a
a <- (675e-9 * I^3) - (771e-7 * I^2 ) + (1792e-5 * I) + 0.49239

# horas de sol para 15°N
NN <- c(11.2, 11.55, 11.9, 12.35, 12.65, 12.85, 12.85, 12.5, 12.1, 11.65, 11.25, 11.15)

df <- data %>% 
  mutate(
    i = (Tpr/5)^1.514,
    ETP = 16 * ((10 * Tpr) / 44.07)^a,
    NN = NN,
    N = 24 - NN,             # conversión a horas en el hemisferio sur
    d = days_in_month(1:12),
    corr = (N/12) * (d/30),
    ETPcorr = ETP * corr, 
    Prc_util = Prc - ETPcorr, 
    reserva = case_when(cumsum(Prc_util) > 100 ~ 100,
                        TRUE ~ cumsum(Prc_util)),
    reserva = case_when(reserva < 0 ~ 0, 
                        TRUE ~ reserva
                        ),
    varReserva = reserva - dplyr::lag(reserva),
    varReserva = ifelse(is.na(varReserva), reserva, varReserva),
    ETR = case_when(Prc > ETPcorr ~ ETPcorr,
                    Prc < ETPcorr ~ (Prc+ abs(varReserva))),
    deficit = ETPcorr - ETR, 
    Exceso = ifelse(Prc > (ETPcorr + varReserva),
                    ETPcorr + varReserva, 
                    0)
  )
df


