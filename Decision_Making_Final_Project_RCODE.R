install.packages("lmtest")

library(lmtest)
library(sandwich)
library(parameters)
library(psych)

FRED_DF <- data.frame(Source_Data_FredGRAPH)

LaLaLa <- lm(NASDAQCOM~VIXCLS, data=FRED_DF)
summary(LaLaLa)

LaLaLA_1 <- lm(NASDAQCOM~GDP, data=FRED_DF)
summary(LaLaLA_1)

LaLaLa_2 <- lm(NASDAQCOM~T5YIE, data=FRED_DF)
summary(LaLaLa_2)

LaLaLa_Test <- lm(NASDAQCOM~VIXCLS+GDP, data=FRED_DF)
summary(LaLaLa_Test)

LaLaLa_3 <- lm(NASDAQCOM~DEXUSEU, data=FRED_DF)
summary(LaLaLa_3)

LaLaLa_4 <- lm(NASDAQCOM~DEXUSUK, data=FRED_DF)
summary(LaLaLa_4)

LaLaLa_5 <- lm(NASDAQCOM~DEXCHUS, data=FRED_DF)
summary(LaLaLa_5)

LaLaLa_6 <- lm(NASDAQCOM~UNRATE, data=FRED_DF)
summary(LaLaLa_6)

LaLaLa_7 <- lm(NASDAQCOM~DGS10, data=FRED_DF)
summary(LaLaLa_7)

LaLaLa_8 <- lm(NASDAQCOM~CPALTT01USM657N, data=FRED_DF)
summary(LaLaLa_8)

LaLaLa_9 <- lm(NASDAQCOM~VIXCLS+GDP+DEXUSUK+UNRATE+DGS10+CPALTT01USM657N, data=FRED_DF)
summary(LaLaLa_9)

LaLaLa_10 <- lm(NASDAQCOM~VIXCLS*GDP, data=FRED_DF)
summary(LaLaLa_10)

LaLaLa_11 <- lm(NASDAQCOM~UNRATE*CPALTT01USM657N, data=FRED_DF)
summary(LaLaLa_11)

LaLaLa_12 <- lm(NASDAQCOM~UNRATE*GDP, data=FRED_DF)
summary(LaLaLa_12)

LaLaLa_13 <- lm(NASDAQCOM~CPALTT01USM657N*GDP, data=FRED_DF)
summary(LaLaLa_13)

LaLaLa_14 <- lm(NASDAQCOM~DEXUSUK*DGS10, data=FRED_DF)
summary(LaLaLa_14)

LaLaLa_15 <- lm(NASDAQCOM~T5YIE*UNRATE, data=FRED_DF)
summary(LaLaLa_15)
