install.packages("lmtest")

library(lmtest)
library(sandwich)
library(parameters)
library(psych)

DF <- data.frame(caschool)

DF_01 <- DF[DF$str>=25 & DF$el_pct>=12,]

DF_02 <- DF[DF$str>=25,]

DF_03 <- DF[DF$el_pct>=12,]

head(DF_01)

Reg1 <- lm(testscr~str, data=DF_02)
summary(Reg1)

coeftest(Reg1, vcov. = vcovHC, type = "HC1")

Reg2 <- lm(testscr~el_pct, data=DF_03)
summary(Reg2)

coeftest(Reg2, vcov. = vcovHC, type = "HC1")

Reg3 <- lm(testscr~str+el_pct, data=DF_01)
summary(Reg3)

coeftest(Reg3, vcov. = vcovHC, type = "HC1")

DF_04 <- DF[DF$str<25 & DF$el_pct<12,]

Reg4 <- lm(testscr~str+el_pct, data = DF_04)
summary(Reg4)

mean(DF$str[DF$str<25])
mean(DF$el_pct[DF$el_pct<12])

DF_05 <- DF[DF$str>=25 & DF$el_pct<12,]

Reg5 <- lm(testscr~str+el_pct,data = DF_05)
summary(Reg5)

DF_06 <- DF[DF$str<25 & DF$el_pct>=12,]

Reg6 <- lm(testscr~str+el_pct,data = DF_06)
summary(Reg6)

mean(DF$str[DF$str<25])
mean(DF$el_pct[DF$el_pct>=12])


Reg7 <- lm(testscr~str, data = DF)
summary(Reg7)

coeftest(Reg7, vcov. = vcovHC, type = "HC1")

Reg8 <- lm(testscr~str+computer,data=DF)
summary(Reg8)

coeftest(Reg8, vcov. = vcovHC, type = "HC1")

Reg9 <- lm(testscr~str+comp_stu,data=DF)
summary(Reg9)

coeftest(Reg9, vcov. = vcovHC, type = "HC1")

Reg10 <- lm(testscr~str+comp_stu+computer,data=DF)
summary(Reg10)

coeftest(Reg10, vcov. = vcovHC, type = "HC1")

Reg11 <- lm(testscr~str+avginc+expn_stu+meal_pct+calw_pct+teachers+enrl_tot,data=DF)
summary(Reg11)

coeftest(Reg11, vcov. = vcovHC, type = "HC1")

########################################################

New_Reg_01 <- lm(testscr~str+el_pct+str*el_pct,data = DF_01)
summary(New_Reg_01)

coeftest(New_Reg_01, vcov. = vcovHC, type = "HC1")

New_Reg_02 <- New_Reg_01 %>%
  mutate(test = ifelse(DF$str>=25 & DF$el_pct>=12,0,1))

New_Reg_03 <- lm(testscr~str+el_pct+str*el_pct,data = DF_02)
summary(New_Reg_03)

New_Reg_03 <- lm(testscr~str+el_pct+str*el_pct,data = DF)
summary(New_Reg_03)

coeftest(New_Reg_03, vcov. = vcovHC, type = "HC1")

New_Reg_04 <- lm(testscr~str+str*el_pct,data = DF)
summary(New_Reg_04)

New_Reg_05 <- lm(testscr~str*(str*el_pct),data = DF_02)
summary(New_Reg_05)

New_Reg_06 <- lm(testscr~str+str*el_pct,data = DF_02)
summary(New_Reg_06)

coeftest(New_Reg_05, vcov. = vcovHC, type = "HC1")

New_Reg_07 <- lm(testscr~str+el_pct+str*el_pct,data = DF_04)
summary(New_Reg_07)

New_Reg_08 <- lm(testscr~str+el_pct+str*el_pct,data = DF_05)
summary(New_Reg_08)

New_Reg_09 <- lm(testscr~str+el_pct+str*el_pct,data = DF_06)
summary(New_Reg_09)

########################################################
library(dplyr)

LD <- data.frame(lead)

mean(LD$infrate[LD$lead==0])
mean(LD$infrate[LD$lead==1])

a <- mean(LD$infrate[LD$lead==1])
b <- mean(LD$infrate[LD$lead==0])

aa <- c(lead$infrate[lead$lead==0])
bb <- c(lead$infrate[lead$lead==1])

Stat_mean <- t.test(aa,bb, var.equal = TRUE)
Stat_mean

Regr1 <- lm(infrate~lead+ph+lead*ph,data = LD)
summary(Regr1)

DF_06 <- DF[DF$str<25 & DF$el_pct>=12,]

LD_01 <- LD[lead$lead==1,]
LD_00 <- LD[lead$lead==0,]

plot(infrate~ph,
     col = "black",
     pch = 8,
     data = LD_00,
     main = "Without Lead")

plot(infrate~ph,
     col = "black",
     pch = 8,
     data = LD_01,
     main = "With Lead")

Regr2 <- lm(infrate~lead,data = LD)
summary(Regr2)

Regr3 <- lm(infrate~lead+ph, data = LD)
summary(Regr3)

Regr4 <- lm(infrate~lead+ph+typhoid_rate+np_tub_rate,data = LD)
summary(Regr4)

Regr5 <- lm(infrate~lead+ph+hardness+population+typhoid_rate+np_tub_rate+mom_rate+age+foreign_share+precipitation+temperature, data = LD)
summary(Regr5)

###################################################################
###################################################################
###################################################################

NN <- data.frame(inc)

Model1 <- lm(dem_ind~log_gdppc, data = NN)
summary(Model1)

coeftest(Model1, vcov. = vcovHC, type = "HC1")

library(jtools)
library(plm)

NN_01 <- summ(Model1, robust=TRUE, robust.type= "HC1", cluster= "code", digits= 20)
NN_01

Model2 <- plm(dem_ind ~ log_gdppc,
               data = NN,
               index = c("code","year"), 
               model = "within",
               effect = "twoways")     
summary(Model2)

coeftest(Model2, vcov= vcovHC, type = "HC1")



Model3 <- plm(dem_ind ~ log_gdppc,
              data = NN[NN$country!="Azerbaijan",],
              index = c("code","year"), 
              model = "within",
              effect = "twoways")     
summary(Model3)

coeftest(Model3, vcov= vcovHC, type = "HC1")

library(stargazer)

Robust_SE <- list(sqrt(diag(vcovHC(Model1, type = "HC1"))),
               sqrt(diag(vcovHC(Model2, type = "HC1"))),
               sqrt(diag(vcovHC(Model3, type = "HC1"))))

stargazer(Model1, Model2, Model3, type="html", 
          se = Robust_SE,
          dep.var.labels=c("Model_Variations"),
          out="Models123.htm")

