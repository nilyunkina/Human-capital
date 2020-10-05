install.packages("readxl")
install.packages("ggplot2")
install.packages("stargazer")
install.packages("corrplot")
install.packages("ggpubr")
install.packages("tableone")
install.packages("plm")
install.packages("AER")
install.packages("ivpack")
install.packages("gap")

library(readxl)
library(ggplot2)
library(stargazer)
library(corrplot)
library(ggpubr)
library(tableone)
library("plm")
library(car)
library(AER)
library("ivpack")
library("lmtest")
library(gap)

data <- read_excel("R2.xlsx")
str(data)
# выбросили Грецию, Йемен, Кувейт
data1 <- data[-c(22, 44, 60, 123),]
ВВП_2009 <- data$GDP09

data_dev <- subset(data, Developed == 1)
data_undev <- subset(data1, Developed == 0)
data1_dev <- subset(data1, Developed == 1)
data1_undev <- subset(data, Developed == 0)
data1_oecd <-  subset(data1, OECD == 1)
data1_nonoecd <-  subset(data1, OECD == 0)
DevelopedUN <- subset(data1, DevelopedUN == 1)
DevelopingUN <- subset (data1, DevelopingUN == 1)
TransitionUN <- subset(data1, TransitionUN  == 1)
LeastDUN <- subset (data1, LeastDUN ==1)
Fuel <- subset (data1, Fuel ==1)
High_Inc <- subset (data1, High_Inc ==1)
Upp_Mid_Inc <- subset(data1,Upp_Mid_Inc == 1)
Low_Mid_Inc <- subset(data1,Low_Mid_Inc == 1)
Low_Inc <- subset(data1,Low_Inc == 1)
# функция cse позволяет считать состоятельные в условиях гетероскедастичности стандартные ошибки 
# в случае использования МНК 
cse = function(reg) {
  rob = sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)
}
# функция для расчета робастных стандартных ошибок при использовании 2МНК
ivse = function(reg) {
  rob = robust.se(reg)[,2]
  return(rob)
}
# робастные стандартные ошибки типа HC1
hc1 <- function(x) vcovHC(x, type = "HC1")
Страна1 <- c("Развивающаяся", "Развитая")[factor(data1$Developed)]

#####################################
#описательная статистика
#####################################
summary(data)
dataset <- data[,c(3:5, 10:13,15:17, 19,27,35,44 ) ]
dataset1 <- na.omit(dataset)
desc_stats <- data.frame( 
  Min = apply(dataset1, 2, min), # minimum 
  Max = apply(dataset1, 2, max), # Maximum 
  Med = apply(dataset1, 2, median), # median 
  Mean = apply(dataset1, 2, mean), # mean 
  SD = apply(dataset1, 2, sd), # Standard deviation 
  N = nrow(dataset1)
) 
desc_stats <- round(desc_stats, 3) # 3 знака после запятой 

desc_stats1 <- ggtexttable(desc_stats, 
                           theme = ttheme("classic"))
desc_stats1
desc_stats1 <- ggtexttable(s, theme = ttheme("classic"))
###################################################
#корреляционная матрица
###################################################
Datacor <- cor(dataset1)
corrplot(Datacor, method = "color", addCoef.col = "black",
         addgrid.col = "gray33", tl.col = "black")
#############корреляция с PIACC
sub1 <- subset(dataset[,c(3:6, 19:21)])
sub1 <- na.omit(sub1)
datacor <- cor(sub1)
corrplot(datacor, method = "color", addCoef.col = "black",
         addgrid.col = "gray33", tl.col = "black")
###################################################
#######диаграмма рассеяния
###################################################
###########цвет по группам
Страна <- c("Развивающаяся", "Развитая")[factor(data$Developed)]
###########

plot(log(data$GDP_growth), data1$Cog_skills, col = Страна)
plot(data$Cog_skills, log(data1$GDP09), col = col)
text(log(data$GDP_growth), data$Cog_skills, data$`Country Name`, pos=1, offset=0.1)
#диаграмма рассеяния по всем данным
p <- ggplot(data, aes(x = HW12,y = log(GDP_growth),color = HW12, size = GDP_growth))
p + geom_point() + stat_smooth(method="lm", color = "red", size = 1) + theme_minimal() +
  labs(x="Уровень знаний",
       y="Темп роста реального ВВП на душу населения",
       title="Диаграмма рассеяния")
################красивая диаграмма с делением по группам и трендами
ВВП_2009 <- data1$GDP09
data_dev <- subset(data, Developed == 1)
data_undev <- subset(data, Developed == 0)
p <- ggplot(data1, aes(x = Cog_skills,y = log(GDP_growth),color = Страна1, size = ВВП_2009))
p <-p + geom_point() + stat_smooth(method="lm",data = data1_dev, color = "Darkturquoise", size = 1, fullrange=TRUE, se=FALSE)+
 stat_smooth(method="lm",data = data1_undev, color = "Salmon", size = 1, fullrange=TRUE, se=FALSE) + 
 stat_smooth(method="lm",data = data1, color = "Black", size = 1, fullrange=TRUE, se=FALSE) +
  geom_text(label = data1$`Country Code`,color = "Black", nudge_y = 0.1, size = 2)+
  theme_minimal() +
  labs(x="Средний балл тестирований (баллов)",
       y="Средний темп роста реального ВВП на душу населения ln (%) ",
       title="")+
  theme(text=element_text(size=8))

p
ggsave("D:/университет/диплом/модель/Графики1/plot3.jpg", plot=p, units="cm", dpi=600)
################## диаграмма с подписями
p1 <- ggplot(data1, aes(x = Cog_skills,y = log(GDP09),color = Страна1)) +
  geom_point() + geom_text(label = data1$`Country Code`,color = "Black", nudge_y = 0.1, size = 2)+
  stat_smooth(method="lm",data = data1, color = "Black", size = 1, fullrange=TRUE, se=FALSE) +
    theme_minimal() +
    labs(x="Средний балл тестирований (баллов)",
         y="Логарифм реального ВВП на душу населения 2009 г. ln(долл. США 2010г.)",
         title="")+
  theme(text=element_text(size=8))
p1
ggsave("D:/университет/диплом/модель/Графики1/plot4.jpg", plot=p1, units="cm", dpi=600)
?ggsave
#########################################
##################количество лет обучения
data_dev <- subset(data, Developed == 1)
data_undev <- subset(data, Developed == 0)
p2 <- ggplot(data, aes(x = SchoolingBL,y = log(GDP_growth),color = Страна, size = ВВП_2009))
p2 <-p2 + geom_point() + stat_smooth(method="lm",data = data_dev, color = "Darkturquoise", size = 1, fullrange=TRUE, se=FALSE)+
  stat_smooth(method="lm",data = data_undev, color = "Salmon", size = 1, fullrange=TRUE, se=FALSE) + 
  stat_smooth(method="lm",data = data, color = "Black", size = 1, fullrange=TRUE, se=FALSE) +
  theme_minimal() +
  labs(x="Уровень знаний",
       y="Логарифм средннго темпа роста реального ВВП на душу населения",
       title="Зависимость темпа роста ВВП от уровня знаний")
p2
################## диаграмма с подписями
p3 <- ggplot(data1, aes(x = Schooling,y = log(GDP09),color = Страна1)) +
  geom_point() + geom_text(label = data1$`Country Code`,color = "Black", nudge_y = 0.1, size = 2)+
  stat_smooth(method="lm",data = data1, color = "Black", size = 1, fullrange=TRUE, se=FALSE) +
  theme_minimal() +
  labs(x="Среднее число лет обучения (лет)",
       y="Логарифм реального ВВП на душу населения 2009 г. ln(долл. США 2010г.)",
       title="")+
  theme(text=element_text(size=8))
p3
ggsave("D:/университет/диплом/модель/Графики1/plot5.jpg", plot=p3, units="cm", dpi=600)
#матрица диаграмм рассеяния, выбрать столбцы
col <- c("Salmon", "Darkturquoise")[factor(data$Developed)]
pairs(data[c(4,5, 10, 13, 14, 44,3)], col = col, upper.panel=NULL)
###################################################
##### проверим баланс
table1<- CreateTableOne(vars= c("GDP_growth", "GDP09", "Investment", "Life_exp", "Pop","Trade", "IEF","Inflation",
                                "Cog_skills", "Schooling", "HW12", "PISA_math", "PISA_science", "Advanced", "Basic",
                                "TopHW", "BasicHW"),
                        strata="Developed", data=data, test=TRUE)
print(table1)
t.test(Trade ~ OECD, data = data)
##############OECD
table1<- CreateTableOne(vars= c("GDP_growth", "GDP09", "Investment", "Life_exp", "Pop","Trade", "EIF","Inflation",
                                "Cog_skills", "Schooling", "HW12", "PISA_math", "PISA_science", "Advanced", "Basic",
                                "TopHW", "BasicHW"),
                        strata="OECD", data=data, test=TRUE)
print(table1)
#::::::::::::::::::::::::::::::::::::::
# Вычислить описательные статистики Cog_skills по группам OECD
stable <- desc_statby(data, measure.var = "Cog_skills",
                      grps = "OECD")
stable <- stable[, c("OECD", "length", "min", "max", "mean","median", "sd")]
stable <- round(stable,3)
stable.p <- ggtexttable(stable, rows = NULL, 
                        theme = ttheme("classic"))

stable.p
################################
###############логарифм 
data1$ln_growth <- log(data1$GDP_growth)
data1$ln_GDP09 <- log(data1$GDP09)
##матрица диаграмм рассеяния, выбрать столбцы
col <- c("Salmon", "Darkturquoise")[factor(data$Developed)]
pairs(data1[c(4,5, 10, 13, 14, 44, 48, 3,47)], col = col, upper.panel=NULL)
###################################################
##регрессии
reg1 <- lm(log(GDP_growth) ~ log(GDP09) +Cog_skills + Schooling + Investment + Life_exp + Pop + Trade  + IEF + Inflation
            + Developed , data = data1)

reg2 <- lm(log(GDP_growth) ~ log(GDP09) + Cog_skills + Schooling + Investment + Life_exp + Trade  + IEF
           + Developed , 
           data = data1)
reg3 <- lm(log(GDP_growth) ~ log(GDP09) + Cog_skills + Investment + Life_exp + Trade  + IEF
           + Developed , 
           data = data1)
reg4 <- lm(log(GDP_growth) ~ log(GDP09) + Cog_skills + Investment + Trade  + IEF
           + Developed , 
           data = data1)
reg5 <- lm(log(GDP_growth) ~ log(GDP09) + Cog_skills + Investment
           + Developed , 
           data = data1)
stargazer(reg1, reg2, reg3, reg4, reg5,
          se=list(cse(reg1), cse(reg2), cse(reg3), cse(reg4), cse(reg5)),
          title="", type="text", 
          column.labels=c("reg1", "reg2", "reg3", "reg4", "reg5"), 
          df=FALSE, digits=4)
vif(reg5)
###################################################
##регрессии без логарифмов
reg1 <- lm(ln_growth ~ GDP09 +Cog_skills + Schooling + Investment + Life_exp + Pop + Trade  + IEF + Inflation
           + Developed , data = data1)

reg2 <- lm(ln_growth ~ GDP09 + Cog_skills + Schooling + Investment + Life_exp + Trade  + IEF
           + Developed , 
           data = data1)
reg3 <- lm(ln_growth ~ GDP09 + Cog_skills + Investment + Life_exp + Trade  + IEF
           + Developed , 
           data = data1)
reg4 <- lm(ln_growth ~ GDP09 + Cog_skills + Investment + Trade  + IEF
           + Developed , 
           data = data1)
reg5 <- lm(ln_growth ~ GDP09 + Cog_skills  + Life_exp + Trade  + IEF
           + Developed , 
           data = data1)
stargazer(reg1, reg2, reg3, reg4, reg5,
          se=list(cse(reg1), cse(reg2), cse(reg3), cse(reg4), cse(reg5)),
          title="", type="text", 
          column.labels=c("reg1", "reg2", "reg3", "reg4", "reg5"), 
          df=FALSE, digits=4)
vif(reg4)
###################################################
##регрессии без логарифмов
reg1 <- lm(GDP_growth~ GDP09 + Cog_skills + Schooling + Investment  + Pop 
           + Developed, data = data1)

reg2 <- lm(GDP_growth~ GDP09 + Cog_skills + Investment  + Pop 
           + Developed , 
           data = data1)
reg3 <- lm(GDP_growth~ GDP09 + Cog_skills + Schooling + Investment +  IEF 
           + Developed , 
           data = data1)
reg4 <- lm(GDP_growth ~ GDP09 + Cog_skills + Investment +  IEF
           + Developed , 
           data = data1)
reg5 <- lm(GDP_growth~ GDP09 + Cog_skills + SchoolingBL + Investment
           + Developed , 
           data = data1)
stargazer(reg1, reg2, reg3, reg4, reg5,
          se=list(cse(reg1), cse(reg2), cse(reg3), cse(reg4), cse(reg5)),
          title="", type="text", 
          column.labels=c("reg1", "reg2", "reg3", "reg4", "reg5"), 
          df=FALSE, digits=4)
vif(reg5)
#####Для развитых
reg1 <- lm(GDP_growth ~ GDP09 +Cog_skills+Investment+ Trade, 
           data = data1_dev)

reg2 <- lm(GDP_growth ~ GDP09+Cog_skills + Schooling + Investment+ Trade, 
           data = data1_dev)
reg3 <- lm(GDP_growth ~ GDP09 +Cog_skills+ Schooling +Investment+ Life_exp+ Trade, 
           data = data1_dev)
reg4 <- lm(GDP_growth ~ GDP09 +Cog_skills+ Schooling +Investment, 
           data = data1_dev)
reg5 <- lm(GDP_growth ~ GDP09 + Cog_skills + Schooling +Investment + IEF , 
           data = data1_dev)
stargazer(reg1, reg2, reg3, reg4, reg5,
          se=list(cse(reg1), cse(reg2), cse(reg3), cse(reg4), cse(reg5)),
          title="", type="text", 
          column.labels=c("reg1", "reg2", "reg3", "reg4", "reg5"), 
          df=FALSE, digits=4)
#####Для развивающихся
reg1 <- lm(log(GDP_growth) ~ log(GDP09) + Schooling + Investment + Life_exp + Pop + Trade + GEI + Inflation+ IEF, 
           data = data1_undev)
reg2 <- lm(log(GDP_growth) ~ log(GDP09) +Cog_skills + Schooling + Investment + Life_exp + Pop + Trade+ IEF
           + Inflation, 
           data = data1_undev)
reg3 <- lm(log(GDP_growth) ~ log(GDP09) +Cog_skills + Schooling + Investment + Life_exp + Pop+ IEF, 
           data = data1_undev)
reg4 <- lm(log(GDP_growth) ~ log(GDP09) + Cog_skills + Schooling + Investment + Life_exp + IEF, 
           data = data1_undev)
reg5 <- lm(log(GDP_growth) ~ log(GDP09) + Cog_skills + Investment+ Life_exp  +  SSA+ IEF + NetMig, 
           data = data1_undev)
stargazer(reg1, reg2, reg3, reg4, reg5,
          se=list(cse(reg1), cse(reg2), cse(reg3), cse(reg4), cse(reg5)),
          title="", type="text", 
          column.labels=c("reg1", "reg2", "reg3", "reg4", "reg5"), 
          df=FALSE, digits=4)
#####Для развивающихся
data1_nonFuel <- subset(data1, Fuel == 0)
reg1 <- lm(GDP_growth ~ GDP09 +Cog_skills+ Schooling + Investment + Life_exp + Pop + Trade + Inflation+ IEF+ Fuel, 
           data = data1_undev)
reg2 <- lm(GDP_growth ~ GDP09 +Cog_skills  + Investment  + Life_exp, 
           data = data1_undev)
reg3 <- lm(GDP_growth ~ GDP09 + Cog_skills+ Schooling + Investment, 
           data = data1_undev)
reg4 <- lm(GDP_growth ~ GDP09 + Cog_skills  + Investment+ Fuel, 
           data = data1_undev)

reg5 <- lm(GDP_growth ~ GDP09 + Cog_skills+ Schooling + Investment + Fuel +  Fuel*Cog_skills, 
           data = data1_undev)
stargazer(reg1, reg2, reg3, reg4, reg5,
          se=list(cse(reg1), cse(reg2), cse(reg3), cse(reg4), cse(reg5)),
          title="", type="text", 
          column.labels=c("reg1", "reg2", "reg3", "reg4", "reg5"), 
          df=FALSE, digits=4)


####Для развивающихся
data1_nonFuel <- subset(data1_undev, Fuel == 0)
reg1 <- lm(GDP_growth ~ GDP09 + Schooling + Investment + Life_exp + Pop + Trade + GEI + Inflation+ IEF, 
           data = data1_nonFuel)
reg2 <- lm(GDP_growth ~ GDP09 +Cog_skills + Schooling + Investment + Life_exp + Pop + Trade+ IEF
           + Inflation, 
           data = data1_nonFuel)
reg3 <- lm(GDP_growth ~ GDP09 +Cog_skills + Schooling + Investment  + Pop, 
           data = data1_nonFuel)
reg4 <- lm(GDP_growth ~ GDP09 + Cog_skills  + Investment+ Fuel, 
           data = data1_nonFuel)
reg5 <- lm(GDP_growth ~ GDP09 + Cog_skills+ Schooling + Investment +, 
           data = data1_nonFuel)
stargazer(reg1, reg2, reg3, reg4, reg5,
          se=list(cse(reg1), cse(reg2), cse(reg3), cse(reg4), cse(reg5)),
          title="", type="text", 
          column.labels=c("reg1", "reg2", "reg3", "reg4", "reg5"), 
          df=FALSE, digits=4)
vif(reg1)
#####Для развивающихся без Сахары
#####с Сахарой
data3_undev <- subset(data1_undev, SSA ==0)
data3<- subset(data1_undev, SSA ==1)
reg1 <- lm(log(GDP_growth) ~ log(GDP09) + Schooling + Investment + Life_exp + Pop + Trade + GEI + Inflation+ SSA, 
           data = data3)
reg2 <- lm(log(GDP_growth) ~ log(GDP09) +Cog_skills + Schooling + Investment + Life_exp + Pop + Trade
           + Inflation, 
           data = data3)
reg3 <- lm(log(GDP_growth) ~ log(GDP09) +Cog_skills + Schooling + Investment + Life_exp + Pop, 
           data = data3)
reg4 <- lm(log(GDP_growth) ~ log(GDP09) + Cog_skills + Schooling + Investment + Life_exp  + GEI, 
           data = data3)
reg5 <- lm(log(GDP_growth) ~ log(GDP09) + Cog_skills + Investment+ Life_exp  +  SSA*Cog_skills + EAP*Cog_skills, 
           data = data3)
stargazer(reg1, reg2, reg3, reg4, reg5,
          se=list(cse(reg1), cse(reg2), cse(reg3), cse(reg4), cse(reg5)),
          title="", type="text", 
          column.labels=c("reg1", "reg2", "reg3", "reg4", "reg5"), 
          df=FALSE, digits=4)


#############Общая таблица
reg1 <- lm(GDP_growth ~ GDP09 +Cog_skills+ Schooling+Investment+ Life_exp + Trade  + IEF, 
           data = data1_dev)
reg2 <- lm(GDP_growth ~ GDP09 +Cog_skills+ Schooling +Investment, 
           data = data1_dev)

reg3 <- lm(GDP_growth ~ GDP09 + Cog_skills  + Schooling+ Investment+ Life_exp + Trade  + IEF, data = data1_undev+Pop)
reg4 <- lm(GDP_growth ~ GDP09 + Cog_skills+ Schooling + Investment, 
           data = data1_undev)
reg5 <- lm(GDP_growth ~ GDP09 + Cog_skills+ Schooling + Investment + Fuel +  Fuel*Cog_skills, 
           data = data1_undev)
stargazer(reg1, reg2, reg3, reg4, reg5,
          se=list(cse(reg1), cse(reg2), cse(reg3), cse(reg4), cse(reg5)),
          title="", type="text", 
          column.labels=c("reg1", "reg2", "reg3", "reg4", "reg5"), 
          df=FALSE, digits=4)
vif(reg)

#############Общая таблица OECD
reg1 <- lm(log(GDP_growth) ~ log(GDP09) + Cog_skills + Schooling + Investment+ NetMig, 
           data = data1_oecd)
reg2 <- lm(log(GDP_growth) ~ log(GDP09) + Cog_skills + Investment+ NetMig, 
           data = data1_oecd)
reg3 <- lm(log(GDP_growth) ~ log(GDP09) + Cog_skills + Schooling  + NetMig, 
           data = data1_nonoecd)
reg4 <- lm(log(GDP_growth) ~ log(GDP09) + Cog_skills + Investment + NetMig, 
           data = data1_nonoecd)
stargazer(reg1, reg2, reg3, reg4,
          se=list(cse(reg1), cse(reg2), cse(reg3), cse(reg4)),
          title="", type="text", 
          column.labels=c("reg1", "reg2", "reg3", "reg4"), 
          df=FALSE, digits=4)

vif(reg4)
cor(data1$Cog_skills, data1$GEI)
#############с учетом распределения знаний
#матрица диаграмм рассеяния, выбрать столбцы
col <- c("Salmon", "Darkturquoise")[factor(data$Developed)]
pairs(data[c(3,4,9:12,24,25)], col = col, upper.panel=NULL)
###################################################

################красивая диаграмма с делением по группам и трендами доля умных

p <- ggplot(data1, aes(x = Advanced,y = GDP_growth,color = Страна1, size = Cog_skills))
p <-p + geom_point() + stat_smooth(method="lm",data = data1_dev, color = "Darkturquoise", size = 1, fullrange=TRUE, se=FALSE)+
  stat_smooth(method="lm",data = data1_undev, color = "Salmon", size = 1, fullrange=TRUE, se=FALSE) + 
  stat_smooth(method="lm",data = data1, color = "Black", size = 1, fullrange=TRUE, se=FALSE) +
  geom_text(label = data1$`Country Code`,color = "Black", nudge_y = 0.1, size = 2)+
  theme_minimal() +
  labs(x="Доля набравших высокий балл (долей единицы)",
       y="Средний темп роста реального ВВП на душу населения (%)",
       title="")
p
ggsave("D:/университет/диплом/модель/Графики1/plot11.jpg", plot=p, units="cm", dpi=600)
################красивая диаграмма с делением по группам и трендами доля умных

ВВП_2009 <- data$GDP09
data1_dev <- subset(data1, Developed == 1)
data1_undev <- subset(data1, Developed == 0)
p <- ggplot(data1, aes(x = Basic,y = GDP_growth,color = Страна1, size = Cog_skills))
p <-p + geom_point() + stat_smooth(method="lm",data = data1_dev, color = "Darkturquoise", size = 1, fullrange=TRUE, se=FALSE)+
  stat_smooth(method="lm",data = data1_undev, color = "Salmon", size = 1, fullrange=TRUE, se=FALSE) + 
  stat_smooth(method="lm",data = data1, color = "Black", size = 1, fullrange=TRUE, se=FALSE) +
  geom_text(label = data1$`Country Code`,color = "Black", nudge_y = 0.1, size = 2)+
  theme_minimal() +
  labs(x="Доля набравших больше порогового балла (долей единицы)",
       y="Средний темп роста реального ВВП на душу населения (%)",
       title="")
p
ggsave("D:/университет/диплом/модель/Графики1/plot21.jpg", plot=p, units="cm", dpi=600)
#####################################развитые модели 4-6!!!!!!!!!!!!!!!!!!!!!!!
##############################
##############################

reg1 <- lm(GDP_growth ~ GDP09 + Advanced + Basic + Schooling + Investment+ Life_exp + Pop + Trade + IEF + Inflation, 
           data = data1_dev)
reg2 <- lm(GDP_growth ~ GDP09 + Advanced + Basic + SchoolingBL + Investment+ Life_exp + Pop + Trade + IEF + Inflation, 
           data = data1_dev)
reg3 <- lm(GDP_growth ~ GDP09 + Advanced + Basic + Schooling + Investment+ Life_exp + Pop + IEF, 
           data = data1_dev)
reg4 <- lm(GDP_growth ~ GDP09 + Advanced + Basic + Investment, 
           data = data1_dev)
reg5 <- lm(GDP_growth ~ GDP09 + Advanced + Basic + Schooling + Investment, 
           data = data1_dev)
reg6 <- lm(GDP_growth ~ GDP09 + Advanced + Basic + SchoolingBL + Investment, 
           data = data1_dev)
stargazer(reg1, reg2, reg3, reg4,reg5,reg6,
          se=list(cse(reg1), cse(reg2), cse(reg3), cse(reg4), cse(reg5),cse(reg6)),
          title="", type="text", 
          column.labels=c("reg1", "reg2", "reg3", "reg4", "reg5", "reg6"), 
          df=FALSE, digits=4)

vif(reg6)
#####################################3
data_oecd <- subset(data, OECD == 1)
data_nonoecd <- subset(data, OECD == 0)
########################################3

########################################для развивающихся - регрессии 5,6
reg1 <- lm(GDP_growth ~ GDP09 + Advanced + Basic + Schooling1 + Investment + GEI  + NetMig + Pop+ Trade, 
           data = data1_undev)
reg2 <- lm(GDP_growth ~ GDP09 + Advanced + Basic + Investment,  
           data = data1_undev)
reg3 <- lm(GDP_growth ~ GDP09 + Advanced + Basic + Schooling1 + Investment + GEI  + NetMig+ Trade + Inflation,
           data = data1_undev)
reg4 <- lm(GDP_growth ~ GDP09 + Advanced + Basic + SchoolingBL + Investment + GEI + NetMig, 
           data = data1_nonFuel)
reg5 <- lm(GDP_growth ~ GDP09 + Advanced + Basic + Schooling + Investment + GEI + NetMig, 
           data = data1_nonFuel)
reg6 <- lm(GDP_growth ~ GDP09 + Advanced + Basic + Investment + GEI + NetMig, 
           data = data1_nonFuel)
stargazer(reg1, reg2, reg3, reg4,reg5,reg6,
          se=list(cse(reg1), cse(reg2), cse(reg3), cse(reg4), cse(reg5),cse(reg6)),
          title="", type="text", 
          column.labels=c("reg1", "reg2", "reg3", "reg4", "reg5", "reg6"), 
          df=FALSE, digits=4)
vif(reg6)

#################################объединяем
##матрица диаграмм рассеяния, выбрать столбцы
col <- c("Salmon", "Darkturquoise")[factor(data$Developed)]
pairs(data1[c(4,5, 11,12, 14, 44, 48, 3,47)], col = col, upper.panel=NULL)
###################################################
data3 <- subset(data1_nonFuel, SSA == 0)
data1_Fuel <- subset(data1, SSA == 1)
dataL <- subset(data1_nonFuel, SSA == 1|Fuel ==1 )
data4 <- subset(data1_undev, SSA == 0)

reg1 <- lm(log(GDP_growth) ~ GDP09 + Advanced  + Investment + Pop+Trade+IEF, 
           data = data1_dev)
reg2 <- lm(log(GDP_growth) ~ GDP09 + Basic  + Investment+ Pop+Trade+IEF, 
           data = data1_dev)
reg3 <- lm(log(GDP_growth) ~ GDP09 + Advanced + Investment+ Pop+Trade+IEF, 
           data = data3)
reg4 <- lm(log(GDP_growth) ~ GDP09 + Basic + Investment + Pop+Trade+IEF, 
           data = data3)
reg5 <- lm(log(GDP_growth) ~ GDP09 + Advanced  + Investment + Pop+Trade+IEF , 
           data = data1_Fuel)
reg6 <- lm(log(GDP_growth) ~ GDP09 + Basic + Investment + Pop+Trade+IEF+Fuel*Basic, 
           data = data1_Fuel)


reg5 <- lm(log(GDP_growth) ~ GDP09 + Advanced  + Investment + Pop+Trade+IEF+Fuel +SSA, 
           data = dataL)
reg6 <- lm(log(GDP_growth) ~ GDP09 + Basic + Investment + Pop+Trade+IEF +Fuel +SSA, 
           data = dataL)


reg3 <- lm(log(GDP_growth) ~ GDP09 + Advanced + Investment+ Pop+Trade+IEF +Fuel*Advanced, 
           data = data4)
reg4 <- lm(log(GDP_growth) ~ GDP09 + Basic + Investment + Pop+Trade+IEF +Fuel*Basic, 
           data = data4)


reg1 <- lm(log(GDP_growth) ~ GDP09 + Advanced  + Investment + Pop+Trade+IEF +Fuel*Advanced + SSA*Advanced, 
           data = data1_undev)
reg2 <- lm(log(GDP_growth) ~ GDP09 + Basic  + Investment+ Pop+Trade+IEF+Fuel*Basic+ SSA*Basic, 
           data = data1_undev)

reg1 <- lm(log(GDP_growth) ~ GDP09 + Advanced  + Investment + Pop+Trade+IEF +Fuel*Advanced  + Developed*Advanced, 
           data = data1)
reg2 <- lm(log(GDP_growth) ~ GDP09 + Basic  + Investment+ Pop+Trade+IEF+Fuel*Basic+ Developed*Basic, 
           data = data1)
reg3 <- lm(log(GDP_growth) ~ GDP09 + Advanced  + Investment +Fuel*Advanced   + Developed*Advanced, 
           data = data1)
reg4 <- lm(log(GDP_growth) ~ GDP09 + Basic  + Investment+Fuel*Basic+ Developed*Basic, 
           data = data1)

stargazer(reg1, reg2, reg3, reg4,reg5,reg6,
          se=list(cse(reg1), cse(reg2), cse(reg3), cse(reg4), cse(reg5),cse(reg6)),
          title="", type="html", 
          column.labels=c("reg1", "reg2", "reg3", "reg4", "reg5", "reg6"), 
          df=FALSE, digits=4)
vif(reg1)
data1$AD <- data1$Advanced*data1$Developed

###############Инструменты
iv <- data.frame('GDP_growth1'= lnGDP_growth,'GDP091' = lnGDP09, GDP_growth = data1$GDP_growth,GDP09 = data1$GDP09, Cog_skills = data1$Cog_skills, Investment=data1$Investment,
                 Pup_teach= data1$Pup_teach,GEI= data1$GEI, Schooling= data1$Schooling, 
                 Developed = data1$Developed, Trade = data1$Trade, IEF = data1$IEF, Pop = data1$Pop)
iv <- na.omit(iv)
lnGDP_growth = (log(data1$GDP_growth))
lnGDP09 = (log(data1$GDP09))
iv <- data.frame('GDP_growth1'= lnGDP_growth,'GDP091' = lnGDP09, Cog_skills = data1$Cog_skills, Investment=data1$Investment,
                 Pup_teach= data1$Pup_teach,GEI= data1$GEI, Schooling= data1$Schooling, Life_exp = data1$Life_exp,
                 Developed = data1$Developed, Trade = data1$Trade, Pop = data1$Pop)
iv <- na.omit(iv)
iv1 <- ivreg(GDP_growth1 ~ GDP091 + Cog_skills + Investment + Developed + Trade|GDP091 + Pup_teach + Investment + Developed + GEI + Trade, data=iv)
summary(iv1)
stargazer(iv1, 
          se=list(ivse(iv1)),
          title="", type="text", 
          column.labels=c("reg1"), 
          df=FALSE, digits=4)

########################################норм, но слабые инструменты!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
iv1 <- ivreg(GDP_growth ~ GDP09 + Cog_skills+ Investment + Developed  |GDP09 + Pup_teach
             +Investment+ Schooling +Developed + GEI, data=iv)
iv2 <- ivreg(GDP_growth ~ GDP09 + Cog_skills+ Investment + Developed + Trade |GDP09 + Pup_teach
             +Investment+ Schooling +Developed + GEI + Trade , data=iv)

stargazer(iv1, iv2, 
          se=list(ivse(iv1), ivse(iv2)),
          title="", type="text", 
          column.labels=c("reg1", "reg2"), 
          df=FALSE, digits=4)
vif(iv1)
#проверка релевантности инструментов
MNK1_rel <- lm(Cog_skills ~ GDP09 + Pup_teach
               +Investment+ Schooling +Developed + GEI, data = iv)
MNK2_rel <- lm(Cog_skills ~ GDP09 + Pup_teach
               +Investment+ Schooling +Developed + GEI + Trade, data = iv)
linearHypothesis(MNK1_rel, c("Pup_teach = 0", "Schooling = 0", "GEI = 0"), vcov = hc1)
linearHypothesis(MNK2_rel, c("Pup_teach = 0", "Schooling = 0", "GEI = 0"), vcov = hc1)

#проверка эндогенности
fm_or <- lm(residuals(iv1) ~ GDP09 + Pup_teach
            +Investment+ Schooling +Developed + GEI, data = iv)
fm_or2 <- lm(residuals(iv2) ~ GDP09 + Pup_teach
             +Investment+ Schooling +Developed + GEI + Trade, data = iv)
# значение J-статистики
(fm_or_test <- linearHypothesis(fm_or, c("Pup_teach = 0", "Schooling = 0", "GEI = 0"), test = "Chisq"))
(fm_or_test2 <- linearHypothesis(fm_or2, c("Pup_teach = 0", "Schooling = 0", "GEI = 0"), test = "Chisq"))
# p-value
pchisq(fm_or_test[2,5], df.residual(iv1) - df.residual(fm_or), lower.tail = FALSE)
pchisq(fm_or_test2[2,5], df.residual(iv2) - df.residual(fm_or2), lower.tail = FALSE)
# p-значение - большое, принимаем н0 об экзогенности инструментов
stargazer(MNK1_rel,
          se=list(cse(MNK1_rel)),
          title="", type="text", 
          column.labels=c("MNK1_rel"), 
          df=FALSE, digits=4)
stargazer(MNK2_rel,
          se=list(cse(MNK2_rel)),
          title="", type="text", 
          column.labels=c("MNK2_rel"), 
          df=FALSE, digits=4)

stargazer(MNK1_rel, MNK2_rel,
          se=list(cse(MNK1_rel), cse(MNK2_rel)),
          title="", type="text", 
          column.labels=c("MNK1_rel", "MNK2_rel"), 
          df=FALSE, digits=4)

stargazer(reg1, reg2, reg3, reg4,reg5,reg6,
          se=list(cse(reg1), cse(reg2), cse(reg3), cse(reg4), cse(reg5),cse(reg6)),
          title="", type="text", 
          column.labels=c("reg1", "reg2", "reg3", "reg4", "reg5", "reg6"), 
          df=FALSE, digits=4)

########################################норм, но слабые инструменты

iv <- na.omit(iv)
iv1 <- ivreg(GDP_growth ~ GDP09 + Cog_skills+ Investment + Developed|GDP09  +Pup_teach + Investment+ Schooling +Developed, data=iv)
summary(iv1)
stargazer(iv1, 
          se=list(ivse(iv1)),
          title="", type="text", 
          column.labels=c("reg1"), 
          df=FALSE, digits=4)
#проверка релевантности инструментов
MNK1_rel <- lm(Cog_skills ~ GDP091  +Pup_teach + Investment+ Schooling +Developed, data = iv)
linearHypothesis(MNK1_rel, c("Pup_teach = 0", "Schooling = 0"), vcov = hc1)
linearHypothesis(MNK1_rel, "Pup_teach = 0", vcov = hc1)
#проверка эндогенности
fm_or <- lm(residuals(iv1) ~ GDP091  +Pup_teach + Investment+ Schooling +Developed, data = iv)
# значение J-статистики
(fm_or_test <- linearHypothesis(fm_or, c("Pup_teach = 0", "GEI = 0"), test = "Chisq"))
# p-value
pchisq(fm_or_test[2,5], df.residual(iv1) - df.residual(fm_or), lower.tail = FALSE)
# p-значение - большое, принимаем н0 об экзогенности инструментов

################################


##регрессии
#############исключили Индию, Канаду и США
data2 <- data1[-c(19, 48, 117),]

data2$SSA_Skills <- data2$Cog_skills*data2$SSA
data2$EAP_Skills <- data2$Cog_skills*data2$EAP
data2$MENA_Skills <- data2$Cog_skills*data2$MENA
data2$LAC_Skills <- data2$Cog_skills*data2$LAC
data2$ECA_Skills <- data2$Cog_skills*data2$ECA


reg1 <- lm(log(GDP_growth) ~ log(GDP09)+Cog_skills + Schooling + Investment  + Pop + Trade + NetMig
           + ECA_Skills + EAP_Skills + MENA_Skills + LAC_Skills + Developed, data = data2)
reg2 <- lm(log(GDP_growth) ~ log(GDP09)+Cog_skills + Investment + Trade + NetMig
           + ECA_Skills + EAP_Skills + MENA_Skills + LAC_Skills + Developed, data = data2)


reg3 <- lm(log(GDP_growth) ~ log(GDP09)+Cog_skills + Schooling + Investment  + Pop + Trade + NetMig
           + ECA_Skills+ Developed, data = data2)
reg4 <- lm(log(GDP_growth) ~ log(GDP09)+Cog_skills + Schooling + Investment  + Pop + Trade + NetMig
           + EAP_Skills + Developed, data = data2)
reg5 <- lm(log(GDP_growth) ~ log(GDP09)+Cog_skills + Schooling + Investment  + Pop + Trade+ NetMig
           + MENA_Skills + Developed, data = data2)

reg6  <- lm(log(GDP_growth) ~ log(GDP09)+Cog_skills + Schooling + Investment  + Pop + Trade + NetMig
            + LAC_Skills + Developed, data = data2)
stargazer(reg1, reg2, reg3, reg4, reg5,reg6,
          se=list(cse(reg1), cse(reg2), cse(reg3), cse(reg4), cse(reg5), cse(reg6)),
          title="", type="text", 
          column.labels=c("reg1", "reg2", "reg3", "reg4", "reg5", "reg6"), 
          df=FALSE, digits=4)
vif(reg2)

##############по сравнению с европой


reg1 <- lm(log(GDP_growth) ~ log(GDP09)+Cog_skills + Schooling + Investment  + Trade + NetMig + Pop
           + SSA_Skills + EAP_Skills + MENA_Skills + LAC_Skills , data = data2)
reg2 <- lm(log(GDP_growth) ~ log(GDP09)+Cog_skills + Investment + Trade + NetMig + Pop
           + SSA_Skills + EAP_Skills + MENA_Skills + LAC_Skills, data = data2)


reg3 <- lm(log(GDP_growth) ~ log(GDP09)+Cog_skills  + Investment + Trade + NetMig + Pop
           + SSA_Skills, data = data2)
reg4 <- lm(log(GDP_growth) ~ log(GDP09)+Cog_skills  + Investment  + Trade + NetMig + Pop
           + EAP_Skills , data = data2)
reg5 <- lm(log(GDP_growth) ~ log(GDP09)+Cog_skills  + Investment   + Trade+ NetMig + Pop
           + MENA_Skills , data = data2)

reg6  <- lm(log(GDP_growth) ~ log(GDP09)+Cog_skills  + Investment   + Trade + NetMig + Pop
           + LAC_Skills, data = data2)
stargazer(reg1, reg2, reg3, reg4, reg5,reg6,
          se=list(cse(reg1), cse(reg2), cse(reg3), cse(reg4), cse(reg5), cse(reg6)),
          title="", type="text", 
          column.labels=c("reg1", "reg2", "reg3", "reg4", "reg5", "reg6"), 
          df=FALSE, digits=4)
vif(reg2)


reg1 <- lm(log(GDP_growth) ~ log(GDP09)+Cog_skills + Schooling + Investment 
           + SSA_Skills + EAP_Skills + MENA_Skills + ECA_Skills, data = data2)
reg2 <- lm(log(GDP_growth) ~ log(GDP09)+Cog_skills + Investment + Trade + NetMig
           + SSA_Skills + EAP_Skills + MENA_Skills + ECA_Skills, data = data2)


reg3 <- lm(log(GDP_growth) ~ log(GDP09)+Cog_skills  + Investment + Trade + NetMig
           + SSA_Skills, data = data2)
reg4 <- lm(log(GDP_growth) ~ log(GDP09)+Cog_skills  + Investment  + Trade + NetMig
           + EAP_Skills , data = data2)
reg5 <- lm(log(GDP_growth) ~ log(GDP09)+Cog_skills  + Investment   + Trade+ NetMig
           + MENA_Skills, data = data2)

reg6  <- lm(log(GDP_growth) ~ log(GDP09)+Cog_skills  + Investment   + Trade + NetMig
            + ECA_Skills, data = data2)
stargazer(reg1, reg2, reg3, reg4, reg5,reg6,
          se=list(cse(reg1), cse(reg2), cse(reg3), cse(reg4), cse(reg5), cse(reg6)),
          title="", type="text", 
          column.labels=c("reg1", "reg2", "reg3", "reg4", "reg5", "reg6"), 
          df=FALSE, digits=4)
vif(reg2)




data_SSA <- subset(data2, SSA == 1)
reg1 <- lm(log(GDP_growth) ~ log(GDP09) + Schooling + Investment + Life_exp + Pop + Trade + GEI + Inflation, 
           data = data_SSA)
reg2 <- lm(log(GDP_growth) ~ log(GDP09) +Cog_skills + Schooling + Investment + Life_exp + Pop + Trade + GEI
           + Inflation, 
           data = data_SSA)
reg3 <- lm(log(GDP_growth) ~ log(GDP09) +Cog_skills + Investment + Life_exp + Pop + Trade + GEI
           + Inflation, 
           data = data_SSA)
reg4 <- lm(log(GDP_growth) ~ log(GDP09) + Cog_skills + Schooling + Investment + Life_exp  + GEI, 
           data = data_SSA)
reg5 <- lm(log(GDP_growth) ~ log(GDP09) + Cog_skills + Investment+ Life_exp  + GEI, 
           data = data_SSA)
stargazer(reg1, reg2, reg3, reg4, reg5,
          se=list(cse(reg1), cse(reg2), cse(reg3), cse(reg4), cse(reg5)),
          title="", type="text", 
          column.labels=c("reg1", "reg2", "reg3", "reg4", "reg5"), 
          df=FALSE, digits=4)

data_ECA <- subset(data2, ECA == 1)
reg1 <- lm(log(GDP_growth) ~ log(GDP09) + Schooling + Investment + Life_exp + Pop + Trade + GEI + Inflation + Developed, 
           data = data_ECA)
reg2 <- lm(log(GDP_growth) ~ log(GDP09) +Cog_skills + Schooling + Investment + Life_exp + Pop + Trade + GEI + Developed
           + Inflation, 
           data = data_ECA)
reg3 <- lm(log(GDP_growth) ~ log(GDP09) +Cog_skills + Investment + Life_exp + Pop + Trade + GEI + Developed
           + Inflation, 
           data = data_ECA)
reg4 <- lm(log(GDP_growth) ~ log(GDP09) + Cog_skills + Schooling + Investment + Life_exp  + GEI + Developed, 
           data = data_ECA)
reg5 <- lm(log(GDP_growth) ~ log(GDP09) + Cog_skills + Investment+ Life_exp  + GEI + Developed, 
           data = data_ECA)
stargazer(reg1, reg2, reg3, reg4, reg5,
          se=list(cse(reg1), cse(reg2), cse(reg3), cse(reg4), cse(reg5)),
          title="", type="text", 
          column.labels=c("reg1", "reg2", "reg3", "reg4", "reg5"), 
          df=FALSE, digits=4)










#####Для развитых
reg1 <- lm(log(GDP_growth) ~ log(GDP09) + Schooling + Investment + Life_exp + Pop + Trade + GEI + Inflation, 
           data = data1_dev)

reg2 <- lm(log(GDP_growth) ~ log(GDP09) +Cog_skills + Schooling + Investment + Life_exp + Pop + Trade + GEI
           + Inflation, 
           data = data1_dev)
reg3 <- lm(log(GDP_growth) ~ log(GDP09) +Cog_skills + Investment + Life_exp + Pop + Trade + GEI
           + Inflation, 
           data = data1_dev)
reg4 <- lm(log(GDP_growth) ~ log(GDP09) + Cog_skills + Schooling + Investment, 
           data = data1_dev)
reg5 <- lm(log(GDP_growth) ~ log(GDP09) + Cog_skills + Investment, 
           data = data1_dev)
stargazer(reg1, reg2, reg3, reg4, reg5,
          se=list(cse(reg1), cse(reg2), cse(reg3), cse(reg4), cse(reg5)),
          title="", type="text", 
          column.labels=c("reg1", "reg2", "reg3", "reg4", "reg5"), 
          df=FALSE, digits=4)
#####Для развивающихся
reg1 <- lm(log(GDP_growth) ~ log(GDP09) + Schooling + Investment + Life_exp + Pop + Trade + GEI + Inflation, 
           data = data_undev)
reg2 <- lm(log(GDP_growth) ~ log(GDP09) +Cog_skills + Schooling + Investment + Life_exp + Pop + Trade + GEI
           + Inflation, 
           data = data_undev)
reg3 <- lm(log(GDP_growth) ~ log(GDP09) +Cog_skills + Investment + Life_exp + Pop + Trade + GEI
           + Inflation, 
           data = data_undev)
reg4 <- lm(log(GDP_growth) ~ log(GDP09) + Cog_skills + Schooling + Investment + Life_exp  + GEI, 
           data = data_undev)
reg5 <- lm(log(GDP_growth) ~ log(GDP09) + Cog_skills + Investment+ Life_exp  + GEI, 
           data = data_undev)
stargazer(reg1, reg2, reg3, reg4, reg5,
          se=list(cse(reg1), cse(reg2), cse(reg3), cse(reg4), cse(reg5)),
          title="", type="text", 
          column.labels=c("reg1", "reg2", "reg3", "reg4", "reg5"), 
          df=FALSE, digits=4)
vif(reg5)
#############Общая таблица
reg1 <- lm(log(GDP_growth) ~ log(GDP09) + Cog_skills + Schooling + Investment, 
           data = data1_dev)
reg2 <- lm(log(GDP_growth) ~ log(GDP09) + Cog_skills + Investment, 
           data = data1_dev)
reg3 <- lm(log(GDP_growth) ~ log(GDP09) + Cog_skills + Schooling + Investment  + GEI + NetMig, 
           data = data1_undev)
reg4 <- lm(log(GDP_growth) ~ log(GDP09) + Cog_skills + Investment + GEI + NetMig, 
           data = data1_undev)
stargazer(reg1, reg2, reg3, reg4,
          se=list(cse(reg1), cse(reg2), cse(reg3), cse(reg4)),
          title="", type="text", 
          column.labels=c("reg1", "reg2", "reg3", "reg4"), 
          df=FALSE, digits=4)


reg1 <- lm(log(GDP_growth) ~ log(GDP09) + Cog_skills + Investment+ Life_exp + GEI, 
           data = DevelopedUN)
reg2 <- lm(log(GDP_growth) ~ log(GDP09) + Cog_skills + Investment+ Life_exp + GEI, 
           data = DevelopingUN)
reg3 <- lm(log(GDP_growth) ~ log(GDP09) + Cog_skills + Investment+ Life_exp + GEI, 
           data = TransitionUN)
reg4 <- lm(log(GDP_growth) ~ log(GDP09) + Cog_skills + Investment+ Life_exp  + GEI, 
           data = LeastDUN)
reg5 <- lm(log(GDP_growth) ~ log(GDP09) + Cog_skills + Investment+ Life_exp + GEI , 
           data = data1_undev)
stargazer(reg1, reg2, reg3, reg4, reg5,
          se=list(cse(reg1), cse(reg2), cse(reg3), cse(reg4), cse(reg5)),
          title="", type="text", 
          column.labels=c("reg1", "reg2", "reg3", "reg4", "reg5"), 
          df=FALSE, digits=4)

reg1 <- lm(log(GDP_growth) ~ log(GDP09) + Cog_skills + Investment+ Life_exp + GEI, 
           data = High_Inc)
reg2 <- lm(log(GDP_growth) ~ log(GDP09) + Cog_skills + Investment+ Life_exp + GEI, 
           data = Upp_Mid_Inc)
reg3 <- lm(log(GDP_growth) ~ log(GDP09) + Cog_skills + Investment+ Life_exp + GEI, 
           data = Low_Mid_Inc)
reg4 <- lm(log(GDP_growth) ~ log(GDP09) + Cog_skills + Investment+ Life_exp  + GEI, 
           data = Low_Inc)
reg5 <- lm(log(GDP_growth) ~ log(GDP09) + Cog_skills + Investment+ Life_exp + GEI +Developed, 
           data = data1)
stargazer(reg1, reg2, reg3, reg4, reg5,
          se=list(cse(reg1), cse(reg2), cse(reg3), cse(reg4), cse(reg5)),
          title="", type="text", 
          column.labels=c("reg1", "reg2", "reg3", "reg4", "reg5"), 
          df=FALSE, digits=4)
##############################2мнк для топ и мин
lnGDP_growth = (log(data1$GDP_growth))
lnGDP09 = (log(data1$GDP09))
iv <- data.frame('GDP_growth1'= lnGDP_growth,'GDP091' = lnGDP09,GDP09 = data1$GDP09, Cog_skills = data1$Cog_skills, Investment=data1$Investment,
                 Pup_teach= data1$Pup_teach,GEI= data1$GEI, Schooling= data1$Schooling, Life_exp = data1$Life_exp,
                 Developed = data1$Developed, Trade = data1$Trade, Pop = data1$Pop, Advanced=data1$Advanced, Basic = data1$Basic)
iv <- data.frame(Investment=data1$Investment,
                 Pup_teach= data1$Pup_teach,GEI= data1$GEI, Schooling= data1$Schooling,GDP_growth = data1$GDP_growth,GDP09 = data1$GDP09, Life_exp = data1$Life_exp,
                 Developed = data1$Developed, Trade = data1$Trade, Pop = data1$Pop, Advanced =data1$Advanced, Basic = data1$Basic)
iv_dev <- subset(iv, Developed == 1)
iv_undev <- subset(iv, Developed == 0)
iv <- na.omit(iv)

########################################норм, но слабые инструменты!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
iv1 <- ivreg(GDP_growth1 ~ GDP09  +Basic + Advanced + Investment  |GDP09 + Pup_teach
             +Investment+ Schooling  + GEI, data=iv)
iv2 <- ivreg(GDP_growth1 ~ GDP09 + Advanced +Basic + Investment + Pop  |GDP09 + Pup_teach
             +Investment+ Schooling  + GEI + Pop, data=iv)

stargazer(iv1, iv2, 
          se=list(ivse(iv1), ivse(iv2)),
          title="", type="text", 
          column.labels=c("reg1", "reg2"), 
          df=FALSE, digits=4)
vif(iv1)
#проверка релевантности инструментов
MNK1_rel <- lm(Cog_skills ~ GDP09 + Pup_teach
               +Investment+ Schooling +Developed + GEI, data = iv)
MNK2_rel <- lm(Cog_skills ~ GDP09 + Pup_teach
               +Investment+ Schooling +Developed + GEI + Trade, data = iv)
linearHypothesis(MNK1_rel, c("Pup_teach = 0", "Schooling = 0", "GEI = 0"), vcov = hc1)
linearHypothesis(MNK2_rel, c("Pup_teach = 0", "Schooling = 0", "GEI = 0"), vcov = hc1)

#проверка эндогенности
fm_or <- lm(residuals(iv1) ~ GDP09 + Pup_teach
            +Investment+ Schooling +Developed + GEI, data = iv)
fm_or2 <- lm(residuals(iv2) ~ GDP09 + Pup_teach
             +Investment+ Schooling +Developed + GEI + Trade, data = iv)
# значение J-статистики
(fm_or_test <- linearHypothesis(fm_or, c("Pup_teach = 0", "Schooling = 0", "GEI = 0"), test = "Chisq"))
(fm_or_test2 <- linearHypothesis(fm_or2, c("Pup_teach = 0", "Schooling = 0", "GEI = 0"), test = "Chisq"))
# p-value
pchisq(fm_or_test[2,5], df.residual(iv1) - df.residual(fm_or), lower.tail = FALSE)
pchisq(fm_or_test2[2,5], df.residual(iv2) - df.residual(fm_or2), lower.tail = FALSE)
# p-значение - большое, принимаем н0 об экзогенности инструментов
stargazer(MNK1_rel,
          se=list(cse(MNK1_rel)),
          title="", type="text", 
          column.labels=c("MNK1_rel"), 
          df=FALSE, digits=4)
stargazer(MNK2_rel,
          se=list(cse(MNK2_rel)),
          title="", type="text", 
          column.labels=c("MNK2_rel"), 
          df=FALSE, digits=4)

stargazer(MNK1_rel, MNK2_rel,
          se=list(cse(MNK1_rel), cse(MNK2_rel)),
          title="", type="text", 
          column.labels=c("MNK1_rel", "MNK2_rel"), 
          df=FALSE, digits=4)







