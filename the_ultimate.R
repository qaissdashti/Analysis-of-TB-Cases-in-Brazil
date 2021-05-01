library(MASS)
library(mgcv)
library(lme4)
library(ggplot2)
install.packages("tidyverse")
install.packages("ggplot2")
load("datasets.RData")
load("TB.RData")
install.packages("corrplot")

head(TBdata)
TBdata$Year_f = as.factor(TBdata$Year)



##
bbnf <- gam(TB~s(Indigenous, k=30)+
              s(Illiteracy, k=30)+
              s(Urbanisation, k=30)+
              s(Density)+
              s(Poverty, k=30)+
              s(Poor_Sanitation, k=30)+
              s(Unemployment)+
              s(Timeliness)+
              s(lon, lat, bs="ds")+
              s(Year, k=3) + 
              te(lon, lat, by=Year_f, k=c(10,10,3), bs=c("tp", "tp")),
            data = TBdata, family = nb(link = 'log'))

summary(bbnf)
par(mfrow=c(2,2))
gam.check(bbnf,pch=20)
plot(bbnf, scheme=2, shade=T, seWithMean=TRUE)

1 - pchisq(bbnf$deviance,bbnf$df.residual)

bbn <- gam(TB~s(Indigenous, k=30)+
             s(Illiteracy, k=30)+
             s(Urbanisation, k=30)+
             s(Density)+
             s(Poverty, k=30)+
             s(Poor_Sanitation, k=30)+
             s(Unemployment)+
             s(Timeliness)+
             s(lon, lat, bs="ds")+
             s(Year, k=3) + 
             te(lon, lat, by=Year, k=c(10,10,3), bs=c("tp", "tp")),
           data = TBdata, family = nb(link = 'log'))
AIC(bbn,bbnf)
summary(bbn)
par(mfrow=c(2,2))
gam.check(bbn,pch=20)
plot(bbn, scheme=2, shade=T, seWithMean=TRUE)
1 - pchisq(bbn$deviance,bbn$df.residual)


bf <- gam(TB~s(Indigenous, k=30)+
               s(Illiteracy, k=30)+
               s(Urbanisation, k=30)+
               s(Density)+
               s(Poverty, k=30)+
               s(Poor_Sanitation, k=30)+
               s(Unemployment)+
               s(Timeliness)+
               Year_f+
               s(lon, lat, bs="ds", by=Year_f),
             data = TBdata, family = nb(link = 'log'))
summary(bf)
bf$aic
par(mfrow=c(2,2))
gam.check(bf,pch=20)
plot(bf, scheme=2, shade=T, seWithMean=TRUE, page=1)
AIC(bf,bbnf)
anova(bf, bbnf, test = "F")

ll3 <- logLik(bbnf)
ll3
ll2 <- logLik(bf)
ll2
LRT <- -2*(ll2-ll3)
LRT <- as.numeric(LRT)
LRT
1 - pchisq(LRT,16)




bfo <- gam(TB~offset(I(log(Population)))+s(Indigenous, k=30)+
            s(Illiteracy, k=30)+
            s(Urbanisation, k=30)+
            s(Density)+
            s(Poverty, k=30)+
            s(Poor_Sanitation, k=30)+
            s(Unemployment)+
            s(Timeliness)+
            Year_f+
            s(lon, lat, bs="ds", by=Year_f),
          data = TBdata, family = nb(link = 'log'))

summary(bfo)
bfo$aic
par(mfrow=c(2,2))
gam.check(bfo,pch=20)
plot(bfo, scheme=2, shade=T, seWithMean=TRUE, page=1)

AIC(bbnf, bfo)
anova(bbnf,bfo,test = "F")
1 - pchisq(bfo$deviance,bfo$df.residual)


ll3 <- logLik(bbnf)
ll3
ll2 <- logLik(bfo)
ll2
LRT <- -2*(ll3-ll2)
LRT <- as.numeric(LRT)
LRT
1 - pchisq(LRT,32)


bfo1 <- gam(TB~offset(I(log(Population)))+s(Indigenous, k=3)+
             s(Illiteracy, k=5)+
             s(Urbanisation, k=10)+
             s(Density, k=6)+
             s(Poverty, k=3)+
             s(Poor_Sanitation, k=12)+
             s(Unemployment)+
             s(Timeliness)+
             Year_f+
             s(lon, lat, bs="ds", by=Year_f),
           data = TBdata, family = nb(link = 'log'))

summary(bfo1)
bfo1$aic
par(mfrow=c(2,2))
gam.check(bfo1,pch=20)
plot(bfo1, scheme=2, shade=T, seWithMean=TRUE)



AIC(bfo, bfo1)
l1 = logLik(bfo)
l1
l2 = logLik(bfo1)
l2
LRT <- -2*(l2-l1)
LRT <- as.numeric(LRT)
LRT
1 - pchisq(LRT,2)
anova(bfo,bfo1, test='F')
1 - pchisq(bfo1$deviance,bfo1$df.residual)

bfo2 <- gam(TB~offset(I(log(Population)))+s(Indigenous, k=3)+
              Illiteracy+
              s(Urbanisation)+
              s(Density)+
              s(Poverty)+
              s(Poor_Sanitation)+
              s(Unemployment)+
              s(Timeliness)+
              Year_f+
              s(lon, lat, bs="ds", by=Year_f),
            data = TBdata, family = nb(link = 'log'))
summary(bfo2)
par(mfrow=c(2,2))
gam.check(bfo2,pch=20)
plot(bfo2, scheme=2, shade=T, seWithMean=TRUE, page=1)
AIC(bfo1, bfo2)
anova(bfo1, bfo2)
1 - pchisq(bfo2$deviance,bfo2$df.residual)

ggplot(TBdata, aes(x = Indigenous, y = log(TB), colour = Year_f)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  scale_colour_brewer(type = 'qual', palette = 'Dark2') +
  theme(legend.position = 'top')

ggplot(TBdata, aes(x = Illiteracy, y = log(TB), colour = Year_f)) +
  geom_point() +
  geom_smooth(method = 'loess', se = FALSE) +
  scale_colour_brewer(type = 'qual', palette = 'Dark2') +
  theme(legend.position = 'top')

ggplot(TBdata, aes(x = Urbanisation, y = log(TB), colour = Year_f)) +
  geom_point() +
  geom_smooth(method = 'loess', se = FALSE) +
  scale_colour_brewer(type = 'qual', palette = 'Dark2') +
  theme(legend.position = 'top')

head(TBdata)

ggplot(TBdata, aes(x = Density, y = log(TB), colour = Year_f)) +
  geom_point() +
  geom_smooth(method = 'loess', se = FALSE) +
  scale_colour_brewer(type = 'qual', palette = 'Dark2') +
  theme(legend.position = 'top')


ggplot(TBdata, aes(x = Poverty, y = log(TB), colour = Year_f)) +
  geom_point() +
  geom_smooth(method = 'loess', se = FALSE) +
  scale_colour_brewer(type = 'qual', palette = 'Dark2') +
  theme(legend.position = 'top')


ggplot(TBdata, aes(x = Poor_Sanitation, y = log(TB), colour = Year_f)) +
  geom_point() +
  geom_smooth(method = 'loess', se = TRUE) +
  scale_colour_brewer(type = 'qual', palette = 'Dark2') +
  theme(legend.position = 'top')


ggplot(TBdata, aes(x = Unemployment, y = log(TB), colour = Year_f)) +
  geom_point() +
  geom_smooth(method = 'lm', se = TRUE) +
  scale_colour_brewer(type = 'qual', palette = 'Dark2') +
  theme(legend.position = 'top')

ggplot(TBdata, aes(x = Timeliness, y = log(TB), colour = Year_f)) +
  geom_point() +
  geom_smooth(method = 'loess', se = TRUE) +
  scale_colour_brewer(type = 'qual', palette = 'Dark2') +
  theme(legend.position = 'top')



pred = predict(bfo2)
length(pred)
length(TBdata$Year_f)
length(TBdata$Year_f[])
length(TBdata$Year_f)
boxplot(sum(TBdata$TB[TBdata$Year==2014])
sum(TBdata$TB[TBdata$Year==2013])
sum(TBdata$TB[TBdata$Year==2012]))

ggplot(TBdata, aes(x=TB)) + 
  geom_histogram(binwidth=100)+ggtitle("TB count Histogram")+
  geom_vline(aes)mean(TBdata$TB)))

ggplot(TBdata, aes(x=TB)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 100)+
  geom_density(alpha=.2, fill="#FF6666")

ggplot(TBdata, aes(x=TB)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 1000)+
  geom_density(alpha=.2, fill="#FF6666")

mean(TBdata$TB)
median(TBdata$TB)
quantile(TBdata$TB, 0.25) # first quartile
quantile(TBdata$TB, 0.50)
quantile(TBdata$TB, 0.75)
quantile(TBdata$TB, 0.95)

boxplot(TBdata$TB)
ggplot(TBdata, aes(x=TB))+
  geom_boxplot()+

  bb <- gam(TB~s(Indigenous, k=30)+
              s(Illiteracy, k=30)+
              s(Urbanisation, k=30)+
              s(Density)+
              s(Poverty, k=30)+
              s(Poor_Sanitation, k=30)+
              s(Unemployment)+
              s(lon, lat, bs="ds")+
              s(Year, k=3) + 
              ti(lon, lat, Year, k=c(10,10,3), bs=c("ds", "tp")),
            data = TBdata, family = poisson(link = 'log'))
bb
summary(bb)
par(mfrow=c(2,2))
gam.check(bfo,pch=20)
plot(bfo, scheme=2, shade=T, seWithMean=TRUE, page=1)

mean(TBdata$TB)
var(TBdata$TB)


