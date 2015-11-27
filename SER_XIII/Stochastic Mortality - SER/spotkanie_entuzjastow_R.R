####################################
## Stochastic Mortality Modelling ##
####################################

rm(list=ls())

## packages ##
library(reshape2)
library(dplyr)
library(StMoMo)
library(ggplot2)
library(rgl)
library(readxl)

## prameters ##
starting_year <- 1958
last_year <- 2014
forecast_end <- 2060
forecast_length <- forecast_end - last_year
gender <- "M"
options(scipen = 20)

## reading GUS mortality tables ##
source('mortality_table_import.R')


## reading and processing data from Human Mortality Database (HMD) ##
source('HMD.R')


## 2D plot - last year ##
jpeg("qx_plot.jpeg",height = 2500, width = 3500, res =400)
ggplot(PTTZ %>% filter(sex == gender, Year == 2014), aes(x = Age, y = qx, group = Year, colour = Year)) + geom_line(lwd=2) + 
  scale_y_log10(breaks = c(0.0001,0.001,0.01,0.1)) + ylab("qx - death probability within one year") + ggtitle("Death probability") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size =24))#, axis.title.y = element_text(size = 20))
dev.off()

## 2D plot - 1990 - 2014##
ggplot(PTTZ %>% filter(sex == gender), aes(x = Age, y = qx, group = Year, colour = Year)) + geom_line() + 
  scale_y_log10(breaks = c(0.0001,0.001,0.01,0.1))


## 3D scatterplot without age restictions ##
plot3d(full_tables$Age,as.numeric(full_tables$Year),log(full_tables$qx), col = 'blue', ylim = c(1958, 2030),
       xlab = 'Age', ylab = 'Year', zlab = 'ln(qx)')


## surface plots without age restictions ##
library(reshape2)
A <- dcast(full_tables %>% select(Age,Year,qx),Age~Year)
persp3d(A$Age,as.numeric(colnames(A)[2:length(colnames(A))]), A %>% select(-Age) %>% as.matrix() %>% log(),col="lightblue", axes = F,
        xlab = 'age', ylab = 'year', zlab = 'deaht probability (qx)')
axes3d(c("x", "y"))
axis3d("z", at=pretty(A %>% select(-Age) %>% as.matrix() %>% log()), labels=10^pretty(A %>% select(-Age) %>% as.matrix() %>% log()))


## surface plots with age restictions ## 
A<- dcast(full_tables %>% filter(Age %in% c(25:85)) %>% select(Age,Year,qx),Age~Year)

persp3d(A$Age,as.numeric(colnames(A)[2:length(colnames(A))]), A %>% select(-Age) %>% as.matrix() %>% log(),col="lightblue", axes = F,
        xlab = 'age', ylab = 'year', zlab = 'death probability (qx)', ylim = c(starting_year,1980), main = 'Mortality table')
persp3d(A$Age,as.numeric(colnames(A)[2:length(colnames(A))]), A %>% select(-Age) %>% as.matrix() %>% log(), ylim = c(1970,1990),
        add = T,col="lightblue")
persp3d(A$Age,as.numeric(colnames(A)[2:length(colnames(A))]), A %>% select(-Age) %>% as.matrix() %>% log(),ylim = c(1990,2014),
        add = T,col="lightblue")
axes3d(c("x", "y"))
axis3d("z", at=pretty(A %>% select(-Age) %>% as.matrix() %>% log()), labels=10^pretty(A %>% select(-Age) %>% as.matrix() %>% log()))


# defining mortality models
source('model_definitions.R')


# model parameters
ages <- 0:max(full_tables$Age)
years <- starting_year:last_year
ages.fit <- 25:85
drop_cohort <- 3
wxt <- genWeightMat(ages = ages.fit, years = years, clip = drop_cohort)


## fitting Lee Carter model ##
LCfit <- fit(LC, Dxt = Dxt, Ext = Ext, ages = ages, years = years,
             ages.fit = ages.fit, wxt = wxt)
plot(LCfit, nCol = 3)

LCres <- residuals(LCfit)
plot(LCres, type = "colourmap", reslim = c(-3.5, 3.5))
plot(LCres, type = "scatter", reslim = c(-3.5, 3.5))


## fitting full PLAT model ##
PLATfit <- fit(extended_PLAT, Dxt = Dxt, Ext = Ext, ages = ages, years = years,
                        ages.fit = ages.fit, wxt = wxt)

plot(PLATfit, nCol = 3,parametricbx = F)

PLATres <- residuals(PLATfit)
plot(PLATres, type = "colourmap", reslim = c(-3.5, 3.5))
plot(PLATres, type = "scatter", reslim = c(-3.5, 3.5))

plot(LCres, type = "colourmap", reslim = c(-3.5, 3.5), main = 'Lee-Carter residuals')
plot(PLATres, type = "colourmap", reslim = c(-3.5, 3.5), main = 'Plat residuals')

plot(LCres, type = "scatter", reslim = c(-3.5, 3.5), main = 'Lee-Carter residuals')
plot(PLATres, type = "scatter", reslim = c(-3.5, 3.5), main = 'Plat residuals')

# bootsrap for parameter uncertainty 
#PLATfit_boot <- bootstrap(PLATfit,nBoot = 100, type = 'semiparametric')
#plot(PLATfit_boot, nCol = 3,parametricbx = F)

# Akaike information criterion (AIC)
AIC(LCfit)[1]
AIC(PLATfit)[1]

# Bayesian information criterion (BIC)
BIC(LCfit)[1]
BIC(PLATfit)[1]

##############
## Forecast ##
##############

# structural break assesment
data_for_test <- data.frame(kt = as.numeric(PLATfit$kt[1,]), year = starting_year:last_year, t= 1:(length(starting_year:last_year)))
data_for_test <- data_for_test %>% mutate(SB = ifelse(year >= 1990,1,0))

model <- lm(kt~t+SB+t*SB, data=data_for_test)
summary(model)


old <- data_for_test %>% select(-kt,-year)
new <- data.frame(t = (last(old$t)+1):(last(old$t)+1+20), SB = 1)
df <- rbind(old,new)
est <- predict(model, df, se.fit = TRUE)

plot(est$fit, type = 'l',x= starting_year:(last_year+1+20), ylab = 'kt', xlab = 'year', lwd = 3,
     main = expression(paste(kappa^(1),' parameter modeled with linear regression ', kappa,' ~ t+SB+t*SB')))
lines(data_for_test$kt, col ='blue',x= starting_year:last_year, lwd =3)
grid()
legend("bottomleft",c("modeleled with stuctural break at 1990",expression(paste(kappa^(1), " fitted by Plat"))),lwd=c(2.5,2.5),col=c("black","blue"))


# structural break implementation for extended PLAT model (choosen model)
structural_break_year <- 1990
multivariat_ts <- mrwd(PLATfit$kt)
multivariat_ts <- mrwd(PLATfit$kt[,as.character(c(structural_break_year:last_year))]) # 33 - 1990; 36 - 1993
multivariat_ts_forecast <- forecast(multivariat_ts, h = forecast_length)

kt_all <- rbind(t(multivariat_ts$x), t(multivariat_ts_forecast$mean))

plot(multivariat_ts$fitted[1,],x=names(multivariat_ts$fitted[1,]), type='l', xlim=c(starting_year,forecast_end), ylim = c(-2,2), col ='red',
     main = expression(paste(kappa^(1)," parameter modelling")), ylab = expression(paste(kappa^(1),'value')),xlab='Year',lwd = 2)
lines(PLATfit$kt[1,],x=names(PLATfit$kt[1,]), type = 'o',lwd = 1)
lines(multivariat_ts_forecast$mean[1,], x = (last_year+1):forecast_end, col = 'blue',lwd = 2)
lines(multivariat_ts_forecast$upper[1,,"95%"], x = (last_year+1):forecast_end, lty=2, col = 'blue',lwd = 2)
lines(multivariat_ts_forecast$lower[1,,"95%"], x = (last_year+1):forecast_end, lty=2, col = 'blue',lwd = 2)
grid()
legend('topright',c(expression(paste('Fitted ',kappa^(1),' parameter')),'Fitted Multivariat random walk with drift','Projection','Projection confidence intervals'),
       col = c('black','red','blue','blue'),lty = c(1,1,1,2),pch = c(1,NA,NA,NA),lwd = c(1,2,2,2))

plot(multivariat_ts$fitted[1,],x=names(multivariat_ts$fitted[1,]), type='o', xlim=c(1990,forecast_end), ylim = c(-2,0.2))
lines(multivariat_ts$x[1,],x=names(multivariat_ts$fitted[1,]), col ='red')
lines(multivariat_ts_forecast$mean[1,], x = (last_year+1):forecast_end)
lines(multivariat_ts_forecast$upper[1,,"95%"], x = (last_year+1):forecast_end, lty=2)
lines(multivariat_ts_forecast$lower[1,,"95%"], x = (last_year+1):forecast_end, lty=2)

plot(multivariat_ts$fitted[2,],x=names(multivariat_ts$fitted[2,]), type='o', xlim=c(1990,forecast_end), ylim = c(-0.025,0.01))
lines(multivariat_ts$x[2,],x=names(multivariat_ts$fitted[2,]), col ='red')
lines(forecast(multivariat_ts, h = forecast_length)$mean[2,], x = (last_year+1):forecast_end)
lines(multivariat_ts_forecast$upper[2,,"95%"], x = (last_year+1):forecast_end, lty=2)
lines(multivariat_ts_forecast$lower[2,,"95%"], x = (last_year+1):forecast_end, lty=2)

plot(multivariat_ts$fitted[3,],x=names(multivariat_ts$fitted[3,]), type='o', xlim=c(1990,forecast_end), ylim = c(-0.05,0.03))
lines(multivariat_ts$x[3,],x=names(multivariat_ts$fitted[3,]), col ='red')
lines(forecast(multivariat_ts, h = forecast_length)$mean[3,], x = (last_year+1):forecast_end)
lines(multivariat_ts_forecast$upper[3,,"95%"], x = (last_year+1):forecast_end, lty=2)
lines(multivariat_ts_forecast$lower[3,,"95%"], x = (last_year+1):forecast_end, lty=2)



#########################
## kt paths simulation ##
#########################

kt_paths <- array(data = NA, dim = c(3,forecast_length+20,2500))

for (i in 1:2500){
  simulated_one_kt_path  <- simulate(multivariat_ts, nsim = forecast_length+20)
  kt_paths[,,i] <- simulated_one_kt_path
}


kt1 <- kt_paths[1,,] %>% as.matrix
row.names(kt1) <- as.character((last_year+1):2080)
kt2 <- kt_paths[2,,] %>% as.matrix
row.names(kt2) <- as.character((last_year+1):2080)
kt3 <- kt_paths[3,,] %>% as.matrix
row.names(kt3) <- as.character((last_year+1):2080)


par(mfrow=c(1,3))
plot(multivariat_ts$fitted[1,],x=names(multivariat_ts$fitted[1,]), type = 'l',xlim=c(1990,2060), ylim = c(-2,0.5),
     xlab = 'year', ylab = expression(paste(kappa," parameter value")), 
     main = expression(paste(kappa^(1)," projection - scenarios")),lwd = 3,
     cex.main = 1.8,cex.lab = 1.5)
matlines(kt1[,1:250], x = row.names(kt1[,1:250]),  type = 'l')
grid()
plot(multivariat_ts$fitted[2,],x=names(multivariat_ts$fitted[2,]), type = 'l', xlim=c(1990,2060), ylim = c(-0.05,0.01),
     xlab = 'year', ylab = expression(paste(kappa," parameter value")),
     main = expression(paste(kappa^(2)," projection - scenarios")),lwd = 3,
     cex.main = 1.8,cex.lab = 1.5)
matlines(kt2[,1:250], x = row.names(kt2[,1:250]),  type = 'l')
grid()
plot(multivariat_ts$fitted[3,],x=names(multivariat_ts$fitted[3,]), type = 'l', xlim=c(1990,2060), ylim = c(-0.05,0.05),
     xlab = 'year', ylab = expression(paste(kappa," parameter value")),
     main = expression(paste(kappa^(3)," projection - scenarios")),lwd = 3,
     cex.main = 1.8,cex.lab = 1.5)
matlines(kt3[,1:250], x = row.names(kt3[,1:250]),  type = 'l')
grid()
par(mfrow=c(1,1))

## modelling gamma parameter
gc_ARIMA <- arima(PLATfit$gc[drop_cohort:(length(PLATfit$gc)-drop_cohort)], c(2,1,0),include.mean = F)
gc_PLAT_projection <- predict(gc_ARIMA,n.ahead = forecast_end - last_year + drop_cohort)$pred %>% as.numeric()
gc_PLAT_fit<- as.numeric(PLATfit$gc[c((drop_cohort+1):(length(PLATfit$gc)-drop_cohort))])
gc_PLAT_all <- c(gc_PLAT_fit,gc_PLAT_projection) 
plot(gc_PLAT_all, type='l', col ='red',x=as.character((as.numeric(names(PLATfit$gc)[1])+drop_cohort):(forecast_end - 25)))
lines(PLATfit$gc,x=names(PLATfit$gc))

#########################
## gc paths simulation ##
#########################

gc_paths <- matrix(data = NA, ncol = forecast_length+20+drop_cohort,nrow=2500)

for (i in 1:2500){
  simulated_one_gc_path<- simulate(gc_ARIMA, nsim = forecast_length+20+drop_cohort)
  gc_paths[i,] <- simulated_one_gc_path
}


par(mfrow=c(1,1))
colnames(gc_paths) <- as.character(names(PLATfit$gc)[length(PLATfit$gc)-drop_cohort+1]:
                                   (names(PLATfit$gc)[length(PLATfit$gc)-drop_cohort] %>% as.numeric()+
                                    forecast_length+20+drop_cohort))

plot(PLATfit$gc, type='l', col ='black',x=names(PLATfit$gc), xlim = c(1876,1985+70), ylim = c(-0.6,0.4),
     xlab = "year", ylab = "gamma parameter", main = "gamma projection - scenarios")
matlines(t(gc_paths)[,1:100],x = colnames(gc_paths), type = 'l', main = 'gc')
grid()

colors <- c('black','red','green','blue','orange','white',"yellow")

#####################
## main projection ##
#####################
prediction_PLAT_stoch <- array(data = NA, dim=c(2500,61,2080-last_year))
for (i in 1:2500){
  gc_paths_2015_2080 <- c(PLATfit$gc[as.character(as.character((names(PLATfit$gc)[length(names(PLATfit$gc))-59])):(names(PLATfit$gc)[length(names(PLATfit$gc))-3]))],gc_paths[i,])
  
  prediction_PLAT_instance <- predict(PLATfit, years = (last_year+1):2080, kt = kt_paths[,,i],
                                               gc = gc_paths_2015_2080, type = "rates")
  prediction_PLAT_stoch[i,,] <- prediction_PLAT_instance
  prediction_PLAT_instance
  if(i == 1) {plot(prediction_PLAT_instance["40",], type = 'l', ylim = c(0,max(prediction_PLAT_instance["40",])*2.1),
                   x= (last_year+1):2080, xlim = c(1990,2080), xlab = "year",ylab = "qx", main = "qx projection for 40 years old male")} else {lines(prediction_PLAT_instance["40",],x = (last_year+1):2080, col = i)}
}
mean <- apply(prediction_PLAT_stoch[,(16),],2, mean)
quantile <- apply(prediction_PLAT_stoch[,(16),],2, quantile, probs = c(0.05,0.95))
qxt <- Dxt / Ext
history <- qxt[as.character(40), as.character(1990:last_year)]
lines(history,x=1990:last_year, lwd = 2, col = colors[2])
lines(mean,x=(last_year+1):2080, lwd = 2, col = colors[1])
lines(quantile[1,],x=(last_year+1):2080, lwd = 3, lty=2, col = colors[1])
lines(quantile[2,],x=(last_year+1):2080, lwd = 3, lty=2, col = colors[1])
grid()
legend("topright",c("history","mean","95% and 5% quantiles"),col=c("red","black","black"),lty = c(1,1,2), lwd = 2)



mean_over_scenarios <- apply(prediction_PLAT_stoch,c(2,3), mean)
quantile_over_scenarios <- apply(prediction_PLAT_stoch,c(2,3), quantile, probs = c(0.01,0.99))

A <- dcast(full_tables %>% filter(Age %in% c(25:85)) %>% select(Age,Year,qx),Age~Year)
persp3d(A$Age,as.numeric(colnames(A)[2:length(colnames(A))]), A %>% select(-Age) %>% as.matrix() %>% log(),col="lightblue", axes = F,
        xlab = 'age', ylab = 'year', zlab = 'death probability (qx)', ylim = c(1958,2014),smooth = T,lit = T,
        texmagfilter = 'linear',texminfilter = 'nearest.mipmap.nearest',point_antialias = T,line_antialias = T,fog = T)
B <- mean_over_scenarios %>% data.frame(check.names = F)
persp3d(ages.fit,c(last_year+1):2080, B %>% as.matrix() %>% log(),col="chartreuse3", axes = F,
        xlab = 'age', ylab = 'year', zlab = 'death probability (qx)', ylim = c(last_year,2080),smooth = T,lit = T,
        texmagfilter = 'linear',texminfilter = 'nearest.mipmap.nearest',point_antialias = T,line_antialias = T,fog = T,
        add = T)
C <- quantile_over_scenarios[1,,] %>% data.frame(check.names = F)
persp3d(ages.fit,c(last_year+1):2080, C %>% as.matrix() %>% log(),col="darkorange",ylim = c(last_year,2080),
        add = T, alpha = 0.2)

D <- quantile_over_scenarios[2,,] %>% data.frame(check.names = F)
persp3d(ages.fit,c(last_year+1):2080, D %>% as.matrix() %>% log(),col="darkorange",ylim = c(last_year,2080),
        add = T, alpha = 0.2)
axes3d(c("x", "y"))
axis3d("z", at=pretty(A %>% select(-Age) %>% as.matrix() %>% log()), labels=10^pretty(A %>% select(-Age) %>% as.matrix() %>% log()))

