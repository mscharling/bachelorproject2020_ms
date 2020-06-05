# bachelorproject2020
This repository contains the coding in R to provide the plots from Martin Scharling's bachelor thesis in math-econ 2020.

##########MARTIN SCHARLING 2020##########
##########BACHELOR THESIS##########
##########UNIVERSITY OF COPENHAGEN##########

##########LOADING PACKAGES##########
library(quantmod)
library(ggplot2)
library(reshape2)
library(timeDate)
library(gridExtra)
library(PerformanceAnalytics)
library(AnalyzeTS)
library(rugarch)
library(fGarch)
library(plotly)
library(forecast)
library(moments)
library(MASS)
library(tidyverse)
library(tidyr)
library(metRology)
library(dplyr)
require(gridExtra)
require(grid)
library(stats4)
####################


##########LOADING ENVIRONMENT##########
env_sp500 <- new.env()

getSymbols("^GSPC", env = env_sp500 , src = "yahoo", from = as.Date("2016-01-01"), to = as.Date("2020-01-01"), warnings = FALSE)
####################


##########DEFINING DATA##########
sp500 <- env_sp500$GSPC
sp500_close <- sp500$GSPC.Close
sp500_returns <- Return.calculate(sp500_close, method = "discrete") #((sp500_close[2:n_close, 1] - sp500_close[1:(n_close - 1), 1])/sp500_close[1:(n_close - 1), 1])
sp500_logreturns <- (diff(log(sp500_close)))
sp500_index <- index(sp500)

n_close <- length(sp500_close)
n_returns <- length(sp500_returns)
n_logreturns <- length(sp500_logreturns)
n_squaredreturns <- length(sp500_squaredreturns)

sp500_returns[1] <- 0
sp500_logreturns[1] <- 0

dataframe_sp500 <- data.frame(index = 1:n_close, values = sp500_close)
dataframe_sp500_melt <- melt(dataframe_sp500, id = c('index'))

dataframe_sp500_returns <- data.frame(index = 1:n_returns, value = sp500_returns)
dataframe_sp500_returns_melt <- melt(dataframe_sp500_returns, id = c('index'))

#LOGRETURNS
dataframe_sp500_logreturns <- data.frame(index = 1:n_logreturns, value = sp500_logreturns)
dataframe_sp500_logreturns_melt <- melt(dataframe_sp500_logreturns, id = c('index'))

#ABSOLUTE RETURNS
sp500_absreturn <- abs(sp500_logreturns)

dataframe_sp500_absreturn <- data.frame(index = 1:n_logreturns, value = abs(sp500_logreturns))
dataframe_sp500_absreturn_melt <- melt(dataframe_sp500_absreturn, id = c('index'))

#SQUARED RETURNS
sp500_squaredreturns <- sp500_logreturns^2

dataframe_sp500_squaredreturns <- data.frame(index = 1:n_squaredreturns, values = sp500_squaredreturns)
dataframe_sp500_squaredreturns_melt <- melt(dataframe_sp500_squaredreturns, id = c('index'))
####################


##########PLOTS OF PRICES AND RETURNS###############
p1 <- (ggplot(data = dataframe_sp500_melt, 
              aes(x = 1:n_close, y = value)) 
       + geom_line() 
       + theme(plot.title = element_text(hjust = 0.5)) 
       + ggtitle("Daily closing price of S&P500")
       + xlab("Index") 
       + xlim(0,n_close)
       + ylab("Adjusted close"))

p2 <- (ggplot(data = dataframe_sp500_returns_melt, 
              aes(x = 1:n_returns, y = value)) 
       + geom_line() 
       + theme(plot.title = element_text(hjust = 0.5)) 
       + ggtitle("Daily returns of S&P500")
       + xlab("Index") 
       + xlim(0,n_returns)
       + ylab("Returns"))

p3 <-(ggplot(data = dataframe_sp500_logreturns_melt, 
        aes(x = 1:n_logreturns, y = value)) 
        + geom_line() 
        + theme(plot.title = element_text(hjust = 0.5)) 
        + ggtitle("Daily log-returns of S&P500")
        + xlab("Index") 
        + xlim(0,n_logreturns)
        + ylab("Log-returns"))

p6 <- (ggplot(data = dataframe_sp500_squaredreturns_melt, 
              aes(x = 1:n_squaredreturns, y = value), geom = 'line')
       + geom_line()
       + xlab("Index")
       + ylab("Squared returns")
       + ggtitle("Daily squared returns of S&P500")
       + theme(plot.title = element_text(hjust = 0.5)))

pabsret <- (ggplot(data = dataframe_sp500_absreturn_melt, 
                   aes(x = 1:n_logreturns, y = value)) 
            + geom_line() 
            + theme(plot.title = element_text(hjust = 0.5)) 
            + ggtitle("Daily absolute log-returns of S&P500")
            + xlab("Index") 
            + xlim(0,n_logreturns)
            + ylab("Absolute returns"))

p1
grid.arrange(p3, arrangeGrob(p6, pabsret, widths = c(0.5,0.5)), nrow = 2)
#########################


##########DESCRIPTIVE STATISTICS##########
summary(sp500_logreturns)
sd(sp500_logreturns)

min(as.numeric(sp500_logreturns))
median(sp500_logreturns)
quantile(sp500_logreturns, 0.75)
quantile(sp500_logreturns, 0.25)

match(quantile(sp500_logreturns, 0.75, type = 1), as.numeric(sp500_logreturns))

options(digits=8)

moment(sp500_logreturns, order = 1)
moment(sp500_logreturns, order = 2)
moment(sp500_logreturns, order = 3)
moment(sp500_logreturns, order = 4)

kurtosis(as.numeric(sp500_logreturns))
skewness(as.numeric(sp500_logreturns))

summaryFull(sp500_logreturns)
#########################


##########HISTOGRAM AND DENSITY OF LOGRETURNS##########
phistdens <- (ggplot(dataframe_sp500_logreturns_melt, 
        aes(x=value)) + 
        geom_histogram(aes(y=..density..), 
                       bins = 100,
                       colour="black", 
                       fill="white")+
        geom_density(alpha=.4, fill="lightblue")+
        geom_vline(xintercept = mean(sp500_logreturns), 
                             color = 'blue')+
        labs(x = "Log-returns", y = "Frequency")+
        ggtitle("Histogram and density of Log-returns")+
        theme(plot.title = element_text(hjust = 0.5)))

phistdens
####################


##########MAXIMUM LIKELIHOOD ESTIMATION OF LOGRETURNS##########

#NORMAL DISTRIBUTION
loglik.n <- function(mu, sigma){x <- as.numeric(sp500_logreturns)
-sum(log((1/(sqrt(2*pi)*sigma))*exp(-(1/2)*((x-mu)/sigma)^2)))
}
est.n <- mle(minuslogl = loglik.n, start = list(mu = mean(sp500_logreturns), sigma = sd(sp500_logreturns)))
summary(est.n)

#T DISTRIBUTION
loglik.t <- function(location, scale, nu){x <- as.numeric(sp500_logreturns)
-sum(log((gamma((nu+1)/2)/(gamma((nu)/2)*sqrt(pi*nu)*scale))*(1+(1/nu)*((x-location)/scale)^2)^(-(nu+1)/2)))
}
est.t <- mle(minuslogl = loglik.t, start = list(location = mean(sp500_logreturns), scale = sd(sp500_logreturns), nu = 2.5))
summary(est.t)
####################


##########BOXPLOT AND QQPLOT##########

#BOXPLOTS
set.seed(11)
data <- data.frame(
       Raw = as.numeric(sp500_logreturns),
       Normal = rnorm(n = 1006, mean = 0.0004704437, sd = 0.0081082567),
       Student = rt.scaled(n = 1006, 
                      mean = 0.0008358039, 
                      sd = 0.0046666930, 
                      df = 2.4677865665))
data2 <- pivot_longer(data, cols=everything()) %>%
         mutate(name=factor(name, levels=c("Raw","Normal","Student")))
data3 <- data2 %>% summarise(min=min(value), max=max(value))

pbox1 <- (filter(data2, name %in% c("Raw","Normal","Student")) %>%
                  ggplot(aes(y=value, fill=name)) +
                  stat_boxplot(geom = "errorbar", width = 0.3)+
                  geom_boxplot(fill = c('#b3e6ff', '#ffaaaa', '#caffb1')) +
                  facet_grid(~name) +
                  ylab("Log-returns") +
                  ylim(data3$min, data3$max) +
                  theme(legend.position = "none",
                        axis.ticks.x=element_blank(),
                        panel.grid.major.x = element_blank(),
                        panel.grid.minor.x = element_blank(),
                        axis.text.x=element_text(color="white"))+
                  ggtitle("Boxplot comparison")+
                  theme(plot.title = element_text(hjust = 0.5)))
pbox1

#QQPLOT
params_n <- as.list(c(mean = 0.0004704437, sd = 0.0081082567))
params_t <- as.list(c(mean = 0.0008358039, sd = 0.0046666930, df = 2.4677865665))

pqqn <- (ggplot(dataframe_sp500_logreturns_melt, aes(sample = value))+
           stat_qq(distribution = qnorm, dparams = params_n[]) +
           stat_qq_line(distribution = qnorm, dparams = params_n[]) +
           coord_cartesian(ylim = c(-0.05,0.05))+
           labs(x = "Theoretical quantiles (1)", y = "Sample quantiles"))

pqqt <- (ggplot(dataframe_sp500_logreturns_melt, aes(sample = value))+
           coord_cartesian(ylim = c(-0.05,0.05))+
           stat_qq(distribution = qt.scaled, dparams = params_t[]) +
           stat_qq_line(distribution = qt.scaled, dparams = params_t[]) +
           labs(x = "Theoretical quantiles (2)", y = ''))

grid.arrange(pqqn, pqqt, nrow = 1, top = textGrob("QQ-plot comparison",gp=gpar(fontsize=14)))

jarque.test(as.numeric(sp500_logreturns))
####################


##########EXTREME VALUES##########
which(as.numeric(sp500_logreturns) >= 0.021)
which(as.numeric(sp500_logreturns) <= -0.035)

#SIMULATIONS
simnorm <- rnorm(n = 1006, mean = 0.0004704437, sd = 0.0081082567)
psimnorm <- (qplot(y = simnorm, x = 1:1006, geom = 'line')+
                     geom_line((aes(x = 1:1006, y = simnorm)))+
                     coord_cartesian(ylim = c(-0.07, 0.05))+
                     labs(x = "", y = ""))

simt <- rt.scaled(n = 1006, mean = 0.0008358039, sd = 0.0046666930, df = 2.4677865665)
psimt <- (qplot(y = simt, x = 1:1006, geom = 'line')+
                  geom_line((aes(x = 1:1006, y = simt)))+
                  coord_cartesian(ylim = c(-0.07, 0.05))+
                  labs(x = 'Index', y = ""))

grid.arrange(p3, psimnorm, psimt, nrow = 3, top = textGrob("Distribution comparison",gp=gpar(fontsize=14)))
####################


##########AUTOCORRELATION##########

#ACF FOR RAW AND SIMULATIONS
plotacf <- (ggAcf(sp500_logreturns, lag.max = 60) +
                    theme(plot.title = element_text(hjust = 0.5)) +
                    ggtitle("") +
                    ylim(-0.1,0.5)+
                    annotation_custom(gTree(children=gList(textGrob("S&P 500", hjust=1.5,x=1,vjust=2,y=1, 
                                                                    gp=gpar(col="black", fontsize=11, fontface="bold")))))+
                    xlab(""))

plotacfnorm <- (ggAcf(simnorm, lag.max = 60) +
                        theme(plot.title = element_text(hjust = 0.5)) +
                        ggtitle("") +
                        ylim(-0.1,0.5)+
                        theme(legend.title = element_blank())+
                        annotation_custom(gTree(children=gList(textGrob("Normal ", hjust=1.5,x=1,vjust=2,y=1, 
                                                                        gp=gpar(col="black", fontsize=11, fontface="bold")))))+
                        xlab(""))

plotacft <- (ggAcf(simt, lag.max = 60) +
                     theme(plot.title = element_text(hjust = 0.5)) +
                     ggtitle("") +
                     annotation_custom(gTree(children=gList(textGrob("t      ", hjust=1.5,x=1,vjust=2,y=1, 
                                                                     gp=gpar(col="black", fontsize=11, fontface="bold")))))+
                     ylim(-0.1,0.5))

grid.arrange(plotacf, plotacfnorm, plotacft, top = textGrob("Sample Autocorrelation Function comparison of raw data",gp=gpar(fontsize=14)))

#ACF FOR TRANSFORMED DATA
simnormabs <- abs(simnorm)
simtabs <- abs(simt)

plotacfabs <- (ggAcf(sp500_absreturn, lag.max = 60) +
                    theme(plot.title = element_text(hjust = 0.5)) +
                    ggtitle("Sample ACF comparison of absolute data") +
                    ylim(-0.1,0.5)+
                    annotation_custom(gTree(children=gList(textGrob("S&P 500", hjust=1.5,x=1,vjust=2,y=1, 
                                                                    gp=gpar(col="black", fontsize=11, fontface="bold")))))+
                    xlab(""))

plotacfnormabs <- (ggAcf(simnormabs, lag.max = 60) +
                           theme(plot.title = element_text(hjust = 0.5)) +
                           ggtitle("") +
                           ylim(-0.1,0.5)+
                           annotation_custom(gTree(children=gList(textGrob("Normal ", hjust=1.5,x=1,vjust=2,y=1, 
                                                                           gp=gpar(col="black", fontsize=11, fontface="bold")))))+
                           xlab(""))
        
plotacftabs <- (ggAcf(simtabs, lag.max = 60) +
                     theme(plot.title = element_text(hjust = 0.5)) +
                     ggtitle("") +
                     annotation_custom(gTree(children=gList(textGrob("t      ", hjust=1.5,x=1,vjust=2,y=1, 
                                                                     gp=gpar(col="black", fontsize=11, fontface="bold")))))+
                     ylim(-0.1,0.5))

grid.arrange(plotacfabs, plotacfnormabs, plotacftabs, top = textGrob("Sample Autocorrelation Function comparison of absolute data",gp=gpar(fontsize=14)))

simnormsq <- (simnorm)^2
simtsq <- (simt)^2

plotacfsq <- (ggAcf(sp500_squaredreturns, lag.max = 60) +
                       theme(plot.title = element_text(hjust = 0.5)) +
                       ggtitle("Sample ACF comparison of squared data") +
                       ylim(-0.1,0.5)+
                       annotation_custom(gTree(children=gList(textGrob("S&P 500", hjust=1.5,x=1,vjust=2,y=1, 
                                                                       gp=gpar(col="black", fontsize=11, fontface="bold")))))+
                       xlab(""))

plotacfnormsq <- (ggAcf(simnormsq, lag.max = 60) +
                           theme(plot.title = element_text(hjust = 0.5)) +
                           ggtitle("") +
                           ylim(-0.1,0.5)+
                           annotation_custom(gTree(children=gList(textGrob("Normal ", hjust=1.5,x=1,vjust=2,y=1, 
                                                                           gp=gpar(col="black", fontsize=11, fontface="bold")))))+
                           xlab(""))

plotacftsq <- (ggAcf(simtsq, lag.max = 60) +
                        theme(plot.title = element_text(hjust = 0.5)) +
                        ggtitle("") +
                        annotation_custom(gTree(children=gList(textGrob("t      ", hjust=1.5,x=1,vjust=2,y=1, 
                                                                        gp=gpar(col="black", fontsize=11, fontface="bold")))))+
                        ylim(-0.1,0.5))

grid.arrange((arrangeGrob(arrangeGrob(plotacfabs, plotacfnormabs, plotacftabs), arrangeGrob(plotacfsq, plotacfnormsq, plotacftsq), widths = c(0.5,0.5))))
####################


##########LJUNG BOX TEST##########
Box.test(sp500_logreturns, lag = 5, type = 'Ljung-Box')
Box.test(sp500_absreturn, lag = 5, type = 'Ljung-Box')
Box.test(sp500_squaredreturns, lag = 5, type = 'Ljung-Box')
####################


##########GARCH IMPLEMENTION##########

#ACF FOR SIMULATED GARCH
garchim <- garchSim(spec = garchSpec(model = list(omega = 1.0e-6, alpha = 0.2, beta = 0.75),
                                     cond.dist = "norm"), n = 1006)

(ggAcf(garchim, lag.max = 60) +
                       theme(plot.title = element_text(hjust = 0.5)) +
                       ggtitle("") +
                       annotation_custom(gTree(children=gList(textGrob("Sim    ", hjust=1.5,x=1,vjust=2,y=1, 
                                                                       gp=gpar(col="black", fontsize=11, fontface="bold")))))+
                       ylim(-0.1,0.5))

(ggAcf((garchim^2), lag.max = 60) +
                theme(plot.title = element_text(hjust = 0.5)) +
                ggtitle("") +
                annotation_custom(gTree(children=gList(textGrob("Sim    ", hjust=1.5,x=1,vjust=2,y=1, 
                                                                gp=gpar(col="black", fontsize=11, fontface="bold")))))+
                ylim(-0.1,0.5))

(ggAcf((abs(garchim)), lag.max = 60) +
                theme(plot.title = element_text(hjust = 0.5)) +
                ggtitle("") +
                annotation_custom(gTree(children=gList(textGrob("Sim    ", hjust=1.5,x=1,vjust=2,y=1, 
                                                                gp=gpar(col="black", fontsize=11, fontface="bold")))))+
                ylim(-0.1,0.5))

#GARCH SPECIFICATION
model.spec = ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1,1)),
                        mean.model = list(armaOrder = c(0,0)),
                        distribution.model = "norm")
model.fit = ugarchfit(spec = model.spec, data = sp500_logreturns, solver = 'solnp')
model.fit@fit$matcoef

#STANDARDIZED RESIDUALS
dataframe_garchfit_stdres <- data.frame(index = 1:n_logreturns, value = as.numeric(sp500_logreturns)/sigma(model.fit))
dataframe_garchfit_stdres_melt <- melt(dataframe_garchfit_stdres, id = c('index'))

pgarchfit <- (ggplot(data = dataframe_garchfit_stdres_melt, 
                           aes(x = 1:n_logreturns, y = value)) 
                    + geom_line() 
                    + theme(plot.title = element_text(hjust = 0.5)) 
                    + ggtitle("Standardized residuals of fitted GARCH(1,1)")
                    + coord_cartesian(ylim = c(-6.5,4.0))
                    + xlab("Index") 
                    + xlim(0,n_logreturns)
                    + ylab("Residuals"))
pgarchfit

#STANDARDIZED RESIDUALS VS DIFFERENT DISTRIBUTIONS
pdenstdres <- (qplot((as.numeric(sp500_logreturns)/sigma(model.fit)), geom = 'density')+
                     geom_density(fill = 'grey',
                                  alpha = 0.4)+
                     labs(x = 'Residuals', y = "Frequency")+
                     ggtitle("Density plot")+
                     theme(plot.title = element_text(hjust = 0.5)))

grid.arrange(pdenstdres,
(ggplot(dataframe_garchfit_stdres_melt, aes(sample = value))+
                stat_qq(distribution = qnorm, dparams = c(mean = 0, sd = 1))  +
                stat_qq_line(distribution = qnorm, dparams = c(mean = 0, sd = 1)) +
                labs(x = "Theoretical quantiles", y = "Residuals quantiles")+
                 coord_cartesian(ylim = c(-6.5,4.0))+
                ggtitle("QQ-plot Standard Normal")+
                theme(plot.title = element_text(hjust = 0.5))),
(ggplot(dataframe_garchfit_stdres_melt, aes(sample = value))+
                stat_qq(distribution = qstd, dparams = c(mean = 0, sd = 1,  nu = 3))  +
                stat_qq_line(distribution = qstd, dparams = c(mean = 0, sd = 1,  nu = 3)) +
                labs(x = "Theoretical quantiles", y = "Residuals quantiles")+
                coord_cartesian(ylim = c(-6.5,4.0))+
                ggtitle("QQ-plot Student's t with df = 3")+
                theme(plot.title = element_text(hjust = 0.5))),
(ggplot(dataframe_garchfit_stdres_melt, aes(sample = value))+
                stat_qq(distribution = qstd, dparams = c(mean = 0, sd = 1,  nu = 4))  +
                stat_qq_line(distribution = qstd, dparams = c(mean = 0, sd = 1,  nu = 4)) +
                labs(x = "Theoretical quantiles", y = "Residuals quantiles")+
                coord_cartesian(ylim = c(-6.5,4.0))+
                ggtitle("QQ-plot Student's t with df = 4")+
                theme(plot.title = element_text(hjust = 0.5))),
nrow = 2, ncol = 2)

#ACF FOR TRANSFORMED STANDARDIZED RESIDUALS
(ggAcf(sp500_logreturns/model.fit@fit$sigma, lag.max = 60) +
              theme(plot.title = element_text(hjust = 0.5)) +
              ggtitle("") +
              ylim(-0.1,0.5)+
              xlab(""))

(ggAcf((sp500_logreturns/model.fit@fit$sigma)^2, lag.max = 60) +
                  theme(plot.title = element_text(hjust = 0.5)) +
                  ggtitle("") +
                  ylim(-0.1,0.5)+
                  theme(legend.title = element_blank())+
                  xlab(""))

(ggAcf(abs(sp500_logreturns/model.fit@fit$sigma), lag.max = 60) +
               theme(plot.title = element_text(hjust = 0.5)) +
               ggtitle("") +
               ylim(-0.1,0.5))

grid.arrange(plotacf, plotacfnorm, plotacft, top = textGrob("Sample Autocorrelation Function comparison of raw data",gp=gpar(fontsize=14)))

Box.test(abs(sp500_logreturns/model.fit@fit$sigma), lag = 10, type = 'Ljung-Box')
####################


##########VALUE AT RISK##########

#EMPIRICAL QUANTILES
options(scipen = 999)
q10 <- quantile(sp500_logreturns, 0.10)
q05 <- quantile(sp500_logreturns, 0.05)
q01 <- quantile(sp500_logreturns, 0.01)

#HISTOGRAMS
phistvar10 <- (qplot(sp500_logreturns, geom = 'histogram')
               + geom_histogram(fill = 'darkgrey',
                                bins = 30) 
               + geom_histogram(aes(sp500_logreturns[sp500_logreturns < q10]), fill = 'green',
                                bins = 30)
               + labs(x = 'Log-returns', y = 'Frequency'))

phistvar05 <- (qplot(sp500_logreturns, geom = 'histogram')
          + geom_histogram(fill = 'darkgray',
                           bins = 30) 
          + geom_histogram(aes(sp500_logreturns[sp500_logreturns < q05]), fill = 'red',
                           bins = 30)
          + labs(x = 'Log-returns', y = 'Frequency'))

phistvar01 <- (qplot(sp500_logreturns, geom = 'histogram')
               + geom_histogram(fill = 'darkgrey',
                                bins = 30) 
               + geom_histogram(aes(sp500_logreturns[sp500_logreturns < q01]), fill = 'blue',
                                bins = 30)
               + labs(x = 'Log-returns', y = ''))

grid.arrange(phistvar10, phistvar05, phistvar01, nrow = 1, top = textGrob("Bad Log-returns",gp=gpar(fontsize=14)))

#CVAR PLOT NORMAL DISTRIBUTION ASSUMPTION
pvar <- (ggplot(data = dataframe_sp500_logreturns_melt, 
                aes(x = 1:n_logreturns, y = value), geom = 'point')
         + geom_point()
         + geom_line(aes(y = model.fit@fit$sigma*(qnorm(p = 0.10)), x = 1:n_logreturns), 
                     color = 'green')
         + geom_line(aes(y = model.fit@fit$sigma*(qnorm(p = 0.05)), x = 1:n_logreturns), 
                     color = 'red')
         + geom_line(aes(y = model.fit@fit$sigma*(qnorm(p = 0.01)), x = 1:n_logreturns), 
                     color = 'blue')
         + labs(x = 'Index', y = 'Returns')
         + ggtitle('Conditional Value at Risk with standard normal noise')
         + coord_cartesian(ylim = c(-0.07,0.050))
         + theme(plot.title = element_text(hjust = 0.5)))

pvar10 <- (ggplot(data = dataframe_sp500_logreturns_melt, 
                aes(x = 1:n_logreturns, y = value), geom = 'point')
         + geom_point()
         + coord_cartesian(ylim = c(-0.07,0.050))
         + geom_line(aes(y = model.fit@fit$sigma*(qnorm(p = 0.10)), x = 1:n_logreturns), 
                     color = 'green')
         + labs(x = 'Index', y = 'Returns')
         + geom_hline(yintercept = quantile(sp500_logreturns, 0.10), color = 'darkgreen')
         +  annotation_custom(gTree(children=gList(textGrob("\u03B1 = 0.10", hjust=1.5,x=0.4,vjust=3,y=1, 
                                                            gp=gpar(col="black", fontsize=11, fontface="bold"))))))
pvar05 <- (ggplot(data = dataframe_sp500_logreturns_melt, 
                  aes(x = 1:n_logreturns, y = value), geom = 'point')
           + geom_point()
           + coord_cartesian(ylim = c(-0.07,0.050))
           + geom_line(aes(y = model.fit@fit$sigma*(qnorm(p = 0.05)), x = 1:n_logreturns), 
                       color = 'red')
           + labs(x = 'Index', y = '')
           + geom_hline(yintercept = quantile(sp500_logreturns, 0.05), color = 'darkred')
           +  annotation_custom(gTree(children=gList(textGrob("\u03B1 = 0.05", hjust=1.5,x=0.4,vjust=3,y=1, 
                                                              gp=gpar(col="black", fontsize=11, fontface="bold"))))))
pvar01 <- (ggplot(data = dataframe_sp500_logreturns_melt, 
                  aes(x = 1:n_logreturns, y = value), geom = 'point')
           + geom_point()
           + coord_cartesian(ylim = c(-0.07,0.050))
           + geom_line(aes(y = model.fit@fit$sigma*(qnorm(p = 0.01)), x = 1:n_logreturns), 
                       color = 'blue')
           + labs(x = 'Index', y = '')
           + geom_hline(yintercept = quantile(sp500_logreturns, 0.01), color = 'darkblue')
           + annotation_custom(gTree(children=gList(textGrob("\u03B1 = 0.01", hjust=1.5,x=0.4,vjust=3,y=1, 
                                                              gp=gpar(col="black", fontsize=11, fontface="bold"))))))
pvar
grid.arrange(pvar10, pvar05, pvar01, ncol = 3, nrow = 1)
grid.arrange(pvar, arrangeGrob(pvar10, pvar05, pvar01, widths = c(0.33,0.33,0.33)), nrow = 2)

#CVAR PLOTS T DISTRIBUTION ASSUMPTION
pvar2 <- (ggplot(data = dataframe_sp500_logreturns_melt, 
                aes(x = 1:n_logreturns, y = value), geom = 'point')
         + geom_point()
         + geom_line(aes(y = model.fit@fit$sigma*(qstd(p = 0.10, mean = 0, sd = 1, nu = 4)), x = 1:n_logreturns), 
                     color = 'green')
         + geom_line(aes(y = model.fit@fit$sigma*(qstd(p = 0.05, mean = 0, sd = 1, nu = 4)), x = 1:n_logreturns), 
                     color = 'red')
         + geom_line(aes(y = model.fit@fit$sigma*(qstd(p = 0.01, mean = 0, sd = 1, nu = 4)), x = 1:n_logreturns), 
                     color = 'blue')
         + labs(x = 'Index', y = 'Returns')
         + ggtitle('Conditional Value at Risk with t-distributed noise')
         + coord_cartesian(ylim = c(-0.07,0.050))
         + theme(plot.title = element_text(hjust = 0.5)))

pvar210 <- (ggplot(data = dataframe_sp500_logreturns_melt, 
                  aes(x = 1:n_logreturns, y = value), geom = 'point')
           + geom_point()
           + coord_cartesian(ylim = c(-0.07,0.050))
           + geom_line(aes(y = model.fit@fit$sigma*(qstd(p = 0.10, mean = 0, sd = 1, nu = 4)), x = 1:n_logreturns), 
                       color = 'green')
           + labs(x = 'Index', y = 'Returns')
           + geom_hline(yintercept = quantile(sp500_logreturns, 0.10), color = 'darkgreen')
           +  annotation_custom(gTree(children=gList(textGrob("\u03B1 = 0.10", hjust=1.5,x=0.4,vjust=3,y=1, 
                                                              gp=gpar(col="black", fontsize=11, fontface="bold"))))))
pvar205 <- (ggplot(data = dataframe_sp500_logreturns_melt, 
                  aes(x = 1:n_logreturns, y = value), geom = 'point')
           + geom_point()
           + coord_cartesian(ylim = c(-0.07,0.050))
           + geom_line(aes(y = model.fit@fit$sigma*(qstd(p = 0.05, mean = 0, sd = 1, nu = 4)), x = 1:n_logreturns), 
                       color = 'red')
           + labs(x = 'Index', y = '')
           + geom_hline(yintercept = quantile(sp500_logreturns, 0.05), color = 'darkred')
           +  annotation_custom(gTree(children=gList(textGrob("\u03B1 = 0.05", hjust=1.5,x=0.4,vjust=3,y=1, 
                                                              gp=gpar(col="black", fontsize=11, fontface="bold"))))))
pvar201 <- (ggplot(data = dataframe_sp500_logreturns_melt, 
                  aes(x = 1:n_logreturns, y = value), geom = 'point')
           + geom_point()
           + coord_cartesian(ylim = c(-0.07,0.050))
           + geom_line(aes(y = model.fit@fit$sigma*(qstd(p = 0.01, mean = 0, sd = 1, nu = 4)), x = 1:n_logreturns), 
                       color = 'blue')
           + labs(x = 'Index', y = '')
           + geom_hline(yintercept = quantile(sp500_logreturns, 0.01), color = 'darkblue')
           + annotation_custom(gTree(children=gList(textGrob("\u03B1 = 0.01", hjust=1.5,x=0.4,vjust=3,y=1, 
                                                             gp=gpar(col="black", fontsize=11, fontface="bold"))))))
pvar2
grid.arrange(pvar210, pvar205, pvar201, ncol = 3, nrow = 1)
grid.arrange(pvar, arrangeGrob(pvar10, pvar05, pvar01, widths = c(0.33,0.33,0.33)), nrow = 2)
####################


##########VALUE AT RISK FORECAST##########

#DIFFERENT ROLLING GARCHMODELS
model.roll1 = ugarchroll(spec = model.spec, data = sp500_logreturns, 
                        n.start = 506, refit.every = 5,
                        refit.window = 'moving')
model.roll2 = ugarchroll(spec = model.spec, data = sp500_logreturns, 
                         n.start = 506, refit.every = 20,
                         refit.window = 'moving')
model.roll3 = ugarchroll(spec = model.spec, data = sp500_logreturns, 
                        n.start = 506, refit.every = 120,
                        refit.window = 'moving')

#VAR FORECAST NORMAL DISTRIBUTION ASSUMPTION
VaR90_n_roll1 <- model.roll1@forecast$density[,'Sigma']*qnorm(p = 0.10)
VaR95_n_roll1 <- model.roll1@forecast$density[,'Sigma']*qnorm(p = 0.05)
VaR99_n_roll1 <- model.roll1@forecast$density[,'Sigma']*qnorm(p = 0.01)

VaR90_n_roll2 <- model.roll2@forecast$density[,'Sigma']*qnorm(p = 0.10)
VaR95_n_roll2 <- model.roll2@forecast$density[,'Sigma']*qnorm(p = 0.05)
VaR99_n_roll2 <- model.roll2@forecast$density[,'Sigma']*qnorm(p = 0.01)

VaR90_n_roll3 <- model.roll3@forecast$density[,'Sigma']*qnorm(p = 0.10)
VaR95_n_roll3 <- model.roll3@forecast$density[,'Sigma']*qnorm(p = 0.05)
VaR99_n_roll3 <- model.roll3@forecast$density[,'Sigma']*qnorm(p = 0.01)

pvarforecastweek <- (qplot(y = VaR95_n_roll1, x = 507:1006, geom = 'line')
        + geom_line(color = 'red')
        + geom_line(aes(y = VaR90_n_roll1, x = 507:1006), color = 'green')
        + geom_line(aes(y = VaR99_n_roll1, x = 507:1006), color = 'blue')
        + geom_line(aes(x = 507:1006, y = sp500_logreturns[507:1006]))
        + geom_line(aes(x = 1:506, y = sp500_logreturns[1:506]))
        + coord_cartesian(ylim = c(-0.07,0.05))
        + labs(y = 'Log returns', x = element_blank()))

pvarforecastmonth <- (qplot(y = VaR95_n_roll2, x = 507:1006, geom = 'line')
        + geom_line(color = 'red')
        + geom_line(aes(y = VaR90_n_roll2, x = 507:1006), color = 'green')
        + geom_line(aes(y = VaR99_n_roll2, x = 507:1006), color = 'blue')
        + geom_line(aes(x = 507:1006, y = sp500_logreturns[507:1006]))
        + geom_line(aes(x = 1:506, y = sp500_logreturns[1:506]))
        + coord_cartesian(ylim = c(-0.07,0.05))
        + labs(y = 'Log returns', x = element_blank()))

pvarforecasthalfyear <- (qplot(y = VaR95_n_roll3, x = 507:1006, geom = 'line')
        + geom_line(color = 'red')
        + geom_line(aes(y = VaR90_n_roll3, x = 507:1006), color = 'green')
        + geom_line(aes(y = VaR99_n_roll3, x = 507:1006), color = 'blue')
        + geom_line(aes(x = 507:1006, y = sp500_logreturns[507:1006]))
        + geom_line(aes(x = 1:506, y = sp500_logreturns[1:506]))
        + coord_cartesian(ylim = c(-0.07,0.05))
        + labs(y = 'Log returns', x = element_blank()))

#VAR FORECAST T DISTRIBUTION ASSUMPTION
VaR90_n_roll21 <- model.roll1@forecast$density[,'Sigma']*(qstd(p = 0.10, mean = 0, sd = 1, nu = 4))
VaR95_n_roll21 <- model.roll1@forecast$density[,'Sigma']*(qstd(p = 0.05, mean = 0, sd = 1, nu = 4))
VaR99_n_roll21 <- model.roll1@forecast$density[,'Sigma']*(qstd(p = 0.01, mean = 0, sd = 1, nu = 4))

VaR90_n_roll22 <- model.roll2@forecast$density[,'Sigma']*(qstd(p = 0.10, mean = 0, sd = 1, nu = 4))
VaR95_n_roll22 <- model.roll2@forecast$density[,'Sigma']*(qstd(p = 0.05, mean = 0, sd = 1, nu = 4))
VaR99_n_roll22 <- model.roll2@forecast$density[,'Sigma']*(qstd(p = 0.01, mean = 0, sd = 1, nu = 4))

VaR90_n_roll23 <- model.roll3@forecast$density[,'Sigma']*(qstd(p = 0.10, mean = 0, sd = 1, nu = 4))
VaR95_n_roll23 <- model.roll3@forecast$density[,'Sigma']*(qstd(p = 0.05, mean = 0, sd = 1, nu = 4))
VaR99_n_roll23 <- model.roll3@forecast$density[,'Sigma']*(qstd(p = 0.01, mean = 0, sd = 1, nu = 4))

pvarforecastweek2 <- (qplot(y = VaR95_n_roll21, x = 507:1006, geom = 'line')
                     + geom_line(color = 'red')
                     + geom_line(aes(y = VaR90_n_roll21, x = 507:1006), color = 'green')
                     + geom_line(aes(y = VaR99_n_roll21, x = 507:1006), color = 'blue')
                     + geom_line(aes(x = 507:1006, y = sp500_logreturns[507:1006]))
                     + geom_line(aes(x = 1:506, y = sp500_logreturns[1:506]))
                     + coord_cartesian(ylim = c(-0.07,0.05))
                     + labs(y = 'Log returns', x = 'Test Observations'))

pvarforecastmonth2 <- (qplot(y = VaR95_n_roll22, x = 507:1006, geom = 'line')
                      + geom_line(color = 'red')
                      + geom_line(aes(y = VaR90_n_roll22, x = 507:1006), color = 'green')
                      + geom_line(aes(y = VaR99_n_roll22, x = 507:1006), color = 'blue')
                      + geom_line(aes(x = 507:1006, y = sp500_logreturns[507:1006]))
                      + geom_line(aes(x = 1:506, y = sp500_logreturns[1:506]))
                      + coord_cartesian(ylim = c(-0.07,0.05))
                      + labs(y = 'Log returns', x = 'Test Observations'))

pvarforecasthalfyear2 <- (qplot(y = VaR95_n_roll23, x = 507:1006, geom = 'line')
                         + geom_line(color = 'red')
                         + geom_line(aes(y = VaR90_n_roll23, x = 507:1006), color = 'green')
                         + geom_line(aes(y = VaR99_n_roll23, x = 507:1006), color = 'blue')
                         + geom_line(aes(x = 507:1006, y = sp500_logreturns[507:1006]))
                         + geom_line(aes(x = 1:506, y = sp500_logreturns[1:506]))
                         + coord_cartesian(ylim = c(-0.07,0.05))
                         + labs(y = 'Log returns', x = 'Test Observations'))

grid.arrange(pvarforecastweek, pvarforecastweek2, nrow = 2, ncol = 1, top = textGrob("VaR-forecasts with refit every week",gp=gpar(fontsize=14)))
grid.arrange(pvarforecastmonth, pvarforecastmonth2, nrow = 2, ncol = 1, top = textGrob("VaR-forecasts with refit every month",gp=gpar(fontsize=14)))
grid.arrange(pvarforecasthalfyear, pvarforecasthalfyear2, nrow = 2, ncol = 1, top = textGrob("VaR-forecasts with refit every sixth month",gp=gpar(fontsize=14)))

#VAR FORECAST NORMAL DISTRIBUTION ASSUMPTION PAGE PLOT
pvarforecastweek90 <- (qplot(y = VaR90_n_roll1, x = 507:1006, geom = 'line')
                     + geom_point(aes(x = 507:1006, y = sp500_logreturns[507:1006]))
                     + geom_line(color = 'green')
                     + coord_cartesian(ylim = c(-0.07,0.05))
                     + labs(y = expression(atop(bold('Model 1'), 'Log returns')), x = element_blank())
                     + ggtitle("\u03B1 = 0.10")
                     + theme(plot.title = element_text(hjust = 0.5, size = 12)))
pvarforecastweek95 <- (qplot(y = VaR95_n_roll1, x = 507:1006, geom = 'line')
                       + geom_point(aes(x = 507:1006, y = sp500_logreturns[507:1006]))
                       + geom_line(color = 'red')
                       + coord_cartesian(ylim = c(-0.07,0.05))
                       + labs(y = element_blank(), x = element_blank())
                       + ggtitle("\u03B1 = 0.05")
                       + theme(plot.title = element_text(hjust = 0.5, size = 12)))
pvarforecastweek99<- (qplot(y = VaR99_n_roll1, x = 507:1006, geom = 'line')
                      + geom_point(aes(x = 507:1006, y = sp500_logreturns[507:1006]))
                       + geom_line(color = 'blue')
                       + coord_cartesian(ylim = c(-0.07,0.05))
                       + labs(y = element_blank(), x = element_blank())
                       + ggtitle("\u03B1 = 0.01")
                       + theme(plot.title = element_text(hjust = 0.5, size = 12)))

pvarforecastmonth90 <- (qplot(y = VaR90_n_roll2, x = 507:1006, geom = 'line')
                        + geom_point(aes(x = 507:1006, y = sp500_logreturns[507:1006]))
                       + geom_line(color = 'green')
                       + coord_cartesian(ylim = c(-0.07,0.05))
                       + labs(y = expression(atop(bold('Model 2'), 'Log returns')), x = element_blank())
                       + theme(plot.title = element_text(hjust = 0.5)))
pvarforecastmonth95 <- (qplot(y = VaR95_n_roll2, x = 507:1006, geom = 'line')
                        + geom_point(aes(x = 507:1006, y = sp500_logreturns[507:1006]))
                        + geom_line(color = 'red')
                        + coord_cartesian(ylim = c(-0.07,0.05))
                        + labs(y = element_blank(), x = element_blank())
                        + theme(plot.title = element_text(hjust = 0.5)))
pvarforecastmonth99 <- (qplot(y = VaR99_n_roll2, x = 507:1006, geom = 'line')
                        + geom_point(aes(x = 507:1006, y = sp500_logreturns[507:1006]))
                        + geom_line(color = 'blue')
                        + coord_cartesian(ylim = c(-0.07,0.05))
                        + labs(y = element_blank(), x = element_blank())
                        + theme(plot.title = element_text(hjust = 0.5)))

pvarforecasthalfyear90 <- (qplot(y = VaR90_n_roll3, x = 507:1006, geom = 'line')
                           + geom_point(aes(x = 507:1006, y = sp500_logreturns[507:1006]))
                        + geom_line(color = 'green')
                        + coord_cartesian(ylim = c(-0.07,0.05))
                        + labs(y = expression(atop(bold('Model 3'), 'Log returns')), x = 'Test observations')
                        + theme(plot.title = element_text(hjust = 0.5)))
pvarforecasthalfyear95 <- (qplot(y = VaR95_n_roll3, x = 507:1006, geom = 'line')
                           + geom_point(aes(x = 507:1006, y = sp500_logreturns[507:1006]))
                          + geom_line(color = 'red')
                          + coord_cartesian(ylim = c(-0.07,0.05))
                          + labs(y = element_blank(), x = 'Test observations')
                          + theme(plot.title = element_text(hjust = 0.5)))
pvarforecasthalfyear99<- (qplot(y = VaR99_n_roll3, x = 507:1006, geom = 'line')
                          + geom_point(aes(x = 507:1006, y = sp500_logreturns[507:1006]))
                          + geom_line(color = 'blue')
                          + coord_cartesian(ylim = c(-0.07,0.05))
                          + labs(y = element_blank(), x = 'Test observations')
                          + theme(plot.title = element_text(hjust = 0.5)))

grob90 <- arrangeGrob(pvarforecastweek90, pvarforecastmonth90, pvarforecasthalfyear90)
grob95 <- arrangeGrob(pvarforecastweek95, pvarforecastmonth95, pvarforecasthalfyear95)
grob99 <- arrangeGrob(pvarforecastweek99, pvarforecastmonth99, pvarforecasthalfyear99)
grid.arrange((arrangeGrob(grob90, grob95, grob99, widths = c(0.33,0.33,0.33))))

#VAR FORECAST T DISTRIBUTION ASSUMPTION PAGE PLOT
pvarforecastweek290 <- (qplot(y = VaR90_n_roll21, x = 507:1006, geom = 'line')
                       + geom_point(aes(x = 507:1006, y = sp500_logreturns[507:1006]))
                       + geom_line(color = 'green')
                       + coord_cartesian(ylim = c(-0.07,0.05))
                       + labs(y = expression(atop(bold('Model 1'), 'Log returns')), x = element_blank())
                       + ggtitle("\u03B1 = 0.10")
                       + theme(plot.title = element_text(hjust = 0.5, size = 12)))
pvarforecastweek295 <- (qplot(y = VaR95_n_roll21, x = 507:1006, geom = 'line')
                       + geom_point(aes(x = 507:1006, y = sp500_logreturns[507:1006]))
                       + geom_line(color = 'red')
                       + coord_cartesian(ylim = c(-0.07,0.05))
                       + labs(y = element_blank(), x = element_blank())
                       + ggtitle("\u03B1 = 0.05")
                       + theme(plot.title = element_text(hjust = 0.5, size = 12)))
pvarforecastweek299<- (qplot(y = VaR99_n_roll21, x = 507:1006, geom = 'line')
                      + geom_point(aes(x = 507:1006, y = sp500_logreturns[507:1006]))
                      + geom_line(color = 'blue')
                      + coord_cartesian(ylim = c(-0.07,0.05))
                      + labs(y = element_blank(), x = element_blank())
                      + ggtitle("\u03B1 = 0.01")
                      + theme(plot.title = element_text(hjust = 0.5, size = 12)))

pvarforecastmonth290 <- (qplot(y = VaR90_n_roll22, x = 507:1006, geom = 'line')
                        + geom_point(aes(x = 507:1006, y = sp500_logreturns[507:1006]))
                        + geom_line(color = 'green')
                        + coord_cartesian(ylim = c(-0.07,0.05))
                        + labs(y = expression(atop(bold('Model 2'), 'Log returns')), x = element_blank())
                        + theme(plot.title = element_text(hjust = 0.5)))
pvarforecastmonth295 <- (qplot(y = VaR95_n_roll22, x = 507:1006, geom = 'line')
                        + geom_point(aes(x = 507:1006, y = sp500_logreturns[507:1006]))
                        + geom_line(color = 'red')
                        + coord_cartesian(ylim = c(-0.07,0.05))
                        + labs(y = element_blank(), x = element_blank())
                        + theme(plot.title = element_text(hjust = 0.5)))
pvarforecastmonth299 <- (qplot(y = VaR99_n_roll22, x = 507:1006, geom = 'line')
                        + geom_point(aes(x = 507:1006, y = sp500_logreturns[507:1006]))
                        + geom_line(color = 'blue')
                        + coord_cartesian(ylim = c(-0.07,0.05))
                        + labs(y = element_blank(), x = element_blank())
                        + theme(plot.title = element_text(hjust = 0.5)))

pvarforecasthalfyear290 <- (qplot(y = VaR90_n_roll23, x = 507:1006, geom = 'line')
                           + geom_point(aes(x = 507:1006, y = sp500_logreturns[507:1006]))
                           + geom_line(color = 'green')
                           + coord_cartesian(ylim = c(-0.07,0.05))
                           + labs(y = expression(atop(bold('Model 3'), 'Log returns')), x = 'Test observations')
                           + theme(plot.title = element_text(hjust = 0.5)))
pvarforecasthalfyear295 <- (qplot(y = VaR95_n_roll23, x = 507:1006, geom = 'line')
                           + geom_point(aes(x = 507:1006, y = sp500_logreturns[507:1006]))
                           + geom_line(color = 'red')
                           + coord_cartesian(ylim = c(-0.07,0.05))
                           + labs(y = element_blank(), x = 'Test observations')
                           + theme(plot.title = element_text(hjust = 0.5)))
pvarforecasthalfyear299<- (qplot(y = VaR99_n_roll23, x = 507:1006, geom = 'line')
                          + geom_point(aes(x = 507:1006, y = sp500_logreturns[507:1006]))
                          + geom_line(color = 'blue')
                          + coord_cartesian(ylim = c(-0.07,0.05))
                          + labs(y = element_blank(), x = 'Test observations')
                          + theme(plot.title = element_text(hjust = 0.5)))

grob290 <- arrangeGrob(pvarforecastweek290, pvarforecastmonth290, pvarforecasthalfyear290)
grob295 <- arrangeGrob(pvarforecastweek295, pvarforecastmonth295, pvarforecasthalfyear295)
grob299 <- arrangeGrob(pvarforecastweek299, pvarforecastmonth299, pvarforecasthalfyear299)
grid.arrange((arrangeGrob(grob290, grob295, grob299, widths = c(0.33,0.33,0.33))))
####################


############VAR BACKTESTING############

#VAR BACKTEST NORMAL DISTRIBUTION ASSUMPTION
VaRdots90_roll1 <- (as.numeric(sp500_logreturns[507:1006]<VaR90_n_roll1))
VaRdots90_roll1[VaRdots90_roll1 == 0] <- NA
VaRdots95_roll1 <- (as.numeric(sp500_logreturns[507:1006]<VaR95_n_roll1))
VaRdots95_roll1[VaRdots95_roll1 == 0] <- NA
VaRdots99_roll1 <- (as.numeric(sp500_logreturns[507:1006]<VaR99_n_roll1))
VaRdots99_roll1[VaRdots99_roll1 == 0] <- NA

VaRdots90_roll2 <- (as.numeric(sp500_logreturns[507:1006]<VaR90_n_roll2))
VaRdots90_roll2[VaRdots90_roll2 == 0] <- NA
VaRdots95_roll2 <- (as.numeric(sp500_logreturns[507:1006]<VaR95_n_roll2))
VaRdots95_roll2[VaRdots95_roll2 == 0] <- NA
VaRdots99_roll2 <- (as.numeric(sp500_logreturns[507:1006]<VaR99_n_roll2))
VaRdots99_roll2[VaRdots99_roll2 == 0] <- NA

VaRdots90_roll3 <- (as.numeric(sp500_logreturns[507:1006]<VaR90_n_roll3))
VaRdots90_roll3[VaRdots90_roll3 == 0] <- NA
VaRdots95_roll3 <- (as.numeric(sp500_logreturns[507:1006]<VaR95_n_roll3))
VaRdots95_roll3[VaRdots95_roll3 == 0] <- NA
VaRdots99_roll3 <- (as.numeric(sp500_logreturns[507:1006]<VaR99_n_roll3))
VaRdots99_roll3[VaRdots99_roll3 == 0] <- NA

pbacktestweek90 <- (qplot(y = VaR90_n_roll1, x = 507:1006, geom = 'line')
                    + geom_line(aes(x = 507:1006, y = sp500_logreturns[507:1006]))
                    + geom_line(color = 'green')
                    + geom_point(aes(x = 507:1006, y = VaRdots90_roll1*sp500_logreturns[507:1006],
                                     fill = 'red'), show.legend = FALSE)
                    + labs(y = expression(atop(bold('Model 1'), 'Log returns')), x = element_blank())
                    + coord_cartesian(ylim = c(-0.07,0.05))
                    + ggtitle("\u03B1 = 0.10")
                    + theme(plot.title = element_text(hjust = 0.5, size = 12)))
pbacktestweek95 <- (qplot(y = VaR95_n_roll1, x = 507:1006, geom = 'line')
                    + geom_line(aes(x = 507:1006, y = sp500_logreturns[507:1006]))
                    + geom_line(color = 'red')
                    + geom_point(aes(x = 507:1006, y = VaRdots95_roll1*sp500_logreturns[507:1006],
                                     fill = 'red'), show.legend = FALSE)
                    + labs(y = element_blank(), x = element_blank())
                    + coord_cartesian(ylim = c(-0.07,0.05))
                    + ggtitle("\u03B1 = 0.05")
                    + theme(plot.title = element_text(hjust = 0.5, size = 12)))
pbacktestweek99 <- (qplot(y = VaR99_n_roll1, x = 507:1006, geom = 'line')
                    + geom_line(aes(x = 507:1006, y = sp500_logreturns[507:1006]))
                    + geom_line(color = 'blue')
                    + geom_point(aes(x = 507:1006, y = VaRdots99_roll1*sp500_logreturns[507:1006],
                                     fill = 'red'), show.legend = FALSE)
                    + labs(y = element_blank(), x = element_blank())
                    + coord_cartesian(ylim = c(-0.07,0.05))
                    + ggtitle("\u03B1 = 0.01")
                    + theme(plot.title = element_text(hjust = 0.5, size = 12)))

pbacktestmonth90 <- (qplot(y = VaR90_n_roll2, x = 507:1006, geom = 'line')
                    + geom_line(aes(x = 507:1006, y = sp500_logreturns[507:1006]))
                    + geom_line(color = 'green')
                    + geom_point(aes(x = 507:1006, y = VaRdots90_roll2*sp500_logreturns[507:1006],
                                     fill = 'red'), show.legend = FALSE)
                    + labs(y = expression(atop(bold('Model 2'), 'Log returns')), x = element_blank())
                    + coord_cartesian(ylim = c(-0.07,0.05)))
pbacktestmonth95 <- (qplot(y = VaR95_n_roll2, x = 507:1006, geom = 'line')
                     + geom_line(aes(x = 507:1006, y = sp500_logreturns[507:1006]))
                     + geom_line(color = 'red')
                     + geom_point(aes(x = 507:1006, y = VaRdots95_roll2*sp500_logreturns[507:1006],
                                      fill = 'red'), show.legend = FALSE)
                     + labs(y = element_blank(), x = element_blank())
                     + coord_cartesian(ylim = c(-0.07,0.05)))
pbacktestmonth99 <- (qplot(y = VaR99_n_roll2, x = 507:1006, geom = 'line')
                     + geom_line(aes(x = 507:1006, y = sp500_logreturns[507:1006]))
                     + geom_line(color = 'blue')
                     + geom_point(aes(x = 507:1006, y = VaRdots99_roll2*sp500_logreturns[507:1006],
                                      fill = 'red'), show.legend = FALSE)
                     + labs(y = element_blank(), x = element_blank())
                     + coord_cartesian(ylim = c(-0.07,0.05)))

pbacktesthalfyear90 <- (qplot(y = VaR90_n_roll3, x = 507:1006, geom = 'line')
                     + geom_line(aes(x = 507:1006, y = sp500_logreturns[507:1006]))
                     + geom_line(color = 'green')
                     + geom_point(aes(x = 507:1006, y = VaRdots90_roll3*sp500_logreturns[507:1006],
                                      fill = 'red'), show.legend = FALSE)
                     + labs(y = expression(atop(bold('Model 3'), 'Log returns')), x = 'Test observations')
                     + coord_cartesian(ylim = c(-0.07,0.05)))
pbacktesthalfyear95 <- (qplot(y = VaR95_n_roll3, x = 507:1006, geom = 'line')
                        + geom_line(aes(x = 507:1006, y = sp500_logreturns[507:1006]))
                        + geom_line(color = 'red')
                        + geom_point(aes(x = 507:1006, y = VaRdots95_roll3*sp500_logreturns[507:1006],
                                         fill = 'red'), show.legend = FALSE)
                        + labs(y = element_blank(), x = 'Test observations')
                        + coord_cartesian(ylim = c(-0.07,0.05)))
pbacktesthalfyear99 <- (qplot(y = VaR99_n_roll3, x = 507:1006, geom = 'line')
                        + geom_line(aes(x = 507:1006, y = sp500_logreturns[507:1006]))
                        + geom_line(color = 'blue')
                        + geom_point(aes(x = 507:1006, y = VaRdots99_roll3*sp500_logreturns[507:1006],
                                         fill = 'red'), show.legend = FALSE)
                        + labs(y = element_blank(), x = 'Test observations')
                        + coord_cartesian(ylim = c(-0.07,0.05)))

grob_test90 <- arrangeGrob(pbacktestweek90, pbacktestmonth90, pbacktesthalfyear90)
grob_test95 <- arrangeGrob(pbacktestweek95, pbacktestmonth95, pbacktesthalfyear95)
grob_test99 <- arrangeGrob(pbacktestweek99, pbacktestmonth99, pbacktesthalfyear99)
grid.arrange((arrangeGrob(grob_test90, grob_test95, grob_test99, widths = c(0.33,0.33,0.33))))

#VAR BACKTEST T DISTRIBUTION ASSUMPTION
VaRdots90_roll21 <- (as.numeric(sp500_logreturns[507:1006]<VaR90_n_roll21))
VaRdots90_roll21[VaRdots90_roll21 == 0] <- NA
VaRdots95_roll21 <- (as.numeric(sp500_logreturns[507:1006]<VaR95_n_roll21))
VaRdots95_roll21[VaRdots95_roll21 == 0] <- NA
VaRdots99_roll21 <- (as.numeric(sp500_logreturns[507:1006]<VaR99_n_roll21))
VaRdots99_roll21[VaRdots99_roll21 == 0] <- NA

VaRdots90_roll22 <- (as.numeric(sp500_logreturns[507:1006]<VaR90_n_roll22))
VaRdots90_roll22[VaRdots90_roll22 == 0] <- NA
VaRdots95_roll22 <- (as.numeric(sp500_logreturns[507:1006]<VaR95_n_roll22))
VaRdots95_roll22[VaRdots95_roll22 == 0] <- NA
VaRdots99_roll22 <- (as.numeric(sp500_logreturns[507:1006]<VaR99_n_roll22))
VaRdots99_roll22[VaRdots99_roll22 == 0] <- NA

VaRdots90_roll23 <- (as.numeric(sp500_logreturns[507:1006]<VaR90_n_roll23))
VaRdots90_roll23[VaRdots90_roll23 == 0] <- NA
VaRdots95_roll23 <- (as.numeric(sp500_logreturns[507:1006]<VaR95_n_roll23))
VaRdots95_roll23[VaRdots95_roll23 == 0] <- NA
VaRdots99_roll23 <- (as.numeric(sp500_logreturns[507:1006]<VaR99_n_roll23))
VaRdots99_roll23[VaRdots99_roll23 == 0] <- NA

pbacktestweek290 <- (qplot(y = VaR90_n_roll21, x = 507:1006, geom = 'line')
                    + geom_line(aes(x = 507:1006, y = sp500_logreturns[507:1006]))
                    + geom_line(color = 'green')
                    + geom_point(aes(x = 507:1006, y = VaRdots90_roll21*sp500_logreturns[507:1006],
                                     fill = 'red'), show.legend = FALSE)
                    + labs(y = expression(atop(bold('Model 1'), 'Log returns')), x = element_blank())
                    + coord_cartesian(ylim = c(-0.07,0.05))
                    + ggtitle("\u03B1 = 0.10")
                    + theme(plot.title = element_text(hjust = 0.5, size = 12)))
pbacktestweek295 <- (qplot(y = VaR95_n_roll21, x = 507:1006, geom = 'line')
                    + geom_line(aes(x = 507:1006, y = sp500_logreturns[507:1006]))
                    + geom_line(color = 'red')
                    + geom_point(aes(x = 507:1006, y = VaRdots95_roll21*sp500_logreturns[507:1006],
                                     fill = 'red'), show.legend = FALSE)
                    + labs(y = element_blank(), x = element_blank())
                    + coord_cartesian(ylim = c(-0.07,0.05))
                    + ggtitle("\u03B1 = 0.05")
                    + theme(plot.title = element_text(hjust = 0.5, size = 12)))
pbacktestweek299 <- (qplot(y = VaR99_n_roll21, x = 507:1006, geom = 'line')
                    + geom_line(aes(x = 507:1006, y = sp500_logreturns[507:1006]))
                    + geom_line(color = 'blue')
                    + geom_point(aes(x = 507:1006, y = VaRdots99_roll21*sp500_logreturns[507:1006],
                                     fill = 'red'), show.legend = FALSE)
                    + labs(y = element_blank(), x = element_blank())
                    + coord_cartesian(ylim = c(-0.07,0.05))
                    + ggtitle("\u03B1 = 0.01")
                    + theme(plot.title = element_text(hjust = 0.5, size = 12)))

pbacktestmonth290 <- (qplot(y = VaR90_n_roll22, x = 507:1006, geom = 'line')
                     + geom_line(aes(x = 507:1006, y = sp500_logreturns[507:1006]))
                     + geom_line(color = 'green')
                     + geom_point(aes(x = 507:1006, y = VaRdots90_roll22*sp500_logreturns[507:1006],
                                      fill = 'red'), show.legend = FALSE)
                     + labs(y = expression(atop(bold('Model 2'), 'Log returns')), x = element_blank())
                     + coord_cartesian(ylim = c(-0.07,0.05)))
pbacktestmonth295 <- (qplot(y = VaR95_n_roll22, x = 507:1006, geom = 'line')
                     + geom_line(aes(x = 507:1006, y = sp500_logreturns[507:1006]))
                     + geom_line(color = 'red')
                     + geom_point(aes(x = 507:1006, y = VaRdots95_roll22*sp500_logreturns[507:1006],
                                      fill = 'red'), show.legend = FALSE)
                     + labs(y = element_blank(), x = element_blank())
                     + coord_cartesian(ylim = c(-0.07,0.05)))
pbacktestmonth299 <- (qplot(y = VaR99_n_roll22, x = 507:1006, geom = 'line')
                     + geom_line(aes(x = 507:1006, y = sp500_logreturns[507:1006]))
                     + geom_line(color = 'blue')
                     + geom_point(aes(x = 507:1006, y = VaRdots99_roll22*sp500_logreturns[507:1006],
                                      fill = 'red'), show.legend = FALSE)
                     + labs(y = element_blank(), x = element_blank())
                     + coord_cartesian(ylim = c(-0.07,0.05)))

pbacktesthalfyear290 <- (qplot(y = VaR90_n_roll23, x = 507:1006, geom = 'line')
                        + geom_line(aes(x = 507:1006, y = sp500_logreturns[507:1006]))
                        + geom_line(color = 'green')
                        + geom_point(aes(x = 507:1006, y = VaRdots90_roll23*sp500_logreturns[507:1006],
                                         fill = 'red'), show.legend = FALSE)
                        + labs(y = expression(atop(bold('Model 3'), 'Log returns')), x = 'Test observations')
                        + coord_cartesian(ylim = c(-0.07,0.05)))
pbacktesthalfyear295 <- (qplot(y = VaR95_n_roll23, x = 507:1006, geom = 'line')
                        + geom_line(aes(x = 507:1006, y = sp500_logreturns[507:1006]))
                        + geom_line(color = 'red')
                        + geom_point(aes(x = 507:1006, y = VaRdots95_roll23*sp500_logreturns[507:1006],
                                         fill = 'red'), show.legend = FALSE)
                        + labs(y = element_blank(), x = 'Test observations')
                        + coord_cartesian(ylim = c(-0.07,0.05)))
pbacktesthalfyear299 <- (qplot(y = VaR99_n_roll23, x = 507:1006, geom = 'line')
                        + geom_line(aes(x = 507:1006, y = sp500_logreturns[507:1006]))
                        + geom_line(color = 'blue')
                        + geom_point(aes(x = 507:1006, y = VaRdots99_roll23*sp500_logreturns[507:1006],
                                         fill = 'red'), show.legend = FALSE)
                        + labs(y = element_blank(), x = 'Test observations')
                        + coord_cartesian(ylim = c(-0.07,0.05)))

grob_test290 <- arrangeGrob(pbacktestweek290, pbacktestmonth290, pbacktesthalfyear290)
grob_test295 <- arrangeGrob(pbacktestweek295, pbacktestmonth295, pbacktesthalfyear295)
grob_test299 <- arrangeGrob(pbacktestweek299, pbacktestmonth299, pbacktesthalfyear299)
grid.arrange((arrangeGrob(grob_test290, grob_test295, grob_test299, widths = c(0.33,0.33,0.33))))

#95% CONFIDENSLEVEL OF BINOMIAL TEST
qbinom(p = c(0.025,0.975), size = 500, prob = 0.10)
qbinom(p = c(0.025,0.975), size = 500, prob = 0.05) 
qbinom(p = c(0.025,0.975), size = 500, prob = 0.01)

#99% CONFIDENSLEVEL OF BINOMIAL TEST
qbinom(p = c(0.005,0.995), size = 500, prob = 0.10)
qbinom(p = c(0.005,0.995), size = 500, prob = 0.05) 
qbinom(p = c(0.005,0.995), size = 500, prob = 0.01)

mean(rbinom(n = 10000, size = 500, prob = 0.10))
mean(rbinom(n = 10000, size = 500, prob = 0.05))
mean(rbinom(n = 10000, size = 500, prob = 0.01))

#EMPIRICAL DOWNCROSSINGS VAR BACKTEST NORMAL DISTRIBUTION ASSUMPTION
sum(as.numeric(sp500_logreturns[507:1006] < VaR90_n_roll1))
VaRTest(conf.level = 0.95, alpha = 0.10, actual = sp500_logreturns[507:1006], VaR = VaR90_n_roll1)
VaRTest(conf.level = 0.99, alpha = 0.10, actual = sp500_logreturns[507:1006], VaR = VaR90_n_roll1)

sum(as.numeric(sp500_logreturns[507:1006] < VaR95_n_roll1))
VaRTest(conf.level = 0.95, alpha = 0.05, actual = sp500_logreturns[507:1006], VaR = VaR95_n_roll1)
VaRTest(conf.level = 0.99, alpha = 0.05, actual = sp500_logreturns[507:1006], VaR = VaR95_n_roll1)

sum(as.numeric(sp500_logreturns[507:1006] < VaR99_n_roll1))
VaRTest(conf.level = 0.95, alpha = 0.01, actual = sp500_logreturns[507:1006], VaR = VaR99_n_roll1)
VaRTest(conf.level = 0.99, alpha = 0.01, actual = sp500_logreturns[507:1006], VaR = VaR99_n_roll1)


sum(as.numeric(sp500_logreturns[507:1006] < VaR90_n_roll2))
VaRTest(conf.level = 0.95, alpha = 0.10, actual = sp500_logreturns[507:1006], VaR = VaR90_n_roll2)
VaRTest(conf.level = 0.99, alpha = 0.10, actual = sp500_logreturns[507:1006], VaR = VaR90_n_roll2)

sum(as.numeric(sp500_logreturns[507:1006] < VaR95_n_roll2))
VaRTest(conf.level = 0.95, alpha = 0.05, actual = sp500_logreturns[507:1006], VaR = VaR95_n_roll2)
VaRTest(conf.level = 0.99, alpha = 0.05, actual = sp500_logreturns[507:1006], VaR = VaR95_n_roll2)

sum(as.numeric(sp500_logreturns[507:1006] < VaR99_n_roll2))
VaRTest(conf.level = 0.95, alpha = 0.01, actual = sp500_logreturns[507:1006], VaR = VaR99_n_roll2)
VaRTest(conf.level = 0.99, alpha = 0.01, actual = sp500_logreturns[507:1006], VaR = VaR99_n_roll2)


sum(as.numeric(sp500_logreturns[507:1006] < VaR90_n_roll3))
VaRTest(conf.level = 0.95, alpha = 0.10, actual = sp500_logreturns[507:1006], VaR = VaR90_n_roll3)
VaRTest(conf.level = 0.99, alpha = 0.10, actual = sp500_logreturns[507:1006], VaR = VaR90_n_roll3)

sum(as.numeric(sp500_logreturns[507:1006] < VaR95_n_roll3))
VaRTest(conf.level = 0.95, alpha = 0.05, actual = sp500_logreturns[507:1006], VaR = VaR95_n_roll3)
VaRTest(conf.level = 0.99, alpha = 0.05, actual = sp500_logreturns[507:1006], VaR = VaR95_n_roll3)

sum(as.numeric(sp500_logreturns[507:1006] < VaR99_n_roll3))
VaRTest(conf.level = 0.95, alpha = 0.01, actual = sp500_logreturns[507:1006], VaR = VaR99_n_roll3)
VaRTest(conf.level = 0.99, alpha = 0.01, actual = sp500_logreturns[507:1006], VaR = VaR99_n_roll3)


#EMPIRICAL DOWNCROSSINGS VAR BACKTEST T DISTRIBUTION ASSUMPTION
sum(as.numeric(sp500_logreturns[507:1006] < VaR90_n_roll21))
VaRTest(conf.level = 0.95, alpha = 0.10, actual = sp500_logreturns[507:1006], VaR = VaR90_n_roll21)
VaRTest(conf.level = 0.99, alpha = 0.10, actual = sp500_logreturns[507:1006], VaR = VaR90_n_roll21)

sum(as.numeric(sp500_logreturns[507:1006] < VaR95_n_roll21))
VaRTest(conf.level = 0.95, alpha = 0.05, actual = sp500_logreturns[507:1006], VaR = VaR95_n_roll21)
VaRTest(conf.level = 0.99, alpha = 0.05, actual = sp500_logreturns[507:1006], VaR = VaR95_n_roll21)

sum(as.numeric(sp500_logreturns[507:1006] < VaR99_n_roll21))
VaRTest(conf.level = 0.95, alpha = 0.01, actual = sp500_logreturns[507:1006], VaR = VaR99_n_roll21)
VaRTest(conf.level = 0.99, alpha = 0.01, actual = sp500_logreturns[507:1006], VaR = VaR99_n_roll21)


sum(as.numeric(sp500_logreturns[507:1006] < VaR90_n_roll22))
VaRTest(conf.level = 0.95, alpha = 0.10, actual = sp500_logreturns[507:1006], VaR = VaR90_n_roll22)
VaRTest(conf.level = 0.99, alpha = 0.10, actual = sp500_logreturns[507:1006], VaR = VaR90_n_roll22)

sum(as.numeric(sp500_logreturns[507:1006] < VaR95_n_roll22))
VaRTest(conf.level = 0.95, alpha = 0.05, actual = sp500_logreturns[507:1006], VaR = VaR95_n_roll22)
VaRTest(conf.level = 0.99, alpha = 0.05, actual = sp500_logreturns[507:1006], VaR = VaR95_n_roll22)

sum(as.numeric(sp500_logreturns[507:1006] < VaR99_n_roll22))
VaRTest(conf.level = 0.95, alpha = 0.01, actual = sp500_logreturns[507:1006], VaR = VaR99_n_roll22)
VaRTest(conf.level = 0.99, alpha = 0.01, actual = sp500_logreturns[507:1006], VaR = VaR99_n_roll22)


sum(as.numeric(sp500_logreturns[507:1006] < VaR90_n_roll23))
VaRTest(conf.level = 0.95, alpha = 0.10, actual = sp500_logreturns[507:1006], VaR = VaR90_n_roll23)
VaRTest(conf.level = 0.99, alpha = 0.10, actual = sp500_logreturns[507:1006], VaR = VaR90_n_roll23)

sum(as.numeric(sp500_logreturns[507:1006] < VaR95_n_roll23))
VaRTest(conf.level = 0.95, alpha = 0.05, actual = sp500_logreturns[507:1006], VaR = VaR95_n_roll23)
VaRTest(conf.level = 0.99, alpha = 0.05, actual = sp500_logreturns[507:1006], VaR = VaR95_n_roll23)

sum(as.numeric(sp500_logreturns[507:1006] < VaR99_n_roll23))
VaRTest(conf.level = 0.95, alpha = 0.01, actual = sp500_logreturns[507:1006], VaR = VaR99_n_roll23)
VaRTest(conf.level = 0.99, alpha = 0.01, actual = sp500_logreturns[507:1006], VaR = VaR99_n_roll23)

#DOWNCROSSINGS FOR UNCONDITIONAL VAR FORECAST
sum(as.numeric(sp500_logreturns[507:1006]) < quantile(sp500_logreturns, p = 0.10))
sum(as.numeric(sp500_logreturns[507:1006]) < quantile(sp500_logreturns, p = 0.05))
sum(as.numeric(sp500_logreturns[507:1006]) < quantile(sp500_logreturns, p = 0.01))
####################


##########TIMEVARYING GARCH PARAMETERS##########
model.spec_time = ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1,1)),
                        mean.model = list(armaOrder = c(0,0)),
                        distribution.model = "norm")

model.fit_time = ugarchfit(spec = model.spec_time, data = sp500_logreturns[1:586], solver = 'hybrid')

#DEFINING GARCH-PARAMETERS
alpha_0_time <- c()
alpha_0_time[1] <- ugarchfit(spec = model.spec_time, data = sp500_logreturns[1:506], solver = 'hybrid')@fit$matcoef[2]
l <- 506
for(i in 1:26){
  alpha_0_time[i] = ugarchfit(spec = model.spec_time, data = sp500_logreturns[1:l], solver = 'hybrid')@fit$matcoef[2]
  l <- l+20
}

alpha_1_time <- c()
alpha_1_time[1] <- ugarchfit(spec = model.spec_time, data = sp500_logreturns[1:506], solver = 'hybrid')@fit$matcoef[3]
l <- 506
for(i in 1:26){
  alpha_1_time[i] = ugarchfit(spec = model.spec_time, data = sp500_logreturns[1:l], solver = 'hybrid')@fit$matcoef[3]
  l <- l+20
}

beta_1_time <- c()
beta_1_time[1] <- ugarchfit(spec = model.spec_time, data = sp500_logreturns[1:506], solver = 'hybrid')@fit$matcoef[4]
l <- 506
for(i in 1:26){
  beta_1_time[i] = ugarchfit(spec = model.spec_time, data = sp500_logreturns[1:l], solver = 'hybrid')@fit$matcoef[4]
  l <- l+20
}

phi_1_time <- alpha_1_time+beta_1_time

index_window <- c(506,526,546,566,586,606,626,646,666,686,706,726,746,766,786,806,826,846,866,886,906,926,946,966,986,1006)

dataframe_alpha_1_time <- data.frame(index = index_window, values = alpha_1_time)
dataframe_alpha_1_time_melt <- melt(dataframe_alpha_1_time, id = c('index'))

dataframe_beta_1_time <- data.frame(index = index_window, values = beta_1_time)
dataframe_beta_1_time_melt <- melt(dataframe_beta_1_time, id = c('index'))

dataframe_phi_1_time <- data.frame(index = index_window, values = phi_1_time)
dataframe_phi_1_time_melt <- melt(dataframe_phi_1_time, id = c('index'))

#PLOT OF GARCH-PARAMETERS
pparameters <- (ggplot(data = dataframe_alpha_1_time_melt, 
                       aes(x = index_window, y = value))
                + geom_line(aes(linetype = "alpha_1"))
                + geom_line(data = dataframe_beta_1_time_melt,
                            aes(x = index_window, y = value, linetype = "beta_1"))
                + geom_line(data = dataframe_phi_1_time_melt,
                            aes(x = index_window, y = value, linetype = "alpha_1+beta_1"))
                + scale_colour_discrete(guide = FALSE)
                + scale_linetype_manual(name = "Parameter values",
                                        values = c("alpha_1" = "dotted",
                                                   "beta_1" = "dashed",
                                                   "alpha_1+beta_1" = "solid"),
                                        labels = c(expression(alpha[1]), 
                                                   expression(alpha[1] + beta[1]),
                                                   expression(beta[1])))
                + theme(legend.text.align = 0)
                + xlab("Index") 
                + xlim(506,1006)
                + ylab("Value"))

pparameteralpha <- (ggplot(data = dataframe_alpha_1_time_melt, 
                           aes(x = index_window, y = value))
                    + geom_line(linetype = "dotted")
                    + coord_cartesian(ylim = c(0.00,0.4), xlim = c(506,1006))
                    + ylab("Value"))

pparameterbeta <- (ggplot(data = dataframe_beta_1_time_melt, 
                          aes(x = index_window, y = value))
                   + geom_line(linetype = "dashed")
                   + labs(x = "Index", y = element_blank()) 
                   + coord_cartesian(ylim = c(0.60,1.00), xlim = c(506,1006)))

pparameterphi <- (ggplot(data = dataframe_phi_1_time_melt, 
                         aes(x = index_window, y = value))
                  + geom_line(linetype = "solid")
                  + labs(x = "Index", y = element_blank()) 
                  + coord_cartesian(ylim = c(0.60,1.00), xlim = c(506,1006)))

grobpara <- arrangeGrob(pparameteralpha, pparameterbeta, pparameterphi, widths = c(0.33,0.33,0.33))
grid.arrange(pparameters, grobpara, nrow = 2, ncol = 1, top = textGrob("GARCH-parameters",gp=gpar(fontsize=14)))
####################


##########COMPARISON BETWEEN IN SAMPLE AND OUT OF SAMPLE FORECAST##########

#NORMAL DISTRIBUTION ASSUMPTION
pvarcomp90_1 <- (qplot(y = VaR90_n_roll1, x = 507:1006, geom = 'line')
                 + geom_point(aes(x = 507:1006, y = sp500_logreturns[507:1006]), size = 0.8)
                 + geom_line(color = 'green')
                 + geom_line(aes(y = model.fit@fit$sigma[507:1006]*(qnorm(p=0.10)), x = 507:1006), 
                             color = 'black')
                 + coord_cartesian(ylim = c(-0.07,0.00))                     
                 + labs(y = expression(atop(bold('Model 1'), 'Log returns')), x = element_blank())
                 + ggtitle("\u03B1 = 0.10")
                 + theme(plot.title = element_text(hjust = 0.5, size = 12)))


pvarcomp90_2 <- (qplot(y = VaR90_n_roll2, x = 507:1006, geom = 'line')
                 + geom_point(aes(x = 507:1006, y = sp500_logreturns[507:1006]), size = 0.8)
                 + geom_line(color = 'green')
                 + geom_line(aes(y = model.fit@fit$sigma[507:1006]*(qnorm(p=0.10)), x = 507:1006), 
                             color = 'black')
                 + coord_cartesian(ylim = c(-0.07,0.00))                     
                 + labs(y = expression(atop(bold('Model 2'), 'Log returns')), x = element_blank())
                 + theme(plot.title = element_text(hjust = 0.5)))

pvarcomp90_3 <- (qplot(y = VaR90_n_roll3, x = 507:1006, geom = 'line')
                 + geom_point(aes(x = 507:1006, y = sp500_logreturns[507:1006]), size = 0.8)
                 + geom_line(color = 'green')
                 + geom_line(aes(y = model.fit@fit$sigma[507:1006]*(qnorm(p=0.10)), x = 507:1006), 
                             color = 'black')
                 + coord_cartesian(ylim = c(-0.07,0.00))                     
                 + labs(y = expression(atop(bold('Model 3'), 'Log returns')), x = 'Test observations')
                 + theme(plot.title = element_text(hjust = 0.5)))

pvarcomp95_1 <- (qplot(y = VaR95_n_roll1, x = 507:1006, geom = 'line')
                 + geom_point(aes(x = 507:1006, y = sp500_logreturns[507:1006]), size = 0.8)
                 + geom_line(color = 'red')
                 + geom_line(aes(y = model.fit@fit$sigma[507:1006]*(qnorm(p=0.05)), x = 507:1006), 
                             color = 'black')
                 + coord_cartesian(ylim = c(-0.07,0.00))                     
                 + labs(y = element_blank(), x = element_blank())
                 + ggtitle("\u03B1 = 0.05")
                 + theme(plot.title = element_text(hjust = 0.5, size = 12)))

pvarcomp95_2 <- (qplot(y = VaR95_n_roll2, x = 507:1006, geom = 'line')
                 + geom_point(aes(x = 507:1006, y = sp500_logreturns[507:1006]), size = 0.8)
                 + geom_line(color = 'red')
                 + geom_line(aes(y = model.fit@fit$sigma[507:1006]*(qnorm(p=0.05)), x = 507:1006), 
                             color = 'black')
                 + coord_cartesian(ylim = c(-0.07,0.00))
                 + labs(y = element_blank(), x = element_blank())
                 + theme(plot.title = element_text(hjust = 0.5)))

pvarcomp95_3 <- (qplot(y = VaR95_n_roll3, x = 507:1006, geom = 'line')
                 + geom_point(aes(x = 507:1006, y = sp500_logreturns[507:1006]), size = 0.8)
                 + geom_line(color = 'red')
                 + geom_line(aes(y = model.fit@fit$sigma[507:1006]*(qnorm(p=0.05)), x = 507:1006), 
                             color = 'black')
                 + coord_cartesian(ylim = c(-0.07,0.00))
                 + labs(y = element_blank(), x = 'Test observations')
                 + theme(plot.title = element_text(hjust = 0.5)))

pvarcomp99_1 <- (qplot(y = VaR99_n_roll1, x = 507:1006, geom = 'line')
                 + geom_point(aes(x = 507:1006, y = sp500_logreturns[507:1006]), size = 0.8)
                 + geom_line(color = 'blue')
                 + geom_line(aes(y = model.fit@fit$sigma[507:1006]*(qnorm(p=0.01)), x = 507:1006), 
                             color = 'black')
                 + coord_cartesian(ylim = c(-0.07,0.00))                     
                 + labs(y = element_blank(), x = element_blank())
                 + ggtitle("\u03B1 = 0.01")
                 + theme(plot.title = element_text(hjust = 0.5, size = 12)))

pvarcomp99_2 <- (qplot(y = VaR99_n_roll2, x = 507:1006, geom = 'line')
                 + geom_point(aes(x = 507:1006, y = sp500_logreturns[507:1006]), size = 0.8)
                 + geom_line(color = 'blue')
                 + geom_line(aes(y = model.fit@fit$sigma[507:1006]*(qnorm(p=0.01)), x = 507:1006), 
                             color = 'black')
                 + coord_cartesian(ylim = c(-0.07,0.00))
                 + labs(y = element_blank(), x = element_blank())
                 + theme(plot.title = element_text(hjust = 0.5)))

pvarcomp99_3 <- (qplot(y = VaR99_n_roll3, x = 507:1006, geom = 'line')
                 + geom_point(aes(x = 507:1006, y = sp500_logreturns[507:1006]), size = 0.8)
                 + geom_line(color = 'blue')
                 + geom_line(aes(y = model.fit@fit$sigma[507:1006]*(qnorm(p=0.01)), x = 507:1006), 
                             color = 'black')
                 + coord_cartesian(ylim = c(-0.07,0.00))
                 + labs(y = element_blank(), x = 'Test observations')
                 + theme(plot.title = element_text(hjust = 0.5)))

grobcomp90 <- arrangeGrob(pvarcomp90_1, pvarcomp90_2, pvarcomp90_3)
grobcomp95 <- arrangeGrob(pvarcomp95_1, pvarcomp95_2, pvarcomp95_3)
grobcomp99 <- arrangeGrob(pvarcomp99_1, pvarcomp99_2, pvarcomp99_3)
grid.arrange((arrangeGrob(grobcomp90, grobcomp95, grobcomp99, widths = c(0.33,0.33,0.33))))


#T DISTRIBUTION ASSUMPTION
pvarcomp90_11 <- (qplot(y = VaR90_n_roll21, x = 507:1006, geom = 'line')
                  + geom_point(aes(x = 507:1006, y = sp500_logreturns[507:1006]), size = 0.8)
                  + geom_line(color = 'green')
                  + geom_line(aes(y = model.fit@fit$sigma[507:1006]*(qstd(p = 0.10, mean = 0, sd = 1, nu = 4)), x = 507:1006), 
                              color = 'black')
                  + coord_cartesian(ylim = c(-0.071,0.00))                     
                  + labs(y = expression(atop(bold('Model 1'), 'Log returns')), x = element_blank())
                  + ggtitle("\u03B1 = 0.10")
                  + theme(plot.title = element_text(hjust = 0.5, size = 12)))

pvarcomp90_12 <- (qplot(y = VaR90_n_roll22, x = 507:1006, geom = 'line')
                  + geom_point(aes(x = 507:1006, y = sp500_logreturns[507:1006]), size = 0.8)
                  + geom_line(color = 'green')
                  + geom_line(aes(y = model.fit@fit$sigma[507:1006]*(qstd(p = 0.10, mean = 0, sd = 1, nu = 4)), x = 507:1006), 
                              color = 'black')
                  + coord_cartesian(ylim = c(-0.071,0.00))                     
                  + labs(y = expression(atop(bold('Model 2'), 'Log returns')), x = element_blank())
                  + theme(plot.title = element_text(hjust = 0.5)))

pvarcomp90_13 <- (qplot(y = VaR90_n_roll23, x = 507:1006, geom = 'line')
                  + geom_point(aes(x = 507:1006, y = sp500_logreturns[507:1006]), size = 0.8)
                  + geom_line(color = 'green')
                  + geom_line(aes(y = model.fit@fit$sigma[507:1006]*(qstd(p = 0.10, mean = 0, sd = 1, nu = 4)), x = 507:1006), 
                              color = 'black')
                  + coord_cartesian(ylim = c(-0.071,0.00))                     
                  + labs(y = expression(atop(bold('Model 3'), 'Log returns')), x = 'Test observations')
                  + theme(plot.title = element_text(hjust = 0.5)))

pvarcomp95_11 <- (qplot(y = VaR95_n_roll21, x = 507:1006, geom = 'line')
                  + geom_point(aes(x = 507:1006, y = sp500_logreturns[507:1006]), size = 0.8)
                  + geom_line(color = 'red')
                  + geom_line(aes(y = model.fit@fit$sigma[507:1006]*(qstd(p = 0.05, mean = 0, sd = 1, nu = 4)), x = 507:1006), 
                              color = 'black')
                  + coord_cartesian(ylim = c(-0.071,0.00))                     
                  + labs(y = element_blank(), x = element_blank())
                  + ggtitle("\u03B1 = 0.05")
                  + theme(plot.title = element_text(hjust = 0.5, size = 12)))

pvarcomp95_12 <- (qplot(y = VaR95_n_roll22, x = 507:1006, geom = 'line')
                  + geom_point(aes(x = 507:1006, y = sp500_logreturns[507:1006]), size = 0.8)
                  + geom_line(color = 'red')
                  + geom_line(aes(y = model.fit@fit$sigma[507:1006]*(qstd(p = 0.05, mean = 0, sd = 1, nu = 4)), x = 507:1006), 
                              color = 'black')
                  + coord_cartesian(ylim = c(-0.071,0.00))
                  + labs(y = element_blank(), x = element_blank())
                  + theme(plot.title = element_text(hjust = 0.5)))

pvarcomp95_13 <- (qplot(y = VaR95_n_roll23, x = 507:1006, geom = 'line')
                  + geom_point(aes(x = 507:1006, y = sp500_logreturns[507:1006]), size = 0.8)
                  + geom_line(color = 'red')
                  + geom_line(aes(y = model.fit@fit$sigma[507:1006]*(qstd(p = 0.05, mean = 0, sd = 1, nu = 4)), x = 507:1006), 
                              color = 'black')
                  + coord_cartesian(ylim = c(-0.071,0.00))
                  + labs(y = element_blank(), x = 'Test observations')
                  + theme(plot.title = element_text(hjust = 0.5)))

pvarcomp99_11 <- (qplot(y = VaR99_n_roll21, x = 507:1006, geom = 'line')
                  + geom_point(aes(x = 507:1006, y = sp500_logreturns[507:1006]), size = 0.8)
                  + geom_line(color = 'blue')
                  + geom_line(aes(y = model.fit@fit$sigma[507:1006]*(qstd(p = 0.01, mean = 0, sd = 1, nu = 4)), x = 507:1006), 
                              color = 'black')
                  + coord_cartesian(ylim = c(-0.071,0.00))                     
                  + labs(y = element_blank(), x = element_blank())
                  + ggtitle("\u03B1 = 0.01")
                  + theme(plot.title = element_text(hjust = 0.5, size = 12)))

pvarcomp99_12 <- (qplot(y = VaR99_n_roll22, x = 507:1006, geom = 'line')
                  + geom_point(aes(x = 507:1006, y = sp500_logreturns[507:1006]), size = 0.8)
                  + geom_line(color = 'blue')
                  + geom_line(aes(y = model.fit@fit$sigma[507:1006]*(qstd(p = 0.01, mean = 0, sd = 1, nu = 4)), x = 507:1006), 
                              color = 'black')
                  + coord_cartesian(ylim = c(-0.071,0.00))
                  + labs(y = element_blank(), x = element_blank())
                  + theme(plot.title = element_text(hjust = 0.5)))

pvarcomp99_13 <- (qplot(y = VaR99_n_roll23, x = 507:1006, geom = 'line')
                  + geom_point(aes(x = 507:1006, y = sp500_logreturns[507:1006]), size = 0.8)
                  + geom_line(color = 'blue')
                  + geom_line(aes(y = model.fit@fit$sigma[507:1006]*(qstd(p = 0.01, mean = 0, sd = 1, nu = 4)), x = 507:1006), 
                              color = 'black')
                  + coord_cartesian(ylim = c(-0.071,0.00))
                  + labs(y = element_blank(), x = 'Test observations')
                  + theme(plot.title = element_text(hjust = 0.5)))

grobcomp902 <- arrangeGrob(pvarcomp90_11, pvarcomp90_12, pvarcomp90_13)
grobcomp952 <- arrangeGrob(pvarcomp95_11, pvarcomp95_12, pvarcomp95_13)
grobcomp992 <- arrangeGrob(pvarcomp99_11, pvarcomp99_12, pvarcomp99_13)
grid.arrange((arrangeGrob(grobcomp902, grobcomp952, grobcomp992, widths = c(0.33,0.33,0.33))))

#DOWNCROSSINGS OF IN-SAMPLE VAR-BANDS
sum(as.numeric(sp500_logreturns)[507:1006] < model.fit@fit$sigma[507:1006]*(qnorm(p = 0.10)))
sum(as.numeric(sp500_logreturns)[507:1006] < model.fit@fit$sigma[507:1006]*(qnorm(p = 0.05)))
sum(as.numeric(sp500_logreturns)[507:1006] < model.fit@fit$sigma[507:1006]*(qnorm(p = 0.01)))

sum(as.numeric(sp500_logreturns)[507:1006] < model.fit@fit$sigma[507:1006]*(qstd(p = 0.10, mean = 0, sd = 1, nu = 4)))
sum(as.numeric(sp500_logreturns)[507:1006] < model.fit@fit$sigma[507:1006]*(qstd(p = 0.05, mean = 0, sd = 1, nu = 4)))
sum(as.numeric(sp500_logreturns)[507:1006] < model.fit@fit$sigma[507:1006]*(qstd(p = 0.01, mean = 0, sd = 1, nu = 4)))
####################
