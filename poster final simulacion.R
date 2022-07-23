require(gamlss)
require(MCMCpack)

beta0 <- 2627
beta1 <- -37
sig2 <- 9244
sig   <- sig2^0.5

# Function to generate one dataset
gendat <- function(n) {
  x <- runif(n=n, min=0, max=26)
  media <- beta0 + beta1 * x
  y <- rnorm(n=n, mean=media, sd=sig)
  data.frame(y=y, x=x)
}

  f<-gendat(20)
  y<-f$y
  x<-f$x
  # sum(y)
  # sum(x)


one.simul <- function(n) {
  dat <- gendat(n=n)
  modlm <- lm(y ~ x )
  
  modbay <- MCMCregress(y ~ x,c0=7.5,d0=0.5)
  bayes<-summary(modbay)
  b2<-bayes$statistics
  round( c(coef(modlm), desvi=summary(modlm)$sigma,b2[,1]),2)
  


}

#one.simul(10)

# Function to simulate nsim times

simul <- function(nsim, n) {
  results <- replicate(n=nsim, one.simul(n=n))
  results <- cbind(t(results), n=n)
  write(x=t(results),
        file='G:/semestre l 2022/simulacion/results.txt',
        ncolumns=7, append=T)
}


# To simulate automatically -----------------------------------------------

N <- seq(from=10, to=50, by=10)
nsim <-100

lapply(1:length(N), function(i) {
  cat(i, " ")
  simul(nsim=nsim, n=N[i])
})


# se cargan los datos simulados----------------------------------------------

datos <- read.table('G:/semestre l 2022/simulacion/results.txt')
colnames(datos) <- c('Intercept', 'Slope_lm', 'Sigma_lm','Intercept_bay','Slope_bayes',
                     'Sigma_bayes', 'n')


require(tidyverse)

# separacion de datos por grupos 

dat <- datos %>% group_by(n) %>% 
  summarise(intercept = mean(Intercept),
            slope_lm = mean(Slope_lm ),
            sigma_lm = mean(Sigma_lm),
            intercept_bay = mean(Intercept_bay),
            slope_bayes = mean(Slope_bayes),
            sigma_bayes = mean(Sigma_bayes))

dat

# Gráficos de comparación -----------------------------------------------------





betacero <- ggplot(dat)+geom_line(aes(x=n,y=intercept,colour="lm"),size=1.2)+
  geom_line(aes (x=n,y=intercept_bay,colour="bayes"),size=1.2)+
  geom_hline(yintercept=beta0,size=1)+
  labs(title = "Comparación de (Beta_0 lm) vs (Beta_0 Bayes)",
       subtitle = "beta 0 esperado = 2627.82",
        y="Beta 0",x="Numero de Datos",
       color="Modelos")

betauno <- ggplot(dat) + geom_line(aes(x=n,y=slope_lm,colour="lm"),size=1.2)+
  geom_line(aes(x=n,y=slope_bayes,colour="bayes"),size=1) +
  geom_hline(yintercept = beta1,size=1) +
  labs(title = "Comparación de (Beta 1 lm) vs (Beta 1 Bayes)",
       subtitle = "beta 1 esperado = - 37.15",
       y="Beta 1",x="Numero de Datos",
       color="Modelos")


sigma <- ggplot(dat)+geom_line(aes(x=n,y=sigma_lm,colour="lm"),size=1)+
  geom_line(aes(x=n,y=sigma_bayes^0.5,colour="bayes"),size=1)+
  geom_hline(yintercept = sig,size=1)+
  labs(title = "Comparación de (sigma lm) vs (sigma Bayes)",
       subtitle = "Sigma esperado = 96.149",
       y="Sigma",x="Numero de Datos",
       color="Modelos")




hisdat<-ggplot()+geom_histogram(aes(x=dat$intercept),col=2,fill="red",alpha=0.2)+
  geom_histogram(aes(x=dat$intercept_bay),col=3,fill="green",alpha=0.2)+
  geom_density(aes(x=dat$intercept_bay),col="red",size=1)+
  geom_density(aes(x=dat$intercept),size=1)


max(dat$intercept)

hist((dat$intercept))
       
       par(mfrow=c(1, 2))
with(dat, plot(x=n, y=intercept, type='l', las=1, main=expression(hat(beta[0]))))
abline(h=beta0, col='dodgerblue2', lty='dashed')

with(dat, plot(x=n, y=intercept_bay, type='l', las=1, 
               main=expression(hat(beta[0]))))
abline(h=beta0, col='dodgerblue2', lty='dashed')

with(dat, plot(x=n, y=slope_lm, type='l', las=1, main=expression(hat(beta[1]))))
abline(h=beta1, col='dodgerblue2', lty='dashed')

with(dat, plot(x=n, y=slope_bayes, type='l', las=1, main=expression(hat(beta[1]))))
abline(h=beta1, col='dodgerblue2', lty='dashed')

with(dat, plot(x=n, y=sigma_lm, type='l', las=1, main=expression(hat(sigma))))
abline(h=sig, col='dodgerblue2', lty='dashed')

with(dat, plot(x=n, y=sigma_bayes^0.5, type='l', las=1, main=expression(hat(sigma))))
abline(h=sig, col='dodgerblue2', lty='dashed')


