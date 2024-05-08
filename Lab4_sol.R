###  Lista 3 


# Zadanie 2 ---------------------------------------------------------------

# a)

N <- 5000
Y <- rcauchy(N)

mediana <- c()
srednia <- c()
for(i in 1:N) {
  mediana[i] <- median(Y[1:i])
  srednia[i] <- mean(Y[1:i])
}

plot(1:N, srednia, type = "l", col = 1)
lines(1:N, mediana, type = "l", col = 2)
abline(h = 0, col = 3)
#legend("topleft", c("mean","median", "a = 0"), lty = 1, col = 1:3, text.col = 1)

# b)

N <- 500
Y <- rcauchy(N)

o_st <- c()
o_cw <- c()
for(i in 2:N){
  o_st[i-1] <- sd(Y[1:i])
  o_cw[i-1] <- IQR(Y[1:i])/2
}
IQR(12)
plot(2:N, o_st, type = "l", log = 'y', ylim = c(0.5, max(o_st)+1), col = 1)
lines(2:N, o_cw, lty = 1, col = 2)
abline(h = 1, col = 3)
#legend("topleft", c("sd","SQR", "b = 1"), lty = 1, col = 1:3, text.col = 1)

# odchylenie standardowe podobnie jak srednia nie istnieje
# odchylenie cwiartkowe SQR -> dobry estymator to prosta y = 1

# Zadanie 3 ---------------------------------------------------------------

# a)

N <- 5000
Y <- rnorm(N)

srednia <- cumsum(Y)/1:N
plot(1:N, srednia, type = "l")

mediana <- c()

for(i in 1:N){
  mediana[i] <- median(Y[1:i])
}

lines(1:N, mediana, lty = 1, col = 2)
abline(h = 0, col = 3)
#legend("bottomright", c("mean", "median", "mu = 0"), lty = 1, col = 1:3)
# w rnorm mamy estymatory dla sredniej i odchylenia -> one istnieja i sa rowne 0

# b)

N <- 5000
Y <- rnorm(N)

o_st <- c()
r <- c()

for(i in 2:N){
  o_st[i-1] <- sd(Y[1:i])
  r[i-1] <- IQR(Y[1:i])/1.35
}

plot(2:N, o_st, type = "l", log = 'y', ylim = c(0.5, max(o_st)+1))
lines(2:N, r, lty = 1, col = 2)
abline(h = 1, col = 3)
#legend("topleft", c("sd", "IQR/1.35", "sigma = 1"))
# tutaj again dobry estymator to prosta y = 1

# Zadanie 4 ---------------------------------------------------------------

n <- 10000
theta <- 1

theta_M <- c()
theta_NW <- c()

# generujemy 10000 20-elementowych probek z rozk jednostajnego jednostkowego
for (i in 1:n){
  proba <- runif(20, 0, theta) # pojedyncza dwudziesto elementowa probka
  # liczymy teraz dla kazdej probki estymatory momentow i NWW
  theta_M[i] <- 2*mean(proba)
  theta_NW[i] <- max(proba)
}
# obciazenie
b_M <- mean(theta_M)-theta
b_NW <- mean(theta_NW)-theta
b_M
b_NW

# blad prognozy
mse_M <- sum((theta_M-theta)^2)/n
# lub: var(theta_M)+b_M^2
mse_NW <- sum((theta_NW-theta)^2)/n
# lub: var(theta_NW)+b_NW^2
mse_M
mse_NW

# Zadanie 5 -----------------  TRUDNE !!!!  ----------------------------

# dla kazdej probki przedzial ufonsci dla wartosci oczekiwanej na poziomie ufnosci 0.95
N <- 10000
n <- 10
alpha = 0.05 
kw = qt(1-alpha/2,n-1) # kwantyl t(1-alpha/2)_[n-1]



m = 10000 # liczba probek
n = 10 # licznosci probek
alpha = 0.05 
kw = qt(1-alpha/2,n-1) # kwantyl t(1-alpha/2)_[n-1]
kw
#generujemy probki z rozk?adu normalnego N(0,1) czyli wartosc oczekiwana to ? = 0

czy.wpada <-  replicate(m, 
                      { 
                        x <- rnorm(n) 
                        sr <- mean(x) 
                        odch <- sd(x) 
                        (sr-kw*odch/sqrt(n) < 0) & (sr+kw*odch/sqrt(n) > 0) #spr.czy mi = 0 wpad?o do prz.ufn.
                      })

czy.wpada[1:10] # pierwszych 10 wynik?w  
?replicate
sum(czy.wpada)/m #frakcja pokryc ? = 0 przez prz.ufn.
prz.ufn = replicate(m, 
                    { 
                      x =rnorm(n) 
                      sr = mean(x) 
                      odch = sd(x) 
                      c(sr-kw*odch/sqrt(n), sr+kw*odch/sqrt(n)) 
                    })

lewy=prz.ufn[1,] 
prawy=prz.ufn[2,] 
plot(c(lewy[1],prawy[1]),c(1,1),type="l", xlim=c(-4,4),ylim=c(1,100),xlab=" " , ylab=" ")
abline(v=0, col = 2)
for (i in 2:100) # dla i=1,2,...,m
{ 
  lines(c(lewy[i],prawy[i]),c(i,i))
}


# Zadanie 5 ---------------------------------------------------------------

N <- 10000
n <- 10
mi <- 9
sigma <- 3
alfa <- 0.05
q <- qt(1 - alfa / 2, n - 1)
k <- 0
for(i in 1:N){
  x <- rnorm(n, mi, sigma)
  m <- mean(x)
  s <- sd(x)
  if( mi >= m - q * s / sqrt(n) & mi <= m + q * s / sqrt(n) ) k <- k + 1
}
paste0(k/N*100, "%")
k/N
##
m = 10000 # liczba probek
n = 10 # licznosci probek
alpha = 0.05 
kw = qt(1-alpha/2,n-1) # kwantyl t(1-alpha/2)_[n-1]
prz.ufn = replicate(m, 
                    { 
                      x =rnorm(n, mi, sigma) 
                      sr = mean(x) 
                      odch = sd(x) 
                      c(sr-kw*odch/sqrt(n), sr+kw*odch/sqrt(n)) 
                    })

lewy=prz.ufn[1,] 
prawy=prz.ufn[2,] 
plot(c(lewy[1],prawy[1]),c(1,1),type="l", xlim=c(-1,14),ylim=c(1,100),xlab=" " , ylab=" ")
abline(v=mi, col = 2)
for (i in 2:100) # dla i=1,2,...,m
{ 
  lines(c(lewy[i],prawy[i]),c(i,i))
}

# Zadanie 6 ---------------------------------------------------------------

n <- 50
miu <- 28.4
sigma <- 4.75
alfa <- 0.05
q <- qnorm(1 - alfa / 2)
c(miu - q * sigma / sqrt(n), miu + q * sigma / sqrt(n))


# Zadanie 7 ---------------------------------------------------------------

temp <- scan(nlines = 3)
330.0 322.0 345.0 328.6 331.0 342.0
342.4 340.4 329.7 334.0 326.5 325.8
337.5 327.3 322.6 341.0 340.0 333.0
temp

prz.ufn.mi <- function(x, alfa){
  n <- length(x)
  m <- mean(x)
  s <- sd(x)
  q <- qt(1 - alfa / 2, n - 1)
  prz <- m + c(-1, 1) * s * q / sqrt(n)
  prz
}
prz.ufn.mi(temp, 0.05)

# teraz mozna pokazac gotowa funkcja w R
t.test(temp, conf.level = 0.95)
t.test(temp, conf.level = 0.95)$conf

prz.ufn.war <- function(x, alfa) {
  n <- length(x)
  q1 <- qchisq(1 - alfa / 2, n - 1)
  q2 <- qchisq(alfa / 2, n - 1)
  prz <- (n - 1) * var(x) / c(q1, q2)
  prz
}
prz.ufn.odch <- function(x, alfa) {
  n <- length(x)
  q1 <- qchisq(1 - alfa / 2, n - 1)
  q2 <- qchisq(alfa / 2, n - 1)
  prz <- sqrt((n - 1) * var(x) / c(q1, q2))
  prz
}
prz.ufn.war(temp, 0.05)
prz.ufn.odch(temp, 0.05)
# teraz mozna pokazac gotowa funkcja w R
#install.packages("TeachingDemos")
library(TeachingDemos)
sigma.test(temp)$conf.int

# przedzia? dla odch. standardowego
sqrt(sigma.test(temp)$conf.int)


# Zadanie 8 ---------------------------------------------------------------

k <- 578
n <- 1014

?binom.test
binom.test(k, n, conf.level = 0.95)$conf
?prop.test
prop.test(k,n,conf.level=0.95)$conf.int # przybli?ony

# moze recznie?
p_dach <- k/n
c(p_dach + c(-1,1)*qnorm(0.975)*sqrt((p_dach*(1-p_dach))/n))

# Zadanie 9 ---------------------------------------------------------------

k <-  3
n <-  12
binom.test(k, n, conf.level = 0.95)$conf


# Zadanie 10 --------------------------------------------------------------

iris
vir <- iris$Petal.Length[iris$Species == "virginica"]

prz.ufn.mi(vir, 0.01)
t.test(vir, conf.level = 0.99)$conf
prz.ufn.war(vir, 0.05)
sigma.test(vir, conf.level = 0.95)$conf.int

# Zadanie 11 --------------------------------------------------------------

k <- 19
n <- 150
binom.test(k, n, conf.level = 0.96)$conf


# Zadanie 12 --------------------------------------------------------------

x <- chickwts$weight[chickwts$feed == "soybean"]
prz.ufn.war(x, 0.07)


# Zadanie 13 --------------------------------------------------------------

t.test(faithful$waiting, conf.level=0.99)$conf

prz.ufn.mi(faithful$waiting, 0.01)


# Zadanie 14 --------------------------------------------------------------

prz.ufn.odch(Orange$circumference, 0.01)
sqrt(sigma.test(Orange$circumference, conf.level = 0.99)$conf.int)

# Zadanie 15 --------------------------------------------------------------

library(MASS)
Pima
k <- length(Pima.te$age[Pima.te$type=="Yes"])
n <- length(Pima.te$age)

binom.test(k,n,conf.level = 0.95)$conf


k <- length(Pima.te$age[Pima.te$type=="Yes" & Pima.te$age >= 35])
n <- length(Pima.te$age[Pima.te$age >= 35])

binom.test(k,n,conf.level = 0.95)$conf





