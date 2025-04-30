


# Zadanie 4 ---------------------------------------------------------------

butelki <- read.csv("http://pages.mini.pw.edu.pl/~grzegorzewskip/www/?download=butelki.csv")
butelki$strength

# a) 
cisnienie <- butelki$strength * 0.0068947
butelki$cisnienie=butelki$strength * 0.0068947

# b) 
hist(cisnienie)
hist(cisnienie, breaks = 6)
hist(cisnienie, breaks = 14)
hist(cisnienie,prob=T) # sumujemy do 1
#Nie rysuj liczby obserwacji w słupkach, tylko ich gęstość prawdopodobieństwa – czyli tak,
#żeby obszar pod histogramem sumował się do 1, a nie same wysokości słupków.

# c)
h <- hist(cisnienie, breaks = 14)
h$mids #srodki klas
h$counts #licznosci klas
lines(h$mids, h$counts, col = 3, lwd = 3)

# d)
cisnienie
stem(cisnienie)
stem(cisnienie, 2)

# e)

boxplot(cisnienie)
summary(cisnienie)
# Q_3+1.5*IQR
# Q_1-1.5*IQR
# askraje to 3 zamiast 1.5


# f)

var(cisnienie)
sd(cisnienie)
IQR(cisnienie)
diff(range(cisnienie)) 
# rozstępu Największa wartość - najmniejsza wartość

#install.packages("moments")
library(moments)
skewness(cisnienie)
kurtosis(cisnienie)

cv=sd(cisnienie)/abs(mean(cisnienie))
cv

# g)

quantile(cisnienie, 0.05)
quantile(cisnienie, 0.1)
quantile(cisnienie, 0.25)
quantile(cisnienie, 0.5)
quantile(cisnienie, 0.75)
quantile(cisnienie, 0.9)
quantile(cisnienie, 0.95)

# h)

mean(cisnienie, trim = 0.1) #ucinasz po 10% z każdego ogona
mean(cisnienie)
median(cisnienie)
?mean
lapply(0:49 / 100, function(x) {
  mean(cisnienie, trim = x)
}) -> trim_means

plot(1:length(trim_means), trim_means)



# Zadanie 6 ---------------------------------------------------------------

samochody <- read.csv2("http://pages.mini.pw.edu.pl/~grzegorzewskip/www/?download=samochody.csv")

samochody$zp <- 1 / samochody$mpg * 3.7851 / 1.609 * 100 

# b)
sort(samochody$zp)
max(samochody$zp, na.rm = T)
stem(samochody$zp)

# c)
hist(samochody$zp)

# d)

zp2 <- na.omit(samochody$zp)
hist(samochody$zp)

# wyklad 15 prof
# statystykanasprzenika

density(zp2, kernel = "epanechnikov")

plot(density(zp2, kernel = "epanechnikov"))
hist(samochody$zp, breaks = 37)
plot(density(zp2, kernel = "epanechnikov", bw = 0.1)) # wspolczynnik wygladzajacy


plot(density(zp2, kernel = "rectangular"))
plot(density(zp2, kernel = "triangular"))


# e)
boxplot(samochody$zp)

# f)
# trzeba usuwac na
mean(samochody$zp, na.rm = TRUE)
median(samochody$zp, na.rm = TRUE)
var(samochody$zp, na.rm = TRUE)
sd(samochody$zp, na.rm = TRUE)
diff(range(samochody$zp, na.rm = TRUE))
IQR(samochody$zp, na.rm = TRUE)

library(moments)
kurtosis(samochody$zp, na.rm = TRUE)
skewness(samochody$zp, na.rm = TRUE)

# g)
quantile(samochody$zp, c(0.05, 0.1, 0.9, 0.95), na.rm = TRUE)

# h)
mean(samochody$zp, na.rm = TRUE, trim = 0.05)


# Zadanie 7 ---------------------------------------------------------------

x <- samochody$zp

malo <- x[x <= 7]
malo
srednio <- x[x > 7 & x <= 10]
duzo <- x[x > 10]

kat <- c("malo", "srednio", "duzo")
licznosci <- c(length(malo), length(srednio), length(duzo))
licznosci
barplot(licznosci, names = kat)

100*licznosci/sum(licznosci)

# 2 sposob kodowania
x.kod <- cut(x,breaks=c(-Inf,7,10,Inf))
x.kod
head(x.kod, 10)
x.kod=cut(x,breaks=c(-Inf,7,10,Inf),labels=c("malo", "srednio", "duzo"))
head(x.kod, 10)
t=table(x.kod)
barplot(t)

prop.table(t)
barplot(prop.table(t))

# Zadanie 8 ---------------------------------------------------------------

boxplot(samochody$zp~samochody$producent,horizontal=T,names=c("A","E","J"))

zpA <- samochody$zp[samochody$producent == 1]
zpE <- samochody$zp[samochody$producent == 2]
zpJ <- samochody$zp[samochody$producent == 3]

boxplot(zpA, zpE, zpJ, horizontal = F, names = c("A", "E", "J"))

# Zadanie 9 ---------------------------------------------------------------

boxplot(samochody$zp)
boxplot(samochody$zp ~ samochody$cylindry)


# Zadanie 10 --------------------------------------------------------------

zp10 <- samochody$zp[samochody$waga < 2500]
mean(zp10)
median(zp10)
sd(zp10)

library(moments)
library(dplyr)

skewness(zp10)
samochody %>% 
  filter(waga < 2500) %>% 
  summarise(srednia = mean(zp),
            mediana = median(zp),
            sd = sd(zp),
            skewness = skewness(zp))


# Zadanie 11 --------------------------------------------------------------

# a)
df <- samochody[samochody$rok >= 79 & samochody$rok <= 81, 'moc']
summary(df)
b <- boxplot(df)
b$out

# b)
quantile(df, 0.95, na.rm = T)

# Zadanie 12 --------------------------------------------------------------

# a)
df <- samochody$przysp[samochody$waga > 2500 & samochody$waga < 3000]
boxplot(df)

# b)
quantile(df, 0.75)

# Zadanie 13 --------------------------------------------------------------

waga <- samochody$waga[samochody$mpg >= 26]

# a)

boxplot(waga)

# b)

quantile(waga, 0.95, na.rm = TRUE)


# Zadanie 14 --------------------------------------------------------------

boxplot(przysp~prod)

### ggplot ###

samochody %>% 
  filter(producent != 2) %>%
  ggplot(aes(x = producent, y = przysp, group = producent)) + 
  geom_boxplot()

###

# Lista 3


# Zadanie 1 ----------------------------------------------------------------

x1 <- rnorm(20)
hist(x)
x2 <- rnorm(200)
plot(ecdf(x1))
plot(ecdf(x2), add = T, col = 2)
curve(pnorm(x), add = T, lw = 3, col = 3)





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

# b)

N <- 500
Y <- rcauchy(N)

o_st <- c()
o_cw <- c()
for(i in 2:N){
  o_st[i-1] <- sd(Y[1:i])
  o_cw[i-1] <- IQR(Y[1:i])/2
}
plot(2:N, o_st, type = "l", log = 'y', ylim = c(0.5, max(o_st)+1), col = 1)
lines(2:N, o_cw, lty = 1, col = 2)
abline(h = 1, col = 3)








