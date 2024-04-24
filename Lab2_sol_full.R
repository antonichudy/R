
###       STATYSTYKA MATEMATYCZNA       ###
###           LABORATORIUM 2            ###
###           Rozwiazania               ###


# Zadanie 10 --------------------------------------------------------------

x <- 0:10
barplot(dbinom(x, 10, 0.5), ylim = c(0, 0.3), 
        names.arg = x)
barplot(dbinom(x, 10, 0.25), ylim = c(0, 0.3), names.arg = x)
x <- 0:50
barplot(dbinom(x, 50, 0.25), ylim = c(0, 0.15), 
        names.arg = x)
# Każda kolumna na wykresie reprezentuje prawdopodobieństwo
#uzyskania określonej liczby sukcesów (od 0 do 10) w ciągu 10 prób.

# Zadanie 11 --------------------------------------------------------------

# dgeom ozn. rozklad geometryczny zdefiniowany tak: P(Y=k)=p*(1-p)^k, k=0,1,...
#p=0.1
##k=0:100
#pk=dgeom(k,prob=p)
#plot(k,pk,type="h")

# a) jesli X=nr osoby ktora jako pierwsza weszla do sklepu to
# P(X=k)=P(Y=k-1), dla k=1,2,...
# bo musimy sobie przesun??, ?eby rozk?ad si? zgadza?

# ile prob z rzedu konczy sie porazka
dgeom(0, 0.1)
dgeom(1, 0.1)
dgeom(2, 0.1)
dgeom(3, 0.1)
1 - pgeom(10, 0.1)


# Zadanie 13 --------------------------------------------------------------

# a)
1 - pexp(1000, 0.0001)
1 - pexp(10000, 0.0001)
1 - pexp(30000, 0.0001)

# b)
qexp(0.1, 0.0001)




# Zadanie 14 (OMIJAM!)-----------------------------------------------------

# a) Y~exp(4)
lambda=4
k=0:100
pk=dpois(k,lambda)
plot(k,pk,type="h")

curve(dexp(x,lambda),0,10)
# b) 
#EY=1/4 VarY=1/16

# c)
pexp(0.5, 4)

# d)
1 - pexp(1, 4)
#dpois(0,a)


# Zadanie 15 --------------------------------------------------------------

# Generujemy probe losowa U1,V1,...,U_{n},V_{n} z rozkladu jednostajnego:
n<-10000
u<-runif(n)
v<-runif(n)

#Zaznaczmy te punkty na wykresie i nakladamy wykres funkcji y=x^2 dla 0<x<1. 
plot(u,v,xlim=c(0,1),ylim=c(0,1))
curve(x*x, col="red", type="l", xlim=c(0,1), add=T, lwd=3)

#Zliczamy teraz punkty z naszej probki, kt?re sa pod wykresem funkcji y=x2:
u
v
u<v*v
number <- sum(u<v*v) 
number
pole<-number/n
pole # przyblizenie numeryczne całki


#Niech X1,Y1,X2,Y3,... beda niezaleznymi zmiennymi losowymi 
#o rozkladzie jednostajnym U([0,1]). Dla funkcji borelowskiej 
#f:[0,1]?[0,1] definiujemy
#Z_{i}=I_{{f(X_{i})>Y_{i}}}. #(indykatory)
#Wowczas, z MPWL, mamy:
#(1/n)suma_i {Zi} -> ca?ka od 0 do 1 z f(x)dx  p.n.
#a)  Pole obszaru A to
#ca?ka_0^1{A}dxdy=ca?ka_0^1(ca?ka_0^1{x2}dy)dx=ca?ka_0^1{x2}dx=1/3. 


#b) (do analizy w domu)

#u = runif(n)
#u=runif(n,min=-1/sqrt(2),max=1/sqrt(2))
u=runif(n,min=-1/sqrt(2),max=1/sqrt(2))
v=runif(n)
plot(u,v,xlim=c(-1,1),ylim=c(0,1))
curve(x*x, col="red", type="l", xlim=c(-1,1), add=T, lwd=3)
curve(1 - x*x, col="blue", type="l", xlim=c(-1,1), add=T, lwd=3)
number<-sum(v>u^2 & v<1-u^2)
number
#pole <- number/n
pole<-sqrt(2)*number/(n)
pole


# Zdaanie 15 -------------------------------------------------------------------
# b) dla 0 < x < 1

# analitycznie
h =function(x) {1-2*x^2}
integrate(h, lower = 0, upper =1/sqrt(2))


# metoda Monte Carlo
n<-10000 #liczba punktów

# losujemy punkty
x<-runif(n, 0, 1)
y <-runif(n, 0, 1)
# wybieramy pkty z obszaru ograniczowego krzywymi
z <- y < 1 - x^2 & y > x^2
mean(z) #przybliżenie numeryczne całki

plot(x, y, xlim=c(0,1), ylim=c(0,1), pch = '.') # chmura punktów
curve(x^2, col='red', add=T)
curve(1-x^2, col='red', add=T)
points(x[z>0], y[z>0], col='green', pch = '.')




#### LISTA 2 ####################################################################


kobiety <- c(17364, 56128, 11239, 8170)
stan <- c("panny", "mężatki", "wdowy", "rozwódki")

# a)

pie(kobiety)
pie(kobiety, labels = stan)
pie(kobiety, paste(stan, kobiety, sep = "\n"), col = c(2, 3, 4, 6))

# b)

barplot(kobiety)
barplot(kobiety, names.arg = stan)


# Zadanie 2 ---------------------------------------------------------------

dane <- read.csv("http://pages.mini.pw.edu.pl/~grzegorzewskip/www/?download=stacje.csv")

length(dane$Answers)
dane$Answers
#rozklad wartosci-licznosci poszczegolnych klas
table(dane)

#rozklad wartosci-czestosci poszczegolnych klas
prop.table(table(dane)) # Funkcja ta dzia?a na tabelach



pie(table(dane))
pie(prop.table(table(dane)))
barplot(table(dane))
barplot(prop.table(table(dane)))

pie(table(dane), labels = paste0(names(table(dane)), "-", paste0(prop.table(table(dane)) * 100, "%")))
pie(table(dane), labels = paste0(names(table(dane)), "-", prop.table(table(dane)) * 100, "%"))

#table(dane[1:800,])
#prop.table(table(dane[1:800,]))

# Zadanie 3 ---------------------------------------------------------------

notowania <- c(23.30, 24.50, 25.30, 25.30, 24.30, 24.80, 
               25.20, 24.50, 24.60, 24.10, 24.30, 26.10, 
               23.10, 25.50, 22.60, 24.60, 24.30, 25.40, 
               25.20, 26.80)

plot(1:20, notowania, type = "l")

## 2 sposob
y=scan(nlines=2)
23.30     24.50     25.30     25.30     24.30     24.80     25.20     24.50     24.60     24.10  
24.30     26.10     23.10     25.50     22.60     24.60     24.30     25.40     25.20     26.80 
y

x=y

plot(1:20,x)
plot(x)
plot(x,type="l")
plot(x,type="b")
plot(x,type="b", pch=16, xlab="dzien", ylab="cena")
plot(x,type="b", pch=1:20, xlab="dzien", ylab="cena")



# Zadanie 4 ---------------------------------------------------------------

butelki <- read.csv("http://pages.mini.pw.edu.pl/~grzegorzewskip/www/?download=butelki.csv")
butelki$strength

# a) 
cisnienie <- butelki$strength * 0.0068947

library(dplyr)
butelki %>% 
  mutate(cisnienie = strength * 0.0068947) -> butelki
###
# mozna tez
#butelki$cisnienie=butelki$strength * 0.0068947

# b) 
hist(cisnienie)
hist(cisnienie, breaks = 6)
hist(cisnienie, breaks = 14)


hist(cisnienie,prob=T) # Przeskalowanie ?eby pola prostok?t?w sumowa?y si? do 1
hist(cisnienie,labels=T,ylim=c(0,45))
hist(cisnienie,labels=T,col="blue",ylim=c(0,50),main="")

h <- hist(cisnienie)
#granice klas
h$breaks
#srodki klas
h$mids
#licznosci klas
h$counts


hist(cisnienie,breaks=c(1.2,1.8,2.6))
seq(1.2, 2.6, 0.4)
hist(cisnienie,breaks=seq(1.2, 2.6,0.1))



#histogram skumulowanych licznosci
hs=hist(cisnienie)
hs$counts 
sum(hs$counts)
cumsum(hs$counts)
hs$counts = cumsum(hs$counts)
plot(hs) 


# c)
h <- hist(cisnienie, breaks = 14)
lines(h$mids, h$counts, col = 3, lwd = 3)

# d)
sort(cisnienie)
stem(cisnienie)
stem(cisnienie, 2)
?stem

# e)
boxplot(cisnienie)
summary(cisnienie)

# f)

var(cisnienie)
sd(cisnienie)
IQR(cisnienie)
diff(range(cisnienie))  #  różnicę między maksymalną a minimalną wartością wektora

#install.packages("moments")
library(moments)
skewness(cisnienie)
kurtosis(cisnienie)

#k-ty moment centralny
Mk <- function(x,k){
  mean((x - mean(x))^k)
}

Mk(cisnienie, 2)
var(cisnienie)

#wspolczynnik asymetrii (skosnosc)
# a= M3/sqrt(M2^3)
a=Mk(cisnienie,3)/(sqrt(Mk(cisnienie,2)^3))
a

#wspolczynnik splaszczenia (kurtoza)
# k= M4/(M2^2)-3
k=(Mk(cisnienie,4)/(Mk(cisnienie,2)^2)) - 3
k

#wspolczynnik zmiennosci
cv=sd(cisnienie)/abs(mean(cisnienie))
cv
round(cv*100,2)


# g)

quantile(cisnienie, 0.05)
quantile(cisnienie, 0.1)
quantile(cisnienie, 0.25)
quantile(cisnienie, 0.5)
quantile(cisnienie, 0.75)
quantile(cisnienie, 0.9)
quantile(cisnienie, 0.95)

# h)

mean(cisnienie, trim = 0.1)
mean(cisnienie)
median(cisnienie)

lapply(0:49 / 100, function(x) {
  mean(cisnienie, trim = x)
}) -> trim_means

plot(1:length(trim_means), trim_means)



# Zadanie 5 ---------------------------------------------------------------


czynsz <- c(334, 436, 425, 398, 424, 429, 392, 428, 339, 389,
            352, 405, 392, 403, 344, 400, 424, 443, 378, 387,
            384, 498, 374, 389, 367, 457, 409, 454 ,345, 422)

summary(czynsz)
var(czynsz)
sd(czynsz)
IQR(czynsz)
diff(range(czynsz))

skewness(czynsz)
kurtosis(czynsz)
b=boxplot(czynsz)
b$out


#read.csv(): Ta funkcja jest używana do odczytywania plików CSV, gdzie przecinek (,) jest używany jako separator 
#dziesiętny, a średnik (;) jako separator pól. Jest to najczęściej spotykany format plików CSV w wielu regionach.

#read.csv2(): Natomiast funkcja read.csv2() jest przeznaczona do odczytywania plików CSV, gdzie średnik (;)
#jest używany jako separator dziesiętny, a przecinek (,) jako separator pól.
#Jest to format spotykany w niektórych krajach, szczególnie w Europie.

# Zadanie 6 ---------------------------------------------------------------

samochody <- read.csv2("http://pages.mini.pw.edu.pl/~grzegorzewskip/www/?download=samochody.csv")

samochody$zp <- 1 / samochody$mpg * 3.7851 / 1.609 * 100 
mean(samochody$zp)
sum(is.na(samochody$zp))
mean(samochody$zp, na.rm = TRUE)

# czyli sa braki danych
x <- samochody$zp
is.na(x)
sum(is.na(x))

# b)
stem(samochody$zp)

# c)
hist(samochody$zp)

# d)
# dodac o jadrach ######################### !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
zp2 <- na.omit(samochody$zp)
density(zp2, kernel = "epanechnikov")
par(mfrow = c(1, 2))
plot(density(zp2, kernel = "epanechnikov"))
plot(density(zp2, kernel = "epanechnikov", bw = 0.1))
?density
par(mfrow = c(2, 3))
plot(density(zp2, kernel = "gaussian"))
plot(density(zp2, kernel = "epanechnikov"))
plot(density(zp2, kernel = "rectangular"))
plot(density(zp2, kernel = "triangular"))
plot(density(zp2, kernel = "cosine"))
########################################################################################################

# e)
par(mfrow = c(1, 1))
boxplot(samochody$zp)


# f)
mean(samochody$zp, na.rm = TRUE)
median(samochody$zp, na.rm = TRUE)
var(samochody$zp, na.rm = TRUE)
sd(samochody$zp, na.rm = TRUE)
range(samochody$zp, na.rm = TRUE)
quantile(samochody$zp, na.rm = TRUE, c(0.25, 0.75, 0.9))
IQR(samochody$zp, na.rm = TRUE)
library(moments)
kurtosis(samochody$zp, na.rm = TRUE)
skewness(samochody$zp, na.rm = TRUE)

# g)
quantile(samochody$zp, c(0.05, 0.1, 0.9, 0.95), na.rm = TRUE)


# h)
mean(samochody$zp, na.rm = TRUE, trim = 0.05)



# Zadanie 7 ---------------------------------------------------------------

dane=read.csv2("http://pages.mini.pw.edu.pl/~grzegorzewskip/www/?download=samochody.csv")
dane$zp=378.5/(1.609*dane$mpg)
x = dane$zp

# podzial na klasy
malo=x[x<=7]
malo
srednio=x[x>7 & x<=10]
srednio
duzo=x[x>10]
duzo

#wykres slupkowy
kategoria=c("malo","srednio","duzo")
licznosci=c(length(malo),length(srednio),length(duzo))
barplot(licznosci,names=kategoria)

100*licznosci/sum(licznosci)

#kodowanie inaczej: funkcja cut
x.kod=cut(x,breaks=c(-Inf,7,10,Inf))
head(x, 10)
head(x.kod, 10)
x.kod
# nadajemy etykietki "malo", "srednio", "duzo"
x.kod=cut(x,breaks=c(-Inf,7,10,Inf),labels=c("malo", "srednio", "duzo"))
x.kod
t=table(x.kod)
t
prop.table(t)
barplot(t)


# Zadanie 8 ---------------------------------------------------------------

zpA=samochody$zp[samochody$producent==1]
zpE=samochody$zp[samochody$producent==2]
zpJ=samochody$zp[samochody$producent==3]
mean(zpA)
sd(zpA)
mean(zpE)
is.na(zpE)
mean(na.omit(zpE))
mean(zpE,na.rm=T)
sd(na.omit(zpE))
mean(zpJ)
sd(zpJ)
boxplot(zpA,zpE,zpJ,horizontal=T,names=c("A","E","J"))
boxplot(samochody$zp~samochody$producent,horizontal=T,names=c("A","E","J"))

#inaczej: funkcja tapply
tapply(samochody$zp,samochody$producent,length)
tapply(samochody$zp,samochody$producent,summary)
tapply(samochody$zp,samochody$producent,sd, na.rm=T)


# Zadanie 9 ---------------------------------------------------------------

boxplot(samochody$zp ~ samochody$cylindry)

### ggplot ###
#install.packages("ggplot2")
library(ggplot2)
ggplot(samochody, aes(x = factor(cylindry), y = zp)) + 
  geom_boxplot()

###

# Zadanie 10 ---------------------------------------------------------------

zp10 <- samochody$zp[samochody$waga < 2500]
mean(zp10)
median(zp10)
sd(zp10)
skewness(zp10)

### dplyr ###

samochody %>% 
  filter(waga < 2500) %>% 
  summarise(srednia = mean(zp),
            mediana = median(zp),
            sd = sd(zp),
            skewness = skewness(zp))

###

# Zadanie 11 ---------------------------------------------------------------

mocrok=samochody$moc[samochody$rok>=79 & samochody$rok<=81]

#a
summary(mocrok)
boxplot(mocrok,horizontal=T)
sort(mocrok)

#b

quantile(mocrok,0.95)
quantile(mocrok,0.95,na.rm=T)



# Zadanie 12 ---------------------------------------------------------------

przyspieszenie <- samochody$przysp[samochody$waga > 2500 & samochody$waga < 3000]

# a)

boxplot(przyspieszenie)

# b)
# 75% mniejsze
quantile(przyspieszenie, 0.75)


# Zadanie 13 ---------------------------------------------------------------

waga <- samochody$waga[samochody$mpg >= 26]

# a)

boxplot(waga)

# b)

quantile(waga, 0.95, na.rm = TRUE)



# Zadanie 14 ---------------------------------------------------------------

boxplot(samochody$przysp~samochody$prod)

przyspA=samochody$przysp[samochody$producent==1]
przyspJ=samochody$przysp[samochody$producent==3]
summary(przyspA)
summary(przyspJ)
### ggplot ###
samochody %>% 
  filter(producent != 2) %>%
  ggplot(aes(x = producent, y = przysp, group = producent)) + 
  geom_boxplot()
###
















