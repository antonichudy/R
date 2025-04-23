

# Zadanie 5 ---------------------------------------------------------------

?dgamma

# a)

#par(mfrow = c(1, 1))
curve(dgamma(x, 1, 1), xlim = c(0, 6), ylim = c(0, 1))
curve(dgamma(x, 0.5, 1), xlim = c(0, 6), col = 'blue', add = TRUE)
curve(dgamma(x, 2, 1), xlim = c(0, 6), col = 'red', add = TRUE)
curve(dgamma(x, 3, 1), xlim = c(0, 6), col = 'green', add = TRUE)
legend ("topright", c("Ga(0,1)","Ga(0.5,1)","Ga(2,1)", "Ga(3,1)"),
        fill=c("black","blue","red", 'green'), cex = 0.5)
#legend("topright", c("Gamma(1,1)","Gamma(0.5,1)", "Gamma(2,1)", "Gamma(3,1)"), lty=1, col=1:4, text.col=1)


# b) 

par(mfrow = c(1, 1))
curve(dgamma(x, 2, 1), xlim = c(0, 6), ylim = c(0, 1.2), col = 'blue')
curve(dgamma(x, 2, 2), xlim = c(0, 6), col = 'red', add = TRUE)
curve(dgamma(x, 2, 3), xlim = c(0, 6), col = 'green', add = TRUE)
legend ("topright", c("Gamma(2,1)","Gamma(2,2)", "Gamma(2,3)"),
        fill=c("blue","red", 'green'), cex = 0.5)



# Zadanie 12 --------------------------------------------------------------

?phyper
phyper(0, 5, 195, 10)

N=200
M=5
n=10
k=0
#sprawdzamy
choose(M,k)*choose(N-M,n-k)/choose(N,n)


#dhyper(x, m, n, k)
#1. x – Number of "successes" in the sample
#The number of items you’re interested in (successes) drawn from the sample.
#Example: If you're drawing 5 cards and want the probability of getting exactly 2 red cards, then x = 2.

#2. m – Number of "successes" in the population
#The total number of "good" or "success" items in the whole population.
#Example: In a standard deck of 52 cards, there are 26 red cards, so m = 26.

#3. n – Number of "failures" in the population
##The total number of "bad" or "non-success" items in the population.
#This is the rest of the population, so if 26 are red (successes), the remaining 26 are black (failures), so n = 26.

#4. k – Number of draws (sample size)
#How many items you are drawing (without replacement).
#Example: If you draw 5 cards, then k = 5.




# Zadanie 13 --------------------------------------------------------------

# a)
1 - pexp(1000, 0.0001)
1 - pexp(10000, 0.0001)
1 - pexp(30000, 0.0001)

a <- 0.0001
abline(v=1000, col="red")
1-pexp(1000,a)
abline(v=10000, col="blue")
1-pexp(10000,a)
abline(v=30000, col="green")
1-pexp(30000,a)
# b)
qexp(0.1, 0.0001)

?qexp
# Zadanie 14 --------------------------------------------------------------

# a) Y~exp(4)
a=4
k=0:100
pk=dpois(k,a)
plot(k,pk,type="h")

curve(dexp(x,a),0,10)
# b) EY=1/4 VarY=1/16

# c)
# jeden na 30min czyli 2 na 1h
pexp(0.5, 4)

# d)

1 - pexp(1, 4)
dpois(0,a)



# Zadanie 15 --------------------------------------------------------------

# a)

# Generujemy probe losowa U_1,V_1,...,U_n,V_n z rozkladu jednostajnego:
n<-10000
u<-runif(n)
v<-runif(n)

#Zaznaczmy te pkt na wykresie, dodajemy wykres funkcji y=x^2 dla 0<x<1
plot(u,v,xlim=c(0,1),ylim=c(0,1))
curve(x*x, col="red", type="l", xlim=c(0,1), add=T, lwd=3)

#Zliczamy pkt z naszej probki, ktore sa pod wykresem funkcji y=x^2:
u
v
u<v*v
number <- sum(u<v*v) 
number
pole<-number/n
# przyblizenie numeryczne całki
pole


#Niech X1,Y1,X2,Y3,... beda niezaleznymi zmiennymi losowymi 
#o rozkladzie jednostajnym U([0,1]). Dla funkcji borelowskiej 
#f:[0,1]?[0,1] definiujemy
#Z_{i}=I_{{f(X_{i})>Y_{i}}}. #(indykatory)
#Wowczas, z MPWL, mamy:
#(1/n)suma_i {Zi} -> ca?ka od 0 do 1 z f(x)dx  p.n.
#a)  Pole obszaru A to
#ca?ka_0^1{A}dxdy=ca?ka_0^1(ca?ka_0^1{x2}dy)dx=ca?ka_0^1{x2}dx=1/3. 




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




### ---------------------------- LISTA 2 ----------------------------------

# Zadanie 1 ---------------------------------------------------------------

kobiety <- c(17364, 56128, 11239, 8170)
stan <- c("panny", "mężatki", "wdowy", "rozwódki")

# a)

pie(kobiety)
pie(kobiety, labels = stan)
pie(kobiety, paste(stan, kobiety, sep = "\n"), col = c(2, 3, 4, 6))

# b)

barplot(kobiety)
barplot(kobiety, names.arg = stan)
barplot(kobiety, names.arg = paste(stan, kobiety, sep = "\n"))


# Zadanie 2 ---------------------------------------------------------------

dane <- read.csv("http://pages.mini.pw.edu.pl/~grzegorzewskip/www/?download=stacje.csv")
dane

table(dane)
prop.table(table(dane)) 

pie(table(dane))
barplot(table(dane), ylim=c(0,400))

## ggplot2  ##########################################################
library(ggplot2)
library(dplyr)
library(forcats)

dane <- read.csv("http://pages.mini.pw.edu.pl/~grzegorzewskip/www/?download=stacje.csv")

ggplot(dane, aes(x = Answers)) +
  geom_bar(fill = "steelblue")
ggplot(dane, aes(x = fct_infreq(Answers))) + 
  geom_bar(fill = "steelblue")


#### ##########################################################
#reorder() sortuje te kategorie według liczby wystąpień (n),
#od największej do najmniejszej (-n – minus oznacza malejąco).
#y = n: oś pionowa pokazuje, ile razy dana stacja się pojawiła.
dane_agg <- dane %>%
  count(Answers) %>%
  mutate(prop = n / sum(n)) 

ggplot(dane_agg, aes(x = reorder(Answers, -n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  ylim(0, 400) +
  labs(title = "Liczba wystąpień stacji", x = "", y = "") +
  theme_minimal()

ggplot(dane_agg, aes(x = "", y = prop, fill = Answers)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  labs(title = "Udział procentowy stacji") +
  theme_void() +
  theme(legend.position = "right")
#####################################################################


# Zadanie 3 ---------------------------------------------------------------

notowania <- c(23.30, 24.50, 25.30, 25.30, 24.30, 24.80, 
               25.20, 24.50, 24.60, 24.10, 24.30, 26.10, 
               23.10, 25.50, 22.60, 24.60, 24.30, 25.40, 
               25.20, 26.80)

plot(1:20, notowania, type = "l")

## 2 sposob
x=scan(nlines=2)
23.30     24.50     25.30     25.30     24.30     24.80     25.20     24.50     24.60     24.10  
24.30     26.10     23.10     25.50     22.60     24.60     24.30     25.40     25.20     26.80 

x
plot(1:20,x)
plot(x)
plot(x,type="l")
plot(x,type="b")

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











