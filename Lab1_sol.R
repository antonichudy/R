# Wektory
parzyste <- c(2, 4, 6)
nieparzyste <- c(1, 3, 5)
litery <- c("a", "b", "c")
litery[1]
litery[4]

vec <- c(parzyste, litery)


x <- 1:10
for(i in x){
  x[i] <- x[i]**2
}
x
x <- 1:10
x**2


ls()   # zmienne ktore obecnie mamy w pamieci


# Wykresy
#dev.new() w nowym oknie
par(mfrow = c(1, 1)) # ile chcemy wykresow obok siebie 
hist(x)
plot(x)
plot(x, type = 'l')
#dorysowanie lini

curve(x^2, from=-4, to=4)
curve(x^2, -4, 4)
curve(x^2, from=-4, to=4, xlab="x", ylab="y")
curve(x^2, from=-4, to=4, xlab="x", ylab="y", main = "rysunek")
# jakies inne wykresy
curve(abs(x),-10,10)

#y=sin(x) oraz cos(x)
curve(sin(x),-2*pi,2*pi)
curve(cos(x),-2*pi,2*pi,add=T,lty="dotted")
#curve(cos(x),-2*pi,2*pi,add=T,lty=2)

abline(v=2) # pionowa
abline(h=0) # Pozioma: y = 0
abline(v=0,col = "gray")  # Pionowa: x = 0
colors()






# Cel: wybrane rozkłady prawdopodobieństwa --------------------------------

# norm - rozkład normalny
# chisq - rozkład chi-kwadrat
# gamma - rozkład gamma
# exp - rozkład wykładniczy
# f - rozkład F-Snedecora

# d - gęstość f(x) lub rozkład prawdopodobieństwa P(X=x)
# p - dystrybuanta F(x) = P(X<=x)
# q - funkcja kwantylowa F^{1}(p)
# r - generowanie liczb pseudolosowych


# Zadanie 1 ---------------------------------------------------------------

curve(dnorm(x), from = -6, to = 6)
curve(dnorm(x,1,1), from = -6, to = 6, col = 'blue', add = T)
curve(dnorm(x,2,1), from = -6, to = 6, col = 'red', add = T)
legend("topright", c("N(0,1)", "N(1,1)", "N(2,1)"),
       fill = c("black", "blue", "red"), cex = 0.5)

#par(mfrow = c(1, 3))
#par(mfrow = c(1, 1))
curve(dnorm(x, 0, 1), xlim = c(-3, 5), ylim = c(0, 0.5))
curve(dnorm(x, 1, 1), xlim = c(-3, 5), col = 'blue', add = TRUE)
curve(dnorm(x, 2, 1), xlim = c(-3, 5), col = 'red', add = TRUE)
legend("topright", c("N(0,1)", "N(1,1)", "N(2,1)"),
       fill = c("black", "blue", "red"), cex = 0.5)
title("Gęstości")


# funkcja przeżycia 

# dystrybuanta: pnorm(x)
# ogon dystrybuanty (funkcja prze?ycia): 1 - pnorm(x) albo pnorm(x,lower.tail=F)
# F(t) = P(X<=t), czyli dystrybuanta jest takim dolnym ogonem
# Czyli jak ustawimy lower.tail = F, to liczymy P(X>t)

curve(1 - pnorm(x, 0, 1), from = -3, to = 5)
curve(1 - pnorm(x, 1, 1), from = -3, to = 5, col = 'blue', add = TRUE)
curve(1 - pnorm(x, 2, 1), from = -3, to = 5, col = 'red', add = TRUE)
legend ("topright", c("N(0,1)","N(1,1)","N(2,1)"),
        fill=c("black","red","blue"), cex = 0.5)
title("Funkcja przeżycia")

# b) N(0, 1), N(0, 0.5), N(0, 2)
# gęstość

curve(dnorm(x, 0, 1), from = -6, to = 5, ylim = c(0,1))
curve(dnorm(x, 0, 0.5), from = -3, to = 5, col = 'blue', add = TRUE)
curve(dnorm(x, 0, 2), from = -6, to = 5, col = 'red', add = TRUE)
legend ("topright", c("N(0,1)","N(1,1)","N(2,1)"),
        fill=c("black","red","blue"), cex = 0.5)
title("Gęstości")

# dystrybuanta
curve(pnorm(x, 0, 1), xlim = c(-5, 5), ylim = c(0,1), col = 1)
curve(pnorm(x, 0, 0.5), col = 'blue', add = TRUE)
curve(pnorm(x, 0, 2), col = 'red', add = TRUE)
legend ("topright", c("N(0,1)","N(1,1)","N(2,1)"),
        fill=c("black","red","blue"), cex = 0.5)
title("Dystrybuanta")

# przezycia -> odwrotnosc dystrybuanty
# funkcja przeżycia
curve(1 - pnorm(x, 0, 1), xlim = c(-3, 5), ylim = c(0, 1))
curve(1 - pnorm(x, 0, 0.5), xlim = c(-3, 5), col = 'blue', add = TRUE)
curve(1 - pnorm(x, 0, 2), xlim = c(-3, 5), col = 'red', add = TRUE)
legend ("topright", c("N(0,1)","N(1,1)","N(2,1)"),
        fill=c("black","red","blue"), cex = 0.5)
title("Funkcja przeżycia")



# Zadanie 2 ---------------------------------------------------------------

pnorm(3, 0, 1) - pnorm(-3, 0, 1)
?pnorm
# graficznie 
curve(dnorm(x), -4,4)
abline(v=-3, col="red")
abline(v=3,col="red")


# Zadanie 3 ---------------------------------------------------------------

# tekstowe wazne !!!

# a) 
pnorm(179, 173, 6)

# b)
pnorm(180, 173, 6) - pnorm(167, 173, 6)

# c)
1 - pnorm(181, 173, 6)

# d)
# 60% jest mniejszych
qnorm(0.6, 173, 6)


# Zadanie 4 ---------------------------------------------------------------

# a)
qnorm(0.95, 0, 1)

# b)
qnorm(0.975, 0, 1)

# c)
?qt
qt(0.95, 10)

# d)
qt(0.99, 20)

# e)
?qchisq
qchisq(0.9, 4)

# f)
qchisq(0.95, 10)

# g)
?qf
qf(0.95, 2, 10)

# h)
qf(0.99, 3, 18)


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


# Zadanie 6 ---------------------------------------------------------------

?dchisq

# a)

par(mfrow = c(1, 1))
curve(dchisq(x, 5), xlim = c(0, 80), ylim = c(0, 0.2))
curve(dchisq(x, 10), xlim = c(0, 80), col = 'blue', add = TRUE)
curve(dchisq(x, 40), xlim = c(0, 80), col = 'red', add = TRUE)
legend ("topright", c("Chi^2(5)","Chi^2(10)","Chi^2(40)"),
        fill=c("black","blue","red"), cex = 0.5)
# b)

curve(dchisq(x,5),from=0,to=150,ylab="y")
curve(dnorm(x,5,sqrt(2*5)),from=0,to=150,col="violet",add=T)

curve(dchisq(x,100),from=0,to=150,col="green",add=T)
curve(dnorm(x,100,sqrt(2*100)),from=0,to=150,col="violet",add=T)

curve(dchisq(x,1000),from=0,to=2000)
curve(dnorm(x,1000,sqrt(2*1000)),from=0,to=2000,col="violet",add=T)


#chi-kw(n)=G(n/2,1/2)

#dla duzych n: chi-kw(n) to z grubsza N(n,sqrt(2n))
#konwencja: sqrt(2n) oznacza odch.std



# Zadanie 7 ---------------------------------------------------------------

par(mfrow = c(1, 1))
curve(dt(x, 1), xlim = c(-5, 5), ylim = c(0, 0.4))
curve(dt(x, 5), col = 'blue', add = TRUE)
curve(dt(x, 30), col = 'red', add = TRUE)
legend ("topright", c("t(1)","t(5)","t(30)"),
        fill=c("black","blue","red"), cex = 0.5)

#par(mfrow = c(1, 3))

curve(dt(x, 5), xlim = c(-5, 5), ylim = c(0,0.4))
curve(dnorm(x, 0, 1), xlim = c(-5, 5), col = 'blue', add = TRUE)
legend ("topright", c("t-student(5)","N(0,1)"),
        fill=c("black","blue"), cex = 0.5)

curve(dt(x, 10), xlim = c(-5, 5), ylim = c(0,0.4))
curve(dnorm(x, 0, 1), xlim = c(-5, 5), col = 'blue', add = TRUE)
legend ("topright", c("t-student(10)","N(0,1)"),
        fill=c("black","blue"), cex = 0.5)

curve(dt(x, 40), xlim = c(-5, 5), ylim = c(0,0.4))
curve(dnorm(x, 0, 1), xlim = c(-5, 5), col = 'blue', add = TRUE)
legend ("topright", c("t-student(40)","N(0,1)"),
        fill=c("black","blue"), cex = 0.5)


# Zadanie 8 ---------------------------------------------------------------

par(mfrow = c(1, 1))

# a)
curve(df(x, 10, 5), ylim = c(0, 1), xlim = c(0, 3))
curve(df(x, 10, 10), col = 'blue', add = TRUE)
curve(df(x, 10, 20), col = 'red', add = TRUE)
legend ("topright", c("F-Sned(10,5)","F-Sned(10,10)", "F-Sned(10,20)"),
        fill=c("black","blue","red"), cex = 0.5)

# b)
curve(df(x, 5, 2), ylim = c(0, 1))
curve(df(x, 3, 2), col = 'blue', add = TRUE)
curve(df(x, 2, 2), col = 'red', add = TRUE)
legend ("topright", c("F-Sned(5,2)","F-Sned(3,2)", "F-Sned(2,2)"),
        fill=c("black","blue","red"), cex = 0.5)


# c)
curve(df(x, 2, 1), ylim = c(0, 1.1), xlim = c(0, 2))
curve(df(x, 2, 5), col = "blue", add = TRUE)
curve(df(x, 2, 10), col = "red", add = TRUE)
curve(df(x, 2, 20), col = "green", add = TRUE)
curve(dexp(x, 1), col = "yellow", add = TRUE)
legend ("topright", c("F-Sned(2,1)","F-Sned(2,5)", "F-Sned(2,10)",
                      "F-Sned(2,20)", 'Exp(1)'),
        fill=c("black","blue","red", "green", 'yellow'), cex = 0.5)


# Zadanie 9 ---------------------------------------------------------------

par(mfrow = c(1, 1))
curve(dbeta(x, 1, 1), ylim = c(0, 3), xlim = c(0, 1))
curve(dbeta(x, 2, 2), col = "blue", add = TRUE)
curve(dbeta(x, 2, 5), col = "red", add = TRUE)
curve(dbeta(x, 5, 2), col = "green", add = TRUE)
legend ("topright", c("Beta(1,1)","Beta(2,2)", "Beta(2,5)",
                      "Beta(5,2)"),
        fill=c("black","blue","red", "green"), cex = 0.5)



# Zadanie 10 --------------------------------------------------------------

par(mfrow = c(1, 1)) 
x <- 0:10
barplot(dbinom(x, 10, 0.5), ylim = c(0, 0.3), names.arg = x)
barplot(dbinom(x, 10, 0.25), col = 'blue', names.arg = x)
x <- 0:50
barplot(dbinom(x, 50, 0.25), names.arg = x)

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
1 - pgeom(10, 0.1) # liczba porazek wieksza od 10
?dgeom

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
pexp(0.5, 4)

# d)
1 - pexp(1, 4)
dpois(0,a)








