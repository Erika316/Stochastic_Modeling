# Solution of the system of equations
# Using the BBsolve package

install.packages("BB")
library(BB)

x = rep(NA,2)
sistema = function(x){
  f = rep(NA, 2)
  f[1] = (-1/((0.1589271)^2))*(-0.089645*Y+(0.089645/64697.861)*XY+0.00202330*Y2)
  -(1/x[1])*(0.00202330*X2 - x[2]*X)
  f[2] = -0.00202330*X + length(na.omit(Base_Datos[, 4]))*x[2]
  f
}

x0= c(sqrt(2), 3564)
BBsolve(par=x0 , fn = sistema)

dat=data.frame(x= Base_Datos[1:length(na.omit(Base_Datos[, 4]))-1, 4], 
               y= Base_Datos[1:length(na.omit(Base_Datos[, 2]))-1, 2], 
               z = Base_Datos[2:length(na.omit(Base_Datos[, 2])), 2])
min.RSS <- function(data, par) {
  with(data, sum((y + (0.00202330*x*y - par[1]*y) + 
                    par[2]*rnorm(1, 0, 1) - z)^2))
}

optim(par = c(0, 1), fn = min.RSS, data = dat)

x0= c(sqrt(2), 3564)
BBsolve(par=x0 , fn = sistema)

dat=data.frame(x= Base_Datos[1:length(na.omit(Base_Datos[, 4]))-1, 4], 
               y= Base_Datos[1:length(na.omit(Base_Datos[, 2]))-1, 2], 
               z = Base_Datos[2:length(na.omit(Base_Datos[, 2])), 2])
min.RSS <- function(data, par) {
  with(data, sum((y + (0.00202330*x*y - par[1]*y) + 
                    par[2]*rnorm(1, 0, 1) - z)^2))
}

optim(c(0,0), min.RSS, data = dat)

dat=data.frame(x= Base_Datos[1:length(na.omit(Base_Datos[, 4]))-1, 4], 
               y= Base_Datos[1:length(na.omit(Base_Datos[, 2]))-1, 2], 
               z = Base_Datos[2:length(na.omit(Base_Datos[, 2])), 2])
min.RSS1 <- function(data, par) {
  with(data, sum((y + par[3]*((0.00202330*x*y - par[1]*y)) + 
                    par[2]*rnorm(1, 0, 1) - z)^2))
}

optim(c(0,0, 1), min.RSS1, data = dat)

dat=data.frame(x= Base_Datos[1:length(na.omit(Base_Datos[, 4]))-1, 4], 
               y= Base_Datos[1:length(na.omit(Base_Datos[, 2]))-1, 2], 
               z = Base_Datos[2:length(na.omit(Base_Datos[, 2])), 2])
min.RSS1 <- function(data, par) {
  with(data, sum((y + par[1]*((0.00202330*x*y - 3564.19*y) + 
                                2.080035*rnorm(1, 0, 1)) - z)^2))
}

optim(c(0), min.RSS1, data = dat)

# Estimated value  3.890321e-05 =  3.890321*10^(-5)

## Graphics:

# 1. Model Fit: 

plot(seq(1, 155, 1), na.omit(Base_Datos$Abundancia_X), pch = 19, xlim = c(0, 155),
     xlab ="Tiempo (meses)", ylab = "Abundancia")
lines(seq(1, 155, 1), Base_Datos$`Abundancia estimada`[1:155], t="l" , 
      col="Blue" , lwd=2)

plot(seq(1, 155, 1), na.omit(Base_Datos$Esfuerzo_Y), pch = 19, xlim = c(0, 155),
     xlab ="Tiempo (meses)", ylab = "Esfuerzo")
lines(seq(1, 155, 1), Base_Datos$`Esfuerzo estimado`[1:155], t="l" , 
      col="Purple" , lwd=2)

# 3 year predictions

plot(seq(1, 155, 1), na.omit(Base_Datos$Abundancia_X), pch = 19, xlim = c(0, 205),
     xlab ="Tiempo (meses)", ylab = "Abundancia", main = "Prediccion de la 
     abundancia a 204 meses (17 annos)")
lines(seq(1, 205, 1), Base_Datos$`Abundancia estimada`[1:205], t="l" , 
      col="Blue" , lwd=2)

plot(seq(1, 155, 1), na.omit(Base_Datos$Esfuerzo_Y), pch = 19, xlim = c(0, 205),
     ylim = c(0, 1), xlab ="Tiempo (meses)", ylab = "Esfuerzo", main = "Prediccion
     del esfuerzo a 204 meses (17 annos)")
lines(seq(1, 205, 1), Base_Datos$`Esfuerzo estimado`[1:205], t="l" , 
      col="Purple" , lwd=2)

# Relationship between the variables 

plot(Base_Datos$Tem_Simul_Orstein[1:155], Base_Datos$`Abundancia estimada`[1:155]
     , xlab = "Temperatura (C)", ylab = "Abundancia", main = "Temperatura Vs
     Abundancia", pch = 19, col = "green")

plot(Base_Datos$Tem_Simul_Orstein[1:155], Base_Datos$`Esfuerzo estimado`[1:155]
     , xlab = "Temperatura (C)", ylab = "Esfuerzo", main = "Temperatura Vs
     Esfuerzo", pch = 20, col = "red")

# Sensitivity analysis plots

Dt = 1 # Spread every month
n = 36 # 3 years
sigma = 0.152565426

theta = NULL 
dtheta = NULL 
t = NULL 
t[1] = Base_Datos$Tem_Observada[1]

for(i in 2:n){
  theta[i-1] = 27.385347 + 0.0008088*t[i-1]
  - 1.98706*(sin((3.14159265359/6)*t[i-1] + 1.256038))
  dtheta[i-1] = 0.0008088
  - 1.98706*(cos((3.14159265359/6)*t[i-1] + 1.256038))*(3.14159265359/6)
  t[i] = t[i-1] + a*(theta[i-1] - t[i-1] + dtheta[i-1])*Dt + 
    sigma*rnorm(1, 0, 1)*Dt
}

plot(seq(1, n, 1), t, t="l", col="Purple" , lwd=2)

d = NULL 
h = NULL 
for(j in 1:n){
  d[j] = rnorm(1)*sqrt(abs((t[j-1]-t[j])))
  h[j] = rnorm(1)*sqrt(Dt)}

funcion <-function(r, k, beta, eta, kappa, sigma1, sigma2){
  
  x = NULL 
  y = NULL 
  producto = NULL
  x[1] = as.numeric(Base_Datos$Abundancia_X[1])
  y[1] = as.numeric(Base_Datos$Esfuerzo_Y[1])
  
  for(j in 1:n){
    producto[j] = x[j]*y[j]
    x[j+1] = x[j] + (k*x[j] - (r/k)*(x[j])^2 - beta*producto[j])*Dt 
    + sigma1*d[j]
    y[j+1] = y[j] + kappa*(beta*producto[j] - eta*y[j])*Dt
    + sigma2*h[j]}
  
  return(c(x[7],y[7]))} 

funcion(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1)
funcion(1, 1, 3, 1, 1, 1, 1)

sensibilidad1 = NULL
sensibilidad2 = NULL 
r = 0.08917645
K = 64000.861
beta = seq(0, 1, by = 0.01) 
eta = 3564.19
kappa = 0.00003890321
sigma1 = sqrt(0.1589271)
sigma2 = sqrt(0.000196)

for(s in 1:length(beta)){
  sensibilidad1[s] = funcion(r, K, beta[s], eta, kappa, sigma1, sigma2)[1]
  sensibilidad2[s] = funcion(r, K, beta[s], eta, kappa, sigma1, sigma2)[2]
}

datos1 <- data.frame(x = beta, y = sensibilidad1, z = sensibilidad2)
library(scatterplot3d)
scatterplot3d(datos1)

par(mfrow = c(1, 2))
plot(beta, sensibilidad1, col = "blue", type = "l", lwd = 1,
     xlab =expression(beta), ylab = "Abundancia")
plot(beta, sensibilidad2, col = "orange", type = "l", lwd = 1, 
     xlab =expression(beta), ylab = "Esfuerzo")

sensibilidad3 = NULL
sensibilidad4 = NULL 
r = seq(0, 1, by = 0.01)
K = 64000.861
beta = 0.00202330 
eta = 3564.19
kappa = 0.00003890321
sigma1 = sqrt(0.1589271)
sigma2 = sqrt(0.000196)

for(s in 1:length(r)){
  sensibilidad3[s] = funcion(r[s], K, beta, eta, kappa, sigma1, sigma2)[1]
  sensibilidad4[s] = funcion(r[s], K, beta, eta, kappa, sigma1, sigma2)[2]
}

par(mfrow = c(1, 2))
plot(r, sensibilidad3, col = "red", type = "l", lwd = 1,
     xlab =expression(r), ylab = "Abundancia")
plot(r, sensibilidad4, col = "brown", type = "l", lwd = 1, 
     xlab =expression(r), ylab = "Esfuerzo")

sensibilidad5 = NULL
sensibilidad6 = NULL 
r = 0.08917645
K = seq(0, 100000, by = 10) 
beta = 0.00202330 
eta = 3564.19
kappa = 0.00003890321
sigma1 = sqrt(0.1589271)
sigma2 = sqrt(0.000196)

for(s in 1:length(K)){
  sensibilidad5[s] = funcion(r, K[s], beta, eta, kappa, sigma1, sigma2)[1]
  sensibilidad6[s] = funcion(r, K[s], beta, eta, kappa, sigma1, sigma2)[2]
}

par(mfrow = c(1, 2))
plot(K, sensibilidad5, col = "red", type = "l", lwd = 1,
     xlab =expression(K), ylab = "Abundancia")
plot(K, sensibilidad6, col = "brown", type = "l", lwd = 1, 
     xlab =expression(K), ylab = "Esfuerzo", ylim = c(-1.858319*10^(101), 
                                                      2.303651*10^(08)))

sensibilidad7 = NULL
sensibilidad8 = NULL 
r = 0.08917645
K = 64000.861
beta = 0.00202330 
eta = seq(0, 10000, by = 1)
kappa = 0.00003890321
sigma1 = sqrt(0.1589271)
sigma2 = sqrt(0.000196)

for(s in 1:length(eta)){
  sensibilidad7[s] = funcion(r, K, beta, eta[s], kappa, sigma1, sigma2)[1]
  sensibilidad8[s] = funcion(r, K, beta, eta[s], kappa, sigma1, sigma2)[2]
}

par(mfrow = c(1, 2))
plot(eta, sensibilidad7, col = "red", type = "l", lwd = 1,
     xlab =expression(eta), ylab = "Abundancia")
plot(eta, sensibilidad8, col = "brown", type = "l", lwd = 1, 
     xlab =expression(eta), ylab = "Esfuerzo")

sensibilidad9 = NULL
sensibilidad10 = NULL 
r = 0.08917645
K = 64000.861
beta = 0.00202330 
eta = 3564.19
kappa = seq(0, 1, 0.01)
sigma1 = sqrt(0.1589271)
sigma2 = sqrt(0.000196)

for(s in 1:length(kappa)){
  sensibilidad9[s] = funcion(r, K, beta, eta, kappa[s], sigma1, sigma2)[1]
  sensibilidad10[s] = funcion(r, K, beta, eta, kappa[s], sigma1, sigma2)[2]
}

par(mfrow = c(1, 2))
plot(kappa, sensibilidad9, col = "red", type = "l", lwd = 1,
     xlab =expression(kappa), ylab = "Abundancia")
plot(kappa, sensibilidad10, col = "brown", type = "l", lwd = 1, 
     xlab =expression(kappa), ylab = "Esfuerzo")

sensibilidad11 = NULL
sensibilidad12 = NULL  
r = 0.08917645
K = 64000.861
beta = 0.00202330 
eta = 3564.19
kappa = 0.00003890321
sigma1 = seq(0, 1, 0.01)
sigma2 = sqrt(0.000196)

for(s in 1:length(sigma1)){
  sensibilidad11[s] = funcion(r, K, beta, eta, kappa, sigma1[s], sigma2)[1]
  sensibilidad12[s] = funcion(r, K, beta, eta, kappa, sigma1[s], sigma2)[2]
}

par(mfrow = c(1, 2))
plot(sigma1, sensibilidad11, col = "red", type = "l", lwd = 1,
     xlab =expression(sigma[1]), ylab = "Abundancia")
plot(sigma1, sensibilidad12, col = "brown", type = "l", lwd = 1, 
     xlab =expression(sigma[1]), ylab = "Esfuerzo")

sensibilidad13 = NULL
sensibilidad14= NULL 
r = 0.08917645
K = 64000.861
beta = 0.00202330 
eta = 3564.19
kappa = 0.00003890321
sigma1 = sqrt(0.1589271)
sigma2 = seq(0, 1, by = 0.01)

for(s in 1:length(sigma2)){
  sensibilidad11[s] = funcion(r, K, beta, eta, kappa, sigma1, sigma2[s])[1]
  sensibilidad12[s] = funcion(r, K, beta, eta, kappa, sigma1, sigma2[s])[2]
}

par(mfrow = c(1, 2))
plot(sigma2, sensibilidad13, col = "red", type = "l", lwd = 1,
     xlab =expression(sigma[2]), ylab = "Abundancia")
plot(sigma2, sensibilidad14, col = "brown", type = "l", lwd = 1, 
     xlab =expression(sigma[2]), ylab = "Esfuerzo")
