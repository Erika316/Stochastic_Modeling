# Temperatura simulada 

# Base_Datos <- read_excel("Base_Datos.xlsx", col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
#                                                           "numeric", "numeric", "numeric", "numeric", "numeric"))
base_de_datos <- na.omit(cbind(Base_completa_estimaciones$Esfuerzo_Y, 
                               Base_completa_estimaciones$Abundancia_X, 
                               Base_completa_estimaciones$Tem_Observada))

temperatura <- matrix(0, nrow = (length(base_de_datos[,1]) + 1), ncol = 10000)
sigma_erika <- matrix(0, nrow = (length(base_de_datos[,1]) + 1), ncol = 10000)
normal_erika <- matrix(rnorm((length(base_de_datos[,1]) + 1)*10000, 0, 1), ncol = 10000)
normal_temperatura <- matrix(rnorm((length(base_de_datos[,1]) + 1)*10000, 0, 1), ncol = 10000)
dim(sigma_erika)

temperatura[1, ] <- base_de_datos[1, 3]
sigma_erika[1, ] <- 0

time <- 1:(length(base_de_datos[,1]) + 1)
theta <- 27.385347 + 0.0008088*time - 1.98706*sin((3.14159265359/6)*time + 1.256038)

plot(theta, type = "l")
points(base_de_datos[, 3], pch = 20)

d_theta <- 0.0008088 - 1.98706*cos(30*time + 1.256038)*(3.14159265359/6)
theta <- matrix(rep(theta, 10000), nrow = (length(base_de_datos[,1]) + 1))
d_theta <- matrix(rep(d_theta, 10000), nrow = (length(base_de_datos[,1]) + 1))

datos_temperatura <- matrix(rep(base_de_datos[, 3], 10000), nrow = 155)
View(datos_temperatura)

for (j in 2:156) {
  sigma_erika[j, ] <- sigma_erika[(j-1), ] + 0.515815007*(0.152565426 - sigma_erika[(j-1), ]) + 0.006358538*normal_erika[(j-1), ]
  }

plot(sigma_erika[, 1], type = "l", col = "blue")
for (j in 2:10000) {
  lines(sigma_erika[, j], type = "l", col = "blue")
}

for (j in 2:156) { # Temperatura simulada 
  for (i in 1:10000) {
    temperatura[j, i] <- datos_temperatura[(j-1), i] + 0.161490323*(theta[(j-1), i] - datos_temperatura[(j-1), i]) + 
                         d_theta[(j-1), i] + sigma_erika[(j-1), i]*normal_temperatura[(j-1), i]
  }
}
View(temperatura)

plot(theta[, 1], col = "red", lwd = 2, type = "l")
for (j in 1:1000) {
  lines(temperatura[, j], type = "l", col = "blue")
}
points(base_de_datos[, 3], pch = 20)

pred_temperatura <- matrix(0, nrow = 50, ncol = 10000) 
pred_temperatura[1, ] <- base_de_datos[155, 3]

for (j in 2:50) { # Predicci?n de la temperatura 
  for (i in 1:10000) {
    pred_temperatura[j, i] <- pred_temperatura[(j-1), i] + 0.161490323*(theta[(j-1), i] - pred_temperatura[(j-1), i]) + 
      d_theta[(j-1), i] + sigma_erika[(j-1), i]*normal_temperatura[(j-1), i]
  }
}
pred_temperatura[, 2]

media_tray_temp <- NULL
for (j in 1:155) {
  media_tray_temp[j] <- mean(temperatura[j, ])
}

pred_media_temperatura <- NULL
for (j in 1:50) {
  pred_media_temperatura[j] <- mean(pred_temperatura[j, ])
}
inferior_temperatura <- NULL 
for (j in 1:50) {
  inferior_temperatura[j] <- quantile(pred_temperatura[j, ], 0.025)
}
superior_temperatura <- NULL 
for (j in 1:50) {
  superior_temperatura[j] <- quantile(pred_temperatura[j, ], 0.975)
}

plot(100:(155+49), 
     c(base_de_datos[, 3], rep(NA, 49))[100:(155+49)], 
     col = "black", type = "p", pch = 20,
     ylim = c(23.5, max(pred_temperatura)), 
     xlab = "Time (months)", ylab = "Temperature (?C)")
for (j in 1:10000) {
  lines(155:(155+49), pred_temperatura[1:50, j], type = "l", col = "gray85")
}
points(base_de_datos[, 3], pch = 20)
lines(155:(155+49), superior_temperatura, col = "lightblue1", type = "l", lwd = 2)
lines(155:(155+49), inferior_temperatura, col = "lightblue1", type = "l", lwd = 2)
polygon(x = c(155:(155+49), (155+49):155), 
        y = c(superior_temperatura, inferior_temperatura[50:1]), 
        col = "lightblue1", border = "lightblue1")
lines(155:(155+49), c(pred_media_temperatura[1:50]), 
      col = "royalblue2", lwd = 2)
lines(c(1:155, 155), c(media_tray_temp[1:155], base_de_datos[155, 3]), 
      col = "royalblue2", lwd = 2)
legend(100, 25.5, legend=c("Data", "Prediction", "Confidence set", 
                            "Stochastic paths"),
       col=c("black", "royalblue2", "lightblue1", 
             "gray85"), lty=c(NA, 1, NA, 1),pch=c(16, NA, 15,NA),
       lwd = c(4, 2, 4, 2), 
       cex=0.7,  bty = "n", ncol=2)

df_temperatura <- data.frame(1:(156+29), c(base_de_datos[, 3], rep(NA, 30)), c(media_tray_temp, 
                             pred_media_temperatura)[1:(156+29)], 
                             c(base_de_datos[, 3], superior_temperatura)[1:(156+29)], 
                             c(base_de_datos[, 3], inferior_temperatura)[1:(156+29)])
intervalo_temp <- data.frame(x = c(156:(156+29), rev(156:(156+29))), 
                             y = c(superior_temperatura[1:30], rev(inferior_temperatura[1:30])))

library(ggplot2)
colores <- c("Mean prediction" = "darkblue", "Data" = "dodgerblue1", "Confidence set for prediction" = "gray85")

ggplot(data = df_temperatura) + # En ggplot 
  geom_polygon(data = intervalo_temp,
                 mapping=aes(x = intervalo_temp[, 1], y = intervalo_temp[, 2]), alpha = 0.4) + 
  geom_point(aes(x = df_temperatura[, 1],  y = df_temperatura[, 2], color = 'Data'), size = 2) + 
  geom_line(aes(x = df_temperatura[, 1],  y = df_temperatura[, 3], color = 'Mean prediction'), size = 0.8) +
  labs(x = 'Time (months)', y = 'Temperature (?C)') + scale_color_manual(values = colores, name = " ", 
       guide = guide_legend(override.aes = list(linetype = c("solid", "blank",
                 "blank"), shape = c(NA, 16, 15)))) +
  theme(legend.position="bottom", panel.background = element_rect(fill = "transparent"), 
        panel.border = element_rect(fill = "transparent", # Necesario para agregar el borde
                      color = 1, size = 0.5))

abundancia <- matrix(0, nrow = (length(base_de_datos[,1]) + 1), ncol = 10000)
esfuerzo <- matrix(0, nrow = (length(base_de_datos[,1]) + 1), ncol = 10000)
normal_esfuerzo <- matrix(rnorm((length(base_de_datos[,1]) + 1)*10000, 0, 1), ncol = 10000)

abundancia[1, ] <- base_de_datos[1, 2] # Valores iniciales para abundancia y esfuerzo 
esfuerzo[1, ] <- base_de_datos[1, 1]

datos_abundancia <- matrix(rep(base_de_datos[, 2], 10000), nrow = 155)
View(datos_abundancia)
datos_esfuerzo <- matrix(rep(base_de_datos[, 1], 10000), nrow = 155)
View(datos_esfuerzo)

for (j in 2:156) { # Abundancia simulada 
  for (i in 1:10000) {
    abundancia[j, i] <- datos_abundancia[(j-1), i] + 0.08917645*datos_abundancia[(j-1), i]*(1-datos_abundancia[(j-1), i]/64000.861) - 
                         0.0020233*datos_abundancia[(j-1), i]*datos_esfuerzo[(j-1), i] + sqrt(0.1589271)*(temperatura[j, i] - 
                        temperatura[(j-1), i]) 
    esfuerzo[j, i] <- datos_esfuerzo[(j-1), i] + 3.89032e-05*(0.0020233*datos_abundancia[(j-1), i]*datos_esfuerzo[(j-1), i] - 
                      3564.19*datos_esfuerzo[(j-1), i]) + ((5.61e-2)/4)*normal_esfuerzo[(j-1), i]
  }
}

View(abundancia) # abundancia Xj = Datos abundancia 
View(esfuerzo) # esfuerzo Yj = Datos esfuerzo 

plot(base_de_datos[, 2], col = "black", pch = 20, type = "p")
for (j in 1:10000) {
  lines(abundancia[1:156, j], type = "l", col = "blue")
}

plot(base_de_datos[, 1], col = "black", pch = 20, type = "p")
for (j in 1:1000) {
  lines(esfuerzo[1:155, j], type = "l", col = "blue")
}

pred_abundancia <- matrix(0, nrow = 300, ncol = 10000) 
pred_abundancia[1, ] <- base_de_datos[155, 2]
pred_esfuerzo <- matrix(0, nrow = 300, ncol = 10000) 
pred_esfuerzo[1, ] <- base_de_datos[155, 1]
pred_temp <- matrix(0, nrow = 300, ncol = 10000) 
pred_temp[1, ] <- base_de_datos[155, 3]

tiempo <- 1:300
theta_1 <- 27.385347 + 0.0008088*tiempo - 1.98706*sin((3.14159265359/6)*tiempo + 1.256038)
d_theta_1 <- 0.0008088 - 1.98706*cos(30*tiempo + 1.256038)*(3.14159265359/6)
theta_1 <- matrix(rep(theta_1, 10000), nrow = 300)
d_theta_1 <- matrix(rep(d_theta_1, 10000), nrow = 300)

normal_prediccion_1 <- matrix(rnorm(300*10000, 0, 1), ncol = 10000)
normal_temperatura_1 <- matrix(rnorm(300*10000, 0, 1), ncol = 10000)
normal_erika_1 <- matrix(rnorm(300*10000, 0, 1), ncol = 10000)

for (j in 2:300) { # Prediccion de la abundancia y esfuerzo 
  for (i in 1:10000) {
    pred_temp[j, i] <- pred_temp[(j-1), i] + 0.161490323*(theta_1[(j-1), i] - pred_temp[(j-1), i]) + 
                       d_theta_1[(j-1), i] + normal_erika_1[(j-1), i]*normal_temperatura_1[(j-1), i]
    pred_abundancia[j, i] <- pred_abundancia[(j-1), i] + 0.08917645*pred_abundancia[(j-1), i]*(1-pred_abundancia[(j-1), i]/64000.861) - 
                             0.0020233*pred_abundancia[(j-1), i]*pred_esfuerzo[(j-1), i] + sqrt(0.1589271)*(pred_temp[j, i] - 
                             pred_temp[(j-1), i]) 
    pred_esfuerzo[j, i] <- pred_esfuerzo[(j-1), i] + 3.89032e-05*(0.0020233*pred_abundancia[(j-1), i]*pred_esfuerzo[(j-1), i] - 
                           3564.19*pred_esfuerzo[(j-1), i]) + ((5.61e-2)/4)*normal_prediccion_1[j, i]
  }
}

View(pred_abundancia)

pred_media_abundancia <- NULL
for (j in 1:300) {
  pred_media_abundancia[j] <- mean(pred_abundancia[j, ])
}
inferior_abundancia <- NULL 
for (j in 1:300) {
  inferior_abundancia[j] <- quantile(pred_abundancia[j, ], 0.025)
}
superior_abundancia <- NULL 
for (j in 1:300) {
  superior_abundancia[j] <- quantile(pred_abundancia[j, ], 0.975)
}
c(inferior_abundancia[15], superior_abundancia[15])

pred_media_esfuerzo <- NULL
for (j in 1:300) {
  pred_media_esfuerzo[j] <- mean(pred_esfuerzo[j, ])
}
inferior_esfuerzo <- NULL 
for (j in 1:300) {
  inferior_esfuerzo[j] <- quantile(pred_esfuerzo[j, ], 0.025)
}
superior_esfuerzo <- NULL 
for (j in 1:300) {
  superior_esfuerzo[j] <- quantile(pred_esfuerzo[j, ], 0.975)
}
c(inferior_esfuerzo[15], superior_esfuerzo[15])

df_abundancia_f <- data.frame(c(1:155, 155:(155+50)), 
                              c(base_de_datos[, 2], 
                              pred_media_abundancia[1:51]), 
                              rbind(abundancia[1:155, ],
                                    pred_abundancia[1:51,]), 
                              c(rep(NA, 155), 
                                inferior_abundancia[1:51]), 
                              c(rep(NA, 155), 
                                superior_abundancia[1:51])) 

plot(df_abundancia_f[153:(155+14), 1], 
     c(df_abundancia_f[153:155, 2], rep(NA, 14)), 
     pch = 20, type = "p", ylab = "Stock", xlab = "Time (months)", 
     ylim = c(63500, 64200))
for (j in 3:10002) {
  lines(155:(155+15), df_abundancia_f[, j][156:(156+15)], type = "l", 
    col = "gray85", 
    lwd = 2)
}
lines(df_abundancia_f[156:(156+15), 1], 
      df_abundancia_f[156:(156+15), 10003], col = "lightblue1", type = "l", lwd = 2)
lines(df_abundancia_f[156:(156+15), 1], 
      df_abundancia_f[156:(156+15), 10004], col = "lightblue1", type = "l", lwd = 2)
polygon(x = c(df_abundancia_f[156:(156+15), 1], 
              sort(df_abundancia_f[156:(156+15), 1], 
                   decreasing = TRUE)), 
        y = c(df_abundancia_f[156:(156+15), 10004], 
              df_abundancia_f[(156+15):156, 10003]), 
        col = "lightblue1", border = "lightblue1")
lines(df_abundancia_f[145:156, 1], df_abundancia_f[145:156,3], 
      col = "royalblue2", lwd = 2)
lines(df_abundancia_f[156:(155+30), 1], df_abundancia_f[156:(155+30),2], 
      col = "royalblue2", lwd = 2)
points(155, df_abundancia_f[155, 2], pch = 20)
legend(157, 63750, legend=c("Data", "Prediction", "Confidence set", 
                               "Stochastic paths"),
       col=c("black", "royalblue2", "lightblue1", 
             "gray85"), lty=c(NA, 1, NA, 1),pch=c(16, NA, 15,NA),
       lwd = c(4, 2, 4, 2), 
       cex=0.7,  bty = "n", ncol=2)

df_esfuerzo_f <- data.frame(c(1:155, 155:(155+50)), 
                            c(base_de_datos[, 1], 
                 pred_media_esfuerzo[1:51]), 
                 rbind(esfuerzo[1:155, ],
                       pred_esfuerzo[1:51,]), 
                 c(rep(NA, 155), 
                   inferior_esfuerzo[1:51]), 
                 c(rep(NA, 155), 
                   superior_esfuerzo[1:51]))

plot(df_esfuerzo_f[1:(155+29), 1], 
     c(df_esfuerzo_f[1:155, 2], rep(NA, 29)), 
     pch = 20, type = "p", ylab = "Effort", 
     xlab = "Time (months)", ylim = 
       c(-0.085, 
         max(df_esfuerzo_f[1:(155+29), 2])))
for (j in 3:10002) {
  lines(155:(155+29), df_esfuerzo_f[, j][156:(156+29)], type = "l", 
        col = "gray85", 
        lwd = 1)
}
lines(df_esfuerzo_f[155:(155+30), 1], 
      df_esfuerzo_f[156:(156+30), 10003], col = "lightblue1", type = "l", lwd = 2)
lines(df_esfuerzo_f[155:(155+30), 1], 
      df_esfuerzo_f[156:(156+30), 10004], col = "lightblue1", type = "l", lwd = 2)
polygon(x = c(df_esfuerzo_f[155:(155+30), 1], 
              sort(df_esfuerzo_f[155:(155+30), 1], 
                   decreasing = TRUE)), 
        y = c(df_esfuerzo_f[156:(156+30), 10004], 
              df_esfuerzo_f[(156+30):156, 10003]), 
        col = "lightblue1", border = "lightblue1")
lines(df_esfuerzo_f[1:156, 1], df_esfuerzo_f[1:156,3], 
      col = "royalblue2", lwd = 2)
lines(df_esfuerzo_f[156:(156+30), 1], df_esfuerzo_f[156:(156+30),2], 
      col = "royalblue2", lwd = 2)
points(155, df_esfuerzo_f[155, 2], pch = 20)
legend(0, 0.04, legend=c("Data", "Prediction", "Confidence set", 
                               "Stochastic paths"),
       col=c("black", "royalblue2", "lightblue1", 
             "gray85"), lty=c(NA, 1, NA, 1), pch=c(16, NA, 15,NA),
       lwd = c(4, 2, 4, 2), 
       cex=0.7,  bty = "n", ncol=2)

##################################################################

plot(130:(156+299), c(base_de_datos[, 1], pred_media_esfuerzo)[130:(156+299)], col = "black", type = "l", lwd = 2)
for (j in 1:10000) {
  lines(156:(156+299), pred_abundancia[, j], type = "l", col = "gray85")
}
points(base_de_datos[, 3], pch = 20)
lines(156:(156+299), pred_media_abundancia, col = "black", type = "l", lwd = 2)
lines(156:(156+299), superior_abundancia, col = "black", type = "l", lwd = 2)
lines(156:(156+299), inferior_abundancia, col = "black", type = "l", lwd = 2)

plot(130:length(df_esfuerzo$Tiempo), c(base_de_datos[, 1][130:length(df_abundancia$Tiempo)], 
     pred_media_esfuerzo[130:length(df_abundancia$Tiempo)]), 
     col = "darkblue", type = "l", lwd = 2, 
     ylim = c(min(inferior_esfuerzo), max(superior_esfuerzo)))
for (j in 1:10000) {
  lines(155:(155+50), pred_esfuerzo[, j][1:51], type = "l", col = "gray85")
}
points(base_de_datos[, 1], pch = 20)
lines(156:(156+299), pred_media_esfuerzo, col = "black", type = "l", lwd = 2)
lines(156:(156+299), superior_esfuerzo, col = "black", type = "l", lwd = 2)
lines(156:(156+299), inferior_esfuerzo, col = "black", type = "l", lwd = 2)

df_esfuerzo <- data.frame(1:(156+49), c(base_de_datos[, 1], rep(NA, 50)), c(esfuerzo[, 1], 
              pred_media_esfuerzo)[1:(156+49)])
names(df_esfuerzo) <- c("Tiempo", "Datos", "Prediccion")

plot(df_esfuerzo$Tiempo[130:length(df_abundancia$Tiempo)], 
     df_esfuerzo$Datos[130:length(df_abundancia$Tiempo)], pch = 20, 
     type = "p", ylim = c(min(pred_esfuerzo), max(pred_esfuerzo)))
for (j in 1:10000) {
  lines(155:(155+50), pred_esfuerzo[, j][1:51], type = "l", col = "gray85", 
        lwd = 3)
}
lines(df_abundancia$Tiempo, df_esfuerzo$Prediccion, col = "blue", 
      lwd = 2)
lines(156:(156+299), superior_esfuerzo, col = "black", type = "l", lwd = 2)
lines(156:(156+299), inferior_esfuerzo, col = "black", type = "l", lwd = 2)

colores_2 <- c("Mean prediction" = "chartreuse4", "Data" = "dodgerblue1")
library(ggplot2)
ggplot(data = df_esfuerzo) + # En ggplot 
  geom_point(aes(x = df_esfuerzo[, 1],  y = df_esfuerzo[, 2], color = 'Data'), size = 2) + 
  geom_line(aes(x = df_esfuerzo[, 1],  y = df_esfuerzo[, 3], color = 'Mean prediction'), size = 0.8) +
  labs(x = 'Time (years)', y = 'Catch per unit of effort') + scale_color_manual(values = colores_2, name = " ", 
  guide = guide_legend(override.aes = list(linetype = c("solid", "blank"), shape = c(NA, 16)))) +
  theme(legend.position="bottom", panel.background = element_rect(fill = "transparent"), 
        panel.border = element_rect(fill = "transparent", # Necesario para agregar el borde
                                    color = 1, size = 0.5))
# _________
library(tidyverse)

dat <- tibble(x = c(base_de_datos[1:155, 2], 
                    c(abundancia[1:155], 
                      pred_media_abundancia[156:205])),
              y = c(1:155, 1:205),
              group = c(rep("Data", length(base_de_datos[1:155, 2])),
                        rep("Mean prediction", length(
                          c(abundancia[1:155], 
                            pred_media_abundancia[156:205]))))
)

dat %>% 
  ggplot(aes(x = y, y = x, colour = group, 
             shape = group, linetype = group)) + 
  geom_point(size = 1.5) + 
  geom_line(size = 1) + 
  scale_shape_manual(values = c(19, NA)) +
  scale_linetype_manual(values = c(0, 1))

dat1 <- tibble(x = c(base_de_datos[152:155, 2], 
                     c(abundancia[152:155], 
                       pred_media_abundancia[1:51]), 
                     c(pred_abundancia[1:51, ])),
               y = c(152:155, c(152:155, 155:205), 
                     rep(155:205, 10000)),
               group = c(rep("Data", length(152:155)),
                         rep("Mean prediction", length(152:205)), 
                         rep("Stochastic paths", length(rep(156:205, 
                                                            10000))))
)

dat1 %>% 
  ggplot(aes(x = y, y = x, colour = group, 
             shape = group, linetype = group)) + 
  geom_point(size = 1.5) + 
  geom_line(size = 1) + 
  geom_line(size = 1) +
  scale_shape_manual(values = c(19, NA, NA)) +
  scale_linetype_manual(values = c(0, 1, 1))

colores_1 <- c("Mean prediction" = "darkorchid4", "Data" = "dodgerblue1")
library(ggplot2)
ggplot(data = df_abundancia_f) + # En ggplot 
  geom_point(aes(x = df_abundancia_f[, 1],  y = df_abundancia_f[, 2], color = 'Data'), size = 2) + 
  geom_line(aes(x = df_abundancia[, 1],  y = df_abundancia[, 3], color = 'Mean prediction'), size = 0.8) +
  labs(x = 'Time (years)', y = 'Stock') + scale_color_manual(values = colores_1, name = " ", 
                                                             guide = guide_legend(override.aes = list(linetype = c("solid", "blank"), shape = c(NA, 16)))) +
  theme(legend.position="bottom", panel.background = element_rect(fill = "transparent"), 
        panel.border = element_rect(fill = "transparent", # Necesario para agregar el borde
                                    color = 1, size = 0.5))
