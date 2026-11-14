setwd('/Users/danielh/Documents/UC3M/Master/Second term /multivarible analysis/heart+disease')
list.files()

#Presentar la base de datos, escribir de dodne se saco.
#Incialmente 303 parcientes, 
#clasificar las variables entre cuantitativas, binarias , categoricas 

col_names <- c("age", "sex", "cp", "trestbps", "chol", "fbs", 
               "restecg", "thalach", "exang", "oldpeak", 
               "slope", "ca", "thal", "num")

data = read.csv('processed.cleveland.data', header = FALSE, col.names = col_names, na.strings = '?') 

#Limpieza de los datos 
# filtro de los 6 registros incompletos 
#nuevo data set 297
data_clean = na.omit(data)

v_quanti = c("age", "trestbps", "chol", "thalach", "oldpeak", "ca")
df_quanti = data_clean[,v_quanti]

#generar en interpretar estadisticos descriptivos 
#visualizaciones generar
library(ggplot2)

par(mfrow = c(2,3))
for (col in names(df_quanti)) {
  hist(df_quanti[[col]],
       main = paste('Histogram of',col),
       xlab = col,
       col = 'cyan4',
       border = 'white')
}

# Restaurar la configuración gráfica a 1x1
par(mfrow = c(1, 1))
box_quanty = boxplot(df_quanti, 
              main = 'Boxplots quantitative variables',
              col = 'cyan3')

pairs(df_quanti,  
      main = 'Pairs plot',
      pch = 19,
      col = 'lightblue')

# Evaluacion de la intercorrelacion 
# matrix de correlacion R y los 6 indices 

S = cov(df_quanti)
print(round(S, 2))

R = cor(df_quanti)
print(round(R,2))

#install.packages("corrplot")
library(corrplot)
corrplot(R, method = 'number',
         type= 'upper', 
         tl.col = 'black',
         title = 'Matrix of Correlation R',
         mar = c(0,0,1,0),
         diag = FALSE)

corrplot(R, method = 'circle',
         type= 'upper', 
         tl.col = 'black',
         title = 'Matrix of Correlation R',
         mar = c(0,0,1,0),
         diag = FALSE)

intercorrelation = function(x){
  n = nrow(x)
  p = ncol(x)
  R = cor(x)
  lambdas = eigen(R)$values
  rjj = diag(solve(R)) #diagonal of the inverse of R
  
  # q1 - Eigenvalues Spread 
  q1 = (1 - min(lambdas)/max(lambdas))^(p+2)
  
  #q2 - harmonic mean of eigenvalues 
  q2 = 1 - p/sum(1/lambdas)
  
  # Determinant - based
  q3 = 1 - sqrt(det(R))
  
  #q4 - Max eigenvalue dominance
  q4 = (max(lambdas)/p)^(3/2)
  
  #q5 - Min eigenvalue magnitude
  q5 = (1 - min(lambdas)/p)^5
  
  #q6 - Average Variance Inflation Factor 
  q6 = sum(1 - 1/rjj)/p
  
  q_vec = c(q1=q1, q2=q2, q3=q3, q4=q4, q5=q5, q6=q6)

  return(round(q_vec,4))
}

q_measures = intercorrelation(df_quanti)
q_measures

#########################################
####### PCA #########
#justificar por que es adecuado aplicar PCA basado en la intercorrelacion

#Eleccion de la matriz 
#justificar por que se esa R en lugar de S
#debido a las diferentes escalas y unidades de variables 

pca = prcomp(df_quanti, center = TRUE, scale. = TRUE)


#calculo y seleccion de componentes:
# presentar los autovalores y % de varianza explicada 

eigenvalues = pca$sdev^2
var_explained = (eigenvalues/sum(eigenvalues))*100

cumsum(var_explained)
plot(eigenvalues, type = 'b')
screeplot(pca, type = 'lines')

#Aplicar los criterios de seleccion para legir cuantos 
#componentes conservar : Kaiser, Jollifffe y el grafico de codo (Scree plot de Cattel)

#kaiser
eigenvalues > 1
sum(eigenvalues > 1)

#Jolliffe
eigenvalues > 0.7
sum(eigenvalues > 0.7)

#Frobenius knee
screeplot(pca, type = 'lines', main = 'Scree plot')
abline(h=1, col = 'red', lty = 2)

plot(eigenvalues, type = 'b', pch =19, col = 'royalblue',
     xlab = 'Number of PCA',
     ylab = 'Eigenvalues',
     main = 'knee of cattell')
abline(h = 1, col = "red", lty = 2)    # Referencia Kaiser
abline(h = 0.7, col = "orange", lty = 3) # Referencia Jolliffe

#Interpretacion de los componentes principales:
#Comentat los coeficientes (pesos) de la combiancion 
#para dar un significado practico de los nuevos ejes


# --- 4.1. Extraer la matriz de autovectores (T_tilde) ---
# 'loadings' es una matriz de 6 filas (variables) x 6 columnas (componentes)
loadings <- pca$rotation

# --- 4.2. Visualizar los coeficientes para las q componentes retenidas (ej. q = 2) ---
q <- 2
print("Matriz de Autovectores / Pesos (Loadings):")
print(round(loadings[, 1:q], 4))


# --- 4.3. Gráfico de barras para comparar los coeficientes de PC1 y PC2 ---
par(mfrow = c(1, 2)) # Dividir pantalla en 2 columnas

barplot(loadings[, 1], 
        main = "Pesos en PC1", 
        col = "steelblue", 
        las = 2, 
        ylim = c(-1, 1))
abline(h = 0, lty = 2, col = "gray")

barplot(loadings[, 2], 
        main = "Pesos en PC2", 
        col = "coral", 
        las = 2, 
        ylim = c(-1, 1))
abline(h = 0, lty = 2, col = "gray")

par(mfrow = c(1, 1)) # Restaurar pantalla


#Visualizacion en el espacio reciduos:
#Pintal el biplot o el grafico de dispersion de las dos primeras compoenetes 
# observando como se distribuyen los pacientes



# --- 5.1. Extraer las puntuaciones (scores) de los 297 pacientes ---
scores <- pca$x

# Calcular el porcentaje de varianza para etiquetar los ejes del gráfico
var_explicada <- (pca$sdev^2 / sum(pca$sdev^2)) * 100


# --- 5.2. Gráfico 2D de Pacientes: PC1 vs PC2 ---
# Clasificamos a los pacientes según su diagnóstico (0 = Sano, >0 = Enfermedad)
grupo_salud <- ifelse(data_clean$num == 0, "Sano", "Enfermo")

plot(scores[, 1], scores[, 2],
     col = ifelse(grupo_salud == "Sano", "forestgreen", "firebrick"),
     pch = 19,
     xlab = paste0("PC1 (", round(var_explicada[1], 1), "% varianza)"),
     ylab = paste0("PC2 (", round(var_explicada[2], 1), "% varianza)"),
     main = "Proyección de Pacientes (PC1 vs PC2)")

# Añadir ejes de referencia en el origen (0,0)
abline(h = 0, v = 0, lty = 2, col = "gray50")

# Leyenda explicativa
legend("topright", 
       legend = c("Sano (num = 0)", "Enfermo (num > 0)"),
       col = c("forestgreen", "firebrick"), 
       pch = 19)


# --- 5.3. Representación Biplot (Pacientes + Vectores de Variables) ---
biplot(pca,
       scale = 0,          # Mantiene la métrica del espacio
       cex = c(0.6, 0.9),  # Tamaño de etiquetas: (pacientes, variables)
       col = c("gray60", "red"),
       xlab = paste0("PC1 (", round(var_explicada[1], 1), "%)"),
       ylab = paste0("PC2 (", round(var_explicada[2], 1), "%)"),
       main = "Biplot: Pacientes y Vectores de Variables")

abline(h = 0, v = 0, lty = 2, col = "gray50")






###########################################################
########### C resultadoas, Codigo y Referencias ######
#Discutir las conclusiones numericas y graficas obtenidad
#Incluir el codigo en R o los paquetes y librerias usada
#Incluir las referencias bibliograficas empleadas





