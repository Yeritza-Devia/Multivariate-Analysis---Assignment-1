# Names:Fabian Encarnacion 
#       Yeritza Devia
#       Lily Yang

# Libraries 
library(ggplot2)
library(corrplot)
library(e1071)
library(GGally)
library(factoextra)
library(gridExtra)
#### DATASET ####
#setwd('/Users/danielh/Documents/UC3M/Master/Second term /multivarible analysis/automobile')
#list.files()

#Assignation of the columns names
col_names = c("symboling","normalized_losses","make","fuel_type","aspiration",
              "num_of_doors","body_style","drive_wheels","engine_location","wheel_base","length",
              "width","height","curb_weight","engine_type","num_cylinders","engine_size",
              "fuel_system","bore","stroke","compression_ratio","horsepower","peak_rpm",
              "city_mpg","highway_mpg","price")
data = read.csv('imports-85.data', header = FALSE, col.names = col_names, na.strings = '?') 
dim(data)
##Data pre-processing 
vars_15 <- c(
  "wheel_base", "length", "width", "curb_weight",     
  "engine_size", "bore", "horsepower", "price",       
  "city_mpg", "highway_mpg", "peak_rpm",              
  "fuel_type", "aspiration",                          
  "body_style", "drive_wheels"                        
)
data <- data[, vars_15]
#Clean the data base to drop incomplete observation (with NA features)
data_clean = na.omit(data)
data_clean<- droplevels(data_clean)
dim(data_clean)

#Dataset specifications
#size: 195 (between 100-1000)
nrow(data_clean)
#vars: 15 (between 10-15)
ncol(data_clean)

# Select quantitative variables 
v_quanti <- c(
  "wheel_base", "length", "width", "curb_weight",
  "engine_size", "bore", "horsepower", "price",
  "city_mpg", "highway_mpg", "peak_rpm"
)
df_quanti = data_clean[,v_quanti]
# Select categorical variables
v_cat <- c("fuel_type", "aspiration", "body_style", "drive_wheels")
#Transform to factors
data_clean[v_cat] <- lapply(data_clean[v_cat], as.factor)

# Generations of  descriptive statistics 
#Summary Statistics
summary(df_quanti)
data.frame(
  Variable = v_quanti,
  Mean     = sapply(df_quanti, mean),
  SD       = sapply(df_quanti, sd),
  Median   = sapply(df_quanti, median),
  IQR      = sapply(df_quanti, IQR),
  Min      = sapply(df_quanti, min),
  Max      = sapply(df_quanti, max),
  Skewness = sapply(df_quanti, skewness)
)

#visualizations Histograms
par(mfrow = c(4,4))
for (col in names(df_quanti)) {
  hist(df_quanti[[col]],
       main = paste('Histogram of',col),
       xlab = col,
       col = 'cyan4',
       border = 'white')
}

# Restaurar la configuración gráfica a 1x1
par(mfrow = c(1, 1))
#Visualization boxPlots
box_quanty = boxplot(df_quanti, 
                     main = 'Boxplots quantitative variables',
                     col = 'cyan3')
#scale data
boxplot(scale(df_quanti), 
        main = 'Boxplots quantitative scale variables',
        col = 'cyan3')

#pairs(df_quanti,  
      #main = 'Pairs plot',
      #pch = 19,
      #col = 'lightblue')
#
#Visualize summary 
ggpairs(df_quanti,
        title = "Pairwise Scatterplot Matrix, Densities, and Correlations",
        lower = list(continuous = wrap("points", alpha = 0.45, size = 0.8, color = "#2166AC")),
        diag  = list(continuous = wrap("densityDiag", fill = "#4393C3", alpha = 0.5, color = "#2166AC")),
        upper = list(continuous = wrap("cor", size = 2.6, color = "black"))) +
  theme_bw(base_size = 8) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    strip.background = element_rect(fill = "#F0F4F8", color = "gray70"),
    strip.text = element_text(face = "bold", size = 6.5, angle = 0),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 5.5),
    axis.text.y = element_text(size = 5.5),
    panel.grid.minor = element_blank()
  ) 
#export
p_pairs <- ggpairs(df_quanti,
                   title = "Pairwise Scatterplot Matrix, Densities, and Correlations",
                   lower = list(continuous = wrap("points", alpha = 0.45, size = 0.8, color = "#2166AC")),
                   diag  = list(continuous = wrap("densityDiag", fill = "#4393C3", alpha = 0.5, color = "#2166AC")),
                   upper = list(continuous = wrap("cor", size = 2.8, color = "black"))) +
  theme_bw(base_size = 8) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 13),
    strip.background = element_rect(fill = "#F0F4F8", color = "gray70"),
    strip.text = element_text(face = "bold", size = 7),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
    axis.text.y = element_text(size = 6),
    panel.grid.minor = element_blank()
  )

ggsave("ggpairs_automobile.png", plot = p_pairs, width = 12, height = 10, dpi = 300)

##### Multicollinearity Diagnostics and Intercorrelation Indices
# Covariance matrix 
S = cov(df_quanti)
print(round(S, 2))

# Correlation Matrix
R = cor(df_quanti)
print(round(R,2))

# Correlation graph
corrplot(R, method = 'number',
         type= 'upper', 
         tl.col = 'black',
         tl.srt = 45,
         title = 'Correlation Matrix (R)',
         mar = c(0,0,1,0),
         diag = FALSE)

#corrplot(R, 
         #method = "color", 
         #type = "upper", 
         ##addCoef.col = "black", 
         #tl.col = "black", 
         ##tl.srt = 45,
         #title = "Correlation Heatmap",
         #mar = c(0, 0, 2, 0))

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

pca = prcomp(df_quanti, center = TRUE, scale. = TRUE)

summary(pca)
#Component selection:

# Eigenvalues of PCA
#eigenvalues = pca$sdev^2
get_eigenvalue(pca)
eigenvalues=get_eigenvalue(pca)$eigenvalue
#Explained variance
#var_explained = (eigenvalues/sum(eigenvalues))*100
fviz_eig(pca,
         ncp = 11,                        
         addlabels = TRUE,
         ylim = c(0, 75),                 
         barfill = "#7FB3D5",             
         barcolor = "#2980B9",            
         linecolor = "#2C3E50",           
         main = "Percentage of Explained Variance by Component",
         xlab = "Principal Components",
         ylab = "Percentage of Explained Variance (%)") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    panel.grid.minor = element_blank()
  )

fviz_pca_var(pca,
             axes = c(1, 2),
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             circle = FALSE,
             legend.title = "Contribution (%)",
             title = "PCA Variable Factor Map (PC1 vs PC2)") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12)
  )

#Cumulative Explained variance 
cum_expl_var = get_eigenvalue(pca)$cumulative.variance.percent

#kKaiser’s criterion.
eigenvalues > 1
sum(eigenvalues > 1)

#Jollife’s criterion
eigenvalues > 0.7
sum(eigenvalues > 0.7)

#Cattell’s scree graph
plot(eigenvalues, type = 'b', pch =19, col = 'royalblue',
     xlab = 'Number of PC',
     ylab = 'Eigenvalues',
     main = 'Cattell’s scree graph')
abline(h = 1, col = "red", lty = 2)    #  Kaiser
abline(h = 0.7, col = "orange", lty = 3) #  Jolliffe
legend("topright", 
       legend = c("Eigenvalues", "Kaiser (λ = 1)", "Jolliffe (λ = 0.7)"),
       col = c("royalblue", "red", "orange"), 
       lty = c(1, 2, 3), pch = c(19, NA, NA), bty = "n")


# Extractions of eigenvalues matrix (T_tilde)'loading'
loadings = pca$rotation

# Visualization of coefficient for selected components
q <- 2
print("Eigenvalues matrix / Weights (Loadings):")
print(round(loadings[, 1:q], 4))

# Contributions graph
df_load <- as.data.frame(pca$rotation[, 1:2])
df_load$Variable <- rownames(df_load)


p1 <- ggplot(df_load, aes(x = reorder(Variable, PC1), y = PC1, fill = PC1 > 0)) +
  geom_col(width = 0.7, color = "gray40", linewidth = 0.3, show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_fill_manual(values = c("TRUE" = "#7FB3D5", "FALSE" = "#F1948A")) +
  coord_flip() +
  ylim(-0.85, 0.85) +
  labs(title = "Variable Loadings on Dim 1",
       x = NULL,
       y = "Eigenvector Weight (Loading)") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 11))

p2 <- ggplot(df_load, aes(x = reorder(Variable, PC2), y = PC2, fill = PC2 > 0)) +
  geom_col(width = 0.7, color = "gray40", linewidth = 0.3, show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_fill_manual(values = c("TRUE" = "#7FB3D5", "FALSE" = "#F1948A")) +
  coord_flip() +
  ylim(-0.85, 0.85) +
  labs(title = "Variable Loadings on Dim 2",
       x = NULL,
       y = "Eigenvector Weight (Loading)") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 11))
# Unir ambos gráficos en dos columnas
grid.arrange(p1, p2, ncol = 2)



# Bar chart coefficients PC1 y PC2
#par(mfrow = c(1, 2)) # split the screen in two columns
#
#barplot(loadings[, 1], 
#        main = "Pesos en PC1", 
#        col = "steelblue", 
#        las = 2, 
#        ylim = c(-1, 1))
#abline(h = 0, lty = 2, col = "gray")
#
#barplot(loadings[, 2], 
#        main = "Pesos en PC2", 
#        col = "coral", 
#        las = 2, 
#        ylim = c(-1, 1))
#abline(h = 0, lty = 2, col = "gray")
#
#par(mfrow = c(1, 1)) 

#Cross-correlations Y1: X1, ..., X15. Y2: X1, ... , X15
cross_cor <- cor(df_quanti, pca$x[, 1:2])
colnames(cross_cor) <- c("Y1 (PC1)", "Y2 (PC2)")

# 2. Graficar con corrplot en disposición horizontal (2 filas x 11 columnas)
corrplot(t(cross_cor),
        method = "color",
        is.corr = TRUE,
        col = colorRampPalette(c("#F1948A", "#FFFFFF", "#7FB3D5"))(200),
        addCoef.col = "black",
        number.cex = 0.75,
        tl.col = "black",
        tl.srt = 45,
        cl.pos = "b",
        title = "Cross-Correlations: Principal Components (Y1, Y2) vs. Original Variables (X)",
        mar = c(0, 0, 2, 0))



#Contributions graph
#p1 <- fviz_contrib(pca, choice = "var", axes = 1, top = 6, title = "Contribución a Dim 1")
#p2 <- fviz_contrib(pca, choice = "var", axes = 2, top = 6, title = "Contribución a Dim 2")
#gridExtra::grid.arrange(p1, p2, ncol = 2)



# Extract scores
scores <- pca$x
#Plot in new coordinates
fviz_pca_biplot(pca,
                label = "var",
                repel = TRUE,
                col.var = "#B2182B",
                col.ind = "#7FB3D5",
                alpha.ind = 0.6,
                title = "PCA Biplot: Technical Variables and Automobiles (PC1 vs. PC2)",
                xlab = paste0("PC1 (", round(summary(pca)$importance[2, 1] * 100, 1), "%)"),
                ylab = paste0("PC2 (", round(summary(pca)$importance[2, 2] * 100, 1), "%)")) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12)
  )


#fviz_pca_ind(pca,
             #geom.ind = "point",       # Muestra solo los puntos (sin números de fila)
             #pointsize = 2,
             #pointshape = 19,
             #col.ind = "steelblue",    # Color único para todos los puntos
             #title = "Proyección de los 690 individuos en las nuevas coordenadas (Dim 1 vs Dim 2)")


#Stability Analysis
# 0. Extraer configuración original directamente de tu objeto 'pca' (ya centrado y escalado)
k <- 2                                      # Componentes retenidas (PC1, PC2)
n <- nrow(pca$x)                            # 197 observaciones
T_full      <- pca$rotation[, 1:k]          # Matriz T original (11 x 2)
lambda_full <- pca$sdev[1:k]^2              # Autovalores originales
var_full    <- (pca$sdev^2 / sum(pca$sdev^2))[1:k] * 100
Y_full      <- pca$x[, 1:k]                 # Configuración original Y = X T (197 x 2)

# Vectores para guardar resultados de las 197 iteraciones
eig_loo      <- matrix(NA, nrow = n, ncol = k)
var_loo      <- matrix(NA, nrow = n, ncol = k)
cos_sim      <- matrix(NA, nrow = n, ncol = k)
mean_dist_Y  <- numeric(n)
max_dist_Y   <- numeric(n)
self_shift_Y <- numeric(n)

# 1. Bucle Jack-Knife (For i = 1, ..., n)
for (i in 1:n) {
  # Eliminar la fila i del dataset cuantitativo original
  df_i <- df_quanti[-i, , drop = FALSE]
  x_i  <- df_quanti[i, , drop = FALSE]
  
  # Calcular PCA sobre X(i) con el mismo centrado y escalado que tu modelo 'pca'
  pca_i <- prcomp(df_i, center = TRUE, scale. = TRUE)
  T_i   <- pca_i$rotation[, 1:k]
  
  # Alinear signos de T(i) respecto a T_full para evitar inversiones de eje
  for (j in 1:k) {
    if (sum(T_full[, j] * T_i[, j]) < 0) {
      T_i[, j] <- -T_i[, j]
    }
    cos_sim[i, j] <- abs(sum(T_full[, j] * T_i[, j]))
  }
  
  eig_loo[i, ] <- pca_i$sdev[1:k]^2
  var_loo[i, ] <- (pca_i$sdev^2 / sum(pca_i$sdev^2))[1:k] * 100
  
  # Configuración Y(i) = X(i) T(i)
  Y_i <- pca_i$x[, 1:k]
  # Asegurar misma orientación de signos en Y(i)
  for (j in 1:k) {
    if (sum(pca$rotation[, j] * pca_i$rotation[, j]) < 0) {
      Y_i[, j] <- -Y_i[, j]
    }
  }
  
  # 3. Proyectar la fila excluida x_i' usando la media y desviación estándar de X(i): y' = x_i' T(i)
  x_i_scaled <- scale(x_i, center = pca_i$center, scale = pca_i$scale)
  y_prime    <- x_i_scaled %*% T_i
  
  # 4. Construir la configuración aumentada Y(i)* reinsertando y' en la posición i
  Y_i_star <- matrix(NA, nrow = n, ncol = k)
  Y_i_star[-i, ] <- Y_i
  Y_i_star[i, ]  <- y_prime
  
  # 5. Calcular distancias euclídeas entre Y e Y(i)*
  dists <- sqrt(rowSums((Y_full - Y_i_star)^2))
  mean_dist_Y[i]  <- mean(dists)
  max_dist_Y[i]   <- max(dists)
  self_shift_Y[i] <- dists[i]
}

# ==============================================================================
# TABLA RESUMEN DE ESTABILIDAD (Lista para copiar al paper)
# ==============================================================================
stability_summary <- data.frame(
  Metric = c("PC1 Eigenvalue (lambda_1)",
             "PC2 Eigenvalue (lambda_2)",
             "PC1 Explained Variance (%)",
             "PC2 Explained Variance (%)",
             "PC1 Eigenvector Cosine Similarity",
             "PC2 Eigenvector Cosine Similarity",
             "Mean Euclidean Shift ||Y - Y(i)*||",
             "Max Euclidean Shift ||Y - Y(i)*||"),
  Full_Sample = round(c(lambda_full[1], lambda_full[2], var_full[1], var_full[2], 1, 1, 0, 0), 4),
  LOO_Mean    = round(c(mean(eig_loo[, 1]), mean(eig_loo[, 2]), mean(var_loo[, 1]), mean(var_loo[, 2]),
                        mean(cos_sim[, 1]), mean(cos_sim[, 2]), mean(mean_dist_Y), mean(max_dist_Y)), 4),
  LOO_Min     = round(c(min(eig_loo[, 1]), min(eig_loo[, 2]), min(var_loo[, 1]), min(var_loo[, 2]),
                        min(cos_sim[, 1]), min(cos_sim[, 2]), min(mean_dist_Y), min(max_dist_Y)), 4),
  LOO_Max     = round(c(max(eig_loo[, 1]), max(eig_loo[, 2]), max(var_loo[, 1]), max(var_loo[, 2]),
                        max(cos_sim[, 1]), max(cos_sim[, 2]), max(mean_dist_Y), max(max_dist_Y)), 4)
)
print(stability_summary)

# ==============================================================================
# GRÁFICOS JACK-KNIFE (Paleta pastel a juego con el resto del paper)
# ==============================================================================
df_jack <- data.frame(
  Observation = 1:n,
  Mean_Dist   = mean_dist_Y,
  Self_Shift  = self_shift_Y,
  Var_PC1     = var_loo[, 1],
  Var_PC2     = var_loo[, 2]
)

p_stab1 <- ggplot(df_jack, aes(x = Observation, y = Mean_Dist)) +
  geom_line(color = "#7FB3D5", linewidth = 0.6) +
  geom_point(color = "#2980B9", size = 1.4, alpha = 0.8) +
  geom_hline(yintercept = mean(mean_dist_Y) + 2 * sd(mean_dist_Y),
             linetype = "dashed", color = "#F1948A", linewidth = 0.8) +
  labs(title = "Jack-Knife Configuration Discrepancy: Mean Euclidean Distance ||Y - Y(i)*||",
       x = "Omitted Observation Index",
       y = "Mean Euclidean Distance") +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10.5))

p_stab2 <- ggplot(df_jack, aes(x = Observation)) +
  geom_line(aes(y = Var_PC1, color = "PC1 Variance (%)"), linewidth = 0.7) +
  geom_line(aes(y = Var_PC2, color = "PC2 Variance (%)"), linewidth = 0.7) +
  scale_color_manual(values = c("PC1 Variance (%)" = "#7FB3D5", "PC2 Variance (%)" = "#F1948A")) +
  ylim(0, 80) +
  labs(title = "Leave-One-Out Explained Variance Stability (PC1 & PC2)",
       x = "Omitted Observation Index ",
       y = "Explained Variance (%)",
       color = NULL) +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10.5),
        legend.position = "bottom")

grid.arrange(p_stab1, p_stab2, ncol = 1)
