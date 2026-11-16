# Names:Fabian Encarnacion 
#       Yeritza Devia
#       Lily Yang

# Libraries 
library(ggplot2)
library(corrplot)
library(e1071)
library(GGally)
library(factoextra)
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
        main = 'Boxplots quantitative variables',
        col = 'cyan3')

#pairs(df_quanti,  
      #main = 'Pairs plot',
      #pch = 19,
      #col = 'lightblue')
#
#Visualize summary 
ggpairs(df_quanti) 


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
         addlabels = TRUE, 
         ylim = c(0, 45),
         barfill = "#2166AC", 
         barcolor = "#2166AC",
         main = "Scree Plot - Varianza Explicada por Componente",
         xlab = "Componentes Principales",
         ylab = "% de Varianza Explicada")

fviz_pca_var(pca,
             axes = c(1, 2),
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             title = "Círculo de Correlaciones (PC1 vs PC2)")

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


# Bar chart coefficients PC1 y PC2
par(mfrow = c(1, 2)) # split the screen in two columns

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

par(mfrow = c(1, 1)) 

#Cross-correlations Y1: X1, ..., X15. Y2: X1, ... , X15
cross_cor <- cor(df_quanti, pca$x[, 1:2])
colnames(cross_cor) <- c("Y1 (PC1)", "Y2 (PC2)")

# 2. Graficar con corrplot en disposición horizontal (2 filas x 11 columnas)
corrplot(t(cross_cor),
         method = "color",
         is.corr = TRUE,
         col = colorRampPalette(c("#B2182B", "white", "#2166AC"))(200),
         addCoef.col = "black",
         number.cex = 0.75,
         tl.col = "black",
         tl.srt = 45,
         cl.pos = "b",
         title = "Correlaciones cruzadas: Componentes (Y1, Y2) vs Variables (X)",
         mar = c(0, 0, 2, 0))



#Contributions graph
p1 <- fviz_contrib(pca, choice = "var", axes = 1, top = 6, title = "Contribución a Dim 1")
p2 <- fviz_contrib(pca, choice = "var", axes = 2, top = 6, title = "Contribución a Dim 2")
gridExtra::grid.arrange(p1, p2, ncol = 2)



# Extract scores
scores <- pca$x
#Plot in new coordinates
fviz_pca_biplot(pca,
                label = "var",
                repel = TRUE,               # Evita que los nombres de las 11 variables se pisen
                col.var = "#B2182B",        # Flechas en rojo oscuro para contrastar
                col.ind = "gray60",         # Puntos de los 197 vehículos en gris suave
                alpha.ind = 0.6,
                title = "PCA Biplot: Variables Técnicas y Automóviles (PC1 vs PC2)") +
  theme_minimal()


#fviz_pca_ind(pca,
             #geom.ind = "point",       # Muestra solo los puntos (sin números de fila)
             #pointsize = 2,
             #pointshape = 19,
             #col.ind = "steelblue",    # Color único para todos los puntos
             #title = "Proyección de los 690 individuos en las nuevas coordenadas (Dim 1 vs Dim 2)")


#Stability Analysis
