# Names:Fabian Encarnacion 
#       Yeritza Devia
#       Lily Yang

# Libraries 
#install.packages("corrplot")
library(ggplot2)
library(corrplot)

setwd('/Users/danielh/Documents/UC3M/Master/Second term /multivarible analysis/automobile')
list.files()

#Assignation of the columns names
col_names = c("symboling","normalized_losses","make","fuel_type","aspiration",
              "num_of_doors","body_style","drive_wheels","engine_location","wheel_base","length",
              "width","height","curb_weight","engine_type","num_cylinders","engine_size",
              "fuel_system","bore","stroke","compression_ratio","horsepower","peak_rpm",
              "city_mpg","highway_mpg","price")

# Read the database
data = read.csv('imports-85.data', header = FALSE, col.names = col_names, na.strings = '?') 
dim(data)

#Clean the data base to drop incomplete observation (with NA features)
data_clean = na.omit(data)
dim(data_clean)

# Dataset with only quantitative variables 
v_quanti = c("normalized_losses", "wheel_base","length","width",
             "height","curb_weight","engine_size","bore", "stroke", 
             "compression_ratio", "horsepower","peak_rpm", "city_mpg",
             "highway_mpg","price")
df_quanti = data_clean[,v_quanti]

dim(df_quanti)

# Generations of  descriptive statistics 
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
box_quanty = boxplot(df_quanti, 
              main = 'Boxplots quantitative variables',
              col = 'cyan3')

pairs(df_quanti,  
      main = 'Pairs plot',
      pch = 19,
      col = 'lightblue')

##### Inter correlation assessment
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


#Component selection:

# Eigenvalues of PCA
eigenvalues = pca$sdev^2

#Explained variance
var_explained = (eigenvalues/sum(eigenvalues))*100

#Cumulative Explained variance 
cum_expl_var = cumsum(var_explained)

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
q <- 3
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


# Extract scores
scores <- pca$x

#=====================================================================================
# CATERGORICAL VARIABLES GRAPHS & PCA CATERGORICAL GRAPHS
#=====================================================================================

# Graphing catergorical variables into barplot
cat_var = data_clean[,c("fuel_type","aspiration","body_style","drive_wheels")] # creating a small table with catergorical variables

par(mfrow = c(2,3)) # splitting the plot window into grids to see all the graphs

for(v in names(cat_var)) barplot(table(cat_var[[v]]), main = v, col = "steelblue") # Iterating through each variable 

# Bar plot of Aspiration vs Fuel type
ggplot(data_clean, aes(x = fuel_type, fill = aspiration)) + 
  geom_bar(position = "fill") +
  labs(title = "Aspiration by Fuel type", x = "Fuel type", y = "Proportion", fill = "Aspiration") + 
  scale_fill_discrete(labels = c("std" = "Standard", "turbo" = "Turbo"))

# Bar plot of Drive by wheels vs Body type
ggplot(data_clean, aes(x = body_style, fill = drive_wheels)) + 
  geom_bar(position = "fill") +
  labs(title = "Drive Wheels by Body type", x = "Body type", y = "Proportion", fill = "Drive wheels") + 
  scale_fill_discrete(labels = c("4wd" = "4 Wheel Drive", "fwd" = "Front Wheel Drive", "rwd" = "Rear Wheel Drive"))

# PCA scores coloured by categorical variables
fviz_pca_ind(pca, geom = "point",
             habillage = data_clean$body_style,
             addEllipses = TRUE, ellipse.type = "confidence",
             title = "PCA scores by body style")

fviz_pca_ind(pca, geom = "point",
             habillage = data_clean$drive_wheels,
             addEllipses = TRUE, ellipse.type = "confidence",
             title = "PCA scores by drive wheels")

fviz_pca_ind(pca, geom = "point",
             habillage = data_clean$fuel_type,
             addEllipses = TRUE, ellipse.type = "confidence",
             title = "PCA scores by fuel type")

fviz_pca_ind(pca, geom = "point",
             habillage = data_clean$aspiration,
             addEllipses = TRUE, ellipse.type = "confidence",
             title = "PCA scores by aspiration")

# PCA scores coloured by categorical variables this is all 4 graphs in one figure
cat_plots <- lapply(v_cat, function(v) {
  fviz_pca_ind(pca, geom = "point",
               habillage = data_clean[[v]],
               addEllipses = TRUE, ellipse.type = "confidence",
               title = paste("PCA scores by", v))
})
grid.arrange(grobs = cat_plots, ncol = 2)

# Saves the 4 graph figure 
ggsave("pca_by_category.png", arrangeGrob(grobs = cat_plots, ncol = 2),
       width = 12, height = 10, dpi = 300)


