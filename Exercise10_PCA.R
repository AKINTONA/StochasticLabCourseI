###################################PRINCIPAL COMPONENT ANALYSIS, PCA
library(ggfortify)

##Loading the popular iris dataset
iris_data = as.data.frame(iris)
iris_data1 = iris_data[, -5]       #Discarding the specie column

##PCA with Empirical Covariance Matrix
pca_cov = prcomp(iris_data1, scale = FALSE)
summary(pca_cov)
pca_cov$rotation

##PCA with Empirical Correlation Matrix
pca_cor = prcomp(iris_data1, scale = TRUE)
summary(pca_cor)
pca_cor$rotation

##Convert Petals dataset from cm to mm
iris_data_mm = iris_data1
iris_data_mm$Petal.Length = iris_data_mm$Petal.Length * 10

##PCA with Empirical Covariance Matrix for petal length, mm
pca_cov_mm = prcomp(iris_data_mm, scale = FALSE)
summary(pca_cov_mm)
pca_cov_mm$rotation

##PCA with Empirical Correlation Matrix
pca_cor_mm = prcomp(iris_data_mm, scale = TRUE)
summary(pca_cor_mm)
pca_cor_mm$rotation


pdf("Figure101.pdf")
##Plot of the first two Principal Components
autoplot(pca_cov, data = iris_data, colour = 'Species', loadings = TRUE, loadings.colour = 'maroon',
         loadings.label = TRUE, loadings.label.size = 3)
dev.off()

##Perform KMEANS ON 1-2PC
set.seed(47)
comp = data.frame(pca_cov$x[,1:2])
k = kmeans(comp, 3)

#KMEANS ON entire dataset
set.seed(47)
k_all = kmeans(iris_data1, 3)

##Prediction Error
table(k$cluster, iris$Species)
table(k_all$cluster, iris$Species)