######__________________ Unsupervised learning __________________________###

### Clustering
# Hierarchical clustering
d <- dist(iris[, 1:4])
hcl <- hclust(d)
hcl
plot(hcl)

### Heatmap
X = as.matrix(iris[,-5])
heatmap(t(X), hclustfun = hclust, main = "Heatmap Iris")


### Dimensionality reduction
# Visualize all 3 species and all 4 features:
pairs(iris[, -5], col = iris[, 5], pch = 19)

# PCA
library(ggfortify)
irispca <- prcomp(iris[, -5])
summary(irispca)
biplot(irispca)
autoplot(irispca, data = iris, colour = 'Species')

