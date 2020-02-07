## Install packages
#install.packages("RColorBrewer")
#install.packages("tidyverse")
#install.packages("ggsignif")
#install.packages("ggfortify")
#install.packages("tidyverse")

## Set Working Directory
setwd("C:/")

#####__________________ Data upload/Understanding the data __________________________###
data(iris) #load the data
head(iris) #check the first few rows 
ncol(iris) # number of colums
nrow(iris) # number of rows
dim(iris) # dimensions
sapply(iris, class) # check the class
summary(iris) # Summary statistics


######__________________ Scatterplot __________________________###
plot(iris$Petal.Length ~ iris$Sepal.Length) # if you don't know what the function does: ?plot

# Assign colors
plot(Petal.Length ~ Sepal.Length, data = iris, col = iris$Species, pch = 19) # col means color
legend(x = 4.5, y = 7, legend = levels(iris$Species), col = c(1:3), pch = 19)

# Save as pdf
pdf("iris1.pdf") # open the pdf
plot(Petal.Length ~ Sepal.Length, data = iris, col = iris$Species, pch = 19)
legend(x = 4.5, y = 7, legend = levels(iris$Species), col = c(1:3), pch = 19)
dev.off() # close the pdf 

# Use different colors
library(RColorBrewer)
display.brewer.all(n=3)
plot(Petal.Length ~ Sepal.Length, data = iris, col=brewer.pal(3, "Set2")[iris$Species], pch = 19)
legend(x=4.5, y=7, legend=levels(iris$Species), col=brewer.pal(3, "Set2"), pch=19)

# ggplot2 for more advanced graphics
library(ggplot2)
library(ggsignif)
scatter <- ggplot(data=iris, aes(x = Sepal.Length, y = Petal.Length)) 
scatter + geom_point(aes(color=Species)) +
  xlab("Sepal Length") +  ylab("Sepal Width") +
  ggtitle("Sepal Length-Width") 

# Add a third feature by assigning different colors and shapes
scatter + geom_point(aes(color = Petal.Width, shape = Species), size = 2, alpha = I(1/2)) +
  geom_vline(aes(xintercept = mean(Sepal.Length)), color = "red", linetype = "dashed") +
  geom_hline(aes(yintercept = mean(Sepal.Width)), color = "red", linetype = "dashed") +
  scale_color_gradient(low = "yellow", high = "red") +
  xlab("Sepal Length") +  ylab("Sepal Width") +
  ggtitle("Sepal Length-Width")



######__________________ Boxplot __________________________###
# Simple boxplot for sepal length
box <- ggplot(data=iris, aes(x=Species, y=Sepal.Length))
box + geom_boxplot(aes(fill=Species)) + 
  ylab("Sepal Length") + ggtitle("Iris Boxplot") +
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) 

# Boxplots for all features
library(tidyverse)
iris %>%
  gather(feature, value, 1:4) %>%
  ggplot(aes(x=Species, y=value))+ 
  geom_boxplot(aes(fill=Species)) + 
  facet_wrap( ~feature, scales = "free")+
  ylab("Measurements") + ggtitle("Iris Boxplot") 


######__________________ Statistical Analysis __________________________###

### Data distribution
ggplot(iris, aes(x=Sepal.Width)) +
  geom_density(aes(group=Species, color=Species, fill=Species), alpha=0.3)

### t test 
setosa <- iris[iris$Species == "setosa", ]
versicolor <- iris[iris$Species == "versicolor", ]
t.test(x = setosa$Petal.Length, y = versicolor$Petal.Length)
TwoSpecies <- rbind(setosa, versicolor)

# t-test in Boxplot
ggplot(data=TwoSpecies, aes(x=Species, y=Sepal.Length))+ 
  geom_boxplot(aes(fill=Species)) + 
  ylab("Sepal Length") + ggtitle("Iris Boxplot") +
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) + 
  geom_signif(comparisons = list(c("setosa", "versicolor")), test=t.test)

### Anova
aov <- aov(formula = Sepal.Width ~ Species, data = iris)
summary(object = aov)
# Multiple comparisons
TukeyHSD(aov)

### Pairwise t-test
pairwise.t.test(iris$Sepal.Width, iris$Species, 
                p.adj="bonferroni", paired=FALSE)