# PCA

# Importing the dataset
dataset =newmult

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$contrata, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
training_set[-68] = scale(training_set[-68])
test_set[-68] = scale(test_set[-68])

# Applying PCA
# install.packages('caret')
library(caret)
# install.packages('e1071')
library(e1071)
pca = preProcess(x = training_set[-68], method = 'pca', pcaComp = 18)
training_set = predict(pca, training_set)
#training_set = training_set[c(2, 3, 1)]
test_set = predict(pca, test_set)
#test_set = test_set[c(2, 3, 1)]

# Fitting SVM to the Training set
# install.packages('e1071')
library(e1071)
classifier = svm(formula = contrata~ .,
                 data = training_set[1:5000,],
                 type = 'C-classification',
                 kernel = 'radial',probability=TRUE)

# Predicting the Test set results
mysample <- test_set[sample(1:nrow(test_set), 5000,
                           replace=FALSE),]
y_pred = predict(classifier, newdata = test_set,probability = TRUE)


# Making the Confusion Matrix
cm = table(test_set[, 1], y_pred)

# Visualising the Training set results
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 100)
X2 = seq(min(set[, 3]) - 1, max(set[, 3]) + 1, by = 100)
X3 = seq(min(set[, 4]) - 1, max(set[, 4]) + 1, by = 100)
X4 = seq(min(set[, 3]) - 1, max(set[, 3]) + 1, by = 100)
X5 = seq(min(set[, 5]) - 1, max(set[, 5]) + 1, by = 100)
X6 = seq(min(set[, 6]) - 1, max(set[, 6]) + 1, by = 100)
X7 = seq(min(set[, 7]) - 1, max(set[, 7]) + 1, by = 100)
X8 = seq(min(set[, 8]) - 1, max(set[, 8]) + 1, by = 100)
X9 = seq(min(set[, 9]) - 1, max(set[, 9]) + 1, by = 100)
X10 = seq(min(set[, 10]) - 1, max(set[, 11]) + 1, by = 100)
X11 = seq(min(set[, 12]) - 1, max(set[, 12]) + 1, by = 100)
X12 = seq(min(set[, 13]) - 1, max(set[, 13]) + 1, by = 100)
X13 = seq(min(set[, 14]) - 1, max(set[, 14]) + 1, by = 100)
X14 = seq(min(set[, 15]) - 1, max(set[, 15]) + 1, by = 100)
X15 = seq(min(set[, 16]) - 1, max(set[, 16]) + 1, by = 100)
X16 = seq(min(set[, 17]) - 1, max(set[, 17]) + 1, by = 100)
X17 = seq(min(set[, 18]) - 1, max(set[, 18]) + 1, by = 100)
X18 = seq(min(set[, 19]) - 1, max(set[, 19]) + 1, by = 100)
grid_set = expand.grid(X1, X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18)
colnames(grid_set) = c('PC1', 'PC2','PC3', 'PC4','PC5', 'PC6','PC7', 'PC8','PC9', 'PC10',
                       'PC11', 'PC12','PC13', 'PC14','PC15', 'PC16','PC17', 'PC18')
y_grid = predict(classifier, newdata = grid_set)
plot(set[,2:3],
     main = 'SVM (Training set)',
     xlab = 'PC1', ylab = 'PC2',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 2, 'deepskyblue', ifelse(y_grid == 1, 'springgreen3', 'tomato')))
points(set, pch = 21, bg = ifelse(set[, 1] == 0, 'blue3', ifelse(set[, 1] == 1, 'green4', 'red3')))

# Visualising the Test set results
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('PC1', 'PC2')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3], main = 'SVM (Test set)',
     xlab = 'PC1', ylab = 'PC2',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 2, 'deepskyblue', ifelse(y_grid == 1, 'springgreen3', 'tomato')))
points(set, pch = 21, bg = ifelse(set[, 3] == 2, 'blue3', ifelse(set[, 3] == 1, 'green4', 'red3')))