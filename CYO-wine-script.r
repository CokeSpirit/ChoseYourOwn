# Choose your own! Wines
# Script to accompany the final report on my Capstone project
# Author: Christian Mast
# Date: 23.6.2020



# Date Source:
# Original Owners:  
#   Forina, M. et al, PARVUS -
#   An Extendible Package for Data Exploration, Classification and Correlation.
#   Institute of Pharmaceutical and Food Analysis and Technologies, Via Brigata Salerno, 16147 Genoa, Italy
#   
#   Donor:  
#   Stefan Aeberhard, email: stefan '@' coral.cs.jcu.edu.au



# Installing packages if required
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(Rborist)) install.packages("Rborist", repos = "http://cran.us.r-project.org")
if(!require(kernlab)) install.packages("kernlab", repos = "http://cran.us.r-project.org")



# Getting the data
# Define the urls
wine_data_url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data"
wine_names_url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.names"

#Download and rename files
download.file(wine_data_url, ".\\wine-data.csv")
download.file(wine_names_url, ".\\wine-names.txt")

#Creating a data frame from wine-data.csv
wines <- read.csv(".\\wine-data.csv", header=FALSE, sep=",")

#Adding column names - for the ease of life, this is done manually
#"OD280/OD315 of diluted wines" was shortened to OD280.OD315
#"Nonflavanoid phenols" was shortened to Nonflav. phenols
#Slashes and spaces were removed to make rpart happy - thx to Stackoverflow for the tip!
colnames(wines) <- c("Winery", "Alcohol", "Malic_acid",
                     "Ash", "Alcalinity_of_ash", "Magnesium",
                     "Total_phenols", "Flavanoids",
                     "Nonflav._phenols", "Proanthocyanins",
                     "Color_intensity", "Hue",
                     "OD280.OD315", "Proline")



#Show the first 6 rows of the data frame
head(wines)



#Show Min, 1st Quartile, Median, Mean, 3rd quartile and max of wines
summary(wines)



# Create a grid of histogramms
gather(wines, variable, value) %>% 
  ggplot (aes(value)) +
  geom_histogram(bins=18)+
  facet_wrap(~variable, ncol=4, scales="free")+
  ggtitle("Histograms of variables in 'wines' data frame")+
  xlab("Values")+
  ylab("Count")



#Removing winery, then calling corrplot()
wines %>% subset(select=-Winery) %>% 
  cor(.) %>% 
  corrplot(tl.col="black",
           tl.cex=0.7,
           number.cex=0.6,
           title="Correlation Plot",
           type="lower",
           addCoef.col="black",mar=c(0,0,1,0))



#scaling wines, then rebuilding the winery column
scaled_wines <- as.data.frame(scale(wines))
scaled_wines$Winery <- as.factor(wines$Winery)


# Set a seed to get reproducible train & test sets
set.seed(101, sample.kind = "Rounding")
index <- createDataPartition(scaled_wines$Winery, p = 0.5, list = FALSE)
train_set <- scaled_wines[-index, ]
test_set <- scaled_wines[+index, ]



# Train a knn model
fit_knn <- train(Winery ~.,
                 method="knn",
                 tuneGrid = data.frame(k=seq(3,31,2)),
                 data = train_set)

# Output the fit results
fit_knn

# Plot the fit results to show the best k
plot(fit_knn)

# Create a prediction, calculate the confusion matrix and print the accuracy
prediction_knn <- predict(fit_knn, test_set)
cm_knn <- confusionMatrix(prediction_knn, test_set$Winery)
cat("Accuracy of the knn model:", cm_knn$overall["Accuracy"],"\n\n")



# Train a rf model
fit_rpart <- train(Winery ~.,
                   method="rpart",
                   tuneGrid = data.frame(cp = seq(0.0, 0.05, len = 16)),
                   data = train_set)

# Output the fit results
fit_rpart

# Plot the fit results to show the best k
plot(fit_rpart)

# Create a prediction, calculate the confusion matrix and print the accuracy
prediction_rpart <- predict(fit_rpart, test_set)
cm_rpart <- confusionMatrix(prediction_rpart, test_set$Winery)
cat("Accuracy of the rpart model:", cm_rpart$overall["Accuracy"],"\n\n")



#create a plot of the classification tree and add text labels
plot(fit_rpart$finalModel, margin = 0.1, main="Classification tree")
text(fit_rpart$finalModel, cex = 0.75, pos=3, offset=0)


#Create a randomForest fit and plot it to show the number of trees needed later on
fit_rf <- randomForest(Winery~., data = train_set) 
plot(fit_rf, main="Error of randomForest vs number of trees")



# Train a rborist model
fit_rb <- train(Winery ~.,
                method="Rborist",
                tuneGrid = data.frame(predFixed=2, minNode=seq(3,9,1)),
                nTree=100,   
                data = train_set)

# Output the fit results
fit_rb

# Plot the fit results to show the best k
plot(fit_rb)

# Create a prediction, calculate the confusion matrix and print the accuracy
prediction_rb <- predict(fit_rb, test_set)
cm_rb <- confusionMatrix(prediction_rb, test_set$Winery)
cat("Accuracy of the Rborist model:", cm_rb$overall["Accuracy"],"\n\n")



# Train a "Support Vector Machine with linear Kernel" model
fit_svml <- train(Winery ~.,
                  method="svmLinear",
                  tuneGrid = expand.grid(C = c(0.001, 0.01, 0.1, 1, 10)),
                  data = train_set)

# Output the fit results
fit_svml

# Create a prediction, calculate the confusion matrix and print the accuracy
prediction_svml <- predict(fit_svml, test_set)
cm_svml <- confusionMatrix(prediction_svml, test_set$Winery)
cat("Accuracy of the svmLinear model:", cm_svml$overall["Accuracy"],"\n\n")



#Convert predictions to numbers, calculate the average
prediction_ensemble <- as.factor(round((
  as.numeric(prediction_knn)+
    as.numeric(prediction_rb)+
    as.numeric(prediction_svml))/3))

# Calculate the confusion matrix and print the accuracy
cm_ensemble <- confusionMatrix(prediction_ensemble, test_set$Winery)
cat("Accuracy of the Ensemble:", cm_ensemble$overall["Accuracy"],"\n\n")




