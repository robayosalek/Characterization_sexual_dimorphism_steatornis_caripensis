################################################################################
#                       Data analysis of oil-bird's data set                   #
#                                 Andres F. Robayo                             #
#                                 Adriana Maldonado                            #
#                                  Version: 10.2                               #
#                                Date: 25/05/2022                              #
################################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Clean memory and remove all objects
rm(list=ls())
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Install and load libraries

# install.packages("factoextra")
# install.packages("FactoMineR")
# install.packages("readxl")
# install.packages("ggpubr")
# install.packages("moments")
# install.packages("MASS")
# install.packages("tidyverse")
# install.packages("caret")
# install.packages("klaR")
# install.packages("lme4")
# install.packages("car")
# install.packages("arsenal")
# install.packages("dplyr")
# install.packages("ISLR")


library(factoextra)
library(ggplot2)
library(readxl)
library(FactoMineR)
library(ggpubr)
library(moments)
library(MASS)
library(tidyverse)
library(caret)
library(klaR)
library(lme4)
library(car)
library(arsenal)
library(dplyr)
library(ISLR)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Load data set
#
# Andres
data <- read_excel("C:/Users/piper/Dropbox/AndresRobayo/Tesis/medidas_morfometricas_oilbirds/base_datos/datos_.xlsx")

# Adriana
data <- read_excel("/Users/Adriana/Dropbox/Work/UR/Mentoring/2022/AndresRobayo/Tesis/medidas_morfometricas_oilbirds/base_datos/datos_.xlsx")
data

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Morphometric analysis
#
# There cannot be text to run a PCA analysis, because this analysis only work with
# quantitative variables. By that you have to remove the qualitative or descriptive
# variables that use text in the data.
# remove -> notes, location_country, age_category y museum_name
#
erase_text <- c("museum_name", "individual_id", "location", "age_category", "notes", "bill_width", "bill_height", "visible_nm_peak", "all_nm_peak")
morphometric_data <- data[, !(names(data) %in% erase_text)]
morphometric_data # optional

# Remove rows with incomplete data 
# (https://stackoverflow.com/questions/4862178/remove-rows-with-all-or-some-nas-missing-values-in-data-frame)
morphometric_data_purify <- morphometric_data [complete.cases(morphometric_data), ]
morphometric_data_purify # optional

# Delete "sex" column to perform the PCA
erase_text <- c("sex")
morphometric_data_pca <- morphometric_data_purify[, !(names(morphometric_data_purify) %in% erase_text)]
morphometric_data_pca # optional
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Descriptive analysis
#
# Do not confuse, we are going back to the PCA analysis briefly, we just take 
# advantage from the data created previously. With that run some descriptive
# analysis.
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Histograms
#
# With these graphics you could see how that variable behave in the population
#
# Histogram for the bill length
hist(morphometric_data_purify$bill_length_from_nostrill)
# Now we do the same histogram but separating the sexes
# Female
hist(morphometric_data_purify[morphometric_data_purify$sex=="F",]$bill_length_from_nostrill, 15)
# Male
hist(morphometric_data_purify[morphometric_data_purify$sex=="M",]$bill_length_from_nostrill, 15)

# Histogram for the flattened wing length
hist(morphometric_data_purify$wing_length_flattened, 15) # We see a negative skew, 
# that could indicate differentiation between the sexes
# Let's see what is the value of that skew
skewness(morphometric_data_purify$wing_length_flattened)

# Histogram for the tail length
hist(morphometric_data_purify$tail_length, 15)

# Histogram for the tarsus length
hist(morphometric_data_purify$tarsus_length, 15) # We see a positive skew
skewness(morphometric_data_purify$tarsus_length)

# Histograms for ICN and Humboldt (bill height and width)
#
# With these data we don´t use the purified one, because in that we erased these
# information. We did that because there are not many obs, and if we add that data
# in the purify data when we use the function to remove rows with incomplete cases
# the total obs became less than the half. That is the reason of why we use different 
# data set to work with these information (and for future references, the 
# same happens with the color data).
#
hist(data$bill_height, 15)
hist(data$bill_width, 15)

#end Histograms
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Box plots
#
# To create the box plots and don't depend over other data, we need a new data set,
# in this new data set you will do the box plots of the morphological measurements.
# For reasons regarding the measurements it is need to do two different data sets,
# the first is for do the box plots of: flattened wing, tarsus, tail and bill length,
# the second data is for do the box plots of: color (UV and visible), bill height 
# and width.
#
# Data 1
erase_text <- c("museum_name", "individual_id", "location", "age_category", "notes", "bill_width", "bill_height", "visible_nm_peak", "all_nm_peak")
data_b_1 <- data[, !(names(data) %in% erase_text)]
data_b_1 # optional

# Remove rows with incomplete data 
data_b_2 <- data_b_1 [complete.cases(data_b_1), ]
data_b_2 # optional

# Data 2
erase_text <- c("museum_name", "individual_id", "location", "age_category", "notes", "bill_length_from_nostrill","wing_length_flattened", "tail_length", "tarsus_length")
data_b_3 <- data[, !(names(data) %in% erase_text)]
data_b_3 # optional

# Remove rows with incomplete data 
data_b_4 <- data_b_3 [complete.cases(data_b_3), ]
data_b_4 # optional

# Flattened wing
flattened_b <- ggplot(data_b_2, aes(x = sex, y = wing_length_flattened, 
                                  fill = sex)) +  geom_boxplot() + labs(y = "Flattened wing length (mm)", x = "Sex") + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  
print(flattened_b) # optional

# Bill length from nostril
bill_b <- ggplot(data_b_2, aes(x = sex, y = bill_length_from_nostrill, 
                             fill = sex)) +  geom_boxplot() + labs(y = "Bill length from nostril (mm)", x = "Sex") + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
print(bill_b) # optional

# Tail length
tail_b <- ggplot(data_b_2, aes(x = sex, y = tail_length, 
                             fill = sex)) +  geom_boxplot() + labs(y = "Tail length (mm)", x = "Sex") + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

print(tail_b) # optional

# Tarsus length 
tarsus_b <- ggplot(data_b_2, aes(x = sex, y = tarsus_length, 
                               fill = sex)) +  geom_boxplot() + labs(y = "Tarsus length (mm)", x = "Sex") + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

print(tarsus_b) # optional

#
# Box plot for ICN and Humboldt
#
# Bill height
b_h_b <- ggplot(data_b_4, aes(x = sex, y = bill_height, 
                              fill = sex)) +  geom_boxplot() + labs(y = "Bill heigth (mm)", x = "Sex") + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

print(b_h_b) # optional

# Bill width
bill_w_b <- ggplot(data_b_4, aes(x = sex, y = bill_width, 
                                 fill = sex)) +  geom_boxplot() + labs(y = "Bill width (mm)", x = "Sex") + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  
print(bill_w_b) # optional

#
# Color box plot
#
# Visible nm peak
visible_b <- ggplot(data_b_4, aes(x = sex, y = visible_nm_peak, 
                                  fill = sex)) +  geom_boxplot() + labs(y = "Visible nm (mm)", x = "Sex") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

print(visible_b) # optional

# All nm peak
all_b <- ggplot(data_b_4, aes(x = sex, y = all_nm_peak, 
                              fill = sex)) +  geom_boxplot() + labs(y = "UVC nm (mm)", x = "Sex") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

print(all_b) # optional

#
# Plot multiple box plots in the same page
#
# The code we are going to use is to plot multiple graphs in a same page
#
# For plot the eight box plots that we made before
ggarrange(tail_b, tarsus_b, bill_b, flattened_b, b_h_b, bill_w_b, visible_b, all_b,
          labels = c("A", "B", "C", "D", "E", "F", "G", "H"), ncol = 3, nrow = 3)

# end box plots
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# PCA
#
# Principal component analysis (PCA) simplifies the complexity in high-dimensional 
# data while retaining trends and patterns. It does this by transforming the data 
# into fewer dimensions, which act as summaries of features.
#
# Change the name of the columns
# We do this because we want that in the plot the labels be nicer
colnames(morphometric_data_pca) <- c("Bill length from nostrill","Wing length flattened", "Tail length", "Tarsus length")
morphometric_data_pca #optional

# PCA using the package "FactoMine"
res.pca <- PCA(morphometric_data_pca, scale.unit = TRUE, graph = TRUE)
summary(res.pca)

# end PCA
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# PCA with discrimination
#
# In this part we are going to do the same PCA, but discriminating between the sexes.
# We use the "purify" data set because contain the variable sex, that we erase in
# the "pca" data set for being a text character.
#
# Change the name of the columns
colnames(morphometric_data_purify) <- c("Sex","Bill length from nostrill","Wing length flattened", "Tail length", "Tarsus length")
morphometric_data_purify # optional

# Convert sex to factor
group <- as.factor(morphometric_data_purify$Sex[1:length(morphometric_data_purify$Sex)])
group # optional

# Add cloud to data, id by sex
fviz_pca_biplot(res.pca, title = "",
                col.ind = morphometric_data_purify$Sex, palette = c("#FC4E07", "#00AFBB"), 
                addEllipses = TRUE, label = "var",
                col.var = "black", repel = FALSE,
                legend.title = "Sexos")


#end PCA with discrimination
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# PCA  with color
#
# As it was mentioned before some variables and data set have to be manipulated
# separate. So in this part we are going to do the PCA but wit color data.
#
# Erase the text variables, in addition of the variables we do not need
erase_text <- c("museum_name", "individual_id", "location", "age_category", "notes","bill_length_from_nostrill","wing_length_flattened", "tail_length", "tarsus_length", "bill_height", "bill_width")
pca_color <- data[, !(names(data) %in% erase_text)]
pca_color # optional

# To remove the rows that contains incomplete measurements
pca_color_purify <- pca_color [complete.cases(pca_color), ]
pca_color_purify # optional

# Remove the text variable "sex" to run the PCA
erase_text <- c("sex")
pca_color_ws <- pca_color_purify[, !(names(pca_color_purify) %in% erase_text)]
pca_color_ws # optional


# Change the name of the columns
colnames(pca_color_ws) <- c("Visible nm peak", "UV nm peak")
pca_color_ws # optional

res.pca_color <- PCA(pca_color_ws, scale.unit = TRUE, graph = TRUE) # Do not worry for the warnings
summary(res.pca_color) # optional

#
# Now we do the PCA with the sexes discrimination
#
# Change the name of the columns
colnames(pca_color_purify) <- c("Sex", "Visible nm peak", "All nm peak")
pca_color_purify # optional

# Convert sex to factor
group <- as.factor(pca_color_purify$Sex[1:length(pca_color_purify$Sex)])
group # optional

# Add cloud to data, id by sex
fviz_pca_biplot(res.pca_color, title = "",
                col.ind = pca_color_purify$Sex, palette = c("#FC4E07", "#00AFBB"), 
                addEllipses = TRUE, label = "var",
                col.var = "black", repel = T,
                legend.title = "Sexos")

# end PCA with color
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# QDA 
#
# The QDA is a discriminant analysis used to predict the probability of belonging
# to a given class (or category) based on one or multiple predictor variables. 
# It works with continuous and/or categorical predictor variables.
# For more information visit the link below:
#(http://www.sthda.com/english/articles/36-classification-methods-essentials/146-discriminant-analysis-essentials-in-r/)
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#
# To start we have to train the model, because he cannot predict what he doesn't know
#
#Training part

# create the training data set
erase_text <- c("museum_name", "location", "age_category", "notes", "bill_width", "bill_height", "visible_nm_peak","all_nm_peak")
qda_training_data <- data[, !(names(data) %in% erase_text)]

# Delete the rows with incomplete data
qda_training_data_purify <- qda_training_data [complete.cases(qda_training_data), ]

#
# Subset data for training (80% training data, 20% test data)
# 
# Number of observations
nbobs <- length(qda_training_data_purify$sex)

# Calculate the 80% of the data set
datas <- ceiling(nbobs * 0.8)

# Randomly sample XX rows from data for training
set.seed(12345) # Set seed for reproducibility
data_train_qda <- qda_training_data_purify[sample(1:nrow(qda_training_data_purify), datas), ]  # Sample rows of data with Base R
data_train_qda  # optional

# Estimate pre-processing parameters
preproc.param <- data_train_qda %>% 
  preProcess(method = c("center", "scale"))

# Transform the data using the estimated parameters
train.transformed_qda <- preproc.param %>% predict(data_train_qda)

# Fit the model
# This is going to be the training model
model <- qda(sex ~ scale(bill_length_from_nostrill)+scale(wing_length_flattened)+scale(tail_length)+scale(tarsus_length), data = train.transformed_qda)
model

#
# Test part
#
# In this part we have to create a data set that contains the 20% remaining in the data.
# To know what individuals were put in the training data we use something that every
# single individual have, the individual_id. With that we will know if that individual
# is in the train data or not
#
# Create the test data set
data_test_qda <- anti_join(qda_training_data_purify, data_train_qda, by = c("individual_id"))
data_test_qda # optional

#Transform test data
test.transformed_qda <- preproc.param %>% predict(data_test_qda)

#
# Now we are going to do predictions in the test data, using the model we made 
# with the training data. Because the model in this moment know what to do. 
#
#Make predictions
(predictions <- model %>% predict(test.transformed_qda))


#
# We are going to see how accurate is the model we made, regarding the 
# predictions.
#
# Model accuracy
mean(predictions$class == test.transformed_qda$sex)

#
# Finally we are going to compare the predictions with the reality
#
# Create a new column with the predictions
data_test_qda$sex_predictions <- NA
data_test_qda # optional

# Fill the column that you create before
data_test_qda$sex_predictions <- predictions$class
data_test_qda

# Create a new column with the sexes comparison
data_test_qda$compare_sex <- NA

# Fill the column that you create before
# In this column you have to say if the prediction is right (1) or not (0)
for (cc in 1:length(data_test_qda$compare_sex)) {
  data_test_qda[cc,]$compare_sex <- ifelse(data_test_qda[cc,]$sex == data_test_qda[cc,]$sex_predictions, 1, 0)
}

#end QDA
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#QDA with color values
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Training part with color

# Create the training data set
borrar_texto <- c("museum_name", "location", "age_category", "notes", "bill_length_from_nostrill", "wing_length_flattened", "tail_length", "tarsus_length", "bill_width", "bill_height")
data_f_color <- data[, !(names(data) %in% borrar_texto)]

# Delete the rows with incomplete data
data_z_color <- data_f_color [complete.cases(data_f_color), ]

#
# Subset data for training (80% training data, 20% test data)
# 
# Number of observations
nbobs <- length(data_z_color$sex)

# Calculate the 80% of the data set
datas <- ceiling(nbobs * 0.8)

# Randomly sample XX rows from data for training
set.seed(12345) # Set seed for reproducibility
data_train_color <- data_z_color[sample(1:nrow(data_z_color), datas), ]  # Sample rows of data with Base R
data_train_color  # optional

# Estimate pre-processing parameters
preproc.param_color <- data_train_color %>% 
  preProcess(method = c("center", "scale"))

# Transform the data using the estimated parameters
train.transformed_color <- preproc.param_color %>% predict(data_train_color)

# Fit the model
model_color <- qda(sex ~ scale(all_nm_peak)+ scale(visible_nm_peak), data = train.transformed_color)
model_color


#
# Test part with color
#
# Create the test data set
data_test_color <- anti_join(data_z_color, data_train_color, by = c("individual_id"))
data_test_color # optional

# Transform test data
test.transformed_color <- preproc.param_color %>% predict(data_test_color)

# Make predictions
(predictions_color <- model_color %>% predict(test.transformed_color))


# Model accuracy
mean(predictions_color$class == test.transformed_color$sex)

#
#Compare the predictions with the reality (with color values)
#
# Create a new column with the predictions
data_test_color$sex_predictions <- NA
data_test_color

# Fill the column that you create before
data_test_color$sex_predictions <- predictions_color$class
data_test_color

# Create a new column with the sexes comparison
data_test_color$compare_sex <- NA

# Fill the column that you create before
# In this column you have to say if the prediction is right (1) or not (0)
for (cc in 1:length(data_test_color$compare_sex)) {
  data_test_color[cc,]$compare_sex <- ifelse(data_test_color[cc,]$sex == data_test_color[cc,]$sex_predictions, 1, 0)
}
data_test_color

#end QDA  with color values
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#end of the code
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#