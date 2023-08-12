#' Title: Assignment 2.R
#' Purpose: Hospital Readmission Modeling
#' Author: Luis Arellano
#' Date: April 1, 2023

# Libraries
library(powerjoin)
library(dplyr)
library(modeest)
library(ggthemes)
library(ggplot2)
library(tidyr)
library(purrr)
library(tm)
library(vtreat)
library(MLmetrics)
library(ranger)
library(corrplot)
library(caret)
library(rpart.plot)
library(wordcloud)

# Read files as list
tmp1 <- c ("https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A2_Hospital_Readmission/caseData/diabetesHospitalInfoTrain.csv",
          "https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A2_Hospital_Readmission/caseData/diabetesMedsTrain.csv",
          "https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A2_Hospital_Readmission/caseData/diabetesPatientTrain.csv")

tmp2 <- c ("https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A2_Hospital_Readmission/caseData/diabetesHospitalInfoTest.csv",
           "https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A2_Hospital_Readmission/caseData/diabetesMedsTest.csv",
           "https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A2_Hospital_Readmission/caseData/diabetesPatientTest.csv")
## DATA CLEANING

# Read in a list
allDFtrain <- lapply(tmp1, read.csv)
allDFtest <- lapply(tmp2, read.csv)


# Join all tables on tmpID
patientstrain <- power_left_join(allDFtrain, by = "tmpID")
patientstest <- power_left_join(allDFtest, by = "tmpID")

# Remove missing values
JoinedDatatrain <- patientstrain[complete.cases(patientstrain),]
JoinedDatatest <- patientstest[complete.cases(patientstest),]

## EDA ##
# Plot numerical variables to identify and drop outliers
# Time in Hospital
ggplot(JoinedDatatrain, aes(x = tmpID, y = time_in_hospital)) +
  geom_point() +
  xlab("Patient ID") +
  ylab("Days Spent in Hospital") +
  ggtitle("Days Spent in Hospital by Patient ID")

# Number of Lab Procedures
ggplot(JoinedDatatrain, aes(x = tmpID, y = num_lab_procedures)) +
  geom_point() +
  xlab("Patient ID") +
  ylab("Number of Lab Procedures") +
  ggtitle("Number of Lab Procedures by Patient ID")
# Drop outliers
JoinedDatatrain <- subset(JoinedDatatrain, num_lab_procedures <= 80)

# Number of procedures
ggplot(JoinedDatatrain, aes(x = tmpID, y = num_procedures)) +
  geom_point() +
  xlab("Patient ID") +
  ylab("Number of procedures") +
  ggtitle("Number of Procedures by Patient ID")

# Number of Medications
ggplot(JoinedDatatrain, aes(x = tmpID, y = num_medications)) +
  geom_point() +
  xlab("Patient ID") +
  ylab("Number of Medications") +
  ggtitle("Number of Medications by Patient ID")
# Drop outliers
JoinedDatatrain <- subset(JoinedDatatrain, num_medications <= 40)

# Number of Days Outpatient
ggplot(JoinedDatatrain, aes(x = tmpID, y = number_outpatient)) +
  geom_point() +
  xlab("Patient ID") +
  ylab("Number of Days as Outpatient") +
  ggtitle("Number of Days as Outpatient by Patient ID")
# Drop outliers
JoinedDatatrain <- subset(JoinedDatatrain, number_outpatient <= 5)

# Number of Days in Emergency
ggplot(JoinedDatatrain, aes(x = tmpID, y = number_emergency)) +
  geom_point() +
  xlab("Patient ID") +
  ylab("Number of Days in Emergency") +
  ggtitle("Number of Days in Emergency by Patient ID")
# Drop outliers
JoinedDatatrain <- subset(JoinedDatatrain, number_emergency <= 3)

# Number of Days Inpatient
ggplot(JoinedDatatrain, aes(x = tmpID, y = number_inpatient)) +
  geom_point() +
  xlab("Patient ID") +
  ylab("Number of Days as Inpatient") +
  ggtitle("Number of Days as Inpatient by Patient ID")
# Drop outliers
JoinedDatatrain <- subset(JoinedDatatrain, number_inpatient <= 5)

# Number of Diagnoses
ggplot(JoinedDatatrain, aes(x = tmpID, y = number_diagnoses)) +
  geom_point() +
  xlab("Patient ID") +
  ylab("Number of Diagnoses") +
  ggtitle("Number of Diagnoses by Patient ID")

# Patients Age
ggplot(JoinedDatatrain, aes(x = tmpID, y = age)) +
  geom_point() +
  xlab("Patient ID") +
  ylab("Patient Age") +
  ggtitle("Patients Age by Patient ID")
# Drop outliers
JoinedDatatrain <- subset(JoinedDatatrain, age >= 20)

# Patients Weight
ggplot(JoinedDatatrain, aes(x = tmpID, y = wgt)) +
  geom_point() +
  xlab("Patient ID") +
  ylab("Patient Weight") +
  ggtitle("Patients Weight by Patient ID")
ggsave("Weight.png")
# Drop outliers
JoinedDatatrain <- subset(JoinedDatatrain, wgt <= 260)
JoinedDatatrain <- subset(JoinedDatatrain, wgt >= 110)

## PLOT CORRELATION MATRIC USING NUMERICAL AND LOGICAL VARIABLES ##
corr_data <- JoinedDatatrain %>%
  select_if(~ is.numeric(.) || is.logical(.))

cor_matrix <- cor(corr_data)
# Save correnlation Matrix
png("corrplot.png")

corrplot(cor_matrix, method="color")

dev.off()

# Count the amount of "Expired" in every Dataframe
trainExp <- nrow(JoinedDatatrain[JoinedDatatrain$discharge_disposition_id == "Expired", ])
testExp <- nrow(JoinedDatatest[JoinedDatatest$discharge_disposition_id == "Expired", ])

# Append both DF for transformation
JoinedData <- rbind(JoinedDatatrain, JoinedDatatest)

# Drop rows containing expired patients
# Identify the rows that contain the word "Expired"
rows_to_drop <- grepl("Expired", JoinedData$discharge_disposition_id)

# Remove the rows that contain the word "Expired"
JoinedData <- JoinedData[!rows_to_drop, ]

# drop columns
drops <- c('medical_specialty',       #too many unknowns
           'discharge_disposition_id',
           'admission_source_id') #too many different values to turn categorical
JoinedData <- JoinedData[,!(names(JoinedData) %in% drops)]


## TRANSFORM DIAGNOSTIC DATA USING WORD CLOUDS ##


# Options & Functions
Sys.setlocale('LC_ALL','C')

tryTolower <- function(x){
  # return NA when there is an error
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error = function(e) e)
  # if not an error
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url)) 
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  return(corpus)
}

# Create custom stop words
customStopwords <- c(stopwords('english'), 
                     'site',
                     'unspecified',
                     'type', 
                     'without', 
                     'mention', 
                     'acute',
                     'failure',
                     'uncontrolled',
                     'stated',
                     'complication',
                     'disease')

#subset data for diagnostic
diag <- JoinedData[, c("diag_1_desc", "diag_2_desc", "diag_3_desc")]

#Unite all 3 diagnostics into 1
diag <- unite(diag, diag_desc, diag_1_desc, diag_2_desc, diag_3_desc, sep = ", ")


# Make a volatile corpus
txtCorpus <- VCorpus(VectorSource(diag$diag_desc))

# Preprocess the corpus
txtCorpus <- cleanCorpus(txtCorpus, customStopwords)

# Need to plain text cleaned copy?
diag_df <- data.frame(diag_clean    = unlist(sapply(txtCorpus, `[`, "content")),
                      stringsAsFactors=F)

# Make TDM
diagDTM  <- DocumentTermMatrix(txtCorpus)
diagDTMm <- as.matrix(diagDTM)
dim(diagDTMm)

diagFreq <- colSums(diagDTMm)
diagFreq <- data.frame(word=names(diagFreq),
                        frequency=diagFreq, 
                        row.names = NULL)

#Top 10 words
top_words <- diagFreq %>% arrange(desc(frequency)) %>% head(10)

# Make word cloud and save the word cloud as a PNG image
png("wordcloud.png", width = 800, height = 600)
wordcloud(words = top_words$word, freq = top_words$frequency, min.freq = 1, 
          max.words = 10, random.order = FALSE, rot.per = 0.35, 
          colors = brewer.pal(9, "Blues"))
dev.off()


# drop diagnostic columns and attach clean diagnostic
drops <- c("diag_1_desc", "diag_2_desc", "diag_3_desc")
JoinedData <- JoinedData[,!(names(JoinedData) %in% drops)]

JoinedData <- cbind(JoinedData, diag_df)

for (i in 1:nrow(top_words)) {
  word <- top_words$word[i]
  JoinedData[[word]] <- grepl(word, JoinedData$diag_clean, ignore.case = TRUE)
}

# drop diagnostic data and convert the rest of the strings to factors
JoinedData <- JoinedData[,!(names(JoinedData) %in% "diag_clean")]

# get the column names of character variables
character_cols <- JoinedData %>%
  select_if(is.character) %>% # select character columns
  names()                     # get column names

# convert character variables to factors
JoinedData[, character_cols] <- JoinedData[, character_cols] %>% 
  map_if(is.character, as.factor)

# Split data back into train and test
JoinedDatatrain <- JoinedData %>% head(nrow(JoinedDatatrain)-trainExp)
JoinedDatatest <- JoinedData %>% tail(nrow(JoinedDatatest)-testExp)

## MODELING ##
## LOGISTIC REGRESSION ##

# Identify the informative and target
names(JoinedData)
targetVar       <- names(JoinedData)[39]
informativeVars <- names(JoinedData)[c(1:38, 40:49)] # model without

#### SAMPLE
# Segment the prep data
set.seed(1234)
idx         <- sample(1:nrow(JoinedDatatrain),.1*nrow(JoinedDatatrain))
prepData    <- JoinedDatatrain[idx,]
nonPrepData <- JoinedDatatrain[-idx,]

# Design a "Categorical variable plan 
plan <- designTreatmentsC(prepData, 
                          informativeVars,
                          targetVar, 1)

# Apply to xVars
treatedX <- prepare(plan, nonPrepData)
treatedtest <- prepare(plan, JoinedDatatest)

#### MODIFY Further
# Partition to avoid over fitting
train      <- treatedX
validation <- treatedtest

#### MODEL
# Fit a logistic regression model
fit <- glm(readmitted_y ~., data = train, family ='binomial')
summary(fit)

# Backward Variable selection to reduce chances of multi-colinearity
bestFit <- step(fit, direction='backward')
#saveRDS(bestFit, 'bestFit.rds')
summary(bestFit)

# Compare model size
length(coefficients(fit))
length(coefficients(bestFit))

# Plot Variable importance
VarImp_LGM <- varImp(bestFit)
VarImp_LGM$variables <- row.names(VarImp_LGM)
varImp_LGM <- VarImp_LGM[order(VarImp_LGM$Overall, decreasing = T),]
varImp_LGM <- head(varImp_LGM,10)
ggplot(varImp_LGM, aes(x=Overall, y = reorder(variables, Overall))) + 
  geom_bar(stat='identity', position = 'dodge') + 
  ggtitle('Variable Importance') + 
  theme_gdocs()
ggsave('VariableImportance_GLM.png')


# Get predictions
GLMPreds <- predict(bestFit,  validation, type='response')
tail(GLMPreds)

# Classify 
cutoff      <- 0.5
Readmitted <- ifelse(GLMPreds >= cutoff, 1,0)

#### ASSESS
# Organize w/Actual
resultsglm <- data.frame(tmpID    = validation$tmpID,
                      readmitted_actual  = validation$readmitted_y,
                      readmitted_pred = Readmitted,
                      probabilty   = GLMPreds)
head(resultsglm)


# Get a confusion matrix
(confMat <- ConfusionMatrix(resultsglm$readmitted_pred, resultsglm$readmitted_actual))

# What is the accuracy?
sum(diag(confMat)) / sum(confMat)
acc_glm <- Accuracy(resultsglm$readmitted_pred, resultsglm$readmitted_actual)



## RandomForest ##
# Fit a random forest model with Ranger
RFpred <- ranger(as.factor(readmitted_y) ~ .,
                     data  = train, 
                     num.trees = 100,
                     importance = 'permutation',
                     mtry  = 1, 
                     probability = T)

# Look at improved var importance
varImpDF <- data.frame(variables = names(importance(RFpred)),
                       importance = importance(RFpred),
                       row.names = NULL)


varImpDF <- varImpDF[order(varImpDF$importance, decreasing = T),]
varImpDF <- head(varImpDF,10)
ggplot(varImpDF, aes(x=importance, y = reorder(variables, importance))) + 
  geom_bar(stat='identity', position = 'dodge') + 
  ggtitle('Variable Importance') + 
  theme_gdocs()
ggsave('VarialbleImportance_RF.png')

# Confusion Matrix
trainRF <- predict(RFpred, train)
# In ranger objects, the predictions are within a list and need to be declared
head(trainRF$predictions)

# Using the prediction probability list element, classify with 0.50 cutoff 
classOutcome <- ifelse(trainRF$predictions[,2]>=0.5,1,0)
confusionMatrix(as.factor(classOutcome), 
                as.factor(train$readmitted_y))

### Now let's apply to the validation test set
testRF <- predict(RFpred, validation)


# Accuracy Comparison from MLmetrics
classOutcomeTest <- ifelse(testRF$predictions[,2]>=0.5,
                           1,0)

# Organize w/Actual
resultsRF <- data.frame(tmpID    = validation$tmpID,
                      readmitted_actual  = validation$readmitted_y,
                      readmitted_pred = classOutcomeTest,
                      probabilty   = testRF$predictions)
head(resultsRF)


# Get a confusion matrix
(confMat <- ConfusionMatrix(resultsRF$readmitted_pred, resultsRF$readmitted_actual))

# Store accuracy
sum(diag(confMat)) / sum(confMat)
acc_rf <- Accuracy(resultsRF$readmitted_pred, resultsRF$readmitted_actual)


## DECISION TREE ##
# Fit a decision tree with caret
set.seed(1234)
fit <- train(as.factor(readmitted_y) ~., #formula based
             data = train, #data in
             #"recursive partitioning (trees)
             method = "rpart", 
             #Define a range for the CP to test
             tuneGrid = data.frame(cp = c(0.006,0.00625,0.0065,0.00675,0.007,0.00725,0.0075,0.00775,0.008,0.00825,0.0085,0.00875,0.009)), 
             #ie don't split if there are less than 1 record left and only do a split if there are at least 2+ records
             control = rpart.control(minsplit = 1, minbucket = 2)) 


# Examine
fit

# Plot the CP Accuracy Relationship to adust the tuneGrid inputs
#png('CP-3.png')
plot(fit)
#dev.off()

# Plot a pruned tree
pdf('bestTree.pdf')
prp(fit$finalModel, extra = 1)
dev.off()

# Plot Variable importance
VarImp_DT <- varImp(fit)
VarImp_DT <- VarImp_DT$importance
VarImp_DT$variables <- row.names(VarImp_DT)
VarImp_DT <- VarImp_DT[order(VarImp_DT$Overall, decreasing = T),]
VarImp_DT <- head(VarImp_DT,10)
ggplot(VarImp_DT, aes(x=Overall, y = reorder(variables, Overall))) + 
  geom_bar(stat='identity', position = 'dodge') + 
  ggtitle('Variable Importance') + 
  theme_gdocs()
ggsave('VariableImportance_DT.png')

# Make some predictions on the training set
trainCaret <- predict(fit, train)
head(trainCaret)

# Get the conf Matrix
confusionMatrix(trainCaret, as.factor(train$readmitted_y))

# Make Variables
testCaret <- predict(fit,validation)
testCaretProbs <- predict(fit,validation,type = 'prob')

# Organize w/Actual
resultsDT <- data.frame(tmpID    = validation$tmpID,
                        readmitted_actual  = validation$readmitted_y,
                        readmitted_pred = testCaret,
                        probabilty   = testCaretProbs$"1")
head(resultsDT)


# Get a confusion matrix
(confMat <- ConfusionMatrix(resultsDT$readmitted_pred, resultsDT$readmitted_actual))

# What is the accuracy?
sum(diag(confMat)) / sum(confMat)
acc_dt <- Accuracy(resultsDT$readmitted_pred, resultsDT$readmitted_actual)


## COMPARE MODEL ACCURACY ##
acc_glm
acc_rf
acc_dt

## FURTHER EDA ##

# Check top 100 on best model
Results <- resultsglm[order(resultsglm$probabilty, decreasing = TRUE), ]
Results <- head(Results,100)
write.csv(Results, file = "top_100.csv")

#Check Results
result_summary <- select(Results, tmpID, readmitted_actual)

# actual results
result_summary <- result_summary %>%
  group_by(readmitted_actual) %>%
  summarise(count = n())

#Plot
ggplot(result_summary, aes(x = as.logical(readmitted_actual), y = count)) +
  geom_bar(stat = "summary", fun = "sum", fill = "darkblue") +
  labs(title = "Number of Patients by Readmission Status", x = "Readmitted", y = "Number of Patients")
ggsave('Results.png')

# Join with original DF to get data on the patients
FinalResults <- merge(Results, JoinedData, by = "tmpID", all = FALSE)

#Number Inpatient
Impatient <- select(FinalResults, tmpID, readmitted_y, number_inpatient)

# Create the scatter plot using ggplot2
ggplot(Impatient, aes(x = tmpID, y = number_inpatient, color = readmitted_y)) +
  geom_point() +
  scale_color_manual(values = c("red", "blue")) +
  labs(title = "Days in Inpatient Care", x = "tmpID", y = "Days")
ggsave('Inpatient-2.png')

# group by readmission
Impatient <- Impatient %>%
  group_by(readmitted_y) %>%
  summarise(average_days = mean(number_inpatient))

#Plot
ggplot(Impatient, aes(x = readmitted_y, y = average_days)) +
  geom_bar(stat = "summary", fun = "sum", fill = "darkblue") +
  labs(title = "Average Stay as Inpatient by Readmission", x = "Readmitted", y = "Average Stay Duration")
ggsave('Inpatient.png')

#Age
Age <- select(FinalResults, tmpID, readmitted_y, age)

# Create the scatter plot using ggplot2
ggplot(Age, aes(x = tmpID, y = age, color = readmitted_y)) +
  geom_point() +
  scale_color_manual(values = c("red", "blue")) +
  labs(title = "Patient Ages by Readmission Status", x = "tmpID", y = "Age")
ggsave('Age-1.png')

# create a new dataframe with the average age by readmitted status
age_summary <- aggregate(age ~ readmitted_y, data = Age, FUN = mean)

# create a bar chart of the average age by readmitted status
ggplot(data = age_summary, aes(x = readmitted_y, y = age)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  labs(x = "Readmission Status", y = "Average Age", title = "Average Age by Readmission Status")
ggsave('Age-2.png')

# Diabetes
kw <- select(FinalResults, tmpID, diabetes, mellitus, malignant, heart, kidney, graft, atherosclerosis, hypertension)

df_long <- kw %>%
  pivot_longer(cols = diabetes:hypertension, names_to = "condition", values_to = "value") %>%
  filter(value == TRUE) %>% # Only keep rows where value is TRUE
  group_by(condition) %>% # Group by condition
  summarise(count = n()) # Count the number of occurrences of each condition

# Create bar chart
ggplot(df_long, aes(x = count, y = condition)) +
  geom_col(fill = "darkblue") +
  xlab("Count") +
  ylab("Condition") +
  ggtitle("Number of Occurrences of Each Condition") +
  theme_minimal()

ggsave('diabetes.png')

#Admission type
Admission <- select(FinalResults, tmpID, readmitted_y, admission_type_id)
Admission$admission_type_id[Admission$admission_type_id == ""] <- NA

df_sum <- aggregate(tmpID ~ admission_type_id + readmitted_y, Admission, length)

ggplot(df_sum, aes(x = admission_type_id, y = tmpID, fill = readmitted_y)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("cadetblue", "darkblue"), name = "Readmitted") +
  xlab("Admission Type") +
  ylab("Count") +
  theme_minimal() +
  labs(title = "Readmissions by Admission type")
ggsave('admissions.png')