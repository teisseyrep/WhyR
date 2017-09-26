##################################
########## BR vs CC ##############
##################################

library("mldr")
library("mlr")

#Use mldr to load data:
data(emotions)
emotions1 = emotions$dataset

#Modify data frame to have a proper format for mlr:
emotions1 = emotions1[,-c(79,80)] #remove useless columns
names(emotions1)[73:78]=paste0("Label",1:6) #mlr do not accept "-" in label names.
for(j in 73:78) emotions1[,j] =ifelse(emotions1[,j]==1,TRUE,FALSE) #in mlr labels must be logical.

#Let's use mlr package:

#Create multilabel task:
labels = colnames(emotions1)[73:78]
task = makeMultilabelTask(id = "multi", data = emotions1, target = labels)

#Use classifier chains with rpart classifer:
lrn = makeLearner("classif.rpart")
lrn = makeMultilabelClassifierChainsWrapper(lrn)
lrn = setPredictType(lrn, "prob")
cv1 = makeResampleDesc(method = "CV", stratify = FALSE, iters = 3)
set.seed(123)
measures_list = list(multilabel.hamloss, multilabel.subset01, multilabel.f1)
results = resample(learner = lrn, task = task, resampling = cv1, show.info = FALSE,measures=measures_list)
print(results)

#Use binary relevance with rpart classifer:
lrn = makeLearner("classif.rpart")
lrn = makeMultilabelBinaryRelevanceWrapper(lrn)
lrn = setPredictType(lrn, "prob")
cv1 = makeResampleDesc(method = "CV", stratify = FALSE, iters = 3)
set.seed(123)
results = resample(learner = lrn, task = task, resampling = cv1, show.info = FALSE,measures=measures_list)
print(results)


