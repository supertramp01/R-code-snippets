#get container data from input file
get_container_from_file <- function(file, orig_matrix=NULL) {
  data <- read.csv(file)
  dtMatrix <- create_matrix(data["text"], originalMatrix = orig_matrix)
  train_size <- nrow(data)
  container <- create_container(matrix = dtMatrix, labels = data$label, trainSize = 1:train_size, virgin = FALSE)
  return(container)
}

get_models <- function(c) {
    models <- train_models(c, algorithms=c("MAXENT","SVM","GLMNET","SLDA","TREE","BAGGING","BOOSTING","RF"))
}

train_and_build <- function(file="dataset.csv") {
  #data <- read.csv(training_set)
  data <- read.csv(file)  
  dtMatrix <- create_matrix(data["text"], originalMatrix = NULL, weighting = weightTf, removeStopwords = TRUE)
  train_size <- nrow(data)
  container <- create_container(matrix = dtMatrix, labels = data$label, trainSize = 1:train_size, virgin = FALSE)
  models <- train_models(container, algorithms=c("MAXENT","SVM","GLMNET","SLDA","BAGGING","BOOSTING","RF"))
  return(models)
}

test_and_validate <- function(models=models, file="test_dataset.csv") {
  test_data <- read.csv(file=file)
  test_matrix <- create_matrix(test_data["text"], originalMatrix = dtMatrix, weighting = weightTf, removeStopwords = TRUE)
  test_size <- nrow(test_data)
  test_container <- create_container(matrix = test_matrix, labels = test_data$label, testSize = 1:test_size, virgin = FALSE)
  results <- classify_models(test_container, models)
  return(results)
}