library('kernlab')
library('neuralnet')

dataset = read.table("biodeg.csv", sep = ";", stringsAsFactors = FALSE)

cols = colnames(dataset)
frmla = as.formula(
  paste(cols[length(cols)]," ~ ", 
        paste(cols[1:length(cols)-1], collapse = " + "))
)

# create test and training set
n_cross_folds <- function(data, n = 2) {
  index <- sample(1:dim(data)[1])
  datatrain <- data[index[1:floor(dim(data)[1]*(1/n))], ]
  datatest <- data[index[((ceiling(dim(data)[1]*(1/n))) + 1):dim(data)[1]], ]
  return (list(training = datatrain, test = datatrain))
}

runSVM <- function(curr_kernel, training, test) {
  modelSVM <- ksvm(x = frmla, data = training, kernel = curr_kernel, fit = TRUE)
  predictions <- predict(modelSVM, test[,1:41])
  result <- list ( curr_kernel = curr_kernel,
                   confusion_matrix = table(predictions, test[,42]),
                   error = error(modelSVM)
  )
  return (result)
}

generate_table <- function(item, ft) {
  newrow <- list(folds = item$n_fold, kernel = as.character(item$curr_kernel),
                #confusion_matrix = item$confusion_matrix,
                error = item$error)
  ft <- rbind(ft, newrow)
  ft$kernel <- sapply(ft$kernel, as.character)
  return (ft)
}

checkSVMs <- function() {
  available_kernels <- c("rbfdot", "polydot", "vanilladot", "tanhdot", "laplacedot", "besseldot",
                         "anovadot")
  
  final_table = data.frame( folds = numeric(), 
                            kernel = character(),
                            #confusion_mat = integer(), 
                            error = numeric(),
                            stringsAsFactors = FALSE
  )
  
  for (i in seq(1, 10, 2)) {              # various n_folds
    sets <- n_cross_folds(dataset, n = i)     # generating training and test sets
    results = lapply(available_kernels, runSVM, sets$training, sets$test)   # running SVM
    for (j in 1:length(results)) {
      results[[j]]$n_fold = i
      final_table <- generate_table(results[[j]], final_table)  #generating table
    }
  } 
  
  print (final_table)
}

#activation functions, numbers of hidden units, numbers of epochs, and numbers of cross-validation folds
checkNNs <- function() {
  translate <- function(d) {
    switch(d,
           "RB" = 0,
           "NRB" = 1)
  }
  ds = dataset
  ds[,42] = apply(as.matrix(ds[,42]), 1, translate)
  for (i in seq(1, 5, 2)) {              # various n_folds
    sets <- n_cross_folds(ds, n = i)
    #for(h in seq(1,10,3)) {
      nn = neuralnet(frmla, sets$training, hidden = 3, threshold = 0.01)
      a = compute(nn, sets$test[,1:41])
      b = data.frame(original = ds$test[,42], calc = a$net.result)
      print (b)   
    #}
  }
}