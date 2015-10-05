library('kernlab')

dataset = read.table("biodeg.csv", sep=";")

cols = colnames(dataset)
frmla = as.formula(
    paste(cols[length(cols)]," ~ ", 
        paste(cols[1:length(cols)-1], collapse = " + "))
  )

# fit <- indicates whether the fitted values should be computed and 
#         included in the model or not (default: TRUE)
# kernel <- rbfdot, polydot, vanilladot, tanhdot, laplacedot, besseldot,
#         anovadot

train_test_sets <- function(data, n = 2) {
  # create test and training set
  index <- sample(1:dim(data)[1])
  datatrain <- data[index[1:floor(dim(data)[1]*(1/n))], ]
  datatest <- data[index[((ceiling(dim(data)[1]*(1/n))) + 1):dim(data)[1]], ]
  print (dim(data))
  print (dim(datatrain))
  print (dim(datatest))
}

runSVM <- function(curr_kernel) {
  cat ("***************************************\n")
  cat (" Checking with kernel : ")
  cat (curr_kernel)
  cat ("\n---------------------------------------\n")
  modelSVM <- ksvm(x = frmla, data = dataset, kernel = curr_kernel, fit = TRUE)
  predictions <- predict(modelSVM, dataset[,1:41])
  #print (summary(predictions))
  #print (summary(dataset[,42]))
  cat ("Confusion Matrix : ")
  print (table (predictions,dataset[,42]) )
  cat ("Model Error : ")
  cat (error(modelSVM) )
  cat ("\n***************************************\n")
}

all_kernel <- c("rbfdot", "polydot", "vanilladot", "tanhdot", "laplacedot", "besseldot",
            "anovadot")

train_test_sets(dataset, n= 2)
train_test_sets(dataset, n= 3)
train_test_sets(dataset, n= 4)
train_test_sets(dataset, n= 5)
train_test_sets(dataset, n= 6)

#lapply(all_kernel, runSVM)