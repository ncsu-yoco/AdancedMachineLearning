library('neuralnet')

dataset = read.table("biodeg.csv", sep = ";", stringsAsFactors = FALSE)

cols = colnames(dataset)

frmla = as.formula(
    paste(cols[length(cols)]," ~ ", 
        paste(cols[1:length(cols)-1], collapse = " + "))
  )

n_cross_folds <- function(data, n = 2) {
  index <- sample(1:dim(data)[1])
  datatest <- data[index[1:floor(dim(data)[1]*(1/n))], ]
  datatrain <- data[index[((ceiling(dim(data)[1]*(1/n))) + 1):dim(data)[1]], ]
  return (list(training = datatrain, test = datatest))
}

# mm <- model.matrix(~ V42 + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + 
#              V12 + V13 + V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + 
#              V22 + V23 + V24 + V25 + V26 + V27 + V28 + V29 + V30 + V31 + 
#              V32 + V33 + V34 + V35 + V36 + V37 + V38 + V39 + V40 + V41, data = dataset)

translate <- function(d) {
  switch(d,
         "RB" = 0,
         "NRB" = 1)
}

activation <- function(d, val = 0.5) {
  if(d < val) {
    return (0)
  }
  else {
    return (1) 
  }
}

# nn = neuralnet(frmla, sets$training)
# a = compute(nn, sets$test[,1:41])
# res = apply(as.matrix(a$net.result), 1, activation)
# b = data.frame(original = sets$test[,42], mod = res, calc = a$net.result)
# print (table(sets$test[,42], res))

checkNNs <- function() {
  final_table = data.frame( folds = numeric(), 
                            hidden = character(),
                            error = numeric(),
                            stringsAsFactors = FALSE
  )
  ds = dataset
  ds[,42] = apply(as.matrix(ds[,42]), 1, translate)
  sets <- n_cross_folds(ds)
  for (i in seq(2, 10, 2)) {
    sets <- n_cross_folds(ds, n = i)
    print (i)
    for(h in seq(20,30,3)) {
      print ("hidden")
      print (h)
      nn = neuralnet(frmla, sets$training, rep = 5, hidden = h)
      op = compute(nn, sets$test[,1:41])
      mod <- apply(as.matrix(op$net.result),1,activation, val = a)
      ct <- table(mod, sets$test[,42])
      print (ct)
      print (ct["0","1"])
      err = (ct["1","0"] + ct["0", "1"])/(ct["1","0"] + ct["0", "1"] + ct["0","0"] + ct["1", "1"])
      newrow <- list(folds = i, hidden = h,  error = err)
      final_table < rbind(final_table, newrow)
    }
    return (final_table)
  }
}

n_f = 2
h_l = 20
sets <- n_cross_folds(ds, n = n_f)
nn = neuralnet(frmla, sets$training, rep = 5, hidden = h_l)
