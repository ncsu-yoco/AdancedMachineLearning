library('neuralnet')

dataset = read.table("biodeg.csv", sep = ";", stringsAsFactors = FALSE)

cols = colnames(dataset)

frmla = as.formula(
    paste(cols[length(cols)]," ~ ", 
        paste(cols[1:length(cols)-1], collapse = " + "))
  )

mm <- model.matrix(~ V42 + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + 
             V12 + V13 + V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + 
             V22 + V23 + V24 + V25 + V26 + V27 + V28 + V29 + V30 + V31 + 
             V32 + V33 + V34 + V35 + V36 + V37 + V38 + V39 + V40 + V41, data = dataset)

translate <- function(d) {
  switch(d,
         "RB" = 0,
         "NRB" = 1)
}

dataset[,42] = apply(as.matrix(dataset[,42]), 1, translate)

nn = neuralnet(frmla, dataset)

a = compute(nn, dataset[,1:41])

b = data.frame(original = dataset[,42], calc = a$net.result)
