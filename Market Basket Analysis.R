# 2. Main Data Frame ------------------------------------------------------
dqlab.trans <-
  read.transactions(
    file = "https://storage.googleapis.com/dqlab-dataset/transaksi_dqlab_retail.tsv",
    format = "single",
    sep = "\t",
    cols = c(1, 2),
    skip = 1
  )
print(dqlab.trans)
dqlab.trans <-
  itemFrequency(dqlab.trans, type = "absolute")
print(dqlab.trans)