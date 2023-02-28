# 1. Packages List --------------------------------------------------------
library(tidyverse)
library(arules)
library(arulesViz)
library(plotly)


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


# 3. Transaction Frequency ------------------------------------------------
dqlab.trans.freq <-
  sort(dqlab.trans, decreasing = T)
dqlab.trans.freq <-
  data.frame(
    Product_Name = names(dqlab.trans.freq),
    Total = dqlab.trans.freq,
    row.names = NULL
  )
View(dqlab.trans.freq)
write.csv(dqlab.trans.freq, file = "Transaction Frequency.txt")
# * 3.1. Top 10 -----------------------------------------------------------
dqlab.trans.freq.top.10 <-
  dqlab.trans.freq %>% 
  head(10) %>% 
  arrange(Total)
dqlab.trans.freq.top.10$Product_Name <-
  factor(dqlab.trans.freq.top.10$Product_Name,
         levels = dqlab.trans.freq.top.10$Product_Name)
View(dqlab.trans.freq.top.10)
write.csv(dqlab.trans.freq.top.10, file = "Top 10 Transaction.txt")
# * * 5.1.1. Visualization ------------------------------------------------
color.1 <- brewer.pal(n = 9, name = "Greens")
dqlab.trans.freq.top.10.plot <-
  plot_ly(dqlab.trans.freq.top.10) %>%
  add_trace(
    x = ~ Total,
    y = ~ Product_Name,
    type = "bar",
    color = ~ Product_Name,
    colors = color.1,
    hovertemplate = "<i>%{y}</i><br><b>%{x} Units</b></br><extra></extra>",
    textposition = "none"
  ) %>%
  layout(
    showlegend = F,
    xaxis = list(title = "Total Transaction",
                 range = c(0,2075)),
    yaxis = list(title = "Fashion Item"),
    title = "Top 10 Fashion Item Transaction"
  )
dqlab.trans.freq.top.10.plot

# * 5.2. Bottom 10 --------------------------------------------------------
dqlab.trans.freq.bottom.10 <-
  dqlab.trans.freq %>% 
  tail(10)
dqlab.trans.freq.bottom.10$Product_Name <-
  factor(dqlab.trans.freq.bottom.10$Product_Name,
         levels = dqlab.trans.freq.bottom.10$Product_Name)
View(dqlab.trans.freq.bottom.10)
# * * 5.2.1. Visualization ------------------------------------------------
color.2 <- brewer.pal(n = 9, name = "Reds")
dqlab.trans.freq.bottom.10.plot <-
  plot_ly(dqlab.trans.freq.bottom.10) %>%
  add_trace(
    x = ~ Total,
    y = ~ Product_Name,
    type = "bar",
    color = ~ Product_Name,
    colors = color.2,
    hovertemplate = "<i>%{y}</i><br><b>%{x} Units</b></br><extra></extra>",
    textposition = "none"
  ) %>%
  layout(
    showlegend = F,
    xaxis = list(title = "Total Transaction",
                 range = c(0,2075)),
    yaxis = list(title = "Fashion Item"),
    title = "Bottom 10 Fashion Item Transaction"
  )
dqlab.trans.freq.bottom.10.plot


  # 6. Product Combination Based on Filter ----------------------------------
dqlab.trans.combi <-
  apriori(dqlab.trans,
          parameter = list(
            supp = 10 / length(dqlab.trans),
            confidence = 0.5,
            minlen = 2,
            maxlen = 3
          ))
dqlab.trans.combi <-
  c(head(sort(dqlab.trans.combi, by = "lift"), n = 10))
inspect(dqlab.trans.combi)
write(dqlab.trans.combi, file = "dqlab.transaction.combination.top.10.txt")
dqlab.trans.combi.plot <-
  plot(dqlab.trans.combi, method = "graph", engine = "html")
dqlab.trans.combi.plot


# 7. Product Combination Based on Slow Moving Item ------------------------
dqlab.trans.combi.slow.item <-
  apriori(dqlab.trans,
          parameter = list(
            supp = 10 / length(dqlab.trans),
            confidence = 0.1,
            minlen = 2,
            maxlen = 3
          ))
dqlab.trans.combi.slow.item <-
  c(sort(
    subset(dqlab.trans.combi.slow.item, rhs %in% "Tas Makeup"),
    by = "lift",
    decreasing = T
  )[c(1:3)],
  sort(
    subset(
      dqlab.trans.combi.slow.item,
      rhs %in% "Baju Renang Pria Anak-anak"
    ),
    by = "lift",
    decreasing = T
  )[c(1:3)])
inspect(dqlab.trans.combi.slow.item)
write(dqlab.trans.combi.slow.item, file = "dqlab.transaction.combination.slow.move.item.txt")
dqlab.trans.combi.slow.item.plot <-
  plot(dqlab.trans.combi.slow.item,
       method = "graph",
       engine = "html")
dqlab.trans.combi.slow.item.plot
