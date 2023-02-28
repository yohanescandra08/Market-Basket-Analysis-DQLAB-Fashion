# 1. Packages List --------------------------------------------------------
library(tidyverse)
library(arules)
library(arulesViz)
library(plotly)


# 2. Main Data Frame ------------------------------------------------------
dqlab.trans.1 <-
  read.transactions(
    file = "https://storage.googleapis.com/dqlab-dataset/transaksi_dqlab_retail.tsv",
    format = "single",
    sep = "\t",
    cols = c(1, 2),
    skip = 1
  )
print(dqlab.trans)
dqlab.trans.2 <-
  itemFrequency(dqlab.trans.1, type = "absolute")


# 3. Transaction Frequency ------------------------------------------------
dqlab.trans.freq <-
  sort(dqlab.trans.2, decreasing = T)
dqlab.trans.freq <-
  data.frame(
    Product_Name = names(dqlab.trans.freq),
    Total = dqlab.trans.freq,
    row.names = NULL
  )
View(dqlab.trans.freq)
write.csv(dqlab.trans.freq, file = "Total Transaction.txt")

# * 3.1. Top 10 -----------------------------------------------------------
dqlab.trans.freq.top.10 <-
  dqlab.trans.freq %>%
  head(10) %>%
  arrange(Total)
dqlab.trans.freq.top.10$Product_Name <-
  factor(dqlab.trans.freq.top.10$Product_Name,
         levels = dqlab.trans.freq.top.10$Product_Name)
View(dqlab.trans.freq.top.10)
write.csv(dqlab.trans.freq %>% head(10) %>% arrange(desc(Total)), file = "Top 10 Transaction.txt")
# * * 3.1.1. Visualization ------------------------------------------------
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
                 range = c(0, 2075)),
    yaxis = list(title = "Fashion Item"),
    title = "Top 10 Fashion Item Transaction"
  )
dqlab.trans.freq.top.10.plot

# * 3.2. Bottom 10 --------------------------------------------------------
dqlab.trans.freq.bottom.10 <-
  dqlab.trans.freq %>%
  tail(10) %>%
  arrange(Total)
dqlab.trans.freq.bottom.10$Product_Name <-
  factor(dqlab.trans.freq.bottom.10$Product_Name,
         levels = dqlab.trans.freq.bottom.10$Product_Name)
View(dqlab.trans.freq.bottom.10)
write.csv(dqlab.trans.freq %>% tail(10) %>% arrange(desc(Total)), file = "Bottom 10 Transaction.txt")
# * * 3.2.1. Visualization ------------------------------------------------
color.2 <- rev(brewer.pal(n = 9, name = "Reds"))
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
                 range = c(0, 2075)),
    yaxis = list(title = "Fashion Item"),
    title = "Bottom 10 Fashion Item Transaction"
  )
dqlab.trans.freq.bottom.10.plot


# 4. Product Combination Based on Filter ----------------------------------
dqlab.trans.combi.1 <-
  apriori(dqlab.trans.1,
          parameter = list(
            supp = 10 / length(dqlab.trans.1),
            confidence = 0.5,
            minlen = 2,
            maxlen = 3
          ))
dqlab.trans.combi.1 <-
  c(head(sort(dqlab.trans.combi.1, by = "lift"), n = 10))
dqlab.trans.combi.2 <-
  DATAFRAME(dqlab.trans.combi.1)
View(dqlab.trans.combi.2)
write.csv(dqlab.trans.combi.2, file = "Top 10 Product Combination Based on Filter.txt")
# * 4.1. Visualization ----------------------------------------------------
dqlab.trans.combi.plot <-
  plot(dqlab.trans.combi.1, method = "graph", engine = "html")
dqlab.trans.combi.plot


# 5. Product Combination Based on Slow Moving Item ------------------------
dqlab.trans.combi.slow.item.1 <-
  apriori(dqlab.trans.1,
          parameter = list(
            supp = 10 / length(dqlab.trans.1),
            confidence = 0.1,
            minlen = 2,
            maxlen = 3
          ))
dqlab.trans.combi.slow.item.1 <-
  c(sort(
    subset(dqlab.trans.combi.slow.item.1, rhs %in% "Tas Makeup"),
    by = "lift",
    decreasing = T
  )[c(1:3)],
  sort(
    subset(
      dqlab.trans.combi.slow.item.1,
      rhs %in% "Baju Renang Pria Anak-anak"
    ),
    by = "lift",
    decreasing = T
  )[c(1:3)])
dqlab.trans.combi.slow.item.2 <-
  DATAFRAME(dqlab.trans.combi.slow.item.1)
View(dqlab.trans.combi.slow.item.2)
write.csv(dqlab.trans.combi.slow.item.2, file = "Product Combination on Slow Moving Item.txt")
# * 5.1. Visualization ----------------------------------------------------
dqlab.trans.combi.slow.item.1.plot <-
  plot(dqlab.trans.combi.slow.item.1,
       method = "graph",
       engine = "html")
dqlab.trans.combi.slow.item.1.plot
