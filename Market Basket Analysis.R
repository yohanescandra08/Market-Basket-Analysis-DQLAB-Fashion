# 1. Save & Load Work Space -----------------------------------------------
getwd()
setwd("D:/Coding/YOHANES CANDRA KUSUMAPUTRA PORTFOLIO/MARKET BASKET ANALYSIS")
save.image(
  "D:/Coding/YOHANES CANDRA KUSUMAPUTRA PORTFOLIO/MARKET BASKET ANALYSIS/MARKET BASKET ANALYSIS.RDATA"
)
load(
  "D:/Coding/YOHANES CANDRA KUSUMAPUTRA PORTFOLIO/MARKET BASKET ANALYSIS/MARKET BASKET ANALYSIS.RDATA"
)


# 2. Copy Paste Clipboard -------------------------------------------------
to_clip_board <- function(x) {
  message(paste(as.character(substitute(x)), "written to clipboard."))
  write.table(
    paste0(capture.output(x), collapse = "\n"),
    "clipboard",
    col.names = FALSE,
    row.names = FALSE
  )
}


# 3. Packages List --------------------------------------------------------
library(arules)
library(arulesViz)
library(tidyverse)


# 4. Data Frame List -------------------------------------------------------
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


# 5. Top 10 & Bottom 10 ---------------------------------------------------
dqlab.trans.freq <-
  sort(dqlab.trans, decreasing = T)
dqlab.trans.freq <-
  data.frame(
    Product_Name = factor(names(dqlab.trans.freq),
                          levels = names(dqlab.trans.freq)),
    Total = dqlab.trans.freq,
    row.names = NULL
  )
View(dqlab.trans.freq)
write.csv(dqlab.trans.freq, file = "TRANSACTION FREQUENCY.txt")
# * 5.1. Top 10 -----------------------------------------------------------
dqlab.trans.freq.top.10 <-
  head(dqlab.trans.freq, 10)
View(dqlab.trans.freq.top.10)
# * * 5.1.1. Visualization ------------------------------------------------
dqlab.trans.freq.top.10.plot <-
  dqlab.trans.freq.top.10 %>%
  arrange(dqlab.trans.freq.top.10$Total)
dqlab.trans.freq.top.10.plot$Product_Name <-
  factor(
    dqlab.trans.freq.top.10.plot$Product_Name,
    levels = levels(dqlab.trans.freq.top.10.plot$Product_Name)[10:1]
  )
dqlab.trans.freq.top.10.plot <-
  ggplot(dqlab.trans.freq.top.10.plot,
         aes(x = Total,
             y = Product_Name,
             fill = Product_Name)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Total",
    y = "Product Name",
    title = "Best-Selling Products",
    subtitle = "Top 10"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_line(size = 1),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  geom_text(aes(label = Total),
            position = position_stack(vjust = 1),
            hjust = 1.2)
dqlab.trans.freq.top.10.plot

# * 5.2. Bottom 10 --------------------------------------------------------
dqlab.trans.freq.bottom.10 <-
  tail(dqlab.trans.freq, 10)
View(dqlab.trans.freq.bottom.10)
# * * 5.2.1. Visualization ------------------------------------------------
dqlab.trans.freq.bottom.10.plot <-
  dqlab.trans.freq.bottom.10 %>%
  arrange(dqlab.trans.freq.bottom.10$Total)
dqlab.trans.freq.bottom.10.plot$Product_Name <-
  factor(
    dqlab.trans.freq.bottom.10.plot$Product_Name,
    levels = levels(dqlab.trans.freq.bottom.10.plot$Product_Name)[69:60]
  )
dqlab.trans.freq.bottom.10.plot <-
  ggplot(dqlab.trans.freq.bottom.10.plot,
         aes(x = Total,
             y = Product_Name,
             fill = Product_Name)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Total",
    y = "Product Name",
    title = "Least-Selling Products",
    subtitle = "Bottom 10"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_line(size = 1),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  geom_text(aes(label = Total),
            position = position_stack(vjust = 1),
            hjust = 1.2)
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
