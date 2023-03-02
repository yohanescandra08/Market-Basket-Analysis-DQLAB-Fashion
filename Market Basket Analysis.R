# 1. Packages List --------------------------------------------------------
library(tidyverse)
library(arules)
library(arulesViz)
library(plotly)
library(RColorBrewer)


# 2. Main Data Frame ------------------------------------------------------
dqlab.trans.1 <-
  read.transactions(
    file = "https://storage.googleapis.com/dqlab-dataset/transaksi_dqlab_retail.tsv",
    format = "single",
    sep = "\t",
    cols = c(1, 2),
    skip = 1
  )
print(dqlab.trans.1)
dqlab.trans.2 <-
  itemFrequency(dqlab.trans.1, type = "absolute")


# 3. Total Unit Sell ------------------------------------------------------
dqlab.trans.freq <-
  sort(dqlab.trans.2, decreasing = T)
dqlab.trans.freq <-
  data.frame(
    Product_Name = names(dqlab.trans.freq),
    Total = dqlab.trans.freq,
    row.names = NULL
  )
View(dqlab.trans.freq)
write.csv(dqlab.trans.freq, file = "Total Unit Sell.txt")


# * 3.1. Top 10 Product Sell ----------------------------------------------
dqlab.trans.freq.top.10 <-
  dqlab.trans.freq %>%
  head(10) %>%
  arrange(Total)
dqlab.trans.freq.top.10$Product_Name <-
  factor(dqlab.trans.freq.top.10$Product_Name,
         levels = dqlab.trans.freq.top.10$Product_Name)
View(dqlab.trans.freq.top.10)
write.csv(dqlab.trans.freq %>% head(10) %>% arrange(desc(Total)),
          file = "Top 10 Product Sell.txt")
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
    xaxis = list(title = "Total Unit",
                 range = c(0, 2075)),
    yaxis = list(title = "Fashion Item"),
    title = "Top 10 Fashion Item Sell"
  )
dqlab.trans.freq.top.10.plot

# * 3.2. Bottom 10 Product Sell -------------------------------------------
dqlab.trans.freq.bottom.10 <-
  dqlab.trans.freq %>%
  tail(10) %>%
  arrange(Total)
dqlab.trans.freq.bottom.10$Product_Name <-
  factor(dqlab.trans.freq.bottom.10$Product_Name,
         levels = dqlab.trans.freq.bottom.10$Product_Name)
View(dqlab.trans.freq.bottom.10)
write.csv(dqlab.trans.freq %>% tail(10) %>% arrange(desc(Total)),
          file = "Bottom 10 Product Sell.txt")
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
    xaxis = list(title = "Total Unit",
                 range = c(0, 2075)),
    yaxis = list(title = "Fashion Item"),
    title = "Bottom 10 Fashion Item Sell"
  )
dqlab.trans.freq.bottom.10.plot


# 4. Products Bundles Combination for Bottom 10 Fashion Item --------------
dqlab.trans.combi.bottom.10 <-
  apriori(dqlab.trans.1,
          parameter = list(
            supp = 9 / length(dqlab.trans.1),
            confidence = 0.1,
            minlen = 2,
            maxlen = 3
          ))
dqlab.trans.combi.bottom.10 <-
  c(
    head(sort(
      subset(
        dqlab.trans.combi.bottom.10,
        (rhs %in% "Celana Jeans Sobek Pria")
      ),
      by = "lift"
    ), n = 1),
    head(sort(
      subset(dqlab.trans.combi.bottom.10,
             (rhs %in% "Tas Kosmetik")),
      by = "lift"
    ),
    n = 1),
    head(sort(
      subset(dqlab.trans.combi.bottom.10,
             (rhs %in% "Stripe Pants")),
      by = "lift"
    ),
    n = 1),
    head(sort(
      subset(dqlab.trans.combi.bottom.10,
             (rhs %in% "Pelembab")),
      by = "lift"
    ),
    n = 1),
    head(sort(
      subset(dqlab.trans.combi.bottom.10,
             (rhs %in% "Tali Ban Ikat Pinggang")),
      by = "lift"
    ),
    n = 1),
    head(sort(
      subset(
        dqlab.trans.combi.bottom.10,
        (rhs %in% "Baju Renang Pria Anak-anak")
      ),
      by = "lift"
    ),
    n = 1),
    head(sort(
      subset(dqlab.trans.combi.bottom.10,
             (rhs %in% "Hair Dye")),
      by = "lift"
    ),
    n = 1),
    head(sort(
      subset(dqlab.trans.combi.bottom.10,
             (rhs %in% "Atasan Baju Belang")),
      by = "lift"
    ),
    n = 1),
    head(sort(
      subset(
        dqlab.trans.combi.bottom.10,
        (rhs %in% "Tas Sekolah Anak Perempuan")
      ),
      by = "lift"
    ),
    n = 1),
    head(sort(
      subset(dqlab.trans.combi.bottom.10,
             (rhs %in% "Dompet Unisex")),
      by = "lift"
    ),
    n = 1)
  )
dqlab.trans.combi.bottom.10 <-
  DATAFRAME(dqlab.trans.combi.bottom.10)
View(dqlab.trans.combi.bottom.10)
write.csv(dqlab.trans.combi.bottom.10,
          file = "Products Bundles for Bottom 10 Fashion Item.txt")
# * 4.1. Visualization ----------------------------------------------------
dqlab.trans.combi.bottom.10 <-
  dqlab.trans.combi.bottom.10 %>%
  arrange(desc(lift))
nrow(dqlab.trans.combi.bottom.10)
dqlab.trans.combi.bottom.10$rule <-
  paste("Rule", 1:6)
dqlab.trans.combi.bottom.10$support <-
  round(dqlab.trans.combi.bottom.10$support, digits = 3)
dqlab.trans.combi.bottom.10$confidence <-
  round(dqlab.trans.combi.bottom.10$confidence, digits = 3)
dqlab.trans.combi.bottom.10$coverage <-
  round(dqlab.trans.combi.bottom.10$coverage, digits = 3)
dqlab.trans.combi.bottom.10$lift <-
  round(dqlab.trans.combi.bottom.10$lift, digits = 3)
dqlab.trans.combi.bottom.10.plot <-
  ggplotly(
    ggplot(
      dqlab.trans.combi.bottom.10,
      aes(
        x = str_wrap(LHS, 25),
        y = str_wrap(RHS, 15),
        fill = lift,
        text = paste(
          "<b>",
          "LHS : ",
          LHS,
          "<br>",
          "RHS : ",
          RHS,
          "<br>",
          "Lift : ",
          lift,
          "</b>",
          "<br>",
          "Support : ",
          support,
          "<br>",
          "Confidence : ",
          confidence,
          "<br>",
          "Coverage : ",
          coverage,
          "</br>"
        )
      )
    ) +
      geom_tile() +
      geom_text(
        aes(label = rule),
        position = position_stack(vjust = 1),
        vjust = 0.5
      ) +
      scale_fill_distiller(
        type = "seq",
        palette = "YlOrRd",
        direction = 1
      ) +
      labs(
        title = "Product Bundles for Bottom 10 Fashion Item",
        x = "LHS",
        y = "RHS",
        fill = "Lift"
      ) +
      theme(axis.text.x = element_text(angle = 45))
    ,
    tooltip = c("text")
  )
dqlab.trans.combi.bottom.10.plot


# 5. Products Bundles Based on Requested Filters --------------------------
dqlab.trans.combi <-
  apriori(dqlab.trans.1,
          parameter = list(
            supp = 10 / length(dqlab.trans.1),
            confidence = 0.5,
            minlen = 2,
            maxlen = 3
          ))
dqlab.trans.combi <-
  c(head(sort(dqlab.trans.combi, by = "lift"), n = 10))
dqlab.trans.combi <-
  DATAFRAME(dqlab.trans.combi)
View(dqlab.trans.combi)
write.csv(dqlab.trans.combi, file = "Top 10 Products Bundles Based on Filter.txt")
# * 5.1. Visualization ----------------------------------------------------
dqlab.trans.combi <-
  dqlab.trans.combi[, c(1, 2, 6)]
dqlab.trans.combi$rule <-
  paste("Rule", 1:10)
dqlab.trans.combi.plot <-
  ggplotly(
    ggplot(
      dqlab.trans.combi,
      aes(
        x = str_wrap(LHS, 25),
        y = str_wrap(RHS, 15),
        fill = lift,
        text = paste("LHS: ", LHS, "<br>",
                     "RHS: ", RHS, "<br>",
                     "Lift: ", lift, "<br>")
      )
    ) +
      geom_tile() +
      geom_text(
        aes(label = rule),
        position = position_stack(vjust = 1),
        vjust = 0.5
      ) +
      scale_fill_distiller(
        type = "seq",
        palette = "YlOrRd",
        direction = 1
      ) +
      labs(title = "Top 10 Product Bundles",
           x = "LHS",
           y = "RHS") +
      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 45)
      )
    ,
    tooltip = c("text")
  )
dqlab.trans.combi.plot


# 6. Product Bundles Based on Slow Moving Item ----------------------------
dqlab.trans.combi.slow.item <-
  apriori(dqlab.trans.1,
          parameter = list(
            supp = 10 / length(dqlab.trans.1),
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
dqlab.trans.combi.slow.item <-
  DATAFRAME(dqlab.trans.combi.slow.item)
View(dqlab.trans.combi.slow.item)
write.csv(dqlab.trans.combi.slow.item, file = "Products Bundles on Slow Moving Item.txt")
# * 6.1. Visualization ----------------------------------------------------
dqlab.trans.combi.slow.item <-
  dqlab.trans.combi.slow.item[, c(1, 2, 6)]
dqlab.trans.combi.slow.item <-
  dqlab.trans.combi.slow.item %>%
  arrange(desc(lift))
dqlab.trans.combi.slow.item$rule <-
  paste("Rule", 1:6)
dqlab.trans.combi.slow.item.plot <-
  ggplotly(
    ggplot(
      dqlab.trans.combi.slow.item,
      aes(
        x = str_wrap(LHS, 20),
        y = str_wrap(RHS, 15),
        fill = lift,
        text = paste("LHS: ", LHS, "<br>",
                     "RHS: ", RHS, "<br>",
                     "Lift: ", lift, "<br>")
      )
    ) +
      geom_tile() +
      geom_text(
        aes(label = rule),
        position = position_stack(vjust = 1),
        vjust = 0.5
      ) +
      scale_fill_distiller(
        type = "seq",
        palette = "YlOrRd",
        direction = 1
      ) +
      labs(title = "Product Bundles for Slow Move Item",
           x = "LHS",
           y = "RHS") +
      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 45)
      )
    ,
    tooltip = c("text")
  )
dqlab.trans.combi.slow.item.plot