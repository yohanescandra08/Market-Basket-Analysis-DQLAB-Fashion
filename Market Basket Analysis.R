# 1. Packages List --------------------------------------------------------
library(tidyverse)
library(arules)
library(plotly)
library(RColorBrewer)
library(reactablefmtr)
library(htmltools)
library(htmlwidgets)


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
dqlab.item.freq <-
  itemFrequency(dqlab.trans, type = "absolute")
print(dqlab.item.freq)


# 3. Data Exploration -----------------------------------------------------
dqlab.exp <-
  sort(dqlab.item.freq, decreasing = T)
dqlab.exp <-
  data.frame(
    Product_Name = names(dqlab.exp),
    Total = dqlab.exp,
    row.names = NULL
  )
View(dqlab.exp)
write.csv(dqlab.exp, file = "Total Unit Sell.txt")
# * 3.1. Top 10 Product Sell ----------------------------------------------
dqlab.exp.top.10 <-
  dqlab.exp %>%
  head(10)
dqlab.exp.top.10$Product_Name <-
  factor(dqlab.exp.top.10$Product_Name,
         levels = dqlab.exp.top.10$Product_Name)
View(dqlab.exp.top.10)
write.csv(dqlab.exp %>% head(10) %>% arrange(desc(Total)),
          file = "Top 10 Product Sell.txt")
# * 3.2. Bottom 10 Product Sell -------------------------------------------
dqlab.exp.bottom.10 <-
  dqlab.exp %>%
  tail(10)
dqlab.exp.bottom.10$Product_Name <-
  factor(dqlab.exp.bottom.10$Product_Name,
         levels = dqlab.exp.bottom.10$Product_Name)
View(dqlab.exp.bottom.10)
write.csv(dqlab.exp %>% tail(10) %>% arrange(desc(Total)),
          file = "Bottom 10 Product Sell.txt")
# * 3.3. Visualization ----------------------------------------------------
dqlab.exp.plot <-
  dqlab.exp %>%
  mutate(Rank = 1:nrow(dqlab.exp), .before = 1) %>%
  mutate(
    Percentage = dqlab.exp$Total / 3450,
    Top_bottom =
      c(
        rep(paste("Top", 10), times = 10),
        rep(NA, each = nrow(dqlab.exp) - 20),
        rep(paste("Bottom", 10), times = 10)
      ),
    Has_missing = "",
    Colors = rep(c("green", "grey", "red"), times = c(10, (nrow(
      dqlab.exp
    ) - 20), 10))
  )
dqlab.exp.plot$Has_missing <- complete.cases(dqlab.exp.plot)
dqlab.exp.plot <-
  browsable(tagList(
    tags$label(
      tags$input(type = "checkbox",
                 onclick = "Reactable.setFilter('items-missing', 'Has_missing', event.target.checked)"),
      "Top 10 & Bottom 10 Fashion Item"
    ),
    reactable(
      dqlab.exp.plot,
      theme = pff(centered = T),
      columns = list(
        Rank = colDef(name = "Rank",
                      align = "center"),
        Product_Name = colDef(name = "Product Name",
                              align = "left"),
        Total = colDef(
          name = "Transaction Frequency",
          align = "center",
          cell = color_tiles(
            data = dqlab.exp.plot,
            colors = brewer.pal(5, "RdYlGn"),
            box_shadow = T
          )
        ),
        Top_bottom = colDef(
          name = "Top 10 / Bottom 10",
          align = "center",
          cell = color_tiles(
            data = dqlab.exp.plot,
            color_ref = "Colors",
            box_shadow = T
          )
        ),
        Colors = colDef(show = F),
        Percentage = colDef(
          name = "Transaction Percentage",
          align = "center",
          cell = data_bars(
            data = dqlab.exp.plot,
            fill_color = brewer.pal(5, "RdYlGn"),
            round_edges = T,
            text_position = "outside-end",
            background = "transperent",
            box_shadow = T,
            bar_height = 5,
            number_fmt = scales::label_percent(decimal.mark = ",", accuracy = 0.01)
          )
        ),
        Has_missing = colDef(
          show = FALSE,
          filterMethod = JS(
            "function(rows, columnId, filterValue) {
          if (filterValue === true) {
            return rows.filter(function(row) {
              const hasMissing = row.values[columnId]
              return hasMissing
            })
          }
          return rows
        }"
          )
        )
      ),
      elementId = "items-missing",
      defaultPageSize = 20
    )
  ))
dqlab.exp.plot


# 4. Product Bundle Combinations for Bottom 10 Fashion Item ---------------
dqlab.exp.bottom.10.combi <-
  apriori(dqlab.trans,
          parameter = list(
            supp = 9 / length(dqlab.trans),
            confidence = 0.1,
            minlen = 2,
            maxlen = 3
          ))
dqlab.exp.bottom.10.combi <-
  c(
    head(sort(
      subset(dqlab.exp.bottom.10.combi,
             (rhs %in% "Celana Jeans Sobek Pria")),
      by = "lift"
    ), n = 1),
    head(sort(
      subset(dqlab.exp.bottom.10.combi,
             (rhs %in% "Tas Kosmetik")),
      by = "lift"
    ),
    n = 1),
    head(sort(
      subset(dqlab.exp.bottom.10.combi,
             (rhs %in% "Stripe Pants")),
      by = "lift"
    ),
    n = 1),
    head(sort(
      subset(dqlab.exp.bottom.10.combi,
             (rhs %in% "Pelembab")),
      by = "lift"
    ),
    n = 1),
    head(sort(
      subset(dqlab.exp.bottom.10.combi,
             (rhs %in% "Tali Ban Ikat Pinggang")),
      by = "lift"
    ),
    n = 1),
    head(sort(
      subset(
        dqlab.exp.bottom.10.combi,
        (rhs %in% "Baju Renang Pria Anak-anak")
      ),
      by = "lift"
    ),
    n = 1),
    head(sort(
      subset(dqlab.exp.bottom.10.combi,
             (rhs %in% "Hair Dye")),
      by = "lift"
    ),
    n = 1),
    head(sort(
      subset(dqlab.exp.bottom.10.combi,
             (rhs %in% "Atasan Baju Belang")),
      by = "lift"
    ),
    n = 1),
    head(sort(
      subset(
        dqlab.exp.bottom.10.combi,
        (rhs %in% "Tas Sekolah Anak Perempuan")
      ),
      by = "lift"
    ),
    n = 1),
    head(sort(
      subset(dqlab.exp.bottom.10.combi,
             (rhs %in% "Dompet Unisex")),
      by = "lift"
    ),
    n = 1)
  )
dqlab.exp.bottom.10.combi <-
  DATAFRAME(dqlab.exp.bottom.10.combi)
View(dqlab.exp.bottom.10.combi)
write.csv(dqlab.exp.bottom.10.combi,
          file = "Product Bundle Combinations for Bottom 10 Fashion Item.txt")
# * 4.1. Visualization ----------------------------------------------------
dqlab.exp.bottom.10.combi <-
  dqlab.exp.bottom.10.combi %>%
  arrange(desc(lift))
nrow(dqlab.exp.bottom.10.combi)
dqlab.exp.bottom.10.combi$rule <-
  paste("Rule", 1:6)
dqlab.exp.bottom.10.combi.plot <-
  ggplotly(
    ggplot(
      dqlab.exp.bottom.10.combi,
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
          round(lift, 2),
          "</b>",
          "<br>",
          "Support : ",
          round(support, 4) * 100,
          "%",
          "<br>",
          "Confidence : ",
          round(confidence, 4) * 100,
          "%",
          "<br>",
          "Coverage : ",
          round(coverage, 4) * 100,
          "%",
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
        title = "Product Bundle Combinations for Bottom 10 Fashion Item",
        x = "LHS",
        y = "RHS",
        fill = "Lift"
      ) +
      theme(axis.text.x = element_text(angle = 45))
    ,
    tooltip = c("text")
  )
dqlab.exp.bottom.10.combi.plot


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