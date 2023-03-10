# 1. Packages List --------------------------------------------------------
library(tidyverse)
library(magrittr)
library(scales)
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
  ) %>% print()


# 3. Data Exploration -----------------------------------------------------
dqlab.exp <-
  itemFrequency(dqlab.trans, "absolute") %>% data.frame() %>%
  arrange(desc(.)) %>% rename(!!"Total" := ".") %>%
  rownames_to_column("Product_Name") %>% print() %>%
  write.csv(file = "Total Unit Sell.txt")

# * 3.1. Top 10 Product Sell ----------------------------------------------
dqlab.exp %>% head(10) %>% print() %>% write.csv(file = "Top 10 Product Sell.txt")
# * 3.2. Bottom 10 Product Sell -------------------------------------------
dqlab.exp %>% tail(10) %>% arrange(desc(Total)) %>% print() %>% write.csv(file = "Bottom 10 Product Sell.txt")

# * 3.3. Visualization ----------------------------------------------------
dqlab.exp <-
  dqlab.exp %>% mutate(Rank = rownames(dqlab.exp), .before = 1) %>%
  mutate(
    Percentage = dqlab.exp$Total / 3450,
    Top_bottom = c(
      rep(paste("Top", 10), times = 10),
      rep(NA, each = nrow(dqlab.exp) - 20),
      rep(paste("Bottom", 10), times = 10)
    ),
    Has_missing = complete.cases(Top_bottom),
    Colors = rep(c("green", "grey", "red"),
                 times = c(10, (nrow(
                   dqlab.exp
                 ) - 20), 10))
  )
browsable(tagList(
  tags$label(
    tags$input(type = "checkbox",
               onclick = "Reactable.setFilter('items-missing', 'Has_missing', event.target.checked)"),
    "Top 10 & Bottom 10 Fashion Item"
  ),
  reactable(
    data = dqlab.exp,
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
          data = dqlab.exp,
          colors = brewer.pal(5, "RdYlGn"),
          box_shadow = T
        )
      ),
      Top_bottom = colDef(
        name = "Top 10 / Bottom 10",
        align = "center",
        cell = color_tiles(
          data = dqlab.exp,
          color_ref = "Colors",
          box_shadow = T
        )
      ),
      Colors = colDef(show = F),
      Percentage = colDef(
        name = "Transaction Percentage",
        align = "center",
        cell = data_bars(
          data = dqlab.exp,
          fill_color = brewer.pal(5, "RdYlGn"),
          round_edges = T,
          text_position = "outside-end",
          background = "transperent",
          box_shadow = T,
          bar_height = 5,
          number_fmt = label_percent(decimal.mark = ",", accuracy = 0.01)
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
)) %>% saveWidget(file = "Total Unit Table.html")


# 4. Product Bundle Combinations for Bottom 10 Fashion Item ---------------
dqlab.bundle.bottom.10 <-
  dqlab.trans %>% apriori(parameter = list(
    supp = 9 / length(dqlab.trans),
    confidence = 0.1,
    minlen = 1,
    maxlen = 3
  )) %>% DATAFRAME() %>% mutate(LHS = str_remove_all(LHS, "[{}]"),
                                RHS = str_remove_all(RHS, "[{}]")) %>%
  filter(RHS %in% (dqlab.exp$Product_Name %>% tail(n = 10))) %>%
  arrange(desc(lift)) %>% distinct(RHS, .keep_all = T) %>%
  mutate(
    rule = paste("Rule", 1:6),
    RHS = factor(RHS, levels = rev(fct_inorder(RHS))),
    LHS = factor(LHS, levels = fct_inorder(LHS))
  ) %T>%
  write.csv(file = "Product Bundle Combinations for Bottom 10 Fashion Item.txt") %>%
  print()

# * 4.1. Visualization ----------------------------------------------------
ggplotly(
  ggplot(dqlab.bundle.bottom.10, aes(
    LHS,
    RHS,
    fill = lift,
    text = paste0(
      "<b>",
      "LHS : ",
      LHS,
      "\n",
      "RHS : ",
      RHS,
      "\n",
      "Lift : ",
      round(lift, 2),
      "</b>\n",
      "Confidence : ",
      round(confidence, 4) * 100,
      " %",
      "\n",
      "Support : ",
      round(support, 4) * 100,
      " %",
      "\n"
    )
  )) + geom_tile() + geom_text(aes(label = str_replace_all(rule, " ", "<br>"))) + labs(
    x = "<b>LHS</b>",
    y = "<b>RHS</b>",
    title = "<b>Product Bundle Combinations for Bottom 10 Fashion Items</b>",
    fill = "<b>Lift</b>"
  ) + scale_x_discrete(labels = str_replace_all(dqlab.bundle.bottom.10$LHS, ",", ",<br>")) +
    scale_y_discrete(labels = str_wrap(rev(
      dqlab.bundle.bottom.10$RHS
    ), width = 15)) +
    scale_fill_distiller(
      type = "seq",
      palette = "YlOrRd",
      direction = 1
    ) + theme(plot.title = element_text(hjust = 0.5)),
  tooltip = c("text")
) %>% saveWidget(file = "Product Bundle Combinations for Bottom 10 Fashion Item.html")


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