# 1. Packages List --------------------------------------------------------
library(tidyverse)
library(arules)
library(arulesViz)
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
    Top_bottom =
      c(
        paste("Top", 1:10),
        rep(NA, each = nrow(dqlab.exp) - 20),
        paste("Bottom", 1:10)
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
          name = "Total Unit",
          align = "center",
          cell = data_bars(
            data = dqlab.exp.plot,
            fill_color = brewer.pal(5, "BrBG"),
            round_edges = T,
            text_position = "outside-end",
            background = "transperent",
            box_shadow = T,
            bar_height = 5
          )
        ),
        Top_bottom = colDef(
          name = "Top 10 / Bottom 10",
          align = "center",
          cell = pill_buttons(
            data = dqlab.exp.plot,
            color_ref = "Colors",
            box_shadow = T
          )
        ),
        Colors = colDef(show = F),
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