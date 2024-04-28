# culling graphs
# to show difference between KDD and Purchased

# censor date
end <- mdy("12/06/2023")

cull_data <- lact_1_dc_data |> 
  filter(orgin != "Missing",
         orgin != "SVD",
         breed == "HO") |>
  select(id, fresh_date, orgin, cull_date, culled) |>
  mutate(censor_date = if_else(is.na(cull_date), end, cull_date),
         days = as.numeric(censor_date - fresh_date),
         # to set up table below
         orgin = fct_recode(orgin, KDD = "KDD",
                                   Purchased = "PUR"),
         orgin = fct_relevel(orgin, c("KDD", "Purchased")),
  )

# survivl graph
fitKM <- survfit(Surv(days, culled) ~ orgin, data = cull_data)

km<- ggsurvplot(fitKM, 
                pval = FALSE, 
                conf.int = TRUE,
                censor = FALSE,
                fun = "pct",
                size = 1,
                linetype = c(1, 2
                             ),
                palette = c("#fde725ff", "#440154ff"), 
                legend = "bottom",
                legend.title = "Source",
                legend.labs = c("KDD",
                                "Purchased"),
                xlab = "Days to cull after Calving",
                ylab = "% Cows Alive",
                xlim = c(0,180),
                ylim = c(50,100),
                #add.all = TRUE,
                tables.theme = theme_cleantable(),
                break.time.by = 30,
                short.panel.labs = TRUE)

# table
tbl_surv1 <- tbl_survfit(fitKM, 
                         times = c(30, 60, 90, 120, 180),
                         label = " ",
                         reverse = TRUE, 
                         label_header = "{time} Days") %>% 
  as_flex_table() %>% # makes formatting and pdf better
  add_header_row(top = TRUE,
                 values = c("Source",
                            "Probability of Culling (Confidence Interval) at",
                            "",
                            "",
                            "","")) %>% 
  fontsize(size = 9.5, part = "all") |>
  width(j = 1, 0.5, unit = "in") |> 
  bold(i = 1, bold = TRUE, part = "header") %>% # bolds headers
  bold(i = 2, bold = TRUE, part = "header") %>% 
  merge_at(i = 1:2, j = 1, part = "header") %>% #merges 1st row
  merge_at(i = 1, j = 2:6, part = "header") |>  # merges top columns
  set_table_properties(layout = "autofit") |> 
  fit_to_width(max_width= 7.5) 

tbl_surv1
  