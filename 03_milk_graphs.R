## milk analysis for HO only

milk <- lact_1_dc_data %>%
  select(id, orgin ,breed, week_1_milk, week_4_milk, week_8_milk) |> 
  filter(breed == "HO") |> 
  pivot_longer(cols = starts_with("week"),
               names_to = "week",
               names_pattern = "week_(\\d+)_milk",
               values_to = "milk"
               ) |> 
  mutate(milk = if_else(milk == 0, NA, milk))

# nice graph with violin and Median IQR
milk_graph <- milk |>  
  mutate(orgin = fct_relevel(orgin, c("SVD", "KDD", "PUR", "Missing"))) |> 
  filter(orgin != "Missing") |> 
  ggplot(aes(x = week, y = milk, fill = orgin))+
  geom_violin(aes(fill = orgin, color = orgin),
              alpha = 0.6,
              scale = "count"
              )+
  scale_color_viridis_d()+
  stat_summary(fun.data = median_hilow, 
               geom = "pointrange",
               size = 0.4,
               color = "black",
               position = position_dodge(width = 0.9))+
  stat_summary(fun = median, 
               aes(label = paste("", round(..y.., 0))), 
               geom = "text", 
               size = 5,
               vjust = -0.08,
               color = "black",
               alpha = 0.8,
               hjust = - 0.1,
               position = position_dodge(width = 0.9)) +
  scale_fill_viridis_d()+
  xlab("Week in Milk")+
  ylab("Estimated Milk (lbs)") +
  guides(fill = guide_legend("Source of Animal"),
         colour = guide_legend("Source of Animal")
         )+
  theme_bw(base_size = 12)+
  theme(legend.position = "bottom"
           )
milk_graph
  
# half eye
library(PupillometryR)

# nice graph with violin and Median IQR
milk_graph_half <- milk |>  
  mutate(orgin = fct_relevel(orgin, c("SVD", "KDD", "PUR", "Missing"))) |> 
  filter(orgin != "Missing",
         orgin != "SVD") |> 
  ggplot(aes(x = week, y = milk, fill = orgin))+
  geom_flat_violin(aes(fill = orgin, color = orgin),
              alpha = 0.6,
              scale = "count",
              position = position_dodge(width = 0.9)
  )+
  scale_color_viridis_d()+
  stat_summary(fun.data = median_hilow, 
               geom = "pointrange",
               size = 0.4,
               color = "darkgrey",
               position = position_dodge2(width = c(0.95, 0.85))
               )+
  stat_summary(fun = median, 
               aes(label = paste("", round(..y.., 0))), 
               geom = "text", 
               size = 5,
               vjust = -0.08,
               color = "black",
               alpha = 0.8,
               hjust = - 0.1,
               # need to equal dodge on both sides of the dodge in others
               position = position_dodge2(width = c(0.95, 0.85))
               ) +
  geom_dotplot(binaxis = "y", 
               dotsize = 10.0, 
               alpha = 0.8,
               stackdir = "down", 
               binwidth = 0.1,
               position = position_dodge((width = 0.9)
                                         )
               )+
  scale_fill_viridis_d()+
  xlab("Week in Milk")+
  ylab("Estimated Milk (lbs)") +
  guides(fill = guide_legend("Source of Animal"),
         colour = guide_legend("Source of Animal")
  )+
  theme_bw(base_size = 12)+
  theme(legend.position = "bottom"
  )
milk_graph_half

# milk summary table 
milk_table <- lact_1_dc_data |>  
  filter(orgin != "Missing",
         orgin != "SVD",
         # not enough other breeds 
         breed == "HO") |> 
  mutate(orgin = fct_recode(orgin, 
                            KDD = "KDD",
                            Purchased = "PUR"),
         orgin = fct_relevel(orgin, c("KDD", "Purchased")),
         source = fct_drop(orgin),
         total = 1,
         age_fresh_days = as.numeric(age_fresh_days),
         week_1_milk = if_else(week_1_milk == 0, NA, week_1_milk),
         week_4_milk = if_else(week_4_milk == 0, NA, week_4_milk),
         week_8_milk = if_else(week_8_milk == 0, NA, week_8_milk)
         ) |>
  select(source, total, days_in_closeup, age_fresh_days, xsick, xmast,
         week_1_milk, week_4_milk, week_8_milk, 
         first_test_log_scc, culled ) |> 
  tbl_summary(by = source,
              missing_text = "# with No Test",
              label = list(total ~ "",
                          days_in_closeup ~ "Days in Close Up",
                           age_fresh_days ~ "Age fresh (days)",
                           xsick ~ "Times sick during Lactation",
                          xmast ~ "Times mastitis",
                           culled ~ "% Sold/Died",
                           week_1_milk ~ "Milk in Week 1 (lbs)",
                          week_4_milk ~ "Milk in Week 4 (lbs)",
                          week_8_milk ~ "Milk in Week 8 (lbs)",
                          first_test_log_scc ~ "LS First Test"
              ),
              statistic = total ~ "N = {n}") |> 
  modify_header(label = "**Variable**") |> 
  modify_header(all_stat_cols() ~ "**{level}**") |> 
  modify_spanning_header(all_stat_cols() ~ "**Source**") |> 
  add_difference(include = c(days_in_closeup, xsick, xmast, culled, 
                             starts_with("week"), first_test_log_scc)) |> 
  modify_column_hide(columns = p.value)|>
  bold_labels() |> 
  # removes stats footnote
  modify_footnote(update = estimate ~ NA,
                  ci ~ "Confidence Interval",
                  ci ~ NA) |> 
  # used kable as flextable gave errors and is ugly
  as_kable_extra(booktabs = TRUE) |> 
  #locks the table position to where it is in quarto
  kable_styling(latex_options = "hold_position")


# 1/2 violin for disease
milk_disease <- milk |> 
  filter(orgin == "KDD") |> 
  left_join(gain) |> 
  # filter out cows with no disease status
  # occurs because no arrive KDD command
  filter(disease_status != is.na(disease_status)) |> 
  mutate(disease_status = fct_collapse(disease_status,
                                       Healthy = "Healthy",
                                       "1 Disease" = c("Pneumonia",
                                                       "Diarrhea",
                                                       "Navel",
                                                       "Ear"),
                                       Multiple = "Multiple"),
         disease_status = fct_relevel(disease_status,
                                      c("Healthy",
                                        "1 Disease",
                                        "Multiple"))
         ) 

# graph
milk_disease_graph <- milk_disease |> 
  ggplot(aes(x = week, y = milk, fill = disease_status))+
  geom_flat_violin(aes(fill = disease_status, color = disease_status),
                   alpha = 0.6,
                   scale = "count",
                   position = position_dodge(width = 0.9)
  )+
  scale_color_viridis_d()+
  stat_summary(fun.data = median_hilow, 
               geom = "pointrange",
               size = 0.4,
               color = "darkgrey",
               position = position_dodge2(width = c(1.1, 0.85, 0.75))
  )+
  stat_summary(fun = median, 
               aes(label = paste("", round(..y.., 0))), 
               geom = "text", 
               size = 5,
               vjust = -0.08,
               color = "black",
               alpha = 0.8,
               hjust = - 0.1,
               # need to equal dodge on both sides of the dodge in others
               position = position_dodge2(width = c(1.1, 0.85, 0.75))
  ) +
  geom_dotplot(binaxis = "y", 
               dotsize = 10.0, 
               alpha = 0.8,
               stackdir = "down", 
               binwidth = 0.1,
               position = position_dodge((width = 0.9)
               )
  )+
  scale_fill_viridis_d()+
  xlab("Week in Milk")+
  ylab("Estimated Milk (lbs)") +
  guides(fill = guide_legend("Disease Status"),
         colour = guide_legend("Disease Status")
  )+
  theme_bw(base_size = 12)+
  theme(legend.position = "bottom"
  )
milk_disease_graph

# disease table
 
# milk summary table 
disease_sum_table <- lact_1_dc_data |>
  filter(orgin == "KDD") |> 
  left_join(gain, by = join_by(id, birth_date, breed)) |> 
  filter(disease_status != is.na(disease_status)) |> 
  mutate(disease_status = fct_collapse(disease_status,
                                       Healthy = "Healthy",
                                       "1 Disease" = c("Pneumonia",
                                                       "Diarrhea",
                                                       "Navel",
                                                       "Ear"),
                                       Multiple = "Multiple"),
         disease_status = fct_relevel(disease_status,
                                      c("Healthy",
                                        "1 Disease",
                                        "Multiple"))
  ) |> 
  # not enough HJ yet
  filter(breed == "HO") |> 
  mutate(total = 1,
         age_fresh_days = as.numeric(age_fresh_days),
         week_1_milk = if_else(week_1_milk == 0, NA, week_1_milk),
         week_4_milk = if_else(week_4_milk == 0, NA, week_4_milk),
         week_8_milk = if_else(week_8_milk == 0, NA, week_8_milk)
  ) |>
  select(disease_status,
         total, days_in_closeup, age_fresh_days, xsick, xmast,
         week_1_milk, week_4_milk, week_8_milk, 
         first_test_log_scc, culled ) |> 
  tbl_summary(by = disease_status,
              missing_text = "# with No Test",
              label = list(total ~ "",
                           days_in_closeup ~ "Days in Close Up",
                           age_fresh_days ~ "Age fresh (days)",
                           xsick ~ "Times sick during Lactation",
                           xmast ~ "Times mastitis",
                           culled ~ "% Sold/Died",
                           week_1_milk ~ "Milk in Week 1 (lbs)",
                           week_4_milk ~ "Milk in Week 4 (lbs)",
                           week_8_milk ~ "Milk in Week 8 (lbs)",
                           first_test_log_scc ~ "LS First Test"
              ),
              statistic = total ~ "N = {n}") |> 
  modify_header(label = "**Variable**") |> 
  modify_header(all_stat_cols() ~ "**{level}**") |> 
  bold_labels() |> 
  # used kable as flextable gave errors and is ugly
  as_kable_extra(booktabs = TRUE) |> 
  #locks the table position to where it is in quarto
  kable_styling(latex_options = "hold_position")

