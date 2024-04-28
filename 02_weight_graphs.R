if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  rio,           # import/export
  here,          # filepaths
  lubridate,     # working with dates
  forcats,       # factors
  janitor,       # tables
  survminer,     # survival
  survival,
  gtsummary,
  PupillometryR, # for half violin
  flextable,
  kableExtra,
  tidyverse,     # data mgmt and viz
  readr,         # import 
  zoo,           # dates
  knitr,         # tables
  skimr          # quick summaries     
)


# weight analysis
# graph weight, need to add data checks for weight.
graph_wt <- kdd_all |> 
  filter(breed == "HJ" | breed == "HO") |> 
  mutate(disease_status = fct_relevel(as_factor(disease_status),
                                      c("Healthy", "Pneumonia", "Diarrhea", "Ear",
                                        "Navel", "Multiple"))) |> 
  ggplot(mapping = aes(x = as.numeric(age_at_measure),
                       y = weight, 
                       line_type = disease_status,
                       group = disease_status,
                       color = disease_status))+
  geom_point(alpha = 0.3, size = 0.8)+
  geom_smooth(aes(linetype = disease_status), 
              # sets alpha of CI
              alpha = 0.3,
              show.legend = TRUE)+
  facet_wrap(vars(breed), nrow = 2,
             labeller = labeller(breed = c("HJ" = "HoJo",
                                           "HO" = "Holstein")))+
  guides(color = guide_legend("Disease Status"),
         linetype = guide_legend("Disease Status",
                                 # puts linetype in same graph
                                 override.aes = list(linetype = c(1, 2, 3,
                                                                  4, 5, 6)
                                 )
         )
  )+
  scale_colour_viridis_d(option = "D",
                         direction = -1)+
  labs(color = "Disease Status", linetype  = "Disease Status")+
  xlab("Age (days)")+
  ylab("Weight (lbs)") +
  theme_bw(base_size = 12)+
  theme(#axis.title.x = element_text(face = "bold"),
    axis.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0),
    strip.text.x = element_text(face = "bold.italic"),
    strip.background.x = element_rect(fill = "white"),
    legend.position = "bottom")


# table of gain data for animals gone home

gain <-total_gain |> 
  left_join(disease_combo, by = join_by(id, birth_date)) |> 
  mutate(pneu = if_else(is.na(total_pneu),0,1),
         diarhea = if_else(is.na(total_diarhea),0,1),
         navel = if_else(is.na(total_navel),0,1),
         ear = if_else(is.na(total_ear),0,1),
         disease_status = case_when(pneu == 1 & diarhea == 0
                                    & navel ==0 & ear == 0 ~ "Pneumonia",
                                    pneu == 0 & diarhea == 1
                                    & navel ==0 & ear == 0 ~ "Diarrhea",
                                    pneu == 0 & diarhea == 0
                                    & navel == 1 & ear == 0 ~ "Navel",
                                    pneu == 0 & diarhea == 0
                                    & navel == 0 & ear == 1 ~ "Ear",
                                    pneu == 0 & diarhea == 0
                                    & navel ==0 & ear == 0 ~ "Healthy",
                                    TRUE ~ "Multiple")
  ) 

# summary table of animals gone to KDD

table_sum <- gain |> 
  mutate(disease_status = fct_relevel(as_factor(disease_status),
                                      c("Healthy", "Pneumonia", "Diarrhea", "Ear",
                                        "Navel", "Multiple")),
         # to fix cows with no tokdd data but have weight data
         breed = if_else(is.na(breed),"Missing", breed),  
         sold = if_else(rpro == "SLD/DIE",1,0),
         home = if_else(go_home_date>0, 1, 0, missing = 0),
         breed = fct_recode(as_factor(breed), HoJo = "HJ", 
                            Holstein = "HO"),
         breed = fct_infreq(breed)
         )|> 
  select(breed, disease_status, first_weight, first_measure_age,
         sold, home, days_at_grower) |> 
  # to get total on a separate row
  # necessary as flextable option makes it too big
  mutate(total = 1) |>
  select(total, everything()) |> 
  tbl_summary(by = breed,
              label = list(total ~ "",
                disease_status ~ "Disease Status",
                first_weight ~ "1st Weight (lbs)",
                first_measure_age ~ "Age at 1st weight (lbs)",
                sold ~ "Sold",
                home ~ "Back at Skyview",
                days_at_grower ~ "Days at Grower"
                ),
              statistic = total ~ "N = {n}"
              ) |> 
  add_overall()  |> 
  modify_spanning_header(all_stat_cols() ~ "Breed") |> 
  bold_labels() |> 
  modify_header(label = "**Variable**") |> 
  modify_header(all_stat_cols() ~ "**{level}**") |> 
  # used kable as flextable gave errors and is ugly
  as_kable_extra(booktabs = TRUE) |> 
  #locks the table position and sized down to where it is in quarto
  kable_styling(latex_options = c("scale_down", "hold_position"))


# ADG for animals gone home
# can't add CI yet hence IQR
table_wt <- gain |> 
  filter(breed == "HJ" | breed == "HO") |> 
  mutate(disease_status = fct_relevel(as_factor(disease_status),
                                      c("Healthy", "Pneumonia", "Diarrhea", "Ear",
                                        "Navel", "Multiple")),
         breed = fct_recode(as_factor(breed), HoJo = "HJ", 
                            Holstein = "HO")) |> 
  filter(go_home_date>0) |> 
  select(id, breed, disease_status, avg_gain_grower) |> 
  tbl_continuous(variable = avg_gain_grower,
                 by = breed,
                 include = disease_status,
                 statistic = everything() ~ "{mean} ({p25}, {p75})"
  ) |> 
  add_overall() |> 
  # removes indents as delete_rows in flextable screws it up
  modify_footnote(update = everything() ~ NA) |> 
  modify_column_indent(columns = label, undo = TRUE) |> 
  as_flex_table() |> 
  add_header_row(top = TRUE,
                 values = c("", "Overall", "Breed", "")) |> 
  add_header_row(top = TRUE,
                 values = c("Disease Status",
                            "Average Daily Gain (IQR) at the Grower",
                            "", 
                            "")) %>% 
  bold(i = 1:2, bold = TRUE, part = "header") %>% # bolds headers
  merge_at(i = 1:3, j = 1, part = "header") %>% #merges 1st row
  merge_at(i = 1, j = 2:4, part = "header") %>% # merges top columns
  merge_at(i = 2, j = 3:4, part = "header") %>% # merges top columns
  merge_at(i = 2:3, j = 2, part = "header") %>% #merges 1st row
  autofit() %>% 
  fit_to_width(max_width= 6.5) |> 
  delete_rows(i = 1, part = "body") |> 
  delete_rows(i = 1, part = "footer")|> 
  hline_bottom(part = "body")

# graph of differneces in time at grower for diseases

diseae_time_graph <- gain |>  
  filter(breed == "HO" | breed == "HJ") |> 
  filter(go_home_date>0) |> 
 mutate(disease_status = fct_collapse(disease_status,
                                      Healthy = "Healthy",
                                      "1 Disease" = c("Pneumonia",
                                                      "Diarrhea",
                                                      "Navel",
                                                      "Ear"),
                                      Multiple = "Multiple"),
        breed = fct_recode(as_factor(breed), HoJo = "HJ", 
                            Holstein = "HO")) |> 
  mutate(disease_status = fct_relevel(disease_status,
                                      c("Healthy",
                                        "1 Disease",
                                        "Multiple"))) |>
  ggplot(aes(x = disease_status, y = days_at_grower, fill = breed))+
  geom_flat_violin(aes(fill = breed, color = breed),
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
  xlab("")+
  ylab("Days at the Grower (lbs)") +
  guides(fill = guide_legend("Breed"),
         colour = guide_legend("Breed")
  )+
  theme_bw(base_size = 12)+
  theme(legend.position = "bottom"
  )
diseae_time_graph

# age at going home
go_home_age_graph <- gain |>  
  filter(breed == "HO" | breed == "HJ") |> 
  filter(go_home_date>0) |> 
  mutate(go_home_age = go_home_date - birth_date,
         breed = fct_recode(as_factor(breed), HoJo = "HJ", 
                            Holstein = "HO")) |> 
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
                                        "Multiple"))) |> 
  ggplot(aes(x = breed, y = as.numeric(go_home_age)
             , fill = disease_status))+
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
               dotsize = 20.0, 
               alpha = 0.8,
               stackdir = "down", 
               binwidth = 0.1,
               position = position_dodge((width = 0.9)
               )
  )+
  scale_fill_viridis_d()+
  xlab("Disease")+
  ylab("Age at Go Home (days)") +
  guides(fill = guide_legend("Disease Status"),
         colour = guide_legend("Disease Status")
  )+
  theme_bw(base_size = 12)+
  theme(legend.position = "bottom"
  )
go_home_age_graph



