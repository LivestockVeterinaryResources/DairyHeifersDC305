# Import and Manage Farm Heifer data

#Load Packages----

# rio tips comes from https://epirhandbook.com/suggested-packages-1.html

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  rio,           # import/export
  here,          # filepaths
  lubridate,     # working with dates
  forcats,       # factors
  janitor,       # tables
  tidyverse,     # data mgmt and viz
  readr,         # import 
  zoo,           # dates
  knitr,         # tables
  skimr          # quick summaries     
)

# setup variables used throughout----
farm1_name <- "Skyview" # enter Farm name for reports
filename1 <- "skyview" # enter name used for farm in DC export
start_date <- "2023-07-21"

# import data----

lact_1_dc_data <- import("sourcedata/skyview_heifer.CSV")|>
  clean_names() |> 
  select(-c(v34)) |> 
  mutate(id = as.numeric(id)) |> 
  filter(id != is.na(id)) |> 
  rename(days_in_closeup = dincu) |> 
  mutate(birth_date = mdy(bdat, quiet = TRUE),
         fresh_date = mdy(fdat, quiet = TRUE),
         purchase_date = mdy(edat, quiet = TRUE),
         last_test_date = mdy(tdat, quiet = TRUE),
         cull_date = mdy(ardat, quiet = TRUE),
         age_fresh_days = fresh_date-birth_date,
         breed = as_factor(cbrd),
         sire_breed = as_factor(sbrd),
         dam_breed = as_factor(dbrd),
         last_test_milk = milk,
         last_test_scc = scc,
         last_test_log_scc = lgscc,
         first_test_log_scc = log1,
         second_test_log_scc = lsc2,
         week_1_milk = wmk1,
         week_4_milk = wmk4,
         week_8_milk = wmk8,
         mature_equiv_305m = me305,
         energy_corrected_305m = ec305,
         culled = if_else(evt == "SOLD" | evt == "DIED", 1,0),
         times_mast = xmast,
         times_sick = xsick,
         orgin = as_factor(as.character(orgin))
         ) |> 
  select(-c(bdat, fdat, edat, tdat, cbrd, ardat, sbrd, dbrd, evt, rpro,
            agefr, milk, scc, lgscc, log1, lsc2, ec305, me305, wmk1,
            wmk4, wmk8))

# data checks for factors
# check orgin
lact_1_dc_data |> 
  count(orgin) # need to check once update with Sara's new dc

# fix origin codes
lact_1_dc_data <- lact_1_dc_data |> 
  mutate(orgin = fct_recode(orgin, PUR = "GW",
                            PUR = "PUR",
                          SVD = "SVD",
                            KDD = "KDD",
                            Missing = "-",
                          Missing =  "87"),
  )

# breed data checks
lact_1_dc_data |> 
  count(breed) # need to check once update with Sara's new dc

# fix breed factors
lact_1_dc_data <- lact_1_dc_data |> 
  mutate(breed = fct_recode(breed, HO = "H",
                            HJ = "HJ",
                            Other = "J",
                            Other = "X",
                            Other = "S",
                            Missing = "-"),
         # fix coding errors in DC
         breed = case_when(sire_breed == "J" & dam_breed == "H" ~ "HJ",
                           sire_breed == "LM"~ "Other",
                           TRUE ~ breed))

# arrive at KDD data
# misses some cows that did not have an arrivekdd event
# for example 8757
to_kdd <- import("sourcedata/skyview_to_kdd.CSV")|>
  clean_names() |> 
  select(-c(v15, technician, protocols, remark)) |> 
  mutate(birth_date = mdy(bdat, quiet = TRUE),
         breed = as_factor(cbrd),
         sire_breed = as_factor(sbrd),
         dam_breed = as_factor(dbrd),
                  go_home_date = mdy(ghdat, quiet = TRUE),
         to_kdd_date = mdy(date, quiet = TRUE),
         go_home_age  = go_home_date - birth_date,
         days_at_grower = go_home_date - to_kdd_date,
         birth_weight = brthw,
         birth_weight = if_else(birth_weight != 0, birth_weight, NA),
         age_to_kdd = to_kdd_date-birth_date) |> 
  select(-c(bdat, brthw, rpro, ghdat, event, dim, date))

# remove duplicates
to_kdd <- to_kdd |> 
  distinct(id, birth_date,
           .keep_all = TRUE)

# check breed
to_kdd |> 
  count(breed)

# fix breed
to_kdd <- to_kdd |> 
  mutate(breed = fct_recode(breed, HO = "H",
                            HJ = "HJ",
                            Jersey = "J",
                            Other = "X",
                            Other = "S",
                            Missing = "-"),
         breed = case_when(sire_breed == "J" & dam_breed == "H" ~ "HJ",
                           sire_breed == "H" & dam_breed == "J" ~ "HJ",
                           sire_breed == "H" & dam_breed == "H" ~ "HO",
                           breed == "HO" ~ "HO",
                           breed == "Missing" ~ "Missing",
                           TRUE ~ "Other")
  )

# check breed
to_kdd |> 
  count(breed)

# KDD weights
kdd_weights <- import("sourcedata/skyview_weights.CSV")|>
  clean_names() |> 
  mutate(birth_date = mdy(bdat, quiet = TRUE),
         birth_weight = if_else(bw8t>0, bw8t, NA),
         go_home_date = mdy(ghdat, quiet = TRUE),
         measure_date = mdy(date, quiet = TRUE),
         age_at_measure = measure_date-birth_date) |> 
  select(-c(v13, technician, protocols, bdat,  cbrd,
            ghdat, date, dim, event, bw8t))  |> 
  # filter out heifers with data that is too early
  filter(birth_date > "2021-01-01") |> 
  # count # of measurement
  group_by(id, birth_date) |> 
  mutate(times_weight = dense_rank(measure_date)) |> 
  ungroup() |> 
  # split remark into weight/height
  separate_wider_delim(remark, delim = "/",
                       names = c("weight", "height"),
                       too_few = "align_start",
                       ## fixe error message with 2 //
                       too_many = "drop") |> 
  mutate(weight = as.numeric(weight),
         height = as.numeric(height)
         )

# data checks in case weight/height reversed in Measure REM
# heights too high 
kdd_weights |> 
  filter(height>60) |> 
  count(height) |> 
  print() 

# check to see if reversed
kdd_weights |> 
  filter(height>60) |> 
  select(id, weight, height)

# typos
kdd_weights |> 
  filter(weight<50 | weight>2000) |> 
  select(id, weight, height)

kdd_weights |> 
  filter(weight<200 & age_at_measure>365 & height<60) |> 
  select(id, weight, height, age_at_measure)

# fix reverse weight/height and typos
kdd_weights <- kdd_weights |> 
  mutate(height_old = height,
         height = if_else(height>60, weight, height),
         weight = case_when(is.na(height_old) ~ weight,
                            height_old>60 ~ height_old,
                            TRUE ~ weight),
         # to catch typos
         weight = if_else(weight<50 | weight>2000, NA, weight),
         weight = if_else(weight<400 & age_at_measure>365, NA, weight)
       ) |> 
  select(-c(height_old))

# join weight with breed data
kdd_weight_breed <- to_kdd |> 
  select(id, birth_date, breed, days_at_grower,) |> 
  right_join(kdd_weights, by = join_by(id, birth_date),
             relationship = "one-to-many")

# calc ADG

kdd_weight_adg <- kdd_weight_breed |> 
  group_by(id) |> 
  mutate(prev_weight = lag(weight, order_by = measure_date),
         prev_age = lag(age_at_measure, order_by = measure_date),
         days_between_measure = age_at_measure-prev_age,
         total_gain_period = weight-prev_weight,
         avg_gain_period = total_gain_period/as.numeric(days_between_measure),
         total_gain_birth = weight-birth_weight,
         average_gain_birth = total_gain_birth/as.numeric(age_at_measure)
         )|> 
  ungroup()


# below mutate is only valid for only first row so need to slice to get use
total_gain <- kdd_weight_adg |> 
  group_by(id) |> 
  mutate(first_weight = lag(weight, default = first(weight)),
         first_measure_age = lag(age_at_measure, 
                                 default = first(age_at_measure)),
         last_weight = lag(weight, default = last(weight, 
                                             order_by = measure_date)),
         last_measure_age = lag(age_at_measure, 
                           default = last(age_at_measure,
                                          order_by = measure_date))
         ) |> 
  slice_head() |> 
  ungroup() |> 
  # picked these so I can graph later
  select(c(id, breed, birth_date, rpro, birth_weight,
           first_weight, last_weight, days_at_grower,
           last_measure_age, first_measure_age, go_home_date)) |> 
  mutate(total_gain_grower = last_weight-first_weight,
         days_since_first = last_measure_age - first_measure_age,
         avg_gain_grower = total_gain_grower/as.numeric(days_since_first),
         total_gain_birth = last_weight-birth_weight,
         average_gain_birth = total_gain_birth/as.numeric(last_measure_age)
  )

# disease data
disease <- import("sourcedata/skyview_heifer_disease.CSV")|>
  clean_names() |> 
  filter(lact == 0) |> 
  # create disease dates
  mutate(birth_date = mdy(bdat, quiet = TRUE),
         pneu_date = mdy(case_when(event == "PNEU" ~ date)),
         navel_date = mdy(case_when(event == "NAVEL" ~ date)),
         diarhea_date = mdy(case_when(event == "DIARHEA" ~ date)),
         ear_date = mdy(case_when(event == "EARJNT" ~ date))) |> 
  group_by(id) |> 
  mutate(times_pneu = dense_rank(pneu_date),
         times_diarhea = dense_rank(diarhea_date),
         times_navel = dense_rank(navel_date),
         times_ear = dense_rank(ear_date),
         times_sick = dense_rank(date)) |> 
  ungroup() |> 
  select(-c(bdat, lact, event, dim, date, remark, protocols, technician, v10))
 
# to create single lines for each disease
disease_pneu <- disease |> 
  select(id, birth_date, pneu_date, times_pneu, birth_date) |> 
  filter(pneu_date>0) |> 
  group_by(id) |> 
  add_count(id, name = "total_pneu") |> 
  ungroup() |> 
    pivot_wider(id_cols = c(id, total_pneu, birth_date),
                names_from = times_pneu,
                values_from = pneu_date) |> 
  rename_with(~paste0("pneu_date_", .), any_of(c(4,5,6,7)))

disease_diarhea <- disease |> 
  select(id, birth_date, diarhea_date, times_diarhea) |> 
  filter(diarhea_date>0) |> 
  group_by(id) |> 
  add_count(id, name = "total_diarhea") |> 
  ungroup() |> 
  pivot_wider(id_cols = c(id, total_diarhea, birth_date),
              names_from = times_diarhea,
              values_from = diarhea_date) |> 
  rename_with(~paste0("diarhea_date_", .),  any_of(c(4,5,6,7)))

disease_ear <- disease |> 
  select(id, birth_date, ear_date, times_ear) |> 
  filter(ear_date>0) |> 
  group_by(id) |> 
  add_count(id, name = "total_ear") |> 
  ungroup() |> 
  pivot_wider(id_cols = c(id, total_ear, birth_date),
              names_from = times_ear,
              values_from = ear_date) |> 
  rename_with(~paste0("ear_date_", .), any_of(c(4,5,6,7)))

disease_navel <- disease |> 
  select(id, birth_date, navel_date, times_navel) |> 
  filter(navel_date>0) |> 
  group_by(id) |> 
  add_count(id, name = "total_navel") |> 
  ungroup() |> 
  pivot_wider(id_cols = c(id, total_navel, birth_date),
              names_from = times_navel,
              values_from = navel_date) |> 
  rename_with(~paste0("navel_date_", .), any_of(c(4,5,6,7)))

# combine all together
disease_combo <- disease_pneu |>
  full_join(disease_diarhea) |> 
  full_join(disease_ear) |> 
  full_join(disease_navel) |> 
  # to get total times sick 
  # na.rm = TRUE makes NA count as 0
  mutate(times_sick = rowSums(pick(starts_with("total")), 
                              na.rm = TRUE)
  )

# remove files
rm(disease_diarhea, disease_ear, disease_navel, disease_pneu)

# merge with weight info

kdd_all<- kdd_weight_breed |> 
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

# join with lact data not sure why yet
kdd_growth <- lact_1_dc_data |> 
  select(id, birth_date, breed, cull_date) |> 
  right_join(to_kdd, by = join_by(id, birth_date))


# export files used in analysis
export(kdd_all, "datafiles/kdd_all.csv")
export(total_gain, "datafiles/total_gain.csv")
export(lact_1_dc_data, "datafiles/lact_1_dc_data.csv")
