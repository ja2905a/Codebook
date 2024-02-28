library(tidyverse)
IC <- read_csv('bia_indian_lands_dataset.csv')

#rm unnecessary variables
IC <-
  IC |>
    select(-fid, -objectid, -area, -perimeter, -aiana,
           -ind_source, -ind_scale, -org_code, -region_cod,
           -agency_nam, -osgt_code, -osgt_assoc, -native_cor,
           -mail_name, -cnty_st, -inc_percap, -tribal_num,
           -treaty, -tribal_est, -gov_type, -acres, -agency_cod,
           -poi_on_ima, -field_veri, -shape_leng, -shape_le_1,
           -shape_le_2, -irmp_plan, -short_name, -shape_le_3,
           -shape_area, -air_020107, -air_20108, -r_code,
           -ind_name, -fed_regist, -labor_tot)


#rm observations when not associated with a native entity
IC <-
  IC[!is.na(IC$entity),]

##factorize and reorder aiana_desc

IC$aiana_desc = 
  as_factor(IC$aiana_desc)

fct_count(IC$aiana_desc)

IC <-
  IC |>
  mutate(
    aiana_desc = fct_relevel(
      aiana_desc, # source variable
      'Trust land for which no reservation exists',
      'Federally Recognized Tribal Entity',
      'Trust Land related to a Federally Recognized Reservation',
      'Alaska Native Village',
      'American Indian Reservation'
    )
  )    

levels(IC$aiana_desc)

IC$self_gover = 
  as_factor(IC$self_gover)

fct_count(IC$self_gover)

IC$gaming = 
  as_factor(IC$gaming)

fct_count(IC$gaming)

IC$med_fac = 
  as_factor(IC$med_fac)

fct_count(IC$med_fac)
##med_fac to binary

IC <-
IC|>
mutate(
    med_fac = case_when(grepl("none", med_fac) ~ "NO",
                        grepl("clinic", med_fac, ignore.case = TRUE) ~"YES"))
    

##area pcts

IC <-
  IC |>
    mutate(tribe_area_pct = tribe_area / tot_area * 100,
           fed_area_pct = fed_area / tot_area * 100,
           allot_area_pct = allot_area / tot_area * 100)

IC <-
  IC |>
  select(-tribe_area, - allot_area, -fed_area, -tot_area)

##rm NaN

IC <-
  IC |>
  mutate(
    tribe_area_pct = as.numeric(tribe_area_pct),
    tribe_area_pct = case_when(
      tribe_area_pct == 'NaN' ~ NA, 
      tribe_area_pct == 'Inf' ~ 100,
      TRUE ~ as.numeric(tribe_area_pct)
      )
  )

IC <-
  IC |>
  mutate(
    fed_area_pct = as.numeric(fed_area_pct),
    fed_area_pct = case_when(
      fed_area_pct == 'NaN' ~ NA,
      tribe_area_pct == 'Inf' ~ 100,
      TRUE ~ as.numeric(fed_area_pct)
    )
  )

IC <-
  IC |>
  mutate(
    allot_area_pct = as.numeric(allot_area_pct),
    allot_area_pct = case_when(
      allot_area_pct == 'NaN' ~ NA,
      tribe_area_pct == 'Inf' ~ 100,
      TRUE ~ as.numeric(allot_area_pct)
    )
  )

##pop per sq mi

IC <-
  IC |>
    mutate(sq_miles = round(sq_miles, 1))

IC <-
  IC |>
    mutate(tribe_pop_sq_mi = pop_tot / sq_miles)

IC <-
  IC |>
    mutate(tribe_pop_sq_mi = round(tribe_pop_sq_mi, 1),
           tribe_area_pct = round(tribe_area_pct, 1),
           fed_area_pct = round(fed_area_pct, 1),
           allot_area_pct = round(allot_area_pct, 1))

##mutate the economy

IC$econ_sourc <- stringr::word(IC$econ_sourc, 1)

#standardize obs names

IC$econ_sourc <- str_replace_all(IC$econ_sourc,
                                  c("," = "",
                                    ";" = "",
                                    "Fisheries" = "Fish",
                                    "Fishing" = "Fish",
                                    "Wild" = "Wild rice",
                                    "Retail/service" = "Retail"))

##size factor for tribes
##I could not get this to work so it's not in the final dataset, but I wanted to show that I really tried to make it work

IC =
  IC |>
  mutate(
    tribe_land_base = case_when(
      sq_miles >= 10000 ~ "Largest",
      sq_miles %in% 1000:10000 ~ "Very Large",
      sq_miles %in% 100:1000 ~ "Large",
      sq_miles %in% 10.1:100 ~ "Medium",
      sq_miles %in% 1.1:10 ~ "Small",
      sq_miles %in% 0.1:1 ~ "Very Small",
      sq_miles = 0 ~ "Nonexistent",
      .ptype = factor(
        levels = c("Nonexistent", "Very Small", "Small",
             "Medium", "Large", "Very Large", "Largest"), 
         ordered = TRUE
        )
      )
    )


knitr::kable(fct_count(IC$med_fac))
