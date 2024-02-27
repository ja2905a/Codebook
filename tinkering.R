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
    select(-tribe_area, - allot_area, -fed_area)

##pop per sq mi

IC <-
  IC |>
  mutate(sq_miles = round(sq_miles, 1))

IC <-
  IC |>
    mutate(tribe_pop_sq_mi = pop_tot / sq_miles)

IC <-
  IC |>
  mutate(tribe_pop_sq_mi = round(tribe_pop_sq_mi, 1))

##mutate the economy

##size factor for tribes

##m NaN


