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
           -shape_area, -air_020107, -air_20108, -r_code, -ind_name)


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



