#+ Wrangling 3: Cleaning/Recoding Variables
#+ Austin Hart

  library(tidyverse)

# data: DHS (Zimbabwe, abridged)
  zim = read_csv('water.csv')

  
# VARIABLE TYPES ------------
## Broadly qual: String/character
  zim |>
    count(water_source)
  
# Factors: character + ordering (see "factor" script)
  zim |>
    count(hh_income)
  
# Broadly quant: Integer/numeric/double
  summary(zim$wt.by.ht)
  
  
# RECODING ------------------
## Mutating into new columns
  zim = 
    zim |>
    mutate(
      age_months = age * 12, # basic numeric functions
      devz = wt.by.ht - median(wt.by.ht, na.rm = T),
      
      rural = if_else(hv025 == 'rural', 1, 0), # conditional on values
      ageGroup = case_when(
        age <= 9 ~ '6 to 9', # condition ~ output if met
        age %in% 10:12 ~ '10 to 12',
        age >= 13 ~ '13 to 15'
      ),
      
      region_x_rid = paste(region_id, row_number(), sep = '.'),
      region2 = str_extract(region_x_rid, "[^.]+"),
      rid2 = str_extract(region_x_rid, "[^.]*$"),
      
      .before = 1 # where to place new vars
    )
  
  
# NAs and strings -----------
## Explicit missing values
  summary(zim2$eduyrs) # NA is an explicit absence

## Codes for missing values    
  zim |>
    count(hv121) # misfit value (9) indicates missing scores

## Change code to NA
  zim3 =
    zim |>
    mutate(
      attend_school = na_if(hv121, '9'),
      .before = 1
    )

  zim3 |> count(attend_school)

## Filter out missingness
  zsafe = 
    zim3 |>
    filter(
      complete.cases(attend_school, eduyrs)
    )
    
## unwanted strings
  summary(zim['water_mins'])
  table(zim$water_mins)
  zimUW =
    zim |>
    mutate(
      watEx1 = as.numeric(water_mins), # changes strings to NA
      waterfetchmins = case_when(
        watEx1 == 'on premises' ~ 0,
        watEx1 == '999' ~ NA, 
        TRUE ~ as.numeric(watEx1)
      ),
      .before = 1
    )
  # review the Warning messages

    summary(zimUW$waterfetchmins)


# Group summarise/mutate --------------
## Group-level stats as new dataframe
  zim |>
    group_by(ageGroup) |>
    summarise(
      freq = n(),
      edu = mean(eduyrs, na.rm = TRUE),
      physDev = median(wt.by.ht)
    )

## Group-level deviations as statistics
  zim2 =
    zim |>
    group_by(ageGroup) |>
    mutate(
      eduGroup = mean(eduyrs, na.rm = T),
      eduDev = eduyrs - mean(eduyrs, na.rm = TRUE),
      .keep = 'used' # so we can see the change easily
    )


  