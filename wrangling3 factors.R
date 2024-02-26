#+ Wrangling 3: Factor variables
#+ Austin Hart

  library(tidyverse)

# load data: DHS data (Zimbabwe, abridged)
  zim = read_csv('water.csv')


# Hello, factors ------------
## Create factor (add ordering)    
  fct_count(zim$hh_income) # maintains alpha order
  
  zim$wealthQ = 
    as_factor(zim$hv270)
  
  fct_count(zim$wealthQ) # See the difference?
  
## So what? Order is information    
  qplot(zim$hh_income)
  qplot(zim$wealthQ)
  

# Ordering ------------------
## Reorder levels----
  zim =
    zim |>
    mutate(
      IncQuint = fct_relevel(
        hh_income, # source variable
        'poorest','poorer', 'middle','richer','richest'
      )
    )    
  
  levels(zim$IncQuint) # verify order
  
## New factors ----
### OPTION 1: with case_when
  zim =
    zim |>
    mutate(
      ageGroup = case_when(
        age <= 9 ~ '6 to 9',
        age %in% 10:12 ~ '10 to 12',
        age >= 13 ~ '13 to 15',
        .ptype = factor(
          levels = c('6 to 9', '10 to 12', '13 to 15'), 
          ordered = TRUE
        )
      )
    )
  count(zim, ageGroup) # output is character w/alphanumeric order
  
### OPTION 2: Custom function  
  FctWhen = function(...) {
    args = rlang::list2(...)
    rhs = map(args, rlang::f_rhs)
    cases = case_when( !!!args )
    exec(fct_relevel, cases, !!!rhs)
  }   
  
  zim =
    zim |>
    mutate(
      ageFact = FctWhen(
        age <= 9 ~ '6-9',
        age %in% 10:12 ~ '10-12',
        age >= 13 ~ '13-15'
      )
    )
  
  count(zim, ageFact) 
  
  
## Collapsing levels ----
  zim =
    zim |>
    mutate(
      edLevel = fct_collapse(
        hv109, # factor variable you want to change
        dknr = '9', # newCat = 'oldCat'
        none = 'no education',
        primary = c('complete primary', 'incomplete primary'),
        secondary = c('complete secondary', 'incomplete secondary'),
        tertiary = 'higher'
      )
    )
  
  zim |> count(edLevel) # notice it doesn't keep your order
  
## Lumping into 'other' ----
### custom
  zim = 
    zim |>
    mutate(
      edLevel2 = fct_other(edLevel, drop = c('dknr','none'))
    )
  
  zim |> count(edLevel2)
  
### by frequency
  fct_count(zim$hv205, prop = T)
  zim =
    zim |>
    mutate(
      latrineType = fct_lump(hv205, prop = 0.05), # keep above threshold
      latrine2 = fct_lump(hv205, n = 6) # keep n most freq cats
    )
  
  zim |> count(latrineType)
  zim |> count(latrine2)
  
  
# GRAPHS & TABLES -----------
## Order fac levels by freq
  qplot(zim$water_source) # witness the mess
  
  zim =
    zim |>
    mutate(
      reg1 = fct_infreq(water_source)
    )
  
  qplot(zim$reg1) # note the ordering
  qplot(fct_rev(zim$reg1))
  
## Order by scores on another variable
  zim2 = 
    zim |>
    group_by(hv235) |>
    summarise(
      med.dev = median(wt.by.ht, na.rm = T)
    )
  
  zim2 |>
    ggplot(aes(x = med.dev, y = fct_reorder(hv235, med.dev))) +
    geom_point()
  
  
  
  

  