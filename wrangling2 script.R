#+ Merging and appending data frames
#+ 07 February 2024

# load packages
  library(tidyverse)
  library(tinytex)


# APPEND ------------------------------
# adding new observations

## data
  cd1 = readxl::read_excel('cds Alab NewY.xlsx')
  cd2 = readxl::read_excel('cds NorthC Wyo.xlsx')

## Add to original (requires same col names, not necessarily col order)  
  cds = bind_rows(cd1,cd2)
  cdsB = bind_rows(list(a = cd1, b = cd2), .id = 'source') # adds 'source' id

## appending w/distinct vars &/or var names  
  cd2m =
    cd2 |>
    rename(DistNum = distnum) |>
    select(-PVI.16)
  
  cdsC = bind_rows(cd1,cd2m) # explore data and check the NAs
  
    rm(list = setdiff(ls(),'cds')) # delete all but one object

    

# MERGE/JOIN --------------------------
#+ A mutating join to ad variables
#+ Matches cases by key/id values

## Data to incorporate
  states = read_csv('states.csv')
  inc = read_csv('incumb.csv')
  demog = read_csv('cddem.csv')

## Merge two frames by unique identifier
  df1 = left_join(
    cds,    # df 1
    demog,  # df 2
    by = join_by(District) # unique id
  )
  #+ note what happened to 'party' variable in that join.
  #+ drop duplicates up front where possible (select(-party))

  #+ right_join() ... keeps only records in second frame
  #+ inner_join() ... keeps only records in BOTH frames
  #+ full_join() ... keeps all records from each frame


## Where identifier names don't match: merge sate data
  df2 = left_join(
    df1, 
    states, 
    by = join_by(state == stcode)
  ) 
  # can you find the new additions?


## where multiple columns jointly identify cases: join with inc    
  df3 = left_join(
    df2, 
    inc,
    by = join_by(inc.first, inc.last)
  )


  # we didn't do great with names. so we have duplicates
  # we'd need to clean up now or go back and clean as we code

  rm(list = ls())  


# PIVOT/RESHAPE -----------------------

## data
  wdi = readxl::read_excel('WDI pull.xlsx', na = '..')

## lengthening pivots -----------
### start with a small frame as example
  mx = 
    wdi |>
    filter(
      cName == 'Mexico',
      vCode == 'EN.ATM.CO2E.PP.GD'
    )

  # "tidy" the data by lengthening
 mx2 <- mx |>
      pivot_longer(
        cols = 5:15, # try different specs to see what can go wrong
        names_to = 'Year', # new var recording col names
        names_transform = list(Year = as.integer),
        values_to = 'CO2emit'  # new var recording the data
      ) |>
      ggplot(aes(x = Year, y = CO2emit)) +
      geom_line() + 
      geom_point()


## widening pivot --------------
### start with small frame for example
  y2005 =
    wdi |>
    select(cName:`2005`)  # use `...` when var name is a number

  # widen the data
  wide = y2005 |>
    pivot_wider(
      names_from = c(vCode, vName), # try only one and see what happens
      values_from = `2005`,
      names_glue = '{vCode}' # try w/o this line
    )
    
  wide2 = y2005 |>
    select(-vName) |>
    pivot_wider(
      names_from = vCode,
      values_from = `2005`
    )
  
    rm(mx, wide, wide2, y2005)
    
    
# FIX THE WDI -------------------------
## lengthen first
  longwdi =
    wdi |>
    pivot_longer(
      cols = -c(1:4),
      names_to = 'year',
      names_transform = list(year = as.integer),
      values_to = 'scores'
    )

# widen from there
  wdi2 =
    longwdi |>
    select(-vName) |>
    pivot_wider(
      names_from = vCode,
      values_from = scores
    )


#+ Why bother with pivots?
#+ widening is typically for making a table or calculating across cols
  #+ Compare Brazil and Canada in C02 emissions
    #+ Select the countries and vars from wdi2
    #+ Use group_by() and summarize() to get your stats
    #+ Pivot to make a nicer table

