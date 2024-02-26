#+ -------------------------- #
#+ Wrangling Data 1.0
#+ Spring 2024
#+ -------------------------- #

  library(tidyverse)

# VECTORS -------------------
## Scalars (vector of length 1)
  s1 = 'a'
  s2 = 104
  s3 = 'dogs, cats' # compare to s3 = c('dogs','cats')
  s4 = 'mean(x)/100'

## Vectors 
  vec1 = c('a', 'b', NA, 'mean(x)/100')
  vec2 = c(s3, vec1) # vector as combo of stored values
  vec3 = 1:4 # compare to vec3 = c(1:4, 9:11)

## indexing elements: vector[location]
  vec1[4]
  
  
# MATRIX --------------------
## create (coerce vectors into rectangle)
  mat1 = matrix(c(vec1, vec3), nrow = 4)
  mat2 = matrix(c(vec1, vec3), nrow = 2) # compare to mat1
    matrix(1:20, nrow = 4)
    
## indexing: matrix[row, col]
  mat1[2, 2]
  mat1[3, 2]
  
  mat1[5] # still pretty "vectory"

  
# DATA FRAME ----------------
## create strict rectangular structure
  df1 = tibble(vec1, vec3) # try with vec2. what happens? why?
  df2 = tibble(
    var1 = 1:5,
    var2 = LETTERS[1:5]
  )
  
## index: frame[row.nums, col.nums]
  df1[1, 2] # try df1[1:3, 2]
  df1[2] # compare with: df1[ ,2] and df1[2, ]
  df2['var2']
  

# ORGANIZE VARS -------------
## Pre-fabbed iris data
  iris = iris

## select desired vars
  short1 =
    iris |>
    select(Sepal.Length, Sepal.Width) # name the vars
  
  short2 =
    iris |>
    select(1:2) # by column number
  
  short3 =
    iris |>
    select(starts_with('Sep')) # match on pattern

    rm(short1, short2, short3)
    
## drop unwanted variables
  dr =
    iris |>
    select(-Sepal.Length, -Sepal.Width)
  
## order/arrange variables
  #  df2 =
  #    iris |>
  #    relocate(targetVar, .before = someVar, .after = otherVar) # test
  
  df3 =
    iris |>
    relocate(
      starts_with('Petal'), 
      .before = Sepal.Width
    )
  
  df4 =
    iris |>
    relocate(
      where(is.factor), 
      .after = where(is.numeric)
    )

    rm(df, df3, df4, dr)
  
## Renaming variables
  iris2 = 
    iris |>
    mutate(
      flowerID = row_number(), # create simple ID var
      .before = 1
    ) |>
    rename(
      #oldname = newname,
      sepalLength = Sepal.Length,
      sepalWidth = Sepal.Width
    )

## With a function    
  iris3 = 
    iris |>
    rename_with(
      ~ tolower(gsub(".", "_", .x, fixed = TRUE))
    )

  
# SUBSET DATA ---------------
## filter/subset (keeping cases by criterion)
  vs =
    iris |>
    filter(
      Species == "virginica" |
      Petal.Length > median(Petal.Length)
    )
  
## splitting the frame by group
  iris |>
    group_split(Species) |>
    set_names(levels(iris$Species)) |>
    list2env(globalenv())
  
## keeping some cases by group
  iris |>
    group_by(Species) |>
    slice_max(Sepal.Length) # try slice_min; slice(1:5) compare
  
## keeping unique values of a variable
  iris |>
    distinct(
      Petal.Width, 
      .keep_all = T
    ) |>
    arrange(Petal.Width) # try -Petal.Width
  
  
# AGGREGATING ----------------
## summaries as aggregated data frames
  iris |>
    group_by(Species) |>
    summarise(
      n = n(),
      avgPL = mean(Petal.Length),
      avgPW = mean(Petal.Width)
    )
  
  
# DATA FRAME CHALLENGE ----------------
## download the penguin data direct from a URL:
  path2data = "https://raw.githubusercontent.com/cmdlinetips/data/master/palmer_penguins.csv"
  penguins = read_csv(path2data)
  
#+ Using the penguin data...
#+ 1. Tidy by your org's standard:
#+    - id variable first
#+    - island, species, then sex
#+    - numeric vars at the end
#+    - var names can include "." but not "_"
#+    
#+ 2. How many unique species/island combos are there?
#+ 3. Create a data frame of average bill length and depth by island
#+ 4. Split the penguin data, by species, into unique frames
#+ 5. Create a frame of only the heaviest penguins (species x island)
  