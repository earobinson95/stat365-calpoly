library(tidyverse)

# install and load
library(writexl)

# import data from TidyTuesday
childcare_costs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-09/childcare_costs.csv')
counties <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-09/counties.csv')

# any data joins (if data is too big, you can filter here, but I also want you to use filters in excel)
childcare <- counties |> 
  left_join(childcare_costs,
            by = "county_fips_code"
  )

# save out to an excel file
write_xlsx(childcare, "2024/03-data-journalism/data/childcare-costs.xlsx")
