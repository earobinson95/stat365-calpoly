library(tidyverse)
library(writexl)

## cumulative enrollment

cenroll2022 <- read.delim("https://www3.cde.ca.gov/demo-downloads/ce/cenroll2122.txt") |>
  filter(AggregateLevel == "C")

cenroll2021 <- read.delim("https://www3.cde.ca.gov/demo-downloads/ce/cenroll2021.txt") |> 
  filter(AggregateLevel == "C")

cenroll2020 <- read.delim("https://www3.cde.ca.gov/demo-downloads/ce/cenroll1920.txt") |> 
  filter(AggregateLevel == "C")

cenroll <- cenroll2020 |> 
  bind_rows(cenroll2021) |> 
  bind_rows(cenroll2022) |> 
  select(AcademicYear, CountyName, Charter, ReportingCategory, CumulativeEnrollment) |> 
  mutate(CumulativeEnrollment = as.numeric(CumulativeEnrollment))
cenroll

write_xlsx(cenroll, "97-mini-projects/one-number-story/data/cenroll.xlsx")

## english language learners

ell2022 <- read.delim("https://dq.cde.ca.gov/dataquest/dlfile/dlfile.aspx?cLevel=School&cYear=2022-23&cCat=EL&cPage=fileselsch") |> 
  mutate(AcademicYear = "2022-2023")
ell2021 <- read.delim("https://dq.cde.ca.gov/dataquest/dlfile/dlfile.aspx?cLevel=School&cYear=2021-22&cCat=EL&cPage=fileselsch") |> 
  mutate(AcademicYear = "2021-2022")
ell2020 <- read.delim("https://dq.cde.ca.gov/dataquest/dlfile/dlfile.aspx?cLevel=School&cYear=2020-21&cCat=EL&cPage=fileselsch") |> 
  mutate(AcademicYear = "2020-2021")
ell2019 <- read.delim("https://dq.cde.ca.gov/dataquest/dlfile/dlfile.aspx?cLevel=School&cYear=2019-20&cCat=EL&cPage=fileselsch") |> 
  mutate(AcademicYear = "2019-2020")

ell <- ell2022 |> 
  select(-LC) |> 
  bind_rows(ell2021) |>  
  select(-LC) |> 
  bind_rows(ell2020) |>  
  select(-LC) |> 
  bind_rows(ell2019) |> 
  select(-LC)

write_xlsx(ell, "97-mini-projects/one-number-story/data/ell.xlsx")


## College going rates

cgr2020 <- read.delim("https://www3.cde.ca.gov/demo-downloads/cgr/cgr12mo20.txt")
cgr2019 <- read.delim("https://www3.cde.ca.gov/demo-downloads/cgr/cgr12mo19.txt")
cgr2018 <- read.delim("https://www3.cde.ca.gov/demo-downloads/cgr/cgr12mo18.txt")
cgr2017 <- read.delim("https://www3.cde.ca.gov/demo-downloads/cgr/cgr12mo17.txt")

cgr <- cgr2020 |> 
  bind_rows(cgr2019) |> 
  bind_rows(cgr2018) |> 
  bind_rows(cgr2017) |> 
  filter(AggregateLevel == "C") |> 
  janitor::clean_names() |> 
  mutate(across(high_school_completers:enrolled_out_of_state_2_year_college_public_private_12_months, as.numeric)) |> 
  select(academic_year, county_name, charter_school:enrolled_out_of_state_2_year_college_public_private_12_months)

write_xlsx(cgr, "97-mini-projects/one-number-story/data/cgr.xlsx")


# English Language Arts/Literacy and Mathematics

english_math_test_orig <- read_delim("97-mini-projects/one-number-story/data/original-ca-education-dept/sb_ca2022_all_csv_v1.txt", delim = "^")
english_math_test_county <- read_delim("97-mini-projects/one-number-story/data/original-ca-education-dept/sb_ca2022entities_csv.txt", delim = "^")

english_math_test <- english_math_test_orig |> 
  left_join(english_math_test_county) |> 
  janitor::clean_names() |> 
  select(county_name, district_name, school_name, zip_code, test_year, grade, 
         students_enrolled, students_tested, mean_scale_score,
         percentage_standard_exceeded, percentage_standard_met, percentage_standard_nearly_met, percentage_standard_not_met
) |> 
  # filter(!is.na(district_name)) |>
  filter(county_name != "State of California") |> 
  mutate(across(grade:percentage_standard_not_met, as.numeric)) |> 
  group_by(county_name, test_year, grade) |>
  mutate(total_students_enrolled = sum(students_enrolled, na.rm = T),
         total_students_tested = sum(students_tested, na.rm = T)
         ) |> 
  ungroup() |> 
  mutate(across(.cols = mean_scale_score:percentage_standard_not_met, .fns = ~ .x*(students_tested/total_students_tested))) |> 
  group_by(county_name, test_year, grade, total_students_enrolled, total_students_tested) |>
  summarize(across(mean_scale_score:percentage_standard_not_met, ~ sum(.x, na.rm = T))) |> 
  ungroup() |> 
  mutate(across(mean_scale_score:percentage_standard_not_met, ~ na_if(.x, 0)))

write_xlsx(english_math_test, "97-mini-projects/one-number-story/data/catesting.xlsx")


# GSS data
gss <- readxl::read_excel("97-mini-projects/one-number-story/data/gss.xlsx") |> 
  mutate(year = as.numeric(year)) |> 
  mutate(hrs2 = ifelse(hrs2 == "89+ hrs", 89, hrs2)) |> 
  mutate(hrs2 = as.numeric(hrs2)) |> 
  mutate(commute = ifelse(commute == "97+ MINUTES", 97, commute)) |> 
  mutate(commute = as.numeric(commute)) |> 
  mutate(marital = ifelse(str_detect(marital, pattern = ":"), NA, marital)) |> 
  mutate(sibs = ifelse(str_detect(sibs, pattern = ":"), NA, sibs)) |> 
  mutate(sibs = as.numeric(sibs)) |> 
  mutate(sibs = ifelse(sibs < 0, NA, sibs)) |> 
  mutate(childs = ifelse(str_detect(childs, pattern = ":"), NA, childs)) |> 
  mutate(childs = ifelse(childs == "8 or more", 8, childs)) |> 
  mutate(childs = as.numeric(childs)) |> 
  mutate(across(year:ballot, ~ ifelse(str_detect(.x, pattern = ":"), NA, .x))) |> 
  mutate(emailmin = ifelse(emailmin == "0 minutes", 0, emailmin)) |> 
  mutate(emailmin = as.numeric(emailmin)) |> 
  select(year, age, marital, hapmar, sibs, childs, happy, hrs2, commute, income, satjob, compuse, emailmin, partyid, grassy, educop, relig, cigweek)
  
write_xlsx(gss, "97-mini-projects/one-number-story/data/gss_clean2.xlsx")
