library(dplyr)

# Read IBES data into R
ibes <- haven::read_dta("data/sample_ibes_data.dta")

# Summary statistics
summary (ibes)

# Understand region code (variable reg_code)
str(ibes$reg_code)

(reg_code <- attr(ibes$reg_code, "labels"))

reg_codelist <- data.frame (reg_code = as.numeric(reg_code),
                            reg_name = names(reg_code))

# Contrast reg_name with A2: Region name (variable saq2)
temp <- ibes |> 
  left_join (reg_codelist, by="reg_code") |> 
  group_by (reg_code, reg_name, saq2) |> 
  summarise (n=n())

sum(temp$n)

# Create summary statistics, e.g. mean number of employees
ibes_reg_stats <- ibes |> 
  group_by(reg_code) |> 
  summarize (mean_size = mean (seq1tot)) |> 
  left_join (reg_codelist, by="reg_code") |> 
  mutate (reg_name = stringr::str_c (reg_name, " Region"))

# Save result as R object
saveRDS(ibes_reg_stats, "data/ibes_reg_stats_20241128.rds")
