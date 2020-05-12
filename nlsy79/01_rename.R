rm(list = ls())
library(tidyverse)
library(haven)

nlsy79full <- read_dta("/Users/debis/Documents/90Z/90z_final_project/NLSY79/nlsy79full/nlsy79full.dta") %>%
  rename_all(str_to_lower) %>%
  mutate_all(unclass)

# Note tha depending on how directory is set up the content inside the read_dta
# command may change.

nlsy79full_varlabels <- map(nlsy79full, ~ attr(.x, "label")) %>%
  enframe()

# renaming variables ---------

nlsy79 <- nlsy79full %>%
  rename(sample_1979 = sample_id_1979,
         father_absent_1979 = fam_27_1979,
         num_sibs_1979 = fam_28a_1979,
         hgc_expected_1979 = school_31_1979,
         age_1979 = ageatint_1979,
         race_1979 = sample_race_78scrn,
         sex_1979 = sample_sex_1979,
         urban_1979 = urban_rural_1979,
         hhi_24_1981 = hhr_li_1982,
         afqt3_1981 = afqt_3_1981,
         rotter_1979 = rotter_score_1979) %>%
  # mutate(hhi_24_1981 = NA) %>%
  rename_at(vars(matches("[a-z]\\d\\d_\\d\\d\\d\\d$")),
            ~ str_replace(.x, "\\d\\d_", "_")) %>%
  rename_at(vars(starts_with("tnfi_trunc")),
            ~ str_replace(.x, "tnfi_trunc", "faminc")) %>%
  rename_at(vars(starts_with("enrollmtrev")),
            ~ str_replace(.x, "enrollmtrev", "enrolled")) %>%
  rename_at(vars(starts_with("hgcrev")),
            ~ str_replace(.x, "hgcrev", "hgc")) %>%
  rename_at(vars(starts_with("hhi_24")),
            ~ str_replace(.x, "hhi_24", "hhtype")) %>%
  rename_at(vars(starts_with("q13_5_trunc")),
            ~ str_replace(.x, "q13_5_trunc_revised|q13_5_trunc", "earnings")) %>%
  rename_at(vars(starts_with("q13_9_trunc")),
            ~ str_replace(.x, "q13_9_trunc_revised|q13_9_trunc", "farmbusinc")) %>%
  rename_at(vars(starts_with("q3_3")),
            ~ str_replace(.x, "q3_3", "hga")) %>% 
  rename_at(vars(starts_with("degree_1a_1")),
            ~ str_replace(.x, "degree_1a_1", "degree_1st")) %>%
  rename_at(vars(starts_with("degree_1a_2")),
            ~ str_replace(.x, "degree_1a_2", "degree_2nd")) %>%
  rename_at(vars(starts_with("q3_10a")),
            ~ str_replace(.x, "q3_10a", "degree_any")) %>%
  rename_at(vars(starts_with("q3_10b")),
            ~ str_replace(.x, "q3_10b", "hdr")) %>%
  rename_at(vars(starts_with("q3_10c_y")),
            ~ str_replace(.x, "q3_10c_y", "hdr_year")) %>%
  rename_at(vars(starts_with("q3_10d")),
            ~ str_replace(.x, "q3_10d", "hdr_ever")) %>%
  rename_at(vars(starts_with("q3_10e_y")),
            ~ str_replace(.x, "q3_10e_y", "hdr_ever_year")) %>%
  mutate_all(function(x) {x[x %in% -c(1, 2, 3, 5)] <- NA; x})

str_sort(names(nlsy79))

save(nlsy79, file = "nlsy79.RData")





  
