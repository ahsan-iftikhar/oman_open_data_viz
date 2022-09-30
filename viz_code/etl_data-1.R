# Setting up and preliminaries --------------------------------------------
source(file = "viz_code/setup_pkgs_preliminaries.R")


# Loading visitors data ---------------------------------------------------

vstr <- read_csv("data_frm_odo/ObservationData_htngjuc.csv") |> 
  janitor::clean_names() |> 
  dplyr::mutate(across(.cols = where(is.character), 
                       .fns = ~as_factor(.x)
  )
  ) |> 
  dplyr::mutate(across(.cols = c("date"), 
                       .fns = ~forcats::as_factor(.x))
  ) |> 
  dplyr::mutate(year = forcats::fct_inseq(date))

