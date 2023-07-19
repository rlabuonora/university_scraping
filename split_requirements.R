
programs_reqs <- readRDS("./data/programs_requirements_fee.rds") %>% 
    mutate(requirements=str_trim(requirements)) %>% 
    mutate(requirements=str_replace_all(requirements, "\n\\+\n", "\n0+\n")) %>%
    mutate(requirements=str_replace(requirements, "\n\\+$", "\n0+")) %>% 
    mutate(requirements=str_replace(requirements, "\n \n", "\n\n")) %>% 
  count(program_title, requirements) 
  

df <- programs_reqs %>% 
  mutate(y=str_split(str_trim(requirements), "\n\n")) %>% 
  filter(requirements!="") %>% 
  unnest(y) %>% 
  separate(y, into = c("variable", "value"), sep = "\n") %>% 
  mutate(variable=str_trim(variable)) %>% 
  mutate(value=str_trim(value))

df %>% 
  pivot_wider(id_cols=c(program_title, requirements), names_from = variable, values_from = value, values_fill = NA)

df %>% 
  dplyr::group_by(program_title, requirements,
                  variable) %>%
  dplyr::summarise(n = dplyr::n(),
                   .groups = "drop") %>%
  dplyr::filter(n > 1L) %>% View

res <- df %>% 
  filter(requirements=="A-levels\n0+\n\nBTEC Qualifications\n0+") %>% 
  pivot_wider(names_from = variable, values_from = value, values_fill = NA)

df %>% count(variable)
df %>% count(value) %>% View
