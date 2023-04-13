#   ____________________________________________________________________________
#    Libraries                                                              ####

library(dplyr)
library(tidyr)
library(stringr)
library(rebus)
library(textutils)
library(fs)
library(purrr)
library(readr)

#   ____________________________________________________________________________
#   Locals                                                                  ####

home <- fs::path_home()
Pic_dir <- paste0(home, "/Pictures")
Cat_dir <- paste0(home, "/.local/share/gthumb/catalogs")
Cat_regex <- START %R% ascii_digit(4)
australia_regex <- START %R% "201" %R% or1(c("4", "5")) %R% " - australia" %R% END

xml_prolog <- "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
xml_root <- c("<catalog version=\"1.0\">", "  <files>")
xml_closing_tags <- c("  </files>", "</catalog>")

Cat_list <- list.files(Cat_dir, recursive = TRUE)
# zzz <- read_lines(paste0(Cat_dir, "/", Cat_list[3])) %>% 
#   as_tibble()

#   ____________________________________________________________________________
#   Build the library                                                       ####

df <- tibble(full_path = list.files(path = Pic_dir,
                                    recursive = TRUE)) %>% 
  mutate(root = str_split(full_path %>% tolower(), "/", simplify = TRUE)[, 1],
         root_2 = str_split(full_path %>% tolower(), "/", simplify = TRUE)[, 2],
         root_3 = str_split(full_path %>% tolower(), "/", simplify = TRUE)[, 3],
         ext = str_sub(full_path %>% tolower(), -3, -1),
         year = if_else(str_detect(root_2, australia_regex),
                        str_sub(root_2, 1, 4),
                        str_sub(root, 1, 4)),
         dt_exported = str_detect(full_path %>% tolower(), "darktable_exported"),
         source = str_detect(full_path %>% tolower(), "source"),
         pano = str_detect(full_path %>% tolower(), "pano"),
         cat_path = case_when(
           str_detect(root_2, australia_regex) ~ str_glue("{year}/{root_2}/{root_3}"),
           root == "2014-2015 - australia" ~ str_glue("{year}/{root}/{root_2}"),
           TRUE ~ str_glue("{year}/{root}")
         ))

df <- df %>%
  left_join(df %>%
              count(root, dt_exported) %>%
              filter(dt_exported) %>%
              select(-n, -dt_exported) %>%
              mutate(dt = TRUE),
            by = c("root")) %>%
  mutate(dt = replace_na(dt, FALSE)) %>%
  filter(str_detect(root, Cat_regex)) %>%
  filter(!source,
         !pano,
         ext == "jpg",
         dt == dt_exported)

#   ____________________________________________________________________________
#   Build the .catalog files                                                ####

df$cat_path %>%
  unique() %>% 
  walk(function(x) {
    content <- df %>% 
      filter(cat_path == x) %>% 
      mutate(output = str_glue("    <file uri=\"file://{Pic_dir}/{URLencode(full_path) %>% HTMLencode(encode.only = '&')}\"/>"))
    
    xml <- c(xml_prolog,
             xml_root,
             content$output,
             xml_closing_tags)
    
    dir <- paste0(Cat_dir,
                  "/Script/",
                  str_sub(x, 1, 4))
    subdir <- str_split(x, "/", simplify = TRUE)[, 2]
    cat_full_path <- paste0(Cat_dir,
                            "/Script/",
                            x,
                            ".catalog")
    
    dir_create(dir)
    if (str_detect(x, "australia")) {
      dir_create(str_glue("{dir}/{subdir}"))
    }
    write_lines(xml,
                cat_full_path)
  })
