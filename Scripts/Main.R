#   ____________________________________________________________________________
#    Libraries                                                              ####

library(tidyverse)
library(rebus)
library(fs)

#   ____________________________________________________________________________
#   Locals                                                                  ####

home <- fs::path_home()
Pic_dir <- paste0(home, "/Pictures")
Cat_dir <- paste0(home, "/Documents/catalogs")
Cat_regex <- START %R% ascii_digit(4)

xml_prolog <- "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
xml_root <- c("<catalog version=\"1.0\">", "  <files>")
xml_closing_tags <- c("  </files>", "</catalog>")

Cat_list <- list.files(Cat_dir, recursive = TRUE)
zzz <- read_lines(paste0(Cat_dir, "/", Cat_list[3])) %>% 
  as_tibble()

#   ____________________________________________________________________________
#   Build the library                                                       ####

df <- tibble(full_path = list.files(path = Pic_dir,
                                    pattern = Cat_regex,
                                    recursive = TRUE)) %>% 
  mutate(root = str_split(full_path, "/", simplify = TRUE)[, 1],
         ext = str_sub(full_path, -3, -1),
         year = str_sub(root, 1, 4),
         exported = str_detect(full_path, "darktable_exported"),
         pano = str_detect(full_path, "pano"),
         cat_path = str_glue("{year}/{root}")) %>% 
  filter(exported, !pano)

#   ____________________________________________________________________________
#   Build the .catalog files                                                ####

df$cat_path %>%
  unique() %>% 
  walk(function(x) {
    content <- df %>% 
      filter(cat_path == x) %>% 
      mutate(output = str_glue("    <file uri=\"file://{Pic_dir}/{URLencode(full_path)}\"/>"))
    
    xml <- c(xml_prolog,
             xml_root,
             content$output,
             xml_closing_tags)
    
    dir <- paste0(Cat_dir,
                  "/Output_test/",
                  str_sub(x, 1, 4))
    cat_full_path <- paste0(Cat_dir,
                            "/Output_test/",
                            x,
                            ".catalog")
    
    dir_create(dir)
    write_lines(xml,
                cat_full_path)
  })
