#scrap the flag and the contries info from https://www.countryflags.com/en/
library(httr)
library(xml2)
library(rvest)
url <- "https://www.countryflags.com/en/"
#get the list of countries with their parameters (important to use UTF-8 encoding to keep correct countries names)
countries <- read_html("flags.html", encoding = "UTF-8") %>% 
  xml_nodes("td") %>% 
  as.character()
#the table has 5 columns, 
#thus take one element of the countries vector as a table element
#output is the 5 column table
countries_tab <- lapply(seq(1, length(countries), 5), function(icountry) {
  #set a delay between consequitive requests
  if(icountry > 1)
    Sys.sleep(2)
  cat(icountry, "\n")
  d_country <- countries[icountry:(icountry+4)]
  #country name
  name <- d_country[2] %>% 
    substr(., gregexpr("\">", d_country[2])[[1]][2] + nchar("\">"), 
           gregexpr("</a>", d_country[2])[[1]][1] - 1 ) %>% 
    gsub("Flag of ", "", .)
  #link to the flag image
  link <- d_country[1] %>% 
    substr(., gregexpr("<img src=\"//", d_country[1])[[1]][1] + nchar("<img src=\"//"), 
           gregexpr("\" srcset", d_country[1])[[1]][1] - 1 )   
  #population
  population <- d_country[3] %>% 
    strsplit(., ">") %>% unlist() %>% 
    strsplit(., "</") %>% unlist()
  population <- population[2] %>% gsub(" ", "", .) %>% as.integer()
  #surface in sq.mi
  surface_sq.mi <- d_country[4] %>% 
    strsplit(., ">") %>% unlist() %>% 
    strsplit(., "<") %>% unlist()
  surface_sq.mi <- surface_sq.mi[5] %>%  gsub(" ", "", .) %>% as.integer()
  #density per sq.mi
  density_sq.mi <- d_country[5] %>% 
    strsplit(., ">") %>% unlist() %>% 
    strsplit(., "<") %>% unlist()
  density_sq.mi <- density_sq.mi[5] %>% gsub(" ", "", .) %>% as.integer()
  ##get extra details
  details <- name %>%
    #replace the accents characters 
    gsub("ô", "o", .) %>% gsub("é|è|ê|ё", "e", .) %>% gsub("à|á|â|ã", "a", .) %>% gsub("í|î", "i", .) %>% 
    gsub(",|'", "", .) %>% 
    gsub(" ", "-", .) %>% 
    tolower() %>% 
    paste0(url, "flag-of-", ., ".html") %>% 
    GET(.) %>% 
    read_html()
  #flag ratio
  ratio <- details %>%
    xml_nodes(".prop-ratio") %>%
    as.character()
  ratio %<>% substr(., 
                    gregexpr(">Ratio ", .)[[1]][1] + nchar(">Ratio "),
                    gregexpr("</div", .)[[1]][1] - 1)
  #since when the flag in use
  flag_history <- details %>% 
    xml_nodes(".list-group-item") %>%
    as.character()
  return(
    data.frame(
      name = name,
      link = link,
      flag_ratio = ratio,
      year_first.usage = substr(flag_history[1],
                                gregexpr("<strong>", flag_history[1])[[1]][1]+nchar("<strong>"),
                                gregexpr("</strong>", flag_history[1])[[1]][1]-1),
      year_current.ver = substr(flag_history[2],
                                gregexpr("<strong>", flag_history[2])[[1]][1]+nchar("<strong>"),
                                gregexpr("</strong>", flag_history[2])[[1]][1]-1),
      year_last.change = substr(flag_history[3],
                                gregexpr("<strong>", flag_history[3])[[1]][1]+nchar("<strong>"),
                                gregexpr("</strong>", flag_history[3])[[1]][1]-1),
      population = population,
      surface_sq.mi = surface_sq.mi,
      density_sq.mi = density_sq.mi
    )
  )
}) %>% bind_rows(.) %>% data.frame()
#save the data
# write_csv(countries_tab, "countries_flags.csv", col_names = T)
