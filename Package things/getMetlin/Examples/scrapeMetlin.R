# Obtain data from Metlin by submitting GET request behind their Javascript wall
# Three main functions: getMetlinMz, getMetlinName, getMetlinMS2


# Setup things ----
library(httr)
library(xml2)
library(dplyr)
pm <- function(x, d){c(x-d, x+d)}
mzr <- function(mz, ppm){round(pm(mz, mz*ppm/1000000), digits = 5)}

# amino_masses <- "http://www.matrixscience.com/help/aa_help.html" %>%
#   GET() %>%
#   content() %>%
#   xml_find_all(xpath="//td") %>%
#   xml_text() %>%
#   matrix(ncol=6, byrow=T) %>%
#   gsub("\r\n    ", "", .) %>%
#   `rownames<-`(.[,1]) %>%
#   `[`(1:24, 4) %>%
#   `mode<-`("numeric") %>% 
#   na.omit() %>%
#   `+`(18.010565)

# getMetlin functions ----
getMetlinMz <- function(cmpd_mz, ppm=2.5){
  mz_range <- mzr(cmpd_mz, ppm)
  metlin_data <- paste0("https://metlin.scripps.edu/advanced_search_result.php?",
                       "molid=&mass_min=", min(mz_range),
                       "&mass_max=", max(mz_range), "&Amino",
                       "Acid=add&drug=add&toxinEPA=add&keggIDFilter=add") %>%
    GET() %>%
    stop_for_status() %>%
    content(encoding = "UTF-8") %>%
    xml_find_all(xpath = "//tbody")
  
  search_ids <- metlin_data %>%
    xml_find_all(xpath = "//th[@scope]/a") %>%
    xml_text()
  
  search_data <- metlin_data %>%
    xml_find_all(xpath="//td") %>%
    xml_text() %>%
    matrix(ncol=7, byrow=T) %>%
    cbind(search_ids, .) %>%
    as.data.frame() %>%
    `names<-`(c("cmpd_id", "exact_mass", "cmpd_name", "formula", 
                "CAS", "KEGG", "MSMS", "Structure"))
  
  MSMS_subset <- subset(search_data, MSMS=="experimental")
  
  print(paste("Metlin returned", nrow(search_data), "compound(s) between", 
        min(mz_range), "and", max(mz_range), "m/z with", 
        length(levels(search_data$formula)), "unique formula(s):", 
        paste(levels(search_data$formula), collapse = ", ")))
  if(nrow(MSMS_subset)){
    print(paste("Of those,", nrow(MSMS_subset), 
                "have experimental MS/MS data:", 
                paste(as.character(MSMS_subset$cmpd_name), collapse = ", ")))
  } else {
    print("None of these compounds have experimental MS/MS data")
  }
  
  return(search_data)
}

getMetlinName <- function(name){
  metlin_data <- paste0("https://metlin.scripps.edu/advanced_search_result.php?", 
                        "name=", name, "&AminoAcid=add&drug=add&toxinEPA=add&",
                        "keggIDFilter=add") %>%
    GET() %>%
    stop_for_status() %>%
    content(encoding = "UTF-8") %>%
    xml_find_all(xpath = "//tbody")
  
  search_ids <- metlin_data %>%
    xml_find_all(xpath = "//th[@scope]/a") %>%
    xml_text()
  
  search_data <- metlin_data %>%
    xml_find_all(xpath="//td") %>%
    xml_text() %>%
    matrix(ncol=7, byrow=T) %>%
    cbind(search_ids, .) %>%
    as.data.frame() %>%
    `names<-`(c("cmpd_id", "exact_mass", "cmpd_name", "formula", 
                "CAS", "KEGG", "MSMS", "Structure"))
  
  MSMS_subset <- subset(search_data, MSMS=="experimental")
  
  print(paste("Metlin returned", nrow(search_data), "compound(s) with name", 
              name, " with", 
              length(levels(search_data$formula)), "unique formula(s):", 
              paste(levels(search_data$formula), collapse = ", ")))
  if(nrow(MSMS_subset)){
    print(paste("Of those,", nrow(MSMS_subset), 
                "have experimental MS/MS data:", 
                paste(as.character(MSMS_subset$cmpd_name), collapse = ", ")))
  } else {
    print("None of these compounds have experimental MS/MS data")
  }
  
  return(search_data)
}

getMetlinMS2 <- function(cmpd_id){
  metlin_data <- paste0("https://metlin.scripps.edu/showChart.php?molid=", 
                        cmpd_id,"&etype=experimental") %>%
    GET() %>% stop_for_status()
  ms2_raw <- metlin_data %>%
    content(encoding = "UTF-8") %>%
    xml_find_all(xpath = '/html/head/script[5]') %>%
    xml_text() %>%
    gsub(pattern = ".*series: \\[", replacement = "") %>%
    gsub(pattern = " ]}]\n                }); \n\n            });\n        ",
         replacement = "") %>%
    gsub(pattern = "&nbsp;", replacement = "") %>%
    strsplit(split = "\\{name: ") %>%
    unlist() %>%
    `[`(-1) %>%
    lapply(strsplit, split=",data:\\[")
  
  if(!length(ms2_raw)){stop("No MS2 data found. Are you sure Metlin has this data?")}
  
  ms2_titles <- sapply(ms2_raw, `[[`, 1)[1,] #Not sure why this works but OK
  ms2_polarities <- ifelse(grepl(ms2_titles, pattern = "(\\+)"), "+", "-")
  ms2_voltages <- gregexpr("\\d+\\.?\\d*", ms2_titles) %>%
    regmatches(x=ms2_titles) %>%
    unlist() %>% as.numeric()
  ms2_adducts <- gregexpr("\\[M.*\\]", ms2_titles) %>%
    regmatches(x = ms2_titles) %>% 
    unlist()
  
  ms2_list <- sapply(ms2_raw, `[[`, 1)[2,] %>%
    gsub(pattern = " ]},", replacement = "") %>%
    lapply(function(x){
      wo1 <- substring(x, 2)
      wo2 <- substring(wo1, 1, nchar(wo1))
      ms2_list <- unlist(strsplit(wo2, "},\\{"))
      ms2_list <- lapply(ms2_list, function(x)
        as.numeric(unlist(regmatches(x, m = gregexpr("\\d+\\.?\\d*", x)))))
      ms2_df <- as.data.frame(do.call(rbind, ms2_list))
      names(ms2_df) <- c("mass", "rel_intensity")
      return(ms2_df)
    })
  for(i in seq_along(ms2_list)){
    ms2_list[[i]] <- cbind(ms2_polarities[i], ms2_adducts[i], 
                           ms2_voltages[i], ms2_list[[i]])
  }
  ms2_df <- as.data.frame(do.call(rbind, ms2_list))
  names(ms2_df) <- c("polarity", "adduct", "voltage", "frag_mass", "frag_int")
  
  print(paste("Metlin had", length(ms2_titles), "MS2 records for this compound,",
              "with collision energies of", 
              paste(paste0(ms2_polarities, ms2_voltages), collapse=", ")))
  
  return(ms2_df)
}

# Using getMetlin functions ----

sample_data <- getMetlinMz(117.078979)
sample_data <- getMetlinMz(117.078979, ppm = 500)
sample_data <- getMetlinName("Arsenobetaine")

sample_ms2_cmpd <- sample_data %>% filter(MSMS=="experimental") %>% slice(1)

sample_ms2 <- getMetlinMS2(sample_ms2_cmpd$cmpd_id)


# Visualize the ill-gotten data ----
split_volt_pos_ms2 <- sample_ms2 %>%
  subset(polarity=="+") %>%
  split(.$voltage)
layout(matrix(c(1, rep(2:(length(split_volt_pos_ms2)+1), each=2),  1), ncol = 1))
par(mar=c(0.1, 4.1, 0.1, 0.1))
plot.new()
text(x = 0.5, y=1, labels = sample_ms2_cmpd$cmpd_name, cex=3)
for(i in split_volt_pos_ms2){
  plot(i$frag_mass, i$frag_int, xlab = "", 
       ylab=paste("Voltage", unique(i$voltage)),
       xlim=c(0, max(sample_ms2$frag_mass)), 
       xaxt="n", yaxt="n", type="n", ylim=c(0, 120))
  segments(x0 = i$frag_mass, x1 = i$frag_mass,
           y0 = 0, y1 = i$frag_int)
  axis(side = 2, at = c(0, 50, 100), labels = c(0, 50, 100))
}
axis(side = 1)

library(ggplot2)
gp <- sample_ms2 %>%
  filter(polarity=="+") %>%
    #filter(voltage==20) %>%
    ggplot(label=frag_mass) +
    geom_segment(aes(yend=0, x=frag_mass, y=frag_int, xend=frag_mass)) +
    #geom_hline(yintercept=0) +
    facet_wrap(~voltage, ncol = 1) +
    theme_bw() +
    xlim(0, max(sample_ms2$frag_mass))
gp

library(plotly)
ggplotly(gp, tooltip = c("y", "x"))
