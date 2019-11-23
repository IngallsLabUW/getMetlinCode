#' @import dplyr
#' @import httr
#' @import xml2
NULL


#' Obtain Metlin data for a given mz and ppm.
#'
#' \code{getMetlinMz} queries Metlin for a given mass and ppm. The default ppm
#' is 2.5. This function returns a data frame containing Metlin compound ID,
#' exact mass, compound name, formula, CAS and KEGG numbers, and the MSMS status
#' of each compound (experimental vs. none). It does not currently return
#' structures. This information is scraped from the base url
#' https://metlin.scripps.edu/advanced_search_result.php?molid=&mass_min=[MIN
#' MZ]&mass_max=[MAX MZ]&AminoAcid=add&drug=add&toxinEPA=add&keggIDFilter=add.
#'
#' @param cmpd_mz The mass of the compound you're searching for in Metlin. This
#'   mass will be automatically expanded in to a window based on the provided
#'   \code{ppm}.
#'
#' @param ppm The ppm accuracy of the instrument that you've collected MS data
#'   from. This will be used to expand the \code{cmpd_mz} provided above.
#'
#' @return A data frame containing Metlin compound ID, exact mass, compound
#'   name, formula, CAS and KEGG numbers, and the MSMS status of each compound
#'   (experimental vs. none). It does not currently return the structures.
#'
#' @examples
#' getMetlinMz(117.078979) #Betaine
#' getMetlinMz(135.054495) #Adenine
#' getMetlinMz(mz = 135.054495, ppm = 500)
#'
#' @export
getMetlinMz <- function(cmpd_mz, ppm=2.5){
  if(!is.numeric(cmpd_mz)|cmpd_mz<=0){
    stop("Mass must be positive and numeric")
  }
  if(!is.numeric(ppm)|ppm<=0){
    stop("Mass must be positive and numeric")
  }


  mz_range <- c(cmpd_mz-cmpd_mz*ppm/1000000, cmpd_mz+cmpd_mz*ppm/1000000)
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
                "CAS", "KEGG", "MSMS", "Structure")) %>%
    select(-Structure)

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


#' Obtain Metlin data for a given compound name.
#'
#' \code{getMetlinName} queries Metlin for a given compound name. This function
#' returns a data frame containing Metlin compound ID, exact mass, compound
#' name, formula, CAS and KEGG numbers, and the MSMS status of each compound
#' (experimental vs. none). It does not currently return structures. This
#' information is scraped from the base url
#' https://metlin.scripps.edu/advanced_search_result.php?name=[COMPOUND
#' NAME]&AminoAcid=add&drug=add&toxinEPA=add&keggIDFilter=add. This function
#' behaves identically to searching Metlin via the "Advanced Search" option and
#' providing only a name. It's a bit oversealous in its searching, and will
#' often pull up compounds that aren't even close. This is due to Metlin's
#' search functionality, not this package's internal workings.
#'
#' @param name The name of the compound
#'
#' @return A data frame containing Metlin compound ID, exact mass, compound
#'   name, formula, CAS and KEGG numbers, and the MSMS status of each compound
#'   (experimental vs. none). It does not currently return the structures.
#'
#' @examples
#' getMetlinMz("betaine")
#' getMetlinMz("adenine")
#'
#' @export
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
                "CAS", "KEGG", "MSMS", "Structure")) %>%
    select(-Structure)

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


#' Obtain Metlin MS/MS data for a given compound ID.
#'
#' \code{getMetlinMS2} queries Metlin for MS/MS data corresponding to a given
#' Metlin compound ID. This function returns a long-form data frame with the
#' containing all fragment masses and fragment intensities for a given polarity,
#' adduct, and voltage, as found in the database. The base url here is
#' https://metlin.scripps.edu/showChart.php?molid=[CMPD ID]&etype=experimental,
#' although the actual data is scraped from the Javascript behind the
#' interactive graph. If no MS2 data is found, getMetlinMS2 will tell you and
#' you should check this with the website.
#'
#' @param compound_id The Metlin database number corresponding to the compound
#'   for which MS/MS data is desired
#'
#' @examples
#' getMetlinMS2(35) #L-Valine
#' getMetlinMS2(287) #Betaine
#'
#' @export
getMetlinMS2 <- function(cmpd_id){
  if(!is.numeric(cmpd_id)|cmpd_id<=0){
    stop("Mass must be positive and numeric")
  }
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
