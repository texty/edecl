add_mult_parameters <- function(params, param_name) {
  s <- ""
  for (p in params) {
    s <- paste0(s, paste0(param_name, "=", as.character(p), "&"))
  }
  s
}

char2num <- function(df) {
  num_fields <- c("totalArea", "costAssessment", "costDate", "percent.ownership", 
                  "sizeAssets", "sizeObligation", "costAmount")
  date_fields <- c("owningDate", "dateOrigin")
  datetime_fields <- c("created_date")
  num_fields <- num_fields[num_fields %in% names(df)]
  for (f in num_fields) {
    df[[f]] <- gsub(",", ".", df[[f]], fixed = TRUE)
    df[[f]][df[[f]] == ""] <- NA
    df[[f]] <- as.numeric(df[[f]])
  }
  date_fields <- date_fields[date_fields %in% names(df)]
  for (f in date_fields) {
    df[[f]][df[[f]] == ""] <- NA
    df[[f]] <- as.Date(df[[f]], format = "%d.%m.%Y")
  }
  datetime_fields <- datetime_fields[datetime_fields %in% names(df)]
  for (f in datetime_fields) {
    df[[f]][df[[f]] == ""] <- NA
    df[[f]] <- as.Date(df[[f]], format =  "%Y-%m-%dT%H:%M:%S")
  }
  df
}

decl_request <- function(q = NULL, deepsearch=FALSE, declaration_year = NULL, 
                         doc_type = NULL, post_type = NULL,
                         region_type = NULL, region_value = NULL, page = NULL) {
  Sys.sleep(1.5)
  query <- list(q = q, region_value = region_value, doc_type = doc_type, 
                declaration_year = declaration_year, region_type = region_type,
                page = page)
  url <- "https://declarations.com.ua/search?"
  url <- paste0(url, add_mult_parameters(post_type, "post_type"))
  if (deepsearch) {
    url <- paste0(url, "deepsearch=on&")
  }
  url <- paste0(url, "format=opendata")
  request <- httr::GET(url = url, query = query)
  httr::content(request)
}

get_infocard <- function(d) {
  df <- data.frame(matrix(unlist(d$infocard), byrow = TRUE, nrow = 1), stringsAsFactors = F)
  names(df) <- names(d$infocard)
  #df$guid <- d$guid
  df$fullname <- stringr::str_trim(paste(df$last_name, df$first_name, df$patronymic))

  df[, c("fullname", "office", "position", "id", names(df)[-which(names(df) %in% c("fullname", "office", "position", "id")  )])]
}
#' Related companies
#'
#' Function to find companies related to declarers
#' @param decls
#' @keywords related_companies
#' @export
#' @examples 
#' library(dplyr)
#' poroshenko_companies <- 
#'     download_declarations("порошенко петро олексійович", declaration_year = "2016") %>% 
#'     related_companies()
related_companies <- function(decls) {
  df <- data.frame()
  for (d in decls) {
    companies <- unlist(d$related_entities$companies$all)
    if (length(companies) > 0) {
      new_rows <- get_infocard(d)
      companies_df <- data.frame(company = companies)
      new_rows <- cbind(new_rows, companies_df)
      df <- rbind(df, new_rows)
    }
    
  }
  df
}

doctype_switch <- function(n) {
  switch(n,
         "1" = "Щорічна",
         "2" = "Перед звільненням",
         "3" = "Після звільнення",
         "4" = "Кандидата на посаду",
         "5" = "Форма змін")
}

find_region <- function(s) {
  regions <- c( "м. Київ",
                "Вінницька область",
                "Волинська область",
                "Дніпропетровська область",
                "Донецька область",
                "Житомирська область",
                "Закарпатська область",
                "Запорізька область",
                "Івано-Франківська область",
                "Київська область",
                "Кіровоградська область",
                "Львівська область",
                "Луганська область",
                "Миколаївська область",
                "Одеська область",
                'Полтавська область',
                "Рівненська область",
                "Сумська область",
                "Тернопільська область",
                "Харківська область",
                "Херсонська область",
                "Хмельницька область",
                "Черкаська область",
                "Чернівецька область",
                "Чернігівська область",
                "Кримська Автономна Республіка",
                "м. Севастополь")
  found <- regions[tolower(s) == tolower(regions)]
  if (length(found) == 0) {
    found <- stringr::str_detect(regions, s)
    if (sum(found) == 0) {
      stop(paste0("Cannot find region by substring \"", s, "\""))
    }
    if (sum(found) > 1) {
      print(regions[found])
      s
    } else {
      regions[found]
    }
  }
}

region_type_switch <- function(n) {
  switch(n,
         "1" = "region",
         "2" = "actual_region",
         "3" = "estate_region")
}

post_type_switch <- function(n) {
  switch(n,
         "1" = "державної",
         "2" = "місцевого",
         "3" = "юридичної")
}
  
  
#' Download declarations
#'
#' Downloads declarations from declarations.com.ua
#' @param q Search query
#' @param deepsearch Should website search in all declarations field, not only in name and workpost?
#' @param declaration_year Character or numeric. The year of declaration.
#' @param doc_type Character or numeric. The type of declaration. 1 - "Щорічна", 2 - "Перед звільненням", 3 - "Після звільнення", 4 - "Кандидата на посаду", 5 - "Форма змін".
#' @param post_type Character or numeric. The type of declarer's position. Accepting vector longer than 1 element. 1 or "державної" for state authorities position, 2 or "місцевого" for local authorities, 3 or "юридичної" for state-owned enterprises.
#' @param region_type Should it be search in regions where declarer is registered (1 or "region"), where he lives (2 or "actual_region") or where owns realty (3 or "estate_region")?
#' @param region_value Region query value. Substring that can identify region name. 
#' @keywords download_declarations
#' @export
#' @examples 
#' library(dplyr)
#' poroshenko2016 <- 
#'     download_declarations("порошенко петро олексійович", declaration_year = 2016, declaration_type = 1)
#'
download_declarations <- function(q = NULL, deepsearch=FALSE, declaration_year = NULL, 
                                  doc_type = NULL, post_type = NULL,
                                  region_type = NULL, region_value = NULL) {
  if (!is.null(region_value)) {
    region_value <- find_region(region_value)
  }
  if (class(doc_type) == "numeric") {
    doc_type <- doctype_switch(doc_type)
  }
  if (class(region_type) == "numeric") {
    region_type <- region_type_switch(region_type)
  }
  if (class(post_type) == "numeric") {
    post_type <- sapply(post_type, post_type_switch)
  }
  first_page <- decl_request(q = q, deepsearch = deepsearch, declaration_year = declaration_year,
                             doc_type = doc_type, post_type = post_type, region_type = region_type,
                             region_value = region_value)
  objects <- first_page$results$object_list
  number_pages <- first_page$results$paginator$num_pages
  if (number_pages > 1) {
    pb <- txtProgressBar(min = 0, max = number_pages, style = 3)
    for (i in 2:number_pages) {
      objects <- c(objects, decl_request(q = q, deepsearch = deepsearch, declaration_year = as.character(declaration_year),
                                         doc_type = doc_type, post_type = post_type, region_type = region_type,
                                         region_value = region_value, page = i)$results$object_list)
      setTxtProgressBar(pb, i)
    }
    close(pb)
  }
  objects
}

extract_guids <- function(decls) {
  sapply(decls, function(x) x$guid)
}

#' Exctract names
#'
#' Extracts names of the declarers
#' @param decls The declarations set
#' @keywords extract_names
#' @export
#' @examples 
#' library(dplyr)
#' bitcoins2016 <- 
#'     download_declarations("біткойн", declaration_year = "2016", deepsearch = T) %>% 
#'     extract_names()
extract_names <- function(decls) {
  sapply(decls, function(x) {
    stringr::str_trim(paste(c(x$infocard$last_name, x$infocard$first_name, x$infocard$patronymic), collapse = " "))
  })
}

filter_by_guids <- function(decls, guids) {
  decls_ <- list()
  i <- 1
  for (d in decls) {
    if (d$guid %in% guids) {
      decls_[[i]] <- d
      i <- i + 1
    }
  }
  decls_
}

#' Declarations intersection
#'
#' Function to find declarations within other set of declarations (intersect two sets)
#' @param decls1 Declarations set
#' @param decls2 Declarations set
#' @keywords dintersect
#' @export
#' @examples 
#' library(dplyr)
#' mps2016_bitcoins <- 
#'     download_declarations("народний депутат", declaration_year = "2016") %>% 
#'     dintersect(download_declarations("біткойн", deepseach = TRUE))
dintersect <- function(decls1, decls2) {
  filter_by_guids(decls1, extract_guids(decls2))
}

#' Gets only corrected versions of declarations
#'
#' Exclude from a set declarations that have corrected version.
#' @param decls Declarations set
#' @keywords get_corrected
#' @export
#' @examples 
#' library(dplyr)
#' zalishchuk2015_corrected <- 
#'    download_declarations("заліщук світлана петрівна", declaration_year = "2015") %>% 
#'    get_corrected()
get_corrected <- function(decls) {
  decl_names <- extract_names(decls)
  final <- rep(TRUE, length(decls))
  for (i in 1:length(decl_names)) {
    identical_names_number <- which(decl_names == decl_names[i])
    identical_names_number <- identical_names_number[identical_names_number > i]
    for (j in identical_names_number) {
      if (same_person(decls[[i]], decls[[j]])) {
        if(decls[[j]]$infocard$is_corrected) {
          final[i] <- FALSE
        }
      }
    }
  }
  decls[final]
}

same_person <- function(d1, d2) {
  if (d1$infocard$first_name == d2$infocard$first_name & d1$infocard$patronymic == d2$infocard$patronymic & d1$infocard$last_name == d2$infocard$last_name) 
  {
    if (d1$unified_source$step_0$declarationType == d2$unified_source$step_0$declarationType & d1$unified_source$step_0$declarationType != "2") {
      year1_field <- names(d1$unified_source$step_0)[grepl("year", names(d1$unified_source$step_0), ignore.case = T)]
      year2_field <- names(d2$unified_source$step_0)[grepl("year", names(d2$unified_source$step_0), ignore.case = T)]
      if (year1_field == year2_field & d1$unified_source$step_0[[year1_field]] == d2$unified_source$step_0[[year2_field]]) {
        steps <- 2:15
        there_was_identical_step <- FALSE
        for (s in steps) {
          str_s <- paste("step", as.character(s), sep = "_")
          if (length(d1$unified_source[[str_s]]) > 0 & identical(d1$unified_source[[str_s]], d2$unified_source[[str_s]])) {
            there_was_identical_step <- TRUE
          }
        }
        there_was_identical_step
      } else {
        FALSE
      }
    } else {
      FALSE
    }
    
  } 
  else {
    FALSE
  }
}

#' Extract declarations sections
#'
#' Extract specific sections of declarations into dataframe. For now - ignoring "rights" and "guarantor" subsections
#' @param d Declarations set
#' @param step Step (section) of declarations to be extracted. Works correctly if receives values between 2 and 16
#' @param add_rights Logical. Should information on additional (not belonged to declarer or his family members) rights be saved? Default to FALSE.
#' @param guarantor. Logical. Should information on loan guarantors be saved? Defaults to FALSE.
#' @param guarantor_realty Logical. Should information on loan guarantors' realty be saved? Defaults to FALSE.
#' @keywords step_to_df
#' @details The value is always list of 4 data frames. The data frames are available by names "data", "add_rights", "guarantor", "guarantor_realty". If corresponded parameters equal FALSE, these dataframes are always blank. If these parameters are set to TRUE, the function works slower.
#' @examples 
#' library(dplyr)
#' mps2016_realty <- 
#'    download_declarations("народний депутат", declaration_year = "2016") %>% 
#'    step_to_df(3)$data
step_to_df <- function(decls, step, add_rights = FALSE, 
                       guarantor = FALSE, guarantor_realty = FALSE) {
  final_list <- list()

  pb <- txtProgressBar(min = 0, max = length(decls), style = 3)
  count <- 1
  for (d in decls) {
    single_declaration <- single_step_to_df(d, step, add_rights = add_rights ,
                                            guarantor = guarantor, 
                                            guarantor_realty = guarantor_realty)
    final_list$data <- dplyr::bind_rows(final_list$data, single_declaration$data)
    final_list$add_rights <- dplyr::bind_rows(final_list$add_rights, single_declaration$add_rights)
    final_list$guarantor <- dplyr::bind_rows(final_list$guarantor, single_declaration$guarantor)
    final_list$guarantor_realty <- dplyr::bind_rows(final_list$guarantor_realty, single_declaration$guarantor_realty)
    setTxtProgressBar(pb, count)
    count <- count + 1
  }
  final_list
}

#' Exclude declarations from set
#'
#' Excludes declarations from set that belongs to other set
#' @param decls List of declarations.
#' @keywords extract_info
#' @export
#' @examples 
#' library(dplyr)
#' mps2016 <- 
#'    download_declarations("хутро", deepsearch = TRUE, declaration_year = 2016, declaration_type = 1) %>% 
#'    extract_info()
extract_info <- function(decls) {
  df <- data.frame()
  for (d in decls) {
    df <- dplyr::bind_rows(df, get_infocard(d))
  }
  df
}

#' Exclude declarations from set
#'
#' Excludes declarations from set that belongs to other set
#' @param decls1 First set of declarations
#' @param decls2 The second set of declarations, or their ids.
#' @keywords dexclude
#' @export
#' @examples 
#' library(dplyr)
#' mps2016 <- 
#'    download_declarations("народний депутат", declaration_year = "2016") %>% 
#'    dexclude(download_declarations("помічник народного депутата", declaration_year = "2016"))
dexclude <- function(decls1, decls2) {
  d1_guids <- extract_guids(decls1)
  if (class(decls2) == "list") {
    d2_guids <- extract_guids(decls2)
  } else {
    d2_guids <- as.character(decls2)
  }
  
  guids <- d1_guids[!(d1_guids %in% d2_guids)]
  filter_by_guids(decls1, guids)
}


single_step_to_df <- function(d, step, add_rights = FALSE, guarantor = FALSE, guarantor_realty = FALSE) {
  final_list <- list()
  if (class(step) == "numeric") {
    step <- paste0("step_", as.character(step))
  }
  if (step != "step_16") {
    step <- d[['unified_source']][[step]]
  } else {
    st <- list()
    step <-  d[['unified_source']][[step]]
    for (ot in 1:length(step)) {
      org_of_type <- step[[ot]]
      if (length(org_of_type) > 0) {
        for (org_number in 1:length(org_of_type)) {
          org <- org_of_type[org_number]
          org_name <- names(org_of_type)[org_number]
          if (!is.null(org_name)) {
            st[[org_name]] <- list(org)
          } 
        }
      }
      
    }
    step <- st
  }
  add_rights_table <- data.frame(stringsAsFactors = FALSE)
  g_table <- data.frame(stringsAsFactors = FALSE)
  g_r_table <- data.frame(stringsAsFactors = FALSE)
  if (length(step) > 0) {
    df <- data.frame()
    for (i in 1:length(step)) {
      o <- step[[i]]
      #print(o)
      if (class(o) == "list") {
        rights_columns <- data.frame(list())
        if ("rights" %in% names(o)) {
          rights_columns <- o$rights[[o$person]][c("ownershipType", "otherOwnership", "percent-ownership")]
          o$rights[[o$person]] <- NULL
          rights_columns <- data.frame(rights_columns, stringsAsFactors = FALSE)
          if (add_rights) {
            if (length(o$rights) > 0) {
              for (j in 1:length(o$rights)) {
                rights_row <- data.frame(o$rights[[j]], stringsAsFactors = FALSE)
                rights_row <- as.list(apply(rights_row, 2, as.character))
                rights_row[['rightBelongs']] <- names(o$rights)[j]
                rights_row[['object_id']] <- names(step)[i]
                add_rights_table <- bind_rows(add_rights_table, rights_row)
              }
            }
          }
        }
        if (guarantor)
        {
          add_guarantor <- data.frame()
          if ("guarantor" %in% names(o)) {
            if (length(o$guarantor) > 0) {
              for ( j in 1:length(o$guarantor)) {
                g_row <- data.frame(o$guarantor[[j]], stringsAsFactors = FALSE)
                g_row <- as.list(apply(g_row, 2, as.character)) 
                g_row[["guarantor_id"]] <- names(o$guarantor)[j]
                g_row[['object_id']] <- names(step)[i]
                add_guarantor <- bind_rows(add_guarantor, g_row)
              }
            }
          }
        }
        if (guarantor_realty)
        {
          add_guarantor_r <- data.frame()
          if ("guarantor_realty" %in% names(o)) {
            if (length(o$guarantor_realty) > 0) {
              for ( j in 1:length(o$guarantor_realty)) {
                gr_row <- data.frame(o$guarantor_realty[[j]], stringsAsFactors = FALSE)
                gr_row <- as.list(apply(gr_row, 2, as.character)) 
                gr_row[["guarantor_realty_id"]] <- names(o$guarantor_realty)[j]
                gr_row[['object_id']] <- names(step)[i]
                add_guarantor_r <- bind_rows(add_guarantor_r, gr_row)
              }
            }
          }
        }
        o$rights <- NULL
        o$guarantor <- NULL
        o$guarantor_realty <- NULL
        o <- lapply(o, function(x) {ifelse(length(x) == 0, "", x)})
        df_new <- data.frame(o, stringsAsFactors = F)
        df_new <- as.list(apply(df_new, 2, as.character))
        df_new[['object_id']] <- names(step)[i]
        df <- dplyr::bind_rows(df, dplyr::bind_cols(df_new, rights_columns))
      }
    }
    if (nrow(df) > 0) {
      final_list$data <- char2num(cbind(get_infocard(d), df))
      if (add_rights) {
        final_list$add_rights <- add_rights_table
      } 
      if (guarantor) {
        final_list$guarantor <- add_guarantor
      }      
      if (guarantor_realty) {
        final_list$guarantor_realty <- add_guarantor_r
      }
    } 
  }
  final_list
}