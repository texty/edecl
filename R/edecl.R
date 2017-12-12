add_mult_parameters <- function(params, param_name) {
  s <- ""
  for (p in params) {
    s <- paste0(s, paste0(param_name, "=", as.character(p), "&"))
  }
  s
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
  df$guid <- d$guid
  df
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
#' Download declarations
#'
#' Downloads declarations from declarations.com.ua
#' @param q Search query
#' @param deepsearch Should website search in all declarations field, not only in name and workpost?
#' @param declaration_year Character. The year of declaration.
#' @param doc_type Character. The type of declaration.
#' @param post_type Character. The type of post
#' @param region_type Should it be search in regions where declarer is registered, where he lives or where has realty?
#' @param region_value Region query value.
#' @keywords download_declarations
#' @export
#' @examples 
#' library(dplyr)
#' poroshenko2016 <- 
#'     download_declarations("порошенко петро олексійович", declaration_year = "2016")
#'
download_declarations <- function(q = NULL, deepsearch=FALSE, declaration_year = NULL, 
                                  doc_type = NULL, post_type = NULL,
                                  region_type = NULL, region_value = NULL) {
  first_page <- decl_request(q = q, deepsearch = deepsearch, declaration_year = declaration_year,
                             doc_type = doc_type, post_type = post_type, region_type = region_type,
                             region_value = region_value)
  objects <- first_page$results$object_list
  number_pages <- first_page$results$paginator$num_pages
  if (number_pages > 1) {
    pb <- txtProgressBar(min = 0, max = number_pages, style = 3)
    for (i in 2:number_pages) {
      #cat(paste0("Fetching page ", as.character(i)))
      objects <- c(objects, decl_request(q = q, deepsearch = deepsearch, declaration_year = declaration_year,
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
#' @param step Step (section) of declarations to be extracted. Works correctly if receives values between "step_2" and "step_15"
#' @keywords step_to_df
#' @export
#' @examples 
#' library(dplyr)
#' mps2016_realty <- 
#'    download_declarations("народний депутат", declaration_year = "2016") %>% 
#'    step_to_df("step_3")
step_to_df <- function(decls, step) {
  df <- data.frame()
  for (d in decls) {
    df <- dplyr::bind_rows(df, single_step_to_df(d, step))
  }
  df
}
    

single_step_to_df <- function(d, step) {
  step <- d[['unified_source']][[step]]
  if (!is.null(step)) {
    df <- data.frame()
    for (i in 1:length(step)) {
      o <- step[[i]]
      if (class(o) == "list") {
        rights_columns <- data.frame(list())
        if ("rights" %in% names(o)) {
          rights_columns <- o$rights[[o$person]][c("ownershipType", "otherOwnership", "percent-ownership")]
          o$rights[[o$person]] <- NULL
          rights_columns <- data.frame(rights_columns, stringsAsFactors = FALSE)
          if (length(o$rights) > 0) {
            for (j in 1:length(o$rights)) {
              if (names(o$rights)[j] != o$rights[[j]]$rightBelongs) {
                print(d$guid)
                print((names(o$rights)[j]))
                print(o$rights[[j]]$rightBelongs)
              }
            }
          } 
        }
        o$rights <- NULL
        o$guarantor <- NULL
        o$guarantor_realty <- NULL
        df_new <- data.frame(o, stringsAsFactors = F)
        df_new$object_id <- names(step)[i]
        df <- dplyr::bind_rows(df, dplyr::bind_cols(df_new, rights_columns))
      }
      
    }
    if (nrow(df) > 0) {
      cbind(get_infocard(d), df)
    }
  }
}