require(RefManageR)
try(bib <- ReadBib("../Publications.bib"))
try(bib <- ReadBib("../../Publications.bib"))
try(bib <- ReadBib("../../../Publications.bib"))



format_apa <- function(entry, return_html = TRUE) {
  # Veilige veldextractie
  get_field <- function(x) {
    if (is.null(x) || length(x) == 0) return("")
    x <- as.character(x[1])
    if (is.na(x) || x == "") return("")
    return(trimws(x))
  }
  
  # Authors/Editors formatteren
  format_authors <- function(authors, role = "authors") {
    if (is.null(authors) || length(authors) == 0) return("")
    
    auth_strs <- character(length(authors))
    for (i in seq_along(authors)) {
      author <- authors[[i]]
      family <- get_field(author$family)
      given <- get_field(author$given)
      
      if (family == "" && given == "") next
      
      # Maak initialen van given namen
      if (given != "") {
        given_parts <- strsplit(given, "\\s+")[[1]]
        initials <- sapply(given_parts, function(g) {
          g <- gsub("\\.", "", g)
          if (nchar(g) > 0) substr(g, 1, 1) else ""
        })
        initials <- paste(initials[initials != ""], collapse = ". ")
        auth_strs[i] <- paste0(family, ", ", initials, ".")
      } else {
        auth_strs[i] <- family
      }
    }
    
    auth_strs <- auth_strs[auth_strs != ""]
    
    if (length(auth_strs) == 0) return("")
    if (length(auth_strs) == 1) return(auth_strs[1])
    if (length(auth_strs) == 2) return(paste(auth_strs[1], "&", auth_strs[2]))
    
    return(paste(paste(auth_strs[-length(auth_strs)], collapse = ", "),
                 "&", auth_strs[length(auth_strs)]))
  }
  
  # Basis velden
  entry_type <- tolower(get_field(entry$type))
  authors <- format_authors(entry$author)
  year <- get_field(entry$year)
  title <- get_field(entry$title)
  doi <- get_field(entry$doi)
  url <- get_field(entry$url)
  
  # Maak DOI-link
  make_doi_link <- function(doi) {
    if (doi == "") return("")
    doi_clean <- sub("^https?://doi\\.org/", "", doi)
    return(sprintf(" https://doi.org/%s", doi_clean))
  }
  
  # Maak URL-link
  make_url_link <- function(url) {
    if (url == "") return("")
    return(sprintf(" %s", url))
  }
  
  # Veldextractie voor verschillende types
  journal <- get_field(entry$journal)
  volume <- get_field(entry$volume)
  number <- get_field(entry$number)
  pages <- get_field(entry$pages)
  publisher <- get_field(entry$publisher)
  booktitle <- get_field(entry$booktitle)
  edition <- get_field(entry$edition)
  note <- get_field(entry$note)
  
  # APA formatteren naar type
  apa <- ""
  
  # ARTIKEL IN JOURNAL
  if (entry_type %in% c("article", "journal")) {
    apa <- sprintf("%s (%s). %s.", authors, year, title)
    
    if (journal != "") {
      apa <- paste0(apa, " <i>", journal, "</i>")
    }
    
    if (volume != "") {
      apa <- paste0(apa, ", <b>", volume, "</b>")
    }
    
    if (number != "") {
      apa <- paste0(apa, "(", number, ")")
    }
    
    if (pages != "") {
      apa <- paste0(apa, ", ", pages)
    }
    
    apa <- paste0(apa, ".")
  }
  
  # CONFERENCE PAPER / INPROCEEDINGS
  else if (entry_type %in% c("inproceedings", "conference")) {
    apa <- sprintf("%s (%s). %s.", authors, year, title)
    
    if (booktitle != "") {
      apa <- paste0(apa, " In <i>", booktitle, "</i>")
    }
    
    if (pages != "") {
      apa <- paste0(apa, " (pp. ", pages, ")")
    }
    
    if (publisher != "") {
      apa <- paste0(apa, ". ", publisher)
    }
    
    apa <- paste0(apa, ".")
  }
  
  # BOEK
  else if (entry_type %in% c("book", "incollection")) {
    apa <- sprintf("%s (%s). <i>%s</i>.", authors, year, title)
    
    if (publisher != "") {
      apa <- sub("\\.$", paste0(". ", publisher, "."), apa)
    }
  }
  
  # THESIS
  else if (entry_type %in% c("phdthesis", "mastersthesis")) {
    thesis_type <- if (entry_type == "phdthesis") "PhD thesis" else "Master's thesis"
    apa <- sprintf("%s (%s). <i>%s</i>. %s.", authors, year, title, thesis_type)
    
    if (publisher != "") {
      apa <- sub("\\.$", paste0(", ", publisher, "."), apa)
    }
  }
  
  # SOFTWARE
  else if (entry_type == "software") {
    apa <- sprintf("%s (%s). <i>%s</i>.", authors, year, title)
    
    if (note != "") {
      apa <- sub("\\.$", paste0(". ", note, "."), apa)
    }
  }
  
  # SUBMITTED/IN PRESS
  else if (entry_type %in% c("unpublished", "submitted")) {
    status <- if (grepl("submitted|in press", tolower(note))) 
      tolower(note) else "Unpublished manuscript"
    
    apa <- sprintf("%s (%s). %s. %s.", authors, year, title, status)
  }
  
  # DEFAULT (misc, etc.)
  else {
    apa <- sprintf("%s (%s). %s.", authors, year, title)
    
    if (journal != "") {
      apa <- paste0(apa, " <i>", journal, "</i>.")
    } else if (publisher != "") {
      apa <- paste0(apa, " ", publisher, ".")
    }
  }
  
  # Voeg DOI als clickable link toe
  if (doi != "") {
    doi_clean <- sub("^https?://doi\\.org/", "", doi)
    # Zorg dat het een HTML-link wordt
    if (return_html) {
      apa <- paste0(apa, ' <a href="https://doi.org/', doi_clean, 
                    '" target="_blank">https://doi.org/', doi_clean, '</a>')
    } else {
      apa <- paste0(apa, " https://doi.org/", doi_clean)
    }
  } else if (url != "") {
    if (return_html) {
      apa <- paste0(apa, ' <a href="', url, '" target="_blank">', url, '</a>')
    } else {
      apa <- paste0(apa, " ", url)
    }
  }
  
  # ← BELANGRIJK: return als HTML object
  if (return_html) {
    return(htmltools::HTML(apa))
  } else {
    return(apa)
  }
  
}
