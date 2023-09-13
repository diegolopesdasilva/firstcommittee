
# A Textual Analysis of the Speeches at the General Assembly's First Committee in 2021.

# LOAD PACKAGES--------------------------------------------------------
library(tidyverse)
library(readtext)
library(pdftools)
library(tidytext)
library(rvest)
library(dplyr)
library(tidyr)
library(httr)
library(cld2)
library(purrr)
library(syuzhet)
library(ggplot2)
library(ggrepel)
library(wordcloud)
library(stringr)
library(textclean)
library(textcat)
library(stopwords)
library(igraph)
library(tesseract)
library(magick)
library(cld2)
library(textcat)
library(httr)
library(jsonlite)
library(tokenizers)
library(stringi)
library(future)
library(furrr)
library(purrr)
library(lubridate)
library(ggstream)

# BUILDING DATASET--------------------------------------------------------------

## Extracting links-------------------------------------------------------------
pdf_links <- list()
  
  # Loop through the different years
  for (year in 2007:2022) {
    year_links <- data.frame(link = character(), stringsAsFactors = FALSE)
    
    # Loop through the different URLs
    for (start in seq(0, 225, 25)) {
      # Construct the URL for the current page and year
      url <- paste0("https://www.reachingcriticalwill.org/documents/statements?toolbar_year=", year, "&toolbar_forum=4&toolbar_country=0&toolbar_topic=0&start=", start)
      
      # Scrape the links on the current page
      page_links <- read_html(url) %>% 
        html_nodes("#document_list_table a") %>% 
        html_attr("href") %>% 
        data.frame(link = ., stringsAsFactors = FALSE)
      
      # Add the links to the data frame for the current year
      year_links <- rbind(year_links, page_links)
    }
    
    # Store the links for the current year in the list
    pdf_links[[as.character(year)]] <- year_links
  }

  # Combine the links from all years into a single data frame
pdf_links <- do.call(rbind, pdf_links)


  ### create csv file
  write.csv(pdf_links, "pdf_links.csv", row.names = FALSE)
  
  
  ## Extracting titles------------------------------------------------------------
  pdf_text <- data.frame(text = character(), stringsAsFactors = FALSE)
  
  # Loop through the different years
  for (year in 2007:2022) {
    year_text <- data.frame(text = character(), stringsAsFactors = FALSE)
    
    # Loop through the different URLs
    for (start in seq(0, 225, 25)) {
      # Construct the URL for the current page and year
      url <- paste0("https://www.reachingcriticalwill.org/documents/statements?toolbar_year=", year, "&toolbar_forum=4&toolbar_country=0&toolbar_topic=0&start=", start)
      
      # Scrape the text on the current page
      page_text <- read_html(url) %>% 
        html_nodes("#document_list_table a") %>% 
        html_text() %>% 
        data.frame(text = ., stringsAsFactors = FALSE)
      
      # Add the text to the data frame for the current year
      year_text <- rbind(year_text, page_text)
    }
    
    # Store the text for the current year in the data frame
    pdf_text <- rbind(pdf_text, year_text)
  }
  
  # Write the text to a CSV file
  write.csv(pdf_text, "pdf_texts.csv", row.names = FALSE)

  pdf_merged <- read_csv("/Users/diego.lopes/Library/CloudStorage/OneDrive-Personal/R/pdf_merged_backup.csv")
  
  
  ## combining text and titles
  
  ### Add an id column based on row number
  pdf_text <- pdf_text %>% 
    mutate(id = row_number())
  
  pdf_links <- pdf_links%>%
    mutate(id = row_number())
  
  pdf_merged <- merge(pdf_links, pdf_text, by = "id")
  
  # add the first part of the link (I dont know why it wasnt picking that up)
  pdf_merged$link[1:1504] <- paste0("https://www.reachingcriticalwill.org", pdf_merged$link[1:1504])
  
  ## improving the identifiers
  
  # Split the text column into country, date, and forum
  pdf_merged <- cbind(pdf_merged, t(
    apply(
      as.data.frame(pdf_merged$text),
      1,
      function(x) {
        parts <- strsplit(x, ",\\s*")[[1]]
        if (length(parts) >= 3) {
          date_forum <- rev(parts[(length(parts) - 1):length(parts)])
          country <- paste(parts[1:(length(parts) - 2)], collapse = ", ")
          c(
            country,
            paste(date_forum[1], collapse = ", "),
            paste(date_forum[2], collapse = ", ")
          )
        } else {
          c("", "", "")
        }
      }
    )
  ))
  
  
  # renaming columns
  colnames(pdf_merged)[colnames(pdf_merged) == "1"] <- "orator"
  colnames(pdf_merged)[colnames(pdf_merged) == "2"] <- "forum"
  colnames(pdf_merged)[colnames(pdf_merged) == "3"] <- "date"
  
  # fixing different formats of titles
  
  ## Cuba
  pdf_merged$orator[708] <- sub("", "Cuba",
                           pdf_merged$orator[708])
  pdf_merged$forum[708] <- sub("", "First Committee",
                                 pdf_merged$forum[708])
  pdf_merged$date[708] <- sub("", "2010-10-27",
                               pdf_merged$date[708])
  
  pdf_merged$orator[709] <- sub("", "Cuba",
                                 pdf_merged$orator[709])
  pdf_merged$forum[709] <- sub("", "First Committee",
                               pdf_merged$forum[709])
  pdf_merged$date[709] <- sub("", "2010-10-27",
                              pdf_merged$date[709])
  
  ## Algeria
  pdf_merged$orator[820] <- sub("", "Algeria",
                                 pdf_merged$orator[820])
  pdf_merged$forum[820] <- sub("", "First Committee",
                               pdf_merged$forum[820])
  pdf_merged$date[820] <- sub("", "2010-10-18",
                              pdf_merged$date[820])
  
  # Switzerland
  pdf_merged$orator[847] <- sub("", "Switzerland",
                                 pdf_merged$orator[847])
  pdf_merged$forum[847] <- sub("", "First Committee",
                               pdf_merged$forum[847])
  pdf_merged$date[847] <- sub("", "2010-10-15",
                              pdf_merged$date[847])
  
  ## New Agenda Coalition
  pdf_merged$orator[848] <- sub("", "New Agenda Coalition",
                                 pdf_merged$orator[848])
  pdf_merged$forum[848] <- sub("", "First Committee",
                               pdf_merged$forum[848])
  pdf_merged$date[848] <- sub("", "2010-10-15",
                              pdf_merged$date[848])
  
  ## Israel, 7 October 2010. First Committee
  pdf_merged$orator[915] <- sub("", "Israel",
                                 pdf_merged$orator[915])
  pdf_merged$forum[915] <- sub("", "First Committee",
                               pdf_merged$forum[915])
  pdf_merged$date[915] <- sub("", "2010-10-07",
                              pdf_merged$date[915])
  
  ## Holy See, 11 October 2011. First Committee
  pdf_merged$orator[1099] <- sub("", "Holy See",
                                 pdf_merged$orator[1099])
  pdf_merged$forum[1099] <- sub("", "First Committee",
                               pdf_merged$forum[1099])
  pdf_merged$date[1099] <- sub("", "2011-10-11",
                              pdf_merged$date[1099])
  
  ## Maldives. 5 October 2011, First Committee
  pdf_merged$orator[1157] <- sub("", "Maldives",
                                  pdf_merged$orator[1157])
  pdf_merged$forum[1157] <- sub("", "First Committee",
                                pdf_merged$forum[1157])
  pdf_merged$date[1157] <- sub("", "2011-10-05",
                               pdf_merged$date[1157])
  
  ## Sweden 21 October 2015, General Assembly First Committee
  pdf_merged$orator[1889] <- sub("", "Sweden",
                                  pdf_merged$orator[1889])
  pdf_merged$forum[1889] <- sub("", "First Committee",
                                pdf_merged$forum[1889])
  pdf_merged$date[1889] <- sub("", "2015-10-21",
                               pdf_merged$date[1889])
  
  ## Pakistan 30 October 2018, First Committee
  pdf_merged$orator[2239] <- sub("", "Pakistan",
                                  pdf_merged$orator[2239])
  pdf_merged$forum[2239] <- sub("", "First Committee",
                                pdf_merged$forum[2239])
  pdf_merged$date[2239] <- sub("", "2018-10-30",
                               pdf_merged$date[2239])
 
  ## ASEAN 9 October 2020, First Committee
  pdf_merged$orator[2830] <- sub("", "ASEAN",
                                  pdf_merged$orator[2830])
  pdf_merged$forum[2830] <- sub("", "First Committee",
                                pdf_merged$forum[2830])
  pdf_merged$date[2830] <- sub("", "2020-10-09",
                               pdf_merged$date[2830])
  
  
  ## ASEAN 9 October 2020, First Committee
  pdf_merged$forum[223] <- sub("CD", "First Committee",
                                  pdf_merged$forum[223])
  pdf_merged$forum[1627] <- sub("FC", "First Committee",
                               pdf_merged$forum[1627])
  pdf_merged$forum[1878] <- sub("First and Fourth Committee", "First Committee",
                                pdf_merged$forum[1878])
  
  
  pdf_merged$forum <- "First Committee"
  

  # Convert the date column to date format
  pdf_merged$date <- as.Date(pdf_merged$date, format = "%d %B %Y")
  
  write.csv(pdf_merged, "pdf_merged_backup.csv", row.names = FALSE)
  

## Extracting texts from pdfs----------------------------------------------------
  
### Create a new tesseract engine------------------------------------------------
  engine <- tesseract("eng+fra+spa+ara")
  
  for (i in 1:3128) {
    pdf_file <- tempfile()
    
    # Try to download the PDF
    tryCatch(
      {
        GET(pdf_merged$link[i], write_disk(pdf_file))
        
        # Try to convert the PDF to an image
        tryCatch(
          {
            image <- image_read_pdf(pdf_file)
            
            # Try to extract text from the image using OCR
            tryCatch(
              {
                text <- tesseract::ocr(image, engine = engine)
                pdf_merged$speeches[i] <- paste(text, collapse = "\n")
                cat(paste0("Extracted text from PDF ", i, "\n"))
              },
              error = function(e) {
                cat(paste0("Error extracting text from PDF ", i, ": ", conditionMessage(e), "\n"))
              }
            )
          },
          error = function(e) {
            cat(paste0("Error converting PDF ", i, " to image: ", conditionMessage(e), "\n"))
          }
        )
      },
      error = function(e) {
        cat(paste0("Error downloading PDF ", i, ": ", conditionMessage(e), "\n"))
      }
    )
  } # apparently it works!
  
  write.csv(pdf_merged, "pdf_merged_backup.csv", row.names = FALSE)
  
 
# cleaning and tidying the text------------------------------------------------
  for (i in seq_along(pdf_merged$speeches)) {
    # Remove page numbers
    pdf_merged$speeches[i] <- gsub("\\b\\d+\\b", "", pdf_merged$speeches[i])
    
    # Remove multiple spaces and new lines
    pdf_merged$speeches[i] <- gsub("[ \t\r]+", " ", pdf_merged$speeches[i])
    pdf_merged$speeches[i] <- gsub("(\\S)[ ]{2,}(\\S)", "\\1 \\2",
                                   pdf_merged$speeches[i], perl = TRUE)
    pdf_merged$speeches[i] <- gsub("(\\s)+$", "", pdf_merged$speeches[i])
    pdf_merged$speeches[i] <- gsub("^(\\s)+", "", pdf_merged$speeches[i])
  }
  
  pdf_merged$speeches <- gsub("\n", " ", pdf_merged$speeches)#I found that "\n" was messing up the extraction
  pdf_merged$speeches <- gsub(" {2,}", " ", pdf_merged$speeches)
  
  ### counting ID's per year
  
  pdf_merged$date <- lubridate::ymd(pdf_merged$date)
  pdf_merged$year <- lubridate::year(pdf_merged$date)
  
  total_ids_by_year_pdf_merged <- pdf_merged %>%
    group_by(year) %>%
    summarise(total = n_distinct(id))
  
  
  ### standardizing names of orators
  
  unique_orators <- unique(pdf_merged$orator)
  unique_orators <- sort(unique_orators)
  
  unique_orators
  length(unique_orators)
  
  write.csv(unique_orators, "unique_orators.csv", row.names = FALSE)
  
  # Define the recoding vector
  # Create a recode vector
  recode_vector <- c(
"New Agenda Coalition, delivered by Egypt" = "New Agenda Coalition", 
"African group" = "African Group", 
"African Group, delivered by Morocco" = "African Group", 
"African Group, delivered by Zambia" = "African Group", 
"Agency for the Prohibition of Nuclear Weapons in Latin America (OPANAL)" = "Agency for the Prohibition of Nuclear Weapons in Latin America and the Caribbean (OPANAL)",
"Agency of the Prohibition of Nuclear Weapons in Latin America & the Caribbean" = "Agency for the Prohibition of Nuclear Weapons in Latin America and the Caribbean (OPANAL)",
"ASEAN, delivered by Lao PDR" = "ASEAN",
"ASEAN, delivered by Malaysia" = "ASEAN",
"ASEAN, delivered by Viet Nam" = "ASEAN",
"Association of South East Asian States (ASEAN)" = "ASEAN",
"Association of Southeast Asian Nations (ASEAN), delivered by Malaysia" = "ASEAN",
"Association of Southeast Asian Nations (ASEAN), delivered by Myanmar" = "ASEAN",
"Association of Southeast Asian Nations (ASEAN), delivered by Thailand" = "ASEAN",
"Association of Southeast Asian Nations (ASEAN)"= "ASEAN",
"Australia on behalf of a group of countries" = "Australia",
"Australia on behalf of a group of states" = "Australia",
"Arab Group, delivered by Egypt" = "Arab Group",
"Arab Group, delivered by Tunisia" = "Arab Group",
"Côte d'Ivoire" = "Cote d'Ivoire",
"Côte d’Ivoire" = "Cote d'Ivoire",
"Non-Aligned Movement (NAM)" = "Non-Aligned Movement",
"Non-Aligned Movement (NAM), delivered by Indonesia" = "Non-Aligned Movement",
"Non-Aligned Movement, delivered by Indonesia" = "Non-Aligned Movement",
"Non- Aligned Movement" = "Non-Aligned Movement",
"NAM" = "Non-Aligned Movement",
"New-Aligned Movement" = "Non-Aligned Movement",
"Caribbean Community" = "Caribbean Community (CARICOM)",
" New Agenda Coalition, delivered by Egypt" = "New Agenda Coalition, delivered by Egypt",
"Caribbean Community (CARICOM), delivered by Jamaica" = "Caribbean Community (CARICOM)",
"Caribbean Community (CARICOM), delivered by the Bahamas" = "Caribbean Community (CARICOM)",
"CARICOM" = "Caribbean Community (CARICOM)",
"CARICOM, delivered by Jamaica" = "Caribbean Community (CARICOM)",
"Community of Latin American and Caribbean states (CELAC)" = "Community of Latin American and Caribbean States (CELAC)",
"EU" = "European Union",
"European Union (EU)" = "European Union",
"Cote d'Ivorie" = "Cote d'Ivoire",
"El-Salvador" = "El Salvador",
"Lao People's Democratic" = "Lao PDR",
"Lao People’s Democratic Republic" = "Lao PDR",
"Lao Peoples's Democratic Republic" = "Lao PDR",
"Laos" = "Lao PDR",
"Cote d'Ivorie" = "Cote d'Ivoire",
"C\x99te d'Ivoire"   = "Cote d'Ivoire",
"C\x99te d\xd5Ivoire"  = "Cote d'Ivoire",
"Democratic People's Republic of Kongo" = "Democratic People's Republic of Korea",
"DPRK" = "Democratic People's Republic of Korea",
"The Democratic Republic of the Congo" = "Democratic Republic of the Congo",
"The Republic of Korea" = "Republic of Korea",
"Trinidad & Tobago" = "Trinidad and Tobago",
"Organisation for the Prohibition of Chemical Weapons" = "Organization for the Prohibition of Chemical Weapons (OPCW)",
"Organisation for the Prohibition of Chemical Weapons (OPCW)" = "Organization for the Prohibition of Chemical Weapons (OPCW)",
"Organization for the Prevention of Chemical Weapons" = "Organization for the Prohibition of Chemical Weapons (OPCW)",
"Organization for the Prohibition of Chemical Weapons" = "Organization for the Prohibition of Chemical Weapons (OPCW)",
"Non-Aligned movement" = "Non-Aligned Movement",
"Non-Aligned Movement" = "Non-Aligned Movement",
"MEROCOSUR" = "MERCOSUR",
"Non-Proliferation and Disarmament Initiative" = "Non-Proliferation and Disarmament Initiative",
"Non-proliferation and Disarmament initiative, delivered by Australia" = "Non-Proliferation and Disarmament Initiative",
"Nordic countries" = "Nordic Countries",
"Nordic Countries" = "Nordic Countries",
"Lawyer's Committee for Nuclear Policy on behalf of a group of NGOs, joint statement on human rights, nuclear weapons, and the environment" = "Lawyers Committee on Nuclear Policy",
"Lawyers Committee on Nuclear Policy" = "Lawyers Committee on Nuclear Policy",
"League of Arab States" = "League of Arab States",
"Lichtenstein" = "Liechtenstein",
"Kyrgyz Republic" = "Kyrgyzstan",
"International Atomic Energy Agency" = "International Atomic Energy Agency (IAEA)",
"International Atomic Energy Agency (IAEA)" = "International Atomic Energy Agency (IAEA)",
"International Campaign to Abolish Nuclear Weapons" = "International Campaign to Abolish Nuclear Weapons (ICAN)",
"International Campaign to Abolish Nuclear Weapons (ICAN)" = "International Campaign to Abolish Nuclear Weapons (ICAN)",
"International Campaign to Abolish Nuclear Weapons, joint statement on nuclear weapons" = "International Campaign to Abolish Nuclear Weapons (ICAN)",
"International Committee of the Red Cross" = "International Committee of the Red Cross (ICRC)",
"League of Arab States, delivered by Egypt" = "League of Arab States",
"Nordic Countries, delivered by Sweden" = "Nordic Countries",
"De-alerting group" = "De-Alerting Group",
"De-alerting Group" = "De-Alerting Group",
"De-Alerting Group" = "De-Alerting Group",
"Control Arms" = "Control Arms Coalition",
"Chair of the Biological & Toxin Weapons Convention" = "Chair of the Biological and Toxin Weapons Convention",
"Chair of the Biological and Toxin Weapons Convention" = "Chair of the Biological and Toxin Weapons Convention",
"Chair of the Working Group on Long-term sustainability of Outer Space" = "Chair of the Working Group on Long-Term Sustainability of Outer Space Activities",
"Chair of the Working Group on Long-Term Sustainability of Outer Space Activities" = "Chair of the Working Group on Long-Term Sustainability of Outer Space Activities",
"Comprehensive nuclear Test-Ban Treaty Organization" = "Comprehensive Nuclear-Test-Ban Treaty Organization",
"Comprehensive nuclear-test-ban treaty organization" = "Comprehensive Nuclear-Test-Ban Treaty Organization",
"Comprehensive Nuclear-Test-Ban Treaty Organization" = "Comprehensive Nuclear-Test-Ban Treaty Organization",
"Comprehensive Test ban Treaty Organisation" = "Comprehensive Nuclear-Test-Ban Treaty Organization",
"Control Arms Coalition" = "Control Arms Coalition",
"Control Arms Coalition, joint statement on International arms trade" = "Control Arms Coalition",
"De-Alerting Group, delivered by New Zealand" = "De-Alerting Group",
"High Representative for Disarmament Affairs" = "High Representative for Disarmament Affairs",
"High Representative for Disarmament Affairs (UNODA)" = "High Representative for Disarmament Affairs",
"High Representative for Disarmament Affairs, Ms. Izumi Nakamitsu" = "High Representative for Disarmament Affairs",
"Informal Group of Observer States at the Conference on Disarmament" = "Informal Group of Observer States to the Conference on Disarmament",
"Informal Group of Observer States to the CD" = "Informal Group of Observer States to the Conference on Disarmament",
"Syrian Arabic Republic" = "Syrian Arab Republic",
"Syria" = "Syrian Arab Republic",
"Campaign to Stop Killer Robots" = "Campaign to Stop Killer Robots",
"Campaign to Stop Killer Robots, joint statement on race and intersectionality in humanitarian disarmament" = "Campaign to Stop Killer Robots",
"Central American Integration System" = "Central American Integration System (SICA)",
"Central American Integration System (SICA)" = "Central American Integration System (SICA)",
"Centre for Central American Integration System" = "Central American Integration System (SICA)",
"Lao People's Democratic Republic" = "Lao PDR",
"Lao People's Democratic Republic" = "Lao PDR",
"Lao People\xd5s Democratic Republic" = "Lao PDR",
"President of the 69th General Assembly" = "President of the General Assembly",
"President of General Assembly" = "President of the General Assembly",
"United Arab Emirat" = "United Arab Emirates",
"Slovenia on behalf of..." = "Slovenia",
"US" = "United States",
"P5" ="P5 countries",
"Russian Federation" = "Russia",
"UNASUR" = "Union of the South American Nations (UNASUR)",                                                                                                                                                                                                                             
"Union of South American Nations" = "Union of the South American Nations (UNASUR)",
"Union of the South American Nations" = "Union of the South American Nations (UNASUR)",
"Union of South American Nations (UNASUR)"= "Union of the South American Nations (UNASUR)",
"The Arab Group" = "Arab Group",
"The Caribbean Community" = "Caribbean Community (CARICOM)",
"The Collective Security Treaty Organization (CSTO)" = "Collective Security Treaty Organization (CSTO)",
"The International Committee of the Red Cross (ICRC)" = "International Committee of the Red Cross (ICRC)",
"The presidents of the disarmament conventions in Geneva, delivered by France" = "Presidents of the disarmament conventions in Geneva",
"United Kingdom, on behalf of a group of states" = "United Kingdom",
"United Kingdom, \"Strategic Defence & Security Review\"" = "United Kingdom",
"United Nations Institute for Disarmament Research (UNIDIR)" = "UN Institute for Disarmament Research (UNIDIR)"
)

  # Recode the orator variable and create a new column orator_std
  pdf_merged_en$orator_std <- recode_vector[pdf_merged_en$orator]
  
  # Replace NA with original values (for orators not in the recode_vector)
  pdf_merged_en$orator_std[is.na(pdf_merged_en$orator_std)] <- pdf_merged_en$orator[is.na(pdf_merged_en$orator_std)]
  
  
  ### English------------------------

  pdf_merged <- read.csv("/Users/diego.lopes/Library/CloudStorage/OneDrive-Personal/R/pdf_merged_backup.csv")
  
  # identify the language of the texts
  pdf_merged$language <- sapply(pdf_merged$speeches, function(row) {
    detection <- cld2::detect_language(row)
    return(detection[1])
  })
  
  pdf_merged_en <- subset(pdf_merged, language == "en")
  
  
# chatgpt to clean the text-----------

  ## 16k model
  
  api_key <- ""
  
  clean_text <- function(text, api_key) {
    endpoint <- "https://api.openai.com/v1/chat/completions"
    
    headers <- httr::add_headers(
      `Content-Type` = "application/json",
      Authorization = paste0("Bearer ", api_key)
    )
    
    role_system <- "Remove any non-standard characters, and correct punctuation. Keep the output in the original language. This is a speech at the United Nations."
    role_user <- text
    
    # Define the model to use
    model <- "gpt-3.5-turbo-16k"
    
    messages <- list(list(role="system", content=role_system),
                     list(role="user", content=role_user))
    
    data <- list(
      model = model,
      messages = messages,
      max_tokens = 10000,
      temperature = 0.1
    )
    
    response <- httr::POST(url = endpoint, body = jsonlite::toJSON(data, auto_unbox = TRUE), config = headers)
    
    print(paste("HTTP status:", status_code(response)))
    print(paste("Content type:", http_type(response)))
    
    if (http_type(response) == "application/json") {
      print("Content of the response:")
      print(content(response))
      
      cleaned_text <- content(response)$choices[[1]]$message$content
    } else {
      stop("Request failed with status:", status_code(response))
    }
    
    return(cleaned_text)
  }
  
  cleaned_text <- clean_text(pdf_merged$speeches[34], api_key = api_key)
  
  print(cleaned_text)
  
  
  
  ## 16k text recognition
  
  api_key <- ""
  
  clean_text <- function(text, api_key) {
    endpoint <- "https://api.openai.com/v1/chat/completions"
    
    headers <- httr::add_headers(
      `Content-Type` = "application/json",
      Authorization = paste0("Bearer ", api_key)
    )
    
    role_system <- "Does this speech mention military expenditure explicitly?"
    role_user <- text
    
    # Define the model to use
    model <- "gpt-3.5-turbo-16k"
    
    messages <- list(list(role="system", content=role_system),
                     list(role="user", content=role_user))
    
    data <- list(
      model = model,
      messages = messages,
      max_tokens = 2048,
      temperature = 0.5
    )
    
    response <- httr::POST(url = endpoint, body = jsonlite::toJSON(data, auto_unbox = TRUE), config = headers)
    
    print(paste("HTTP status:", status_code(response)))
    print(paste("Content type:", http_type(response)))
    
    if (http_type(response) == "application/json") {
      print("Content of the response:")
      print(content(response))
      
      cleaned_text <- content(response)$choices[[1]]$message$content
    } else {
      stop("Request failed with status:", status_code(response))
    }
    
    return(cleaned_text)
  }
  
  cleaned_text <- clean_text(pdf_merged$speeches[1505], api_key = api_key)
  
  print(cleaned_text)
  
  
  
  ## gpt-4
  
  api_key <- ""
  
  clean_text <- function(text, api_key) {
    endpoint <- "https://api.openai.com/v1/chat/completions"
    
    headers <- httr::add_headers(
      `Content-Type` = "application/json",
      Authorization = paste0("Bearer ", api_key)
    )
    
    role_system <- "Remove any non-standard characters, and correct punctuation. Keep th eoutput in the original language. This is a speech at the United Nations."
    role_user <- text
    
    # Define the model to use
    model <- "gpt-4"
    
    messages <- list(list(role="system", content=role_system),
                     list(role="user", content=role_user))
    
    data <- list(
      model = model,
      messages = messages,
      max_tokens = 6800,
      temperature = 0.1
    )
    
    response <- httr::POST(url = endpoint, body = jsonlite::toJSON(data, auto_unbox = TRUE), config = headers)
    
    print(paste("HTTP status:", status_code(response)))
    print(paste("Content type:", http_type(response)))
    
    if (http_type(response) == "application/json") {
      print("Content of the response:")
      print(content(response))
      
      cleaned_text <- content(response)$choices[[1]]$message$content
    } else {
      stop("Request failed with status:", status_code(response))
    }
    
    return(cleaned_text)
  }
  
  cleaned_text <- clean_text(pdf_merged$speeches[16], api_key = api_key)
  
  print(cleaned_text)
  

# ANALYSIS----------------------------------------------------------------------
# Tokenize the text-------------------------------------------------------------
  
  afinn <- get_sentiments("afinn")
  
  ## lexicon
  ## Create a tidy version of the "speeches" column
  speeches_tidy <- pdf_merged_en %>%
    select(id, speeches) %>%
    unnest_tokens(word, speeches) %>%
    mutate(word = str_remove_all(word, "[^[:alnum:]']")) %>%
    filter(!word %in% stopwords())
  

  ## Compute the sentiment score for each text
  sentiment_scores <- speeches_tidy %>%
    inner_join(afinn, by = "word") %>%
    group_by(id) %>%
    summarise(sentiment = sum(value))
  
  ## Inner join the sentiment summary with the original dataset
  pdf_merged_en <- pdf_merged_en %>%
    left_join(sentiment_scores, by = "id")
 
  
  # create the plot for all years 
  ggplot(pdf_merged_en, aes(x = id, y = sentiment)) +
    geom_col(fill = "lightblue") +
    labs(x = "ID", y = "Total Sentiment Score", title = "Sentiment Scores by ID")+
    theme_classic()
  
  # Extract the year from the date column
  #### there are two observations without the year
  
  pdf_merged_en$date[1189] <- ymd("2014-10-20")
  pdf_merged_en$date[2226] <- ymd("2021-10-20")
  
  pdf_merged_en$date <- lubridate::ymd(pdf_merged_en$date)
  pdf_merged_en$year <- lubridate::year(pdf_merged_en$date)


  # Calculate mean sentiment by year
  mean_sentiment_by_year <- pdf_merged_en %>%
    group_by(year) %>%
    summarise(mean_sentiment = mean(sentiment))
  
  # Plot sentiments foo all years
  ggplot(pdf_merged_en, aes(x = id, y = sentiment)) +
    geom_col(aes(fill = as.factor(year))) +
    # Add a horizontal line at the mean sentiment for each year
    geom_hline(data = mean_sentiment_by_year, aes(yintercept = mean_sentiment), 
               color = "red", linetype = "dashed", size = .5) +
    # Facet the plot by year, with independent x-axes
    facet_wrap(~year, scales = "free_x") +
    labs(x = "ID", y = "Total Sentiment Score", title = "Sentiment Scores by ID") +
    theme_classic() +
    scale_fill_discrete(name = "Year")
  
  
  ggplot(mean_sentiment_by_year, aes(x=year, y=mean_sentiment))+
    geom_line()+
    theme_classic()

  
  # Most common positive and negative words
  bing_word_counts <- speeches_tidy %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) %>%
    ungroup()
  
  # bar chart of words contributing to sentiment
  bing_word_counts %>%
    group_by(sentiment) %>%
    slice_max(n, n = 10) %>% 
    ungroup() %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(n, word, fill = sentiment)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~sentiment, scales = "free_y") +
    labs(x = "Contribution to sentiment",
         y = NULL)+
    theme_classic()
  

  # word cloud
  speeches_tidy %>%
    anti_join(stop_words) %>%
    count(word) %>%
    with(wordcloud(word, n, max.words = 100)) # I really need to cut non-English speeches
  
  # count military spending mentions----------------
  
  # Create a new column with the counts for each search pattern

  # Define the terms you want to count
  terms <- c("military spending", "defense spending", "military expenditure", "defense expenditure", "military expenditures", "defense expenditures")
  
  # Initialize direct_mentions column to 0
  pdf_merged_en$direct_mentions <- 0
  
  # Loop through the terms
  for (term in terms) {
    # Count occurrences of the term in each speech
    term_count <- str_count(str_to_lower(pdf_merged_en$speeches), str_to_lower(term))
    
    # Add the counts to direct_mentions column
    pdf_merged_en$direct_mentions <- pdf_merged_en$direct_mentions + term_count
  }
  

  # Define a regular expression pattern for indirect mentions of military spending
  indirect <- "(spend|allocate|invest|spent|allocated|invested|spending|allocating|investing).*(billions|trillions|resources).* (military|defense|arms|weaponry|armament|weapons|arsenals)"
  
  indirect_mentions <- grepl(indirect, pdf_merged_en$speeches, ignore.case = TRUE)
  
  pdf_merged_en <- pdf_merged_en %>%
    mutate(indirect_mentions = str_count(speeches, regex(indirect, ignore_case = TRUE)))
  
  #creating a combined columns of all mentions (direct and indirect) to military spending
  
  pdf_merged_en <- pdf_merged_en %>%
    mutate(mentions_milex = direct_mentions + indirect_mentions) #this picked up Zimbabwe, which RCW didn't
  
  
  
  # exploring the data ------------
  
  sum(pdf_merged_en$direct_mentions)
  
  pdf_merged_en <- pdf_merged_en %>%
    select(-speeches, everything(), speeches)
  
  avg_mentions_per_year <- pdf_merged_en %>%
    group_by(year) %>%
    summarise(avg_mentions = sum(direct_mentions) / n_distinct(id))
  
  ggplot(avg_mentions_per_year, aes(x = year, y = avg_mentions)) +
    geom_line() +
    labs(x = "Year", y = "Average Mentions of Military Spending per Speech", 
         title = "Trend of Military Spending Mentions Over Time")
  
  pdf_merged_en %>%
    group_by(orator_std) %>%
    summarise(total_mentions = sum(direct_mentions)) %>%
    arrange(desc(total_mentions))

  pdf_merged_en %>%
    group_by(year) %>%
    summarise(total_mentions = sum(direct_mentions))
  
  ### stacked bar of mentions over time for top 15
  # Get the top 15 orators
  top_orators <- pdf_merged_en %>%
    group_by(orator_std) %>%
    summarise(total_mentions = sum(direct_mentions), .groups = "drop") %>%
    arrange(desc(total_mentions)) %>%
    slice_head(n = 15) %>%
    pull(orator_std)
  
  # Filter the original data for the top orators and create the plot
  pdf_merged_en %>%
    filter(orator_std %in% top_orators) %>%
    group_by(year, orator_std) %>%
    summarise(total_mentions = sum(direct_mentions), .groups = "drop") %>%
    ggplot(aes(x = year, y = total_mentions, fill = orator_std)) +
    geom_bar(stat = "identity") +
    labs(x = "Year", y = "Total Mentions",
         title = "Total Mentions by Top 15 Orators Over Time") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    scale_x_continuous(breaks = seq(min(pdf_merged_en$year), max(pdf_merged_en$year), by = 1))
  
  
  ### total mentions of top 15
  pdf_merged_en %>%
    group_by(orator_std) %>%
    summarise(total_mentions = sum(direct_mentions)) %>%
    arrange(desc(total_mentions)) %>%
    slice_max(order_by = total_mentions, n = 15) %>%
    ggplot(aes(x = reorder(orator_std, total_mentions), y = total_mentions)) +
    geom_bar(stat = "identity", fill = "#1c9099") +
    labs(x = "Total Mentions", y = "Orator",
         title = "Total Mentions by Top 15 Orators") +
    theme_classic() +
    coord_flip()
  
  
  ### Germany lost interest
  pdf_merged_en %>%
    filter(orator_std %in% top_orators, orator_std=="Germany") %>%
    group_by(year, orator) %>%
    summarise(total_mentions = sum(direct_mentions), .groups = "drop") %>%
    ggplot(aes(x = year, y = total_mentions, fill = orator)) +
    geom_bar(stat = "identity") +
    labs(x = "Year", y = "Total Mentions",
         title = "Total Mentions by Top 15 Orators Over Time") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_continuous(breaks = seq(min(pdf_merged_en$year), max(pdf_merged_en$year), by = 1))
  
  
  ### Pakistan insisted
  pdf_merged_en %>%
    filter(orator_std %in% top_orators, orator_std=="Pakistan") %>%
    group_by(year, orator) %>%
    summarise(total_mentions = sum(direct_mentions), .groups = "drop") %>%
    ggplot(aes(x = year, y = total_mentions, fill = orator)) +
    geom_bar(stat = "identity") +
    labs(x = "Year", y = "Total Mentions",
         title = "Total Mentions by Top 15 Orators Over Time") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_continuous(breaks = seq(min(pdf_merged_en$year), max(pdf_merged_en$year), by = 1))
  


  ### calculate the relative frequency of the terms in pdf_merged_en$speeches per pdf_merged_en$year by number of-------

  
  library(dplyr)
  library(stringr)
  
  # Define the terms
  terms <- c("military spending", "defense spending", "military expenditure", "defense expenditure", "military expenditures", "defense expenditures")
  
  # Create an empty list to store the data frames
  relative_frequency_data_list <- list()
  
  # Loop through the terms
  for (i in seq_along(terms)) {
    term <- terms[i]
    
    # Count occurrences of the term in each speech
    pdf_merged_en <- pdf_merged_en %>%
      mutate(term_count = str_count(str_to_lower(speeches), str_to_lower(term)))
    
    # Sum counts per year and calculate total number of words per year
    relative_frequency_data <- pdf_merged_en %>%
      group_by(year) %>%
      summarise(
        total_mentions = sum(term_count),
        total_words = sum(str_count(speeches, "\\w+"))
      ) %>%
      mutate(relative_frequency = total_mentions / total_words) %>%
      # Add a column for the term
      mutate(term = term)
    
    # Add the data frame to the list
    relative_frequency_data_list[[i]] <- relative_frequency_data
  }
  
  # Combine the data frames
  combined_relative_frequency_data <- bind_rows(relative_frequency_data_list)
  
  sum(combined_relative_frequency_data$total_mentions)
  
  
  ggplot(combined_relative_frequency_data, aes(x = year, y = relative_frequency)) +
    geom_point(size= 3) +
    geom_smooth(method = "loess", formula = y ~ x) +
    labs(x = "Year", y = "Relative Frequency", title = "Relative frequency of mentions to military spending")+
    theme_minimal()
  
  
  
  ### relative frequency to the number of speeches
  # Define the terms
  terms <- c("military spending", "defense spending", "military expenditure", "defense expenditure", "military expenditures", "defense expenditures")
  
  # Create an empty list to store the data frames
  relative_frequency_data_list <- list()
  
  # Loop through the terms
  for (i in seq_along(terms)) {
    term <- terms[i]
    
    # Count occurrences of the term in each speech
    pdf_merged_en <- pdf_merged_en %>%
      mutate(term_count = str_count(str_to_lower(speeches), str_to_lower(term)))
    
    # Sum counts per year and calculate total number of speeches per year
    relative_frequency_data <- pdf_merged_en %>%
      group_by(year) %>%
      summarise(
        total_mentions = sum(term_count),
        total_speeches = n()
      ) %>%
      mutate(relative_frequency = total_mentions / total_speeches) %>%
      # Add a column for the term
      mutate(term = term)
    
    # Add the data frame to the list
    relative_frequency_data_list[[i]] <- relative_frequency_data
  }
  
 # Streamgraph-----------------------------------------------------------------
  
  # Library
  library(devtools)
  library(streamgraph)
  library(htmlt)
  remotes::install_github("hrbrmstr/streamgraph")
  library(streamgraph)
  install.packages("streamgraphR")
  
  
  # Create data:
  data <- data.frame(
    year=rep(seq(1990,2016) , each=10),
    name=rep(letters[1:10] , 27),
    value=sample( seq(0,1,0.0001) , 270)
  )
  
  # Stream graph with a legend
  pp <- streamgraph(data, key="name", value="value", date="year", height="300px", width="1000px") %>%
    sg_legend(show=TRUE, label="names: ")
  
  pp
  
  library(ggstream)
  # install.packages("ggplot2")
  library(ggplot2)
  
  cols <- c("#FFB400", "#FFC740", "#C20008", "#FF020D", "#13AFEF")
  
  ggplot(data, aes(x = year, y = value, fill = name))+
    geom_stream(extra_span = 0.2) +
    geom_stream(extra_span = 0.2, true_range = "none",
                alpha = 0.3) +
     theme_minimal()
  
  
  
  remotes::install_github("EmilHvitfeldt/paletteer")
  library(paletteer)
  
  ggplot(data, aes(x = year, y = value, fill = name)) +
    geom_stream(extra_span = 0.2) +
    geom_stream(extra_span = 0.2, true_range = "none", alpha = 0.3) +
    scale_fill_manual(values = paletteer_c("grDevices::Viridis", 30)) +
    theme_minimal()
  
  
  # Generate a vector of unique colors based on the number of unique names
  unique_names <- unique(data$name)
  num_colors <- length(unique_names)
  color_palette <- paletteer_c("grDevices::Viridis", num_colors)
  
  # Create the ggplot graph with unique colors for each name
  ggplot(data, aes(x = year, y = value, fill = name)) +
    geom_stream(extra_span = 0.2) +
    geom_stream(extra_span = 0.2, true_range = "none", alpha = 0.3, color = 1, lwd = 0.25) +
    scale_fill_manual(values = setNames(color_palette, unique_names)) +
    theme_minimal()
  
  
  # restructure dataset into panel
  
  pdf_merged_en_agg <- pdf_merged_en %>%
    group_by(orator_std, year) %>%
    summarise(
      id = first(id),
      direct_mentions = sum(direct_mentions, na.rm = TRUE),
      .groups = "drop"
    )
  
  pdf_merged_en_complete <- pdf_merged_en_agg %>%
    complete(orator_std, year = 2007:2022, fill = list(id = NA, direct_mentions = 0))

  
  # Calculate total direct_mentions for each orator_std
  top_orators <- pdf_merged_en_complete %>%
    group_by(orator_std) %>%
    summarise(total_mentions = sum(direct_mentions, na.rm = TRUE)) %>%
    arrange(desc(total_mentions)) %>%
    head(15) %>%
    pull(orator_std)
  
  # Filter the original data to include only the top 15 orators
  pdf_merged_en_top<- pdf_merged_en_complete %>%
    filter(orator_std %in% top_orators)
  
  # Create a color palette
  num_colors <- length(unique(pdf_merged_en_top$orator_std))
  library(viridis)
  color_palette <- viridis(num_colors)
  
  # Create the stream graph with extra range
  ggplot(pdf_merged_en_top, aes(x = year, y = direct_mentions, fill = orator_std)) +
    geom_stream(extra_span = 0.2) +
    geom_stream(extra_span = 0.2, true_range = "none", alpha = 0.3, color = 1, lwd = 0.25) +
    scale_fill_manual(values = color_palette) +
    theme_minimal()
  
  # Create the stream graph
  ggplot(pdf_merged_en_top, aes(x = year, y = direct_mentions, fill = orator_std)) +
    geom_stream() +
    geom_stream(true_range = "none", alpha = 0.3, color = 1, lwd = 0.25) +
    scale_fill_manual(values = color_palette) +
    theme_minimal()
  
  
  
  
  
  
    