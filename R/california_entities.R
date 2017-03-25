generate_url <-
  function(search_name = "Marble Bridge",
           entity_type = "Corporation") {
    wrong_entity <-
      !entity_type %>% str_to_lower() %in% c("corporation", 'llc', 'llp')

    if (wrong_entity) {
      stop("Entity type can only be corporation, llc, or llp")
    }
    df_slugs <-
      data_frame(
        typeEntity = c("corporation", 'llc', 'llp'),
        slugEntity = c('CORP', 'LPLLC', 'LPLLC')
      )

    slug <-
      df_slugs %>%
      filter(typeEntity == entity_type %>% str_to_lower()) %>%
      .$slugEntity

    url <-
      list(
        "https://businesssearch.sos.ca.gov/CBS/SearchResults?SearchType=",
        slug,
        '&SearchCriteria=',
        search_name %>% URLencode(),
        '&SearchSubType=Keyword'
      ) %>%
      purrr::reduce(paste0)

    return(url)

  }


# parse -------------------------------------------------------------------
parse_details <- function(entity_id = "02059138") {
  page <-
    httr::VERB(
      verb = "POST",
      url = "https://businesssearch.sos.ca.gov/CBS/Detail",
      httr::add_headers(
        origin = "https://businesssearch.sos.ca.gov",
        `accept-encoding` = "gzip, deflate, br",
        `accept-language` = "en-US,en;q=0.8",
        cookie = "__RequestVerificationToken=RrXWw5aNQRKcHajs6YKfx8BKtwtFwovNZa36nWE5GNjRZDma0lebI6S4DJ7hwsYqPKsHPBqnXWbNqVbvhGVzgRAQpVM1; visid_incap_1000181=8zareeObSx2BPV8/b+pw6X+M1VgAAAAAQUIPAAAAAABPgjcC6gGzby5QfneeQPBW; nlbi_1000181=9FWfRaTuagisd3jp15tZWQAAAACA2Im5YDLptJIvMWUT5J9v; incap_ses_221_1000181=dvjyROrvmVSTLvs6RCgRA3+M1VgAAAAAiF2CWpa54cJx0g1sNjDs3g==",
        pragma = "no-cache",
        `upgrade-insecure-requests` = "1",
        `user-agent` = "Bromp",
        accept = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
        `cache-control` = "no-cache",
        authority = "businesssearch.sos.ca.gov",
        referer = "https://businesssearch.sos.ca.gov/CBS/SearchResults?SearchType=CORP&SearchCriteria=Marble+Bridge&SearchSubType=Keyword",
        dnt = "1"
      ),
      body = list(
        `__RequestVerificationToken` = "5wQbvl6qyydVHFa4SO5b1OxMuGyce8ih2iuRtxVfXIpfUGzhyFAliOFZRkAKqHVOPEXj-kYs_eJai-ePvEZdXzH_IYU1",
        SearchType = "fuck",
        SearchCriteria = "Fuck+Off",
        SearchSubType = "Keyword",
        enitityTable_length = "1000",
        EntityId = entity_id
      ),
      encode = "form"
    ) %>%
    httr::content(as = "parsed")

  values <-
    page %>%
    html_nodes('.col-sm-8') %>%
    html_text() %>%
    str_trim() %>%
    str_replace_all(
      "Agent Address\r\n|Entity Address\r\n|Entity Mailing Address\r\n|Agent City, State, Zip ",
      ''
    ) %>%
    str_replace_all("\r\n|Entity City, State, Zip", '') %>%
    str_replace_all("Entity Mailing City, State, Zip", '') %>%
    str_replace_all("Agent City, State, Zip", '') %>%
    str_replace("                ", ' ') %>%
    str_replace_all('                 ', ' ') %>%
    str_replace_all("                ", ' ') %>%
    str_trim()

  items <-
    c(
      'dateRegistration',
      'jurisdictionEntity',
      'typeEntity',
      'statusEntity',
      'addressServiceAgent',
      'addressEntity',
      'addressEntityMailing'
    )

  item <- items[1:length(values)]

  df_details <-
    data_frame(item, value = values) %>%
    spread(item, value) %>%
    dplyr::select(one_of(item)) %>%
    mutate(idEntity = entity_id) %>%
    dplyr::select(idEntity, everything())

  df_details <-
    df_details %>%
    mutate_at(df_details %>% dplyr::select(matches("date")) %>% names(),
              funs(. %>% lubridate::mdy()))
  has_si <-
    page %>% html_nodes('td:nth-child(1)') %>% html_text() %>% length() > 0

  if (has_si) {
    si_text <-
      page %>%
      html_nodes('td:nth-child(1)') %>%
      html_text() %>%
      str_trim()

    date_si <-
      page %>%
      html_nodes('td:nth-child(2)') %>%
      html_text() %>%
      str_trim() %>%
      lubridate::mdy()

    id_pdf <-
      page %>%
      html_nodes('td:nth-child(3) label') %>%
      html_attr('for') %>%
      str_trim() %>%
      str_replace_all('btnView-', '')

    df_items <-
      data_frame(idSI = si_text,
                 dateSI = date_si,
                 idPDF = id_pdf) %>%
      arrange((dateSI)) %>%
      mutate(idItem = 1:length(si_text) - 1,
             dateSI = dateSI %>% as.character()) %>%
      gather(item, value, -idItem) %>%
      mutate(item = ifelse(idItem == 0, item, paste(item, idItem, sep = ''))) %>%
      dplyr::select(-idItem)

    col_order <- df_items$item
    df_items <-
      df_items %>%
      spread(item, value) %>%
      dplyr::select(one_of(col_order)) %>%
      mutate(idEntity = entity_id) %>%
      dplyr::select(idEntity, everything())

    df_items <-
      df_items %>%
      mutate_at(df_items %>% dplyr::select(matches("date")) %>% names(),
                funs(. %>% lubridate::ymd()))

    df_details <-
      df_details %>%
      left_join(df_items) %>%
      suppressMessages()

  }

  return(df_details)
}


parse_ca_search_table <-
  function(url = "https://businesssearch.sos.ca.gov/CBS/SearchResults?SearchType=CORP&SearchCriteria=Marble+Bridge&SearchSubType=Keyword") {
    page <-
      url %>%
      read_html()

    has_table <-
      page %>%
      html_table() %>%
      length() == 1

    if (has_table) {
      entities <-
        page %>%
        html_nodes('.EntityLink') %>%
        html_text() %>%
        str_trim()
      df <-
        page %>%
        html_table() %>%
        flatten_df() %>%
        purrr::set_names(
          c(
            'idEntityCA',
            'dateRegistration',
            'statusEntity',
            'nameEntity',
            'jurisdictionEntity',
            'nameAgentService'
          )
        ) %>%
        dplyr::select(-nameEntity) %>%
        mutate(
          nameEntity = entities,
          idEntity = idEntityCA %>% substr(2, nchar(idEntityCA)),
          idEntity = str_c("0", idEntity),
          dateRegistration = dateRegistration %>% lubridate::mdy(),
          urlEntitySearchCA = url
        ) %>%
        dplyr::select(idEntity, nameEntity, everything())
      return(df)
    }
  }


get_data_entity_california <-
  function(search_name = c("Marble Bridge"),
           entity_type = "corporation",
           return_message = TRUE) {
    url <-
      generate_url(search_name = search_name, entity_type = entity_type)

    df_search <-
      parse_ca_search_table(url = url) %>%
      mutate(nameSearch = search_name, typeEntitySearch = entity_type) %>%
      dplyr::select(nameSearch, typeEntitySearch, everything())

    parse_details_safe <-
      purrr::possibly(parse_details, data_frame())
    df_details <-
      df_search$idEntity %>%
      map_df(function(x) {
        parse_details_safe(entity_id = x)
      })

    if (df_details %>% nrow() > 0) {
      df_search <-
        df_search %>%
        left_join(df_details) %>%
        suppressMessages()
    }

    df_search <-
      df_search %>%
      mutate(idEntityCA = idEntityCA %>% as.character())

    if (return_message) {
      list(
        "Found ",
        df_search %>% nrow() %>% formattable::comma(digits = 0),
        " California entities matching the term ",
        search_name,
        ' registered as a ',
        entity_type
      ) %>%
        purrr::reduce(paste0) %>%
        message()
    }
    return(df_search)
  }


#' California Entity Search
#'
#' @param search_names
#' @param entity_types
#' @param return_message
#' @import httr curl tidyr rvest dplyr stringr lubridate xml2 purrr stringi readr formattable
#' @references \href{https://businesssearch.sos.ca.gov}{California Department of State}
#'
#' @return a \code{data_frame}

#' @export
#'
#' @examples
#' \dontrun{
#' get_data_entities_california(parse_bios = TRUE, tidy_columns = TRUE,
#' return_message = TRUE)
#' }

get_data_entities_california <-
  function(search_names = c("Marble Bridge"),
           entity_types = "corporation",
           return_message = TRUE) {
    df_term_matrix <-
      expand.grid(
        nameSearch = search_names,
        entityType = entity_types,
        stringsAsFactors = F
      ) %>%
      dplyr::as_data_frame()

    get_data_entity_california_safe <-
      purrr::possibly(get_data_entity_california, data_frame())

    all_data <-
      1:nrow(df_term_matrix) %>%
      map_df(function(x) {
        get_data_entity_california_safe(
          search_name = df_term_matrix$nameSearch[[x]],
          entity_type = df_term_matrix$entityType[[x]],
          return_message = return_message
        )
      })

    return(all_data)
  }
