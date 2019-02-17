generate_url <-
  function(search_name = "Marble Bridge",
           entity_type = "Corporation") {
    wrong_entity <-
      !entity_type %>% str_to_lower() %in% c("corporation", 'llc', 'llp')

    if (wrong_entity) {
      stop("Entity type can only be corporation, llc, or llp")
    }
    df_slugs <-
      tibble(
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

  item <- items[seq_along(values)]

  df_details <-
    tibble(item, value = values) %>%
    spread(item, value) %>%
    dplyr::select(one_of(item)) %>%
    mutate(idEntity = entity_id) %>%
    dplyr::select(idEntity, everything())

  df_details <-
    df_details %>%
    mutate_at(df_details %>% dplyr::select(dplyr::matches("date")) %>% names(),
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
      tibble(idSI = si_text,
                 dateSI = date_si,
                 idPDF = id_pdf) %>%
      arrange((dateSI)) %>%
      mutate(idItem = seq_along(si_text) - 1,
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
      mutate_at(df_items %>% dplyr::select(dplyr::matches("date")) %>% names(),
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
        dplyr::select(-nameEntity)

      df <-
        df %>%
        mutate(
          nameEntity = entities,
          idEntity =
            ifelse(
            idEntityCA %>% substr(1, 1) == "C",
            idEntityCA %>% substr(2, nchar(idEntityCA)) %>% str_c("0", idEntityCA),
            idEntityCA %>% as.character()
          ),
          dateRegistration = dateRegistration %>% lubridate::mdy(),
          urlEntitySearchCA = url
        ) %>%
        dplyr::select(idEntity, nameEntity, everything())
      return(df)
    }
  }


entity_california <-
  function(search_name = c("Marble Bridge"),
           entity_type = "corporation",
           return_message = TRUE) {
    url <-
      generate_url(search_name = search_name, entity_type = entity_type)

    df_search <-
      parse_ca_search_table(url = url) %>%
      mutate(nameSearch = search_name, typeEntitySearch = entity_type,
             nameEntity = nameEntity %>% str_trim()) %>%
      dplyr::select(nameSearch, typeEntitySearch, everything())

    parse_details_safe <-
      purrr::possibly(parse_details, tibble())
    df_details <-
      df_search$idEntity %>%
      future_map_dfr(function(x) {
        parse_details_safe(entity_id = x)
      }) %>%
      suppressWarnings()

    if (df_details %>% nrow() > 0) {
      df_search <-
        df_search %>%
        left_join(
          df_details %>% mutate(
            addressEntityMailing = ifelse(
              addressEntityMailing == '\\*',
              addressEntity,
              addressEntityMailing
            )
          )
        ) %>%
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
        asbtools::cat_message()
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
#' @return a \code{tibble}

#' @export
#'
#' @examples
#' \dontrun{
#' california_entities(parse_bios = TRUE, tidy_columns = TRUE,
#' return_message = TRUE)
#' }

california_entities <-
  function(search_names = c("Marble Bridge"),
           entity_types = "corporation",
           return_message = TRUE) {
    df_term_matrix <-
      expand.grid(
        nameSearch = search_names,
        entityType = entity_types,
        stringsAsFactors = F
      ) %>%
      dplyr::as_tibble()

    entity_california_safe <-
      purrr::possibly(entity_california, tibble())

    all_data <-
      1:nrow(df_term_matrix) %>%
      future_map_dfr(function(x) {
        entity_california_safe(
          search_name = df_term_matrix$nameSearch[[x]],
          entity_type = df_term_matrix$entityType[[x]],
          return_message = return_message
        )
      })

    return(all_data)
  }


# lenders -----------------------------------------------------------------

# utility -----------------------------------------------------------------
generate_url_reference <-
  function() {
    user_agents <-
      c(
        "Mozilla/5.0 (Linux; U; en-US) AppleWebKit/528.5+ (KHTML, like Gecko, Safari/528.5+) Version/4.0 Kindle/3.0 (screen 600x800; rotate)",
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/42.0.2311.135 Safari/537.36 Edge/12.246",
        "Mozilla/5.0 (X11; CrOS x86_64 8172.45.0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.64 Safari/537.36",
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_2) AppleWebKit/601.3.9 (KHTML, like Gecko) Version/9.0.2 Safari/601.3.9",
        "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36"
      )


    user_agent <-
      user_agents[!user_agents %>% str_detect("bot|slurp")] %>%
      sample(1)

    tl_domain <-
      c('.com', '.gov', '.org', '.mil', '.co') %>%
      sample(1)

    word_length <-
      8:15

    words <-
      word_length %>% sample(1)

    domain_slug <-
      1:words %>%
      map_chr(function(x) {
        sample(letters, 1)
      }) %>%
      paste0(collapse = '')

    url <-
      list('http://', domain_slug, tl_domain) %>%
      purrr::reduce(paste0)
    df <-
      tibble(urlReferer = url,
                 userAgent = user_agent)
    return(df)
  }

system_curl <-
  function(curl_text) {
    page <-
      system(curl_text, intern = TRUE, ignore.stderr = TRUE) %>%
      suppressMessages()

    page <-
      page %>%
      paste0(collapse = '') %>%
      xml2::read_html()
    return(page)
  }


# search ------------------------------------------------------------------
generate_curl_text <-
  function(search_name = "Marble Bridge") {
    search_slug <-
      search_name %>% URLencode()

    df_rf <-
      generate_url_reference()

    curl_text <-
      list(
        "curl 'https://docqnet.dbo.ca.gov/licensesearch/' -H 'Pragma: no-cache' -H 'Origin: ",
        df_rf$urlReferer,
        "' -H 'Accept-Encoding: gzip, deflate, br' -H 'Accept-Language: en-US,en;q=0.8' -H 'Upgrade-Insecure-Requests: 1' -H 'User-Agent:",
        df_rf$userAgent,
        "' -H 'Content-Type: application/x-www-form-urlencoded' -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8' -H 'Cache-Control: no-cache' -H 'Referer: https://docqnet.dbo.ca.gov/licensesearch/' -H 'Cookie: anonprofile=r3IQgcIrTFtHqEqj3qSldNpLtc2NRPj6aDfkwm6Mos_K2szOEEAJ1JIZBZNBh1Szjvw3tm8aHbidmuaZKr4jL_fHW4cNqPSnhWo3JwSPAtIcgHsLej2LgY2pWrkHRiOn2JGj35UJeOM_3eQHXpGIL4YwKjh02jxOWPu1KubEeU32kiLP0; ASP.NET_SessionId=4ijieb3ptv2yduljx50mdr1z' -H 'Connection: keep-alive' -H 'DNT: 1' --data '__EVENTTARGET=ctl00%24MainContent%24gvSearchResults%24ctl02%24btnSelect&__EVENTARGUMENT=&__VIEWSTATE=BmYimfHO0PE7ePwZx6%2Feqjx1HubSY8MYhfeMV9k%2BIU9qWaNLF9LmaKeTjfg2flee9RFfD2f1kyQ%2BBxOLHAgKboo%2By5kH%2BtvTLGUWDzq4hV7Aa6CwwcX77Tdv5%2BKIQV9oxlFOG47iRvowqcQZaK%2BY1nouow7vJU6EbjrAo7aUg1Fdkmi22if5Eg13lm5T4WR7fUqfdK%2F%2FZ92h%2BYihPAZBezx8GCpPYLbNLk658TAnHCKARf8mbUOOo3zIeAw0p5O8JcOYWg%3D%3D&__VIEWSTATEGENERATOR=7E85E2D6&__VIEWSTATEENCRYPTED=&__EVENTVALIDATION=yjD0BrcKTE92cW61C5pdMlTtyz1X4Sr1b%2BC4TaKRLmi7saponup%2FZiyP%2Bt24KXXqTYbsGXQcCn%2BRZMBPyinAUwThEM7VD%2Bp1gbd2LvpnNga9bAOrNM7Vi%2BWvAGVLQSFNS5hGyKzXcWs%2FotYbXvWpgAFPs%2FOtCDwG%2FNQGBmVA4eDJzKIr1qQ0uSv%2B6N5MyuHpPRW%2BljyFl4HxJvZswkvo8%2FGWTSNK%2B2Udxu0oTyG9CVjRhR%2FPVkl%2BSHpX0%2BNR4Re%2BDguQEECwSXCpfS%2F2dQjbZI5UowLD2jcndN2P8ZaSJHll806Nux4ac6iCyqGKbe4bUE2XditAaa4lb4QLaRySC9xDfqRbcu0oZ%2BP6wahfCtkjMkX2cE6ldvs9fcv85pZOq1TnCBEu%2FGRDh2bRfKPg6J8TsIlSWG57rfNGNEdgqLn%2FuBrAKJBWD1SjYxCq9HckLiLXFN7xL8R40Lnmf4nCbJnn0RtOJWv%2BZG%2By5pTkfbvVM%2BxQFcWXuWUp3WX8QAn4CzskeoU9Ot60S52PXvjgyAqitApIZkLeK22CzzXf7dDqa6fB7H%2F2J%2B97HR4TVhDmnlC3CaTVcWiUFeRWI19L7GaOMLqtVG2dF5ihLADlr1WKAoRDqPqM%2BKdCDVexxekGOyjve6Ewqj9s%2Ftjn%2FHBmA5W%2FgZEppcn%2BZxp9xtA3Yp%2Bf7eOS9A%2B9N%2Fw9M4FPMPBi9xbl8ItSPqiBwXX8N98Nyl88K4dWFcOzlH2CGZ72ENZpLdmAp7siyKuf43nQVr%2FrNemJlbdqN%2Bw4Z0h2VC6FB71%2BDaCs%2FB96CWllHsL6pNDL7e93cZUNaxCMiNRqK3WKspI5wkik85tv3jWVg0IhnLdios7rLZ3dIKvugcUVCUhLu5Ud70byRdD6J47EM4t%2FssSUTMxE5N%2FR5n5RviFY6IEfYM%2FIfY1ZuVifa7MgpKv%2FGVlXpJNYdyfrbi1JbJ9xFa4Pj3IJ5ZCNMnEFlHS9V865AEAYS4a1cj32PqLIirzj2sWCjBJi%2FJSE64X43dwEvQ25Pi7nBIse7e34im%2BxlJo3CF4WvSEWgume2wfZu1Ig2zPONY2%2BzcdliyHk4Ptb%2BvSZcPishJVPjvi5yB%2FmLNBn06ciD65NVKNGDBiI6qNeA8q3ifbiNkhxttvvwOZzTgItxjtJPn7%2F06N3oE0Wa7nfURfuZyxVxHl58MThZLHwnlY5IH95GoiYyO2gmFjWHLp0nqu9%2Fxz39H956fGDG%2FgSrVOaF4D8rKpUO9uL%2F0f13pc1TpjxLpohkCXKMOvnxdqU7m3v7ZTfd2%2FvfiJbMJZoZs03Pst3Xj6w0MNwBSPrMC3R9DcIx4LVIgrdiM%2BexfZ9BZKA23n%2FCBFvMBtW7QLaME6SvZi%2F195a%2FEkpovpeTypm8cO5UEqCv9i%2FpSQj5V4yo98uGPXm%2F3iT1W%2Ff5wAXG2EWwrov1ntFFtyVE%2BqXSwdCIHyIbbsArkMI9vvJbaHVmOD7Y0YLfwpxKb9scUOgB8lwJLEtUz%2F8a4zjmAdWJMGf8IGu04nLKi01nuk0nXMxaKlXymbgEThzD18MdTf6w%2BokQ6VURbwNp0b83QscSdFaxdHBacN42PmCtVfrKFrGMboc37y53pTfiTb%2BPVOPUg7WraGWbSJl6BSMXVuUQXXpk5ks2nBZudZVegUsCg73ggoyaf8zaPBrfEVugcQNUuQkQLi5y7ijA62YaV23xcKxaCFhQr1chQRgpimrmDJMLl6wHoGQJWlUe1yCBYnx1VDzAhvGoqY0Rf4exADXdL0OJpKnSyBtGqke75we%2Fjqqk9Srb5TTKWoaT4WYB7WM0tDgb%2FX%2BNXsEL4ylLYguM4bVBWwx1T%2FbLzScwbF%2BQd0Q0tMrwb0CdLyooI2UEisFdxrS3GQiU28YJW9et86ERRCK&ctl00%24MainContent%24hdnSearchSet=1&ctl00%24MainContent%24ddlSearchCriteria=Contains&ctl00%24MainContent%24txtLegalName=",
        search_slug,
        "&ctl00%24MainContent%24txtLicenseNumber=&ctl00%24MainContent%24ddlLicenseType=All&ctl00%24MainContent%24ddlLicenseStatus=All&ctl00%24MainContent%24txtCity=&ctl00%24MainContent%24ddlState=Any&ctl00%24MainContent%24txtZip=&ctl00%24MainContent%24btnSearch=Search' --compressed"
      ) %>%
      purrr::reduce(paste0)
    return(curl_text)
  }

parse_lender_table <-
  function(page) {
    has_data <-
      page %>%
      html_table(fill = TRUE) %>%
      .[[4]] %>%
      data.frame(stringsAsFactors = FALSE) %>%
      tbl_df() %>%
      nrow() > 0

    if (has_data) {
      df <-
        page %>%
        html_table(fill = TRUE) %>%
        .[[4]] %>%
        data.frame(stringsAsFactors = FALSE) %>%
        tbl_df() %>%
        purrr::set_names(c('idLicense',
                           'statusLicense',
                           'dateLicenseEffective',
                           'nameLicensee',
                           'nameLicenseeDBA',
                           'dateLicenseOriginal',
                           'typeLicense',
                           'countEnforcementActions'))

      df <-
        df %>%
        mutate(idLicense = idLicense %>% as.character()) %>%
        mutate_at(c('countEnforcementActions'),
                  funs(. %>% as.character() %>%  readr::parse_number())) %>%
        mutate_at(c('dateLicenseEffective', 'dateLicenseOriginal'),
                  funs(. %>% lubridate::mdy)) %>%
        mutate_at(c('nameLicensee', 'nameLicenseeDBA'),
                  funs(. %>% as.character() %>% stringr::str_to_upper() %>% stringr::str_trim()))
      return(df)
    }
  }

california_lender <-
  function(search_name = "137", return_message = TRUE) {
    page <-
      generate_curl_text(search_name = search_name) %>%
      system_curl()

    df <-
      page %>%
      parse_lender_table() %>%
      mutate(nameSearch = search_name,
             isActive = ifelse(statusLicense == 'Active', TRUE, FALSE)) %>%
      dplyr::select(nameSearch, isActive, statusLicense, everything())

    if (return_message) {
      list("Found ", nrow(df), " entities with a CA lender's license matching ", search_name) %>%
        purrr::reduce(paste0) %>%
        asbtools::cat_message()
    }

    return(df)
  }

#' California Lenders Licensees
#'
#' @param search_names
#' @param return_message
#'
#' @return
#' @export
#' @import purrr dplyr rvest xml2 furrr future
#' @examples
california_lenders <-
  function(search_names = c("137", "Marble Bridge"), return_message = TRUE) {
    california_lender_safe <-
      purrr::possibly(california_lender, tibble())

    all_data <-
      search_names %>%
      future_map_dfr(function(x){
        california_lender_safe(search_name = x, return_message = return_message)
      })

    return(all_data)
  }
