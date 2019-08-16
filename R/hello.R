#' @title Rsemstrom
#' @description Pakiet umożliwia pobieranie danych z SemStorm (widoczność witryny w Top 50 i estymowany ruch)
#' @param domena domena dla której pobieramy dane
#' @param rodzaj_slowa dostępne all (słowa na której obecnie widoczna jest domena), new (nowe słowa), lost (utracone słowa)
#' @param kluczAPI klucz API semstorm
#' @return NULL
#' @examples slowa_domeny('domena.pl',all,"9fxjh-6kEaUTMhPo4")
#' @export slowa_domeny

slowa_domeny <- function(domena,rodzaj_slowa,kluczAPI) {

  require(httr)
  require(dplyr)
  require(jsonlite)
  require(openxlsx)

  URL <- "http://api.semstorm.com/api-v3/explorer/explorer-keywords/get-data"
  ile_wynikow <- 100
  domena <- array(domena)
  rodzaj_slowa <- rodzaj_slowa
  key <- kluczAPI

  #pierwszy post do wyliczenia liczby fraz i pobrania pierwszej partii fraz
  POST_frazy_new <-
    POST(
      URL,
      content_type("application/json"),
      body = list(
        services_token = key,
        domains = domena,
        keywords_type = rodzaj_slowa,
        pager = list(items_per_page = ile_wynikow, page = 0)
      ),
      encode = c("json")
    )


  # przetworzenie pierwszego JSON-a
  JSON_frazy_new <-
    POST_frazy_new %>% content("text") %>% fromJSON(flatten = F)

  liczba_fraz_new <- JSON_frazy_new$results_count
  ile_obrotow_new <- ceiling(liczba_fraz_new / ile_wynikow)
  print(paste("ile obrotów liczba", ile_obrotow_new))

  # pierwsza pętla

  #df zawierająca dane dla 100 fraz
  frazy_i_lp_new <-
    cbind(
      JSON_frazy_new$results$keyword,
      JSON_frazy_new$results$url,
      JSON_frazy_new$results$position,
      JSON_frazy_new$results$volume,
      JSON_frazy_new$results$cpc,
      Sys.Date()
    )
  frazy_i_lp_new_Full <- frazy_i_lp_new
  colnames(frazy_i_lp_new_Full) <- c("fraza","lp","pozycja","ilośc wyszukań","cpc","data")
  #print(frazy_i_lp_new_Full)
  ######## Sprawdzone do tego miejsca

  #pętla do pobierania danych


  for (i in 1:length(ile_obrotow_new)) {
    # Pobranie kolejnych postów
    POST_frazy_new <-
      POST(
        URL,
        content_type("application/json"),
        body = list(
          services_token = key,
          domains = domena,
          keywords_type = rodzaj_slowa,
          pager = list(items_per_page = ile_wynikow, page = i)
        ),
        encode = c("json")
      )
    #
        print(paste("post", i))
    #   #  # przetworzenie kolejnego JSON-a
    JSON_frazy_new <-
      POST_frazy_new %>% content("text") %>% fromJSON(flatten = F)
        print(paste("json", i))
    #   #
    #   #   # df zawierająca dane dla 100 fraz
    frazy_i_lp_new_2 <-
      cbind(
        JSON_frazy_new$results$keyword,
        JSON_frazy_new$results$url,
        JSON_frazy_new$results$position,
        JSON_frazy_new$results$volume,
        JSON_frazy_new$results$cpc,
        Sys.Date()
      )
    colnames(frazy_i_lp_new_2) <- c("fraza","lp","pozycja","ilośc wyszukań","cpc","data")
    #
       print(paste("cbind", i))
    #   #
    #   #   #  df docelowa zawierająca wszystkie frazy
    frazy_i_lp_new_Full <-
      rbind(frazy_i_lp_new_Full,
            frazy_i_lp_new_2)
       print (paste("rbind", i))
  }
  #
  # # wyliczenie ruchu na podstawie pozycji
  #
  # #  print(round(frazy_i_lp_new_Full[1, 3]))
  for (i in 1:length(frazy_i_lp_new_Full$pozycja))
    if (frazy_i_lp_new_Full[i, 3] == 10) {
      frazy_i_lp_new_Full$Ruch[i] <-
        round(as.numeric(as.character(frazy_i_lp_new_Full[i, 4])) *
                0.3258)
    } else if (frazy_i_lp_new_Full[i, 3] == 9) {
      frazy_i_lp_new_Full$Ruch[i] <-
        round(as.numeric(as.character(frazy_i_lp_new_Full[i, 4])) *
                0.1669)
    } else if (round(frazy_i_lp_new_Full[i, 3] == 8)) {
      frazy_i_lp_new_Full$Ruch[i] <-
        round(as.numeric(as.character(frazy_i_lp_new_Full[i, 4])) *
                0.1034)
    } else if (round(frazy_i_lp_new_Full[i, 3] == 7)) {
      frazy_i_lp_new_Full$Ruch[i] <-
        round(as.numeric(as.character(frazy_i_lp_new_Full[i, 4])) *
                0.0724)
    } else if (round(frazy_i_lp_new_Full[i, 3] == 6)) {
      frazy_i_lp_new_Full$Ruch[i] <-
        round(as.numeric(as.character(frazy_i_lp_new_Full[i, 4])) *
                0.0527)
    } else if (round(frazy_i_lp_new_Full[i, 3] == 5)) {
      frazy_i_lp_new_Full$Ruch[i] <-
        round(as.numeric(as.character(frazy_i_lp_new_Full[i, 4])) *
                0.0393)
    } else if (round(frazy_i_lp_new_Full[i, 3] == 4)) {
      frazy_i_lp_new_Full$Ruch[i] <-
        round(as.numeric(as.character(frazy_i_lp_new_Full[i, 4])) *
                0.0302)
    } else if (round(frazy_i_lp_new_Full[i, 3] == 3)) {
      frazy_i_lp_new_Full$Ruch[i] <-
        round(as.numeric(as.character(frazy_i_lp_new_Full[i, 4])) *
                0.0235)
    } else if (round(frazy_i_lp_new_Full[i, 3] == 2)) {
      frazy_i_lp_new_Full$Ruch[i] <-
        round(as.numeric(as.character(frazy_i_lp_new_Full[i, 4])) *
                0.0186)
    } else if (round(frazy_i_lp_new_Full[i, 3] == 1)) {
      frazy_i_lp_new_Full$Ruch[i] <-
        round(as.numeric(as.character(frazy_i_lp_new_Full[i, 4])) *
                0.0153)
    } else {
      frazy_i_lp_new_Full$Ruch[i] <- 0
    }


  write.xlsx(
    frazy_i_lp_new_Full,
    file = file.choose(new = T),
    row.names = TRUE,
    col.names = TRUE,
    sheetName = 'Frazy',
    append = T
  )
}
