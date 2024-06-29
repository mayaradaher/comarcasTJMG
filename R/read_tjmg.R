#' Funcao que baixa o pdf das comarcas do TJMG, trata e cria um data frame com municipios, distritos e suas comarcas
#'
#' @return Data frame dos municípios e distritos de MG, bem como as respectivas comarcas do TJMG
#' @export
read_tjmg <- function() {
  url_tjmg <- "https://www8.tjmg.jus.br/servicos/gj/guia/docs/comarcas.pdf"

  # Cria um arquivo temporario
  tempfile <- tempfile()

  # Realiza o download do resultado da consulta a URL dentro do arquivo temporario
  utils::download.file(url_tjmg, tempfile)

  txt <- pdftools::pdf_text(tempfile)

  unlink(tempfile)

  row <- scan(textConnection(txt),
              what = "character",
              sep = "\n")

  munic_distr_comarca <- row |>
    stringr::str_squish() |>
    purrr::keep(~ stringr::str_detect(.x, "^[:digit:]{4} [:alpha:]+")) |>
    purrr::map(dplyr::as_tibble) |>
    dplyr::bind_rows() |>
    dplyr::filter(!stringr::str_detect(value, "Tel")) |>
    tidyr::separate(col = value,
                    sep = "\\.{2,}",
                    into = c("codigo_munic", "distancias"),
                    fill = "right",
                    extra = "drop"
    ) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), stringr::str_squish),
                  distrito = dplyr::if_else(stringr::str_detect(codigo_munic, "\\d$|-$"), "SIM", "NÃO"),
                  distrito_de = dplyr::case_when(is.na(distancias) & dplyr::lag(distrito) == "NÃO" ~ dplyr::lag(codigo_munic))) |>
    dplyr::select(-distancias) |>
    tidyr::fill(distrito_de) |>
    dplyr::mutate(distrito_de = dplyr::if_else(condition = distrito == "NÃO",
                                               true = NA_character_,
                                               false = stringr::str_sub(distrito_de, start = 6))) |>
    dplyr::with_groups(codigo_munic, ~ dplyr::mutate(.x, comarca = dplyr::case_when(length(codigo_munic) > 1 ~ stringr::str_sub(codigo_munic, start = 6),
                                                                                    TRUE ~ NA_character_))) |>
    tidyr::fill(comarca) |>
    dplyr::distinct(codigo_munic, comarca, .keep_all = TRUE) |>
    dplyr::mutate(munic_distr = stringr::str_squish(stringr::str_remove_all(codigo_munic, "\\d|- -| -")),
                  uf = "MG",
                  munic_distr_ibge = dplyr::case_when(
                    munic_distr == "Abre-Campo" ~ "Abre Campo",
                    munic_distr == "Amparo da Serra" ~ "Amparo do Serra",
                    munic_distr == "Barão do Monte Alto" ~ "Barão de Monte Alto",
                    munic_distr == "Cristiano Otôni" ~ "Cristiano Otoni",
                    munic_distr == "Desterro de Entre-Rios" ~ "Desterro de Entre Rios",
                    munic_distr == "Dona Eusébia" ~ "Dona Euzébia",
                    munic_distr == "Entre-Folhas" ~ "Entre Folhas",
                    munic_distr == "Entre-Rios de Minas" ~ "Entre Rios de Minas",
                    munic_distr == "Estrela-d'Alva" ~ "Estrela Dalva",
                    munic_distr == "Grão-Mogol" ~ "Grão Mogol",
                    munic_distr == "Itamoji" ~ "Itamogi",
                    munic_distr == "Itapajipe" ~ "Itapagipe",
                    munic_distr == "Jabuticatubas" ~ "Jaboticatubas",
                    munic_distr == "Matias Lobato" ~ "Mathias Lobato",
                    munic_distr == "Passa-Quatro" ~ "Passa Quatro",
                    munic_distr == "Passa-Tempo" ~ "Passa Tempo",
                    munic_distr == "Passa-Vinte" ~ "Passa Vinte",
                    munic_distr == "Santa Bárbara do Monte" ~ "Santa Bárbara do Monte Verde",
                    munic_distr == "Santa Rita do Jacutinga" ~ "Santa Rita de Jacutinga",
                    munic_distr == "Santa Rita do Ibitipoca" ~ "Santa Rita de Ibitipoca",
                    munic_distr == "São João del-Rei"~ "São João del Rei",
                    munic_distr == "São Pedro do Suaçui" ~ "São Pedro do Suaçuí",
                    munic_distr == "São Sebastião da Vargem" ~ "São Sebastião da Vargem Alegre",
                    munic_distr == "Teófilo Otôni" ~ "Teófilo Otoni",
                    TRUE ~ as.character(munic_distr)
                  ),
                  comarca = dplyr::case_when(
                    comarca == "Abre-Campo" ~ "Abre Campo",
                    comarca == "Amparo da Serra" ~ "Amparo do Serra",
                    comarca == "Barão do Monte Alto" ~ "Barão de Monte Alto",
                    comarca == "Cristiano Otôni" ~ "Cristiano Otoni",
                    comarca == "Desterro de Entre-Rios" ~ "Desterro de Entre Rios",
                    comarca == "Dona Eusébia" ~ "Dona Euzébia",
                    comarca == "Entre-Folhas" ~ "Entre Folhas",
                    comarca == "Entre-Rios de Minas" ~ "Entre Rios de Minas",
                    comarca == "Estrela-d'Alva" ~ "Estrela Dalva",
                    comarca == "Grão-Mogol" ~ "Grão Mogol",
                    comarca == "Itamoji" ~ "Itamogi",
                    comarca == "Itapajipe" ~ "Itapagipe",
                    comarca == "Jabuticatubas" ~ "Jaboticatubas",
                    comarca == "Matias Lobato" ~ "Mathias Lobato",
                    comarca == "Passa-Quatro" ~ "Passa Quatro",
                    comarca == "Passa-Tempo" ~ "Passa Tempo",
                    comarca == "Passa-Vinte" ~ "Passa Vinte",
                    comarca == "Santa Bárbara do Monte" ~ "Santa Bárbara do Monte Verde",
                    comarca == "Santa Rita do Jacutinga" ~ "Santa Rita de Jacutinga",
                    comarca == "Santa Rita do Ibitipoca" ~ "Santa Rita de Ibitipoca",
                    comarca == "São João del-Rei"~ "São João del Rei",
                    comarca == "São Pedro do Suaçui" ~ "São Pedro do Suaçuí",
                    comarca == "São Sebastião da Vargem" ~ "São Sebastião da Vargem Alegre",
                    comarca == "Teófilo Otôni" ~ "Teófilo Otoni",
                    TRUE ~ as.character(comarca)
                  ),
                  codigo = stringr::word(codigo_munic, 1, 1)) |>
    dplyr::select(codigo_tjmg = codigo,
                  munic_distr_ibge,
                  distrito_de,
                  comarca)

  munic_distr_comarca
}
