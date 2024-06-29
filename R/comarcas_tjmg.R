#' Consolida a lista de municipios do TJMG com o IBGE, retornando as comarcas dos municipios
#'
#' @return Data frame consolidado dos munícipios e comarcas do TJMG, incluindo informações do IBGE
#' @export
comarcas_tjmg <- function() {
  # Faz o join entre a lista de comarcas e a lista de municipios do IBGE
  tjmg <- read_tjmg()
  ibge <- read_municipios_ibge() |>
    dplyr::filter(nome_uf == "Minas Gerais")

  munic_filter <- tjmg |>
    dplyr::filter(is.na(distrito_de))

  munic_total <- dplyr::left_join(x = munic_filter,
                                  y = ibge,
                                  by = c("munic_distr_ibge" = "nome_municipio"))

  munic_total |>
    dplyr::select(-distrito_de) |>
    dplyr::rename(municipio_num = municipio,
                  municipio = munic_distr_ibge) |>
    dplyr::arrange(stringi::stri_trans_general(municipio, "Latin-ASCII")) |>
    tibble::rowid_to_column(var = "order_id")

}
