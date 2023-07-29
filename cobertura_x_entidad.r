library(tidyverse)

# función para saber a que tipo de servicio pertenecen los datos
tipo_servicio <- function(flag_tel, flag_tv, flag_inter) {
  clave <- paste(flag_tel, flag_tv, flag_inter, sep = "-")

  dplyr::case_when(clave == "1-0-0" ~ "telefonia",
                   clave == "0-1-0" ~ "television",
                   clave == "0-0-1" ~ "internet",
                   clave == "1-1-0" ~ "telefonia-television",
                   clave == "1-0-1" ~ "internet-telefonia",
                   clave == "0-1-1" ~ "internet-television",
                   clave == "1-1-1" ~ "triple play",
                   TRUE ~ "Otro")
}

cober_selected <- read_csv("cobertura_selec.csv", locale = locale(encoding = "Latin1"), show_col_types = FALSE)

base <- readxl::read_excel("base.xlsx", .name_repair = janitor::make_clean_names)

# seleccionamos solo las variables que nos interesan
df_selected <- base |>
  mutate(servicio = tipo_servicio(flag_telefonia, flag_tv_paga, flag_internet)) |>
  select(servicio, concesionario = nombre_comercial, coberturas)

# Verifiacmos que concesionarios tiene cobertura Nacional
df_selected |>
  filter(str_detect(coberturas, "Estado: NACIONAL")) |>
  count(servicio, concesionario)

#agregamos la cobertura para casos especiales
df_cobertura <- df_selected |>
  separate_rows("concesionario", sep = "-") |>
  mutate(
    coberturas = case_when(
      concesionario == "Telmex" ~ pull(cober_selected, telmex),
      concesionario == "Telnor" ~ pull(cober_selected, telnor),
      str_detect(coberturas, "Estado: NACIONAL") ~ pull(cober_selected, nacional),
      .default = coberturas
    )
  )

# separamos la cobertura, un renglon por entidad
# separamos para tener una col para la entidad y un col para el municipio
df_cobertura_separada <- df_cobertura |>
  mutate(coberturas = str_squish(coberturas)) |>
  separate_rows("coberturas", sep = "Estado:\\s") |>
  filter(coberturas != "") |>
  separate(coberturas, c("entidad", "municipio"), sep = ",\\sMunicipio:")

# quitamos los valores duplicados
df_cobertura_separada <- df_cobertura_separada |> distinct(servicio, concesionario, entidad)

# guardamos base
write_excel_csv(df_cobertura_separada, "cobertura_separada.csv")
