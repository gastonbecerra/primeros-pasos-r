library(tidyverse)

set.seed(1234)

data <- readRDS("data/raw/data.rds")

encuesta_ia_base <- data %>%
  filter(country == "Argentina") %>%
  filter(between(age, 18, 74)) %>%
  transmute(
    edad = age,
    grupo_edad = case_when(
      edad < 30 ~ "18-29",
      edad < 45 ~ "30-44",
      edad < 60 ~ "45-59",
      edad >= 60 ~ "60+"
    ),
    area_trabajo = workArea,
    frecuencia_uso_ia = uso_ia_frecuencia,
    actitud_positiva = A1,
    actitud_trabajo_estudio = A2,
    actitud_vida_cotidiana = A3,
    actitud_aprender_ia = A4,
    creencia_decisiones_justas = C1,
    creencia_respuestas_certeras = C2,
    creencia_orientacion_psicologica = C3,
    creencia_compania = C4,
    creencia_investigacion = C5,
    creencia_ensenanza = C6
  ) %>%
  mutate(
    area_trabajo = str_replace_all(area_trabajo, "-", " "),
    area_trabajo = str_to_sentence(area_trabajo),
    area_trabajo_grupo = case_when(
      area_trabajo == "Educacion" ~ "Educación",
      area_trabajo == "Salud servicios sociales" ~ "Salud y servicios sociales",
      area_trabajo == "Consultoria servicios profesionales" ~ "Consultoría / servicios profesionales",
      area_trabajo == "Gobierno sector publico" ~ "Gobierno / sector público",
      area_trabajo %in% c("Tecnologia informatica", "Administracion finanzas") ~ "Tecnología / administración",
      area_trabajo == "Otra no aplica" ~ "Otra / no aplica",
      TRUE ~ "Otros sectores"
    ),
    grupo_edad = factor(
      grupo_edad,
      levels = c("18-29", "30-44", "45-59", "60+"),
      ordered = TRUE
    ),
    frecuencia_uso_ia = factor(
      frecuencia_uso_ia,
      levels = c("nunca", "esporadicamente", "ocasional", "frecuente"),
      ordered = TRUE
    ),
    area_trabajo_grupo = factor(
      area_trabajo_grupo,
      levels = c(
        "Educación",
        "Salud y servicios sociales",
        "Consultoría / servicios profesionales",
        "Gobierno / sector público",
        "Tecnología / administración",
        "Otros sectores",
        "Otra / no aplica"
      )
    )
  )

encuesta_ia_base %>%
  count(area_trabajo_grupo)

encuesta_ia_base %>%
  count(frecuencia_uso_ia)

encuesta_ia_base %>%
  summarise(
    casos = n(),
    edad_min = min(edad, na.rm = TRUE),
    edad_max = max(edad, na.rm = TRUE),
    edad_media = mean(edad, na.rm = TRUE)
  )

grupos_taller <- c(
  "Educación",
  "Salud y servicios sociales",
  "Consultoría / servicios profesionales"
)

encuesta_ia_subset <- encuesta_ia_base %>%
  filter(area_trabajo_grupo %in% grupos_taller) %>%
  group_by(area_trabajo_grupo) %>%
  slice_sample(prop = 1) %>%
  slice_head(n = 50) %>%
  ungroup() %>%
  mutate(id_caso = row_number()) %>%
  select(
    id_caso,
    edad,
    grupo_edad,
    area_trabajo_grupo,
    frecuencia_uso_ia,
    actitud_positiva,
    actitud_trabajo_estudio,
    actitud_vida_cotidiana,
    actitud_aprender_ia,
    creencia_decisiones_justas,
    creencia_respuestas_certeras,
    creencia_orientacion_psicologica,
    creencia_compania,
    creencia_investigacion,
    creencia_ensenanza
  )

glimpse(encuesta_ia_subset)

encuesta_ia_subset %>%
  count(area_trabajo_grupo)

encuesta_ia_subset %>%
  count(frecuencia_uso_ia)

encuesta_ia_subset %>%
  count(grupo_edad)

encuesta_ia_subset %>%
  summarise(
    casos = n(),
    edad_min = min(edad, na.rm = TRUE),
    edad_max = max(edad, na.rm = TRUE),
    edad_media = mean(edad, na.rm = TRUE)
  )

encuesta_ia_subset %>%
  summarise(across(everything(), ~ sum(is.na(.))))

encuesta_ia_subset %>%
  summarise(
    across(
      starts_with("actitud_") | starts_with("creencia_"),
      list(
        min = \(x) min(x, na.rm = TRUE),
        max = \(x) max(x, na.rm = TRUE)
      )
    )
  )

write_csv(encuesta_ia_subset, "data/encuesta_ia_subset.csv")
