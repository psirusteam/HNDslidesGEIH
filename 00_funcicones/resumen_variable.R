resumen_variable <- function(.data, var, dominios = NULL, tipo = NULL) {
  library(dplyr)
  library(survey)
  library(labelled)
  
  var_sym <- sym(var)
  
  # Extraer label de la variable (si existe)
  label_pregunta <- var_label(.data$variable[[var]])
  if (is.null(label_pregunta)) {
    label_pregunta <- NA_character_
  }
  
  if (is.labelled(.data$variable[[var]])) {
    .data <- .data %>% mutate(!!var_sym := as_factor(!!var_sym))
  }
  
  # Detectar tipo automáticamente si no se proporciona
  if (is.null(tipo)) {
    valores <- .data$variables[[var]]
    
    if (is.numeric(valores) && all(na.omit(valores) %in% c(0, 1))) {
      tipo_detectado <- "binaria"
    } else if (is.numeric(valores)) {
      tipo_detectado <- "numerica"
    } else {
      tipo_detectado <- "factor"
    }
  } else {
    tipo_detectado <- match.arg(tipo, choices = c("factor", "numerica", "binaria"))
  }
  
  # Si no hay dominios, se agrega uno ficticio
  if (is.null(dominios)) {
    .data <- .data %>% mutate(Dominio = "Total")
    dominios <- "Dominio"
  }
  
  dom_syms <- syms(dominios)
  
  resultado <- .data %>%
    mutate(tot = 1) %>%
    filter(!is.na(!!var_sym)) %>%
    group_by(!!!dom_syms)
  
  if (tipo_detectado == "numerica") {
    resultado <- resultado %>%
      summarise(
        media = survey_mean(!!var_sym, vartype = c("se", "cv"), na.rm = TRUE),
        n = unweighted(n()),
        total_exp = survey_total(tot, vartype = NULL),
        .groups = "drop"
      )
  } else {
    resultado <- resultado %>%
      group_by(!!!dom_syms, !!var_sym) %>%
      summarise(
        prop = survey_mean(vartype = c("se", "cv"), na.rm = TRUE),
        n = unweighted(n()),
        total_exp = survey_total(tot, vartype = NULL),
        .groups = "drop"
      ) %>%
      rename(categoria = !!var_sym)
  }
  
  resultado <- resultado %>%
    mutate(
      var_nombre = var,
      var_label = as.character(label_pregunta),
      tipo_detectado = tipo_detectado,
      .before = 1
    )
  
  return(resultado)
}



guardar_resultados_excel <- function(lista_resultados, archivo = "resultados.xlsx") {
  
  # Crear libro
  wb <- createWorkbook()
  
  # Agregar hoja índice
  addWorksheet(wb, "Índice")
  
  # Obtener nombres de cada tabla (si la lista está nombrada)
  nombres_tablas <- names(lista_resultados)
  if (is.null(nombres_tablas)) {
    nombres_tablas <- paste0("Resultado_", seq_along(lista_resultados))
  }
  
  # Escribir hipervínculos en la hoja índice
  for (i in seq_along(nombres_tablas)) {
    nombre <- nombres_tablas[i]
    writeData(wb, sheet = "Índice", x = nombre, startRow = i, startCol = 1)
    writeFormula(
      wb,
      sheet = "Índice",
      x = makeHyperlinkString(sheet = nombre, row = 1, col = 1, text = paste("Ir a", nombre)),
      startRow = i,
      startCol = 2
    )
  }
  
  # Agregar cada hoja con resultados y botón de regreso al índice
  walk2(lista_resultados, nombres_tablas, ~{
    addWorksheet(wb, .y)
    writeData(wb, .y, .x)
    
    # Agregar hipervínculo de regreso al índice
    last_row <- nrow(.x) + 3
    writeFormula(
      wb,
      sheet = .y,
      x = makeHyperlinkString(sheet = "Índice", row = 1, col = 1, text = "⬅ Regresar al índice"),
      startRow = last_row,
      startCol = 1
    )
  })
  
  # Guardar archivo
  saveWorkbook(wb, archivo, overwrite = TRUE)
  message(glue::glue("Archivo guardado como '{archivo}'"))
}
