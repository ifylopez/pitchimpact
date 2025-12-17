df_raw <- readr::read_csv("C:/Users/matea/OneDrive/Documentos/Proyectos Personales/possesionprueba.csv",
                          col_names = FALSE, show_col_types = FALSE)

new_names <- trimws(as.character(unlist(df_raw[2, ])))
new_names

ncol(df_raw)
