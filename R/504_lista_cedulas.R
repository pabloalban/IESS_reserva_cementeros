message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tLectura de la proyección de precios' )

# Carga de datos -----------------------------------------------------------------------------------
load(paste0(parametros$RData, "IESS_beneficiarios.RData"))
load(paste0(parametros$RData, "IESS_liquidacion.RData"))
load(paste0( parametros$RData, 'IESS_fallecidos.RData'))
#Cargar función tíldes a latex----------------------------------------------------------------------
source( 'R/503_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )

message( '\tGenerando cedula de beneficiarios de liquidaciones' )
#1. Tabla resumen recaudación total-----------------------------------------------------------------
for (j in c(1:nrow(beneficiarios))) {
  
  aux <- beneficiarios %>%
    filter( id == j) %>%
    select( -id ) %>%
    mutate( f1_renta = as.character(f1_renta),
            fecha_derecho_ivm  = as.character(fecha_derecho_ivm ) ) %>%
    dplyr::select( cedula )
  
  write.table(aux, file = paste0( parametros$resultado_tablas, 'iess_cedula_',j,'.tex' ),
              sep = ",",
              col.names = FALSE,
              row.names = FALSE,
              quote = FALSE,
              qmethod = "double")
  
}

for (j in c(fallecidos$id)) {
  
  aux <- fallecidos %>%
    filter( id == j) %>%
    select( -id ) %>%
    mutate( f1_renta = as.character(f1_renta),
            fecha_derecho_ivm  = as.character(fecha_derecho_ivm ) ) %>%
    dplyr::select( cedula )
  
  write.table(aux, file = paste0( parametros$resultado_tablas, 'iess_cedula_',j,'.tex' ),
              sep = ",",
              col.names = FALSE,
              row.names = FALSE,
              quote = FALSE,
              qmethod = "double")
  
}

for (j in c( beneficiarios_v2$id ) ) {
  
  aux <- beneficiarios_v2 %>%
    filter( id == j) %>%
    select( -id ) %>%
    mutate( f1_renta = as.character(f1_renta),
            fecha_derecho_ivm  = as.character(fecha_derecho_ivm ) ) %>%
    dplyr::select( cedula )
  
  aux$cedula <- paste0( aux$cedula, ',' )
  
  write.table(aux, file = paste0( parametros$resultado_tablas, 'iess_cedula_',j,'.tex' ),
              sep = ",",
              col.names = FALSE,
              row.names = FALSE,
              quote = FALSE,
              qmethod = "double")
  
}

#---------------------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()