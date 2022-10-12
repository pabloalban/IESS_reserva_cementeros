message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tLectura de la proyección de precios' )

# Carga de datos -----------------------------------------------------------------------------------
load(paste0(parametros$RData, "IESS_beneficiarios.RData"))
load(paste0(parametros$RData, "IESS_liquidacion.RData"))
load(paste0( parametros$RData, 'IESS_fallecidos.RData'))

#Cargar función tíldes a latex----------------------------------------------------------------------
source( 'R/503_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )

message( '\tGenerando nombres de beneficiarios de liquidaciones' )
#1. Tabla resumen recaudación total-----------------------------------------------------------------
for (j in c(1:nrow(beneficiarios))) {
  
  aux <- beneficiarios %>%
    filter( id == j) %>%
    dplyr::select( nombre) 
  
  aux <- data.frame(lapply(aux, function(x) { if(is.character(x)) gsub("Ñ", "$\\tilde{\\text{N}}$", x, fixed = TRUE) else x }))
  
  write.table(aux, file = paste0( parametros$resultado_tablas, 'iess_nombre_',j,'.tex' ),
              sep = ",",
              col.names = FALSE,
              row.names = FALSE,
              quote = FALSE,
              qmethod = "double")
  
}


for (j in c(unique(fallecidos$id))) {
  
  aux <- beneficiarios %>%
    filter( id == j) %>%
    dplyr::select( nombre) 
  
  aux <- data.frame(lapply(aux, function(x) { if(is.character(x)) gsub("Ñ", "$\\tilde{\\text{N}}$", x, fixed = TRUE) else x }))
  
  write.table(aux, file = paste0( parametros$resultado_tablas, 'iess_nombre_',j,'.tex' ),
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