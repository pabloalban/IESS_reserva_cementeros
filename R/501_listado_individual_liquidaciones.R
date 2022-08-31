message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tLectura de la proyección de precios' )

# Carga de datos -----------------------------------------------------------------------------------
load(paste0(parametros$RData, "IESS_beneficiarios.RData"))
load(paste0(parametros$RData, "IESS_liquidacion.RData"))

#Cargar función tíldes a latex----------------------------------------------------------------------
source( 'R/503_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )

message( '\tGenerando tablas individuales de liquidaciones' )
#1. Tabla resumen recaudación total-----------------------------------------------------------------
for (j in c(1:nrow(beneficiarios))) {
  
  aux <- liquidacion %>%
    filter( id == j) %>%
    dplyr::select( -id ) %>%
    mutate( f1_renta = as.character(f1_renta),
            fecha_derecho_ivm  = as.character(fecha_derecho_ivm ),
            periodo = as.character( periodo ) )
  
  n <- nrow(aux)
  
  aux <- rbind((aux), c("Total", NA, NA, NA, as.character(colSums(aux[,5:ncol(aux)]) ) ) )
  aux[c(5:ncol(aux))] <- lapply(aux[c(5:ncol(aux))], function(x) as.numeric(x))
  
  aux_xtab <- xtable( aux, digits = c( 0, rep(0, 4), rep(2, 6)  ) )
  
  print( aux_xtab, 
         file = paste0( parametros$resultado_tablas, 'iess_liquidacion_',j,'.tex' ),
         type = 'latex',
         include.colnames = FALSE, include.rownames = FALSE,
         format.args = list( decimal.mark = ',', big.mark = '.' ),
         only.contents = TRUE,
         hline.after = c( n ),
         sanitize.text.function = identity)
}
#---------------------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
