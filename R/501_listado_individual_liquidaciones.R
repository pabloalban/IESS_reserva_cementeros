message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tLectura de la proyección de precios' )

# Carga de datos -----------------------------------------------------------------------------------
load(paste0(parametros$RData, "IESS_beneficiarios.RData"))
load(paste0(parametros$RData, "IESS_nomina_intereses.RData"))

#Cargar función tíldes a latex----------------------------------------------------------------------
source( 'R/503_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )

message( '\tGenerando tablas individuales de liquidaciones' )
#1. Tabla resumen recaudación total-----------------------------------------------------------------
for (j in c(unique(nomina$id))) {
  
  aux <- nomina %>%
    filter( id == j) %>%
    dplyr::select( -id ) %>%
    mutate( fecha_derecho_ivm  = as.character(fecha_derecho_ivm ),
            fecha_pago = as.character( fecha_pago ) ) %>%
    dplyr::select( fecha_pago,
                   fecha_derecho_ivm,
                   tasa_interes,
                   pension_aumentos,
                   decimo_tercera_pension,
                   decimo_cuarta_pension,
                   total_pagado,
                   parte_ce,
                   interes_nomina ) %>%
    mutate( tasa_interes = 100 * tasa_interes )
  
  n <- nrow(aux)
  
  aux <- rbind((aux), c("Total", NA, NA, as.character(colSums(aux[,4:ncol(aux)]) ) ) )
  aux[c(4:ncol(aux))] <- lapply(aux[c(4:ncol(aux))], function(x) as.numeric(x))
  
  
  
  aux_xtab <- xtable( aux, digits = c( 0, rep(0, 3), rep(2,6)  ) )
  
  print( aux_xtab, 
         file = paste0( parametros$resultado_tablas, 'iess_liquidacion_',j,'.tex' ),
         type = 'latex',
         include.colnames = FALSE, include.rownames = FALSE,
         format.args = list( decimal.mark = ',', big.mark = '.' ),
         only.contents = TRUE,
         hline.after = c( n, n+1 ),
         sanitize.text.function = identity)
}
#---------------------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
