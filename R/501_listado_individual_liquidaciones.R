message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tLectura de la proyección de precios' )

# Carga de datos -----------------------------------------------------------------------------------
load(paste0(parametros$RData, "IESS_nomina_concesiones.RData"))
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

#2. Tabla resumen recaudación total-----------------------------------------------------------------

aux_a <- nomina_ivm_v2 %>% 
  group_by( cedula ) %>% 
  filter( periodo == max( periodo, na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  dplyr::select( cedula,
                 pension_ivm := valor_pension_aumentos  )

aux_b <- beneficiarios_v2 %>% 
  dplyr::select( id,
                 cedula,
                 ric_ivm,
                 ric,
                 edad,
                 edad_derecho_ivm,
                 fecha_derecho_ivm )

aux_c <- left_join( aux_b,
                    aux_a, 
                    by = 'cedula' ) %>% 
  mutate( pension_ivm = if_else( is.na( pension_ivm ),
                                 ric_ivm,
                                 pension_ivm ) ) %>% 
  mutate( coef = pension_ivm / ric ) %>% 
  mutate( coef = if_else( coef > 1,
                          1,
                          coef ) ) %>% 
  mutate( derecho_ivm = if_else( edad > edad_derecho_ivm,
                                 1,
                                 0 ) ) %>% 
  dplyr::select( id,
                 cedula,
                 coef,
                 derecho_ivm,
                 fecha_derecho_ivm )

aux_3 <- nomina_cem_v2 %>% 
  replace( is.na( . ), 0 ) %>% 
  left_join( ., aux_c, by = c( 'cedula' ) ) %>% 
  mutate( parte_ivm = coef * ( valor_pension_y_aumentos + valor_decimo_tercera_pension ) *  derecho_ivm  + valor_decimo_cuarta_pension * derecho_ivm ) %>% 
  mutate( parte_ce = valor_total_pagado -  parte_ivm ) %>% 
  group_by( cedula ) %>%
  #mutate( nomina  = sum( parte_ce , na.rm = TRUE ) ) %>%
  mutate( interes_nomina  = 0 ) %>%
  ungroup() 


for (j in c( unique( beneficiarios_v2$id ) ) ) {
  
  aux <- aux_3 %>%
    filter( id == j) %>%
    dplyr::select( -id ) %>%
    mutate( fecha_derecho_ivm  = as.character(fecha_derecho_ivm ),
            fecha_pago = as.character( periodo ) ) %>%
    mutate( tasa_interes = 0.0821 ) %>% 
    dplyr::select( fecha_pago,
                   fecha_derecho_ivm,
                   tasa_interes,
                   pension_aumentos := valor_pension_y_aumentos,
                   decimo_tercera_pension := valor_decimo_tercera_pension,
                   decimo_cuarta_pension := valor_decimo_cuarta_pension ,
                   total_pagado := valor_total_pagado,
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
