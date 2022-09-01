message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tLectura de la proyección de precios' )

# Carga de datos -----------------------------------------------------------------------------------
load(paste0(parametros$RData, "IESS_fallecidos.RData"))
load(paste0(parametros$RData, "IESS_liquidacion_intereses_fallecidos.RData"))
load(paste0(parametros$RData, "IESS_tab_resultado.RData"))

#Cargar función tíldes a latex----------------------------------------------------------------------
source( 'R/503_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )

message( '\tGenerando tablas individuales de liquidaciones de fallecidos' )
#1. Tabla individual de las liquidaciones-----------------------------------------------------------
for (j in fallecidos$id ) {
  
  aux <- liquidacion %>%
    filter( id == j) %>%
    dplyr::select( -id ) %>%
    mutate( f1_renta = as.character(f1_renta),
            fecha_derecho_ivm  = as.character(fecha_derecho_ivm ),
            periodo = as.character( periodo ) ) %>%
    dplyr::select(-cedula, -fecha_defuncion )
  
  n <- nrow(aux)
  
  aux <- rbind((aux), c("Total", NA, NA, NA, as.character(colSums(aux[,5:ncol(aux)]) ) ) )
  aux[c(4:ncol(aux))] <- lapply(aux[c(4:ncol(aux))], function(x) as.numeric(x))
  
  aux_xtab <- xtable( aux, digits = c( 0, rep(0, 3), rep(2,7)  ) )
  
  print( aux_xtab, 
         file = paste0( parametros$resultado_tablas, 'iess_liquidacion_',j,'.tex' ),
         type = 'latex',
         include.colnames = FALSE, include.rownames = FALSE,
         format.args = list( decimal.mark = ',', big.mark = '.' ),
         only.contents = TRUE,
         hline.after = c( n, n+1 ),
         sanitize.text.function = identity)
}


message( '\tGenerando tablas de resultados individuales' )
#2. Tabla resumen resultados total------------------------------------------------------------------
for (j in fallecidos$id) {

  aux <- tab_resultado %>%
    filter( id == j) %>%
    dplyr::select( id,
                   cedula,
                   liquidacion,
                   interes,
                   reserva_matematica,
                   montepio,
                   gastos_adm,
                   total ) %>%
    mutate( liquidacion = format( liquidacion,
                                  digits = 2, nsmall = 2, big.mark = '.',
                                  decimal.mark = ',', format = 'f' ),
            interes = format( interes,
                              digits = 2, nsmall = 2, big.mark = '.',
                              decimal.mark = ',', format = 'f' ),
            reserva_matematica = format( reserva_matematica,
                                         digits = 2, nsmall = 2, big.mark = '.',
                                         decimal.mark = ',', format = 'f' ),            
            montepio = format( montepio,
                               digits = 2, nsmall = 2, big.mark = '.',
                               decimal.mark = ',', format = 'f' ),
            gastos_adm = format( gastos_adm,
                                 digits = 2, nsmall = 2, big.mark = '.',
                                 decimal.mark = ',', format = 'f' ),
            total = format( total,
                            digits = 2, nsmall = 2, big.mark = '.',
                            decimal.mark = ',', format = 'f' )
            
    ) 
  
  aux[c(1:ncol(aux))] <- lapply(aux[c(1:ncol(aux))], function(x) as.character(x))
  
  aux <- aux %>%
    pivot_longer(!c(id) ,names_to = "variable", values_to = "valor") %>%
    dplyr::select(-id) %>%
    mutate( variable = c('Cédula de ciudadanía',
                         'Líquidación pensiones desde fecha de derecho',
                         'Intereses de valores cancelados por IVM',
                         'Reserva matemática',
                         'Beneficios de montepío',
                         'Gastos administrativos',
                         'Total a transferir'
    ) )
  
  aux_xtab <- xtable( aux, digits = c( 0, 0, 0 ) )
  
  aux_xtab <- tildes_a_latex(aux_xtab)
  
  print( aux_xtab, 
         file = paste0( parametros$resultado_tablas, 'iess_resultado_individual_',j,'.tex' ),
         type = 'latex',
         include.colnames = FALSE, 
         include.rownames = FALSE,
         format.args = list( decimal.mark = ',', big.mark = '.' ),
         only.contents = TRUE,
         hline.after = nrow(aux) - 1,
         sanitize.text.function = identity )
  
}

#2. Tabla resultados del calculo de intereses-------------------------------------------------------
interes <- interes %>%
  mutate( pago = if_else( pago == 'liquidacion 1', 'Primer pago por pensiones pendientes', 'Segundo pago por pensiones pendientes' ) )

for (j in c(fallecidos$id)) {
  
  aux <- interes %>%
    filter( id == j) %>%
    mutate( tasa_interes = 8.21 ) %>%
    dplyr::select( pago,
                   cedula,
                   fecha_liquidacion,
                   meses_trascurridos:=n_meses,
                   tasa_interes,
                   liquidacion,
                   interes) %>%
    mutate( fecha_liquidacion = as.character( fecha_liquidacion ) )
  
  n <- nrow(aux)
  
  aux <- rbind((aux), c("Total", NA, NA, NA, NA, as.character(colSums(aux[,6:ncol(aux)]) ) ) )
  aux[c(4:ncol(aux))] <- lapply(aux[c(4:ncol(aux))], function(x) as.numeric(x))
  
  
  aux_xtab <- xtable( aux, digits = c( 0, rep(0,3), rep(2,4) ) )
  
  print( aux_xtab, 
         file = paste0( parametros$resultado_tablas, 'iess_interes_individual_',j,'.tex' ),
         type = 'latex',
         include.colnames = FALSE, 
         include.rownames = FALSE,
         format.args = list( decimal.mark = ',', big.mark = '.' ),
         only.contents = TRUE,
         hline.after = c( nrow(aux) - 1, nrow(aux) ),
         sanitize.text.function = identity )
  
}


#---------------------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
