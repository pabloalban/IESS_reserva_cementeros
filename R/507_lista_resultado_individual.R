message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tLectura de tabla de resultados' )

# Carga de datos -----------------------------------------------------------------------------------
load(paste0(parametros$RData, "IESS_beneficiarios.RData"))
load(paste0(parametros$RData, "IESS_liquidacion.RData"))
load(paste0(parametros$RData, "IESS_tab_resultado.RData"))
load(paste0(parametros$RData, "IESS_interes.RData"))
load(paste0(parametros$RData, "IESS_reserva_matematica.RData"))

#Cargar función tíldes a latex----------------------------------------------------------------------
source( 'R/503_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )

message( '\tGenerando tablas de resultados individuales' )
#1. Tabla resumen resultados total------------------------------------------------------------------
for (j in c(1:nrow(tab_resultado))) {
  
  aux <- tab_resultado %>%
    filter( id == j) %>%
    dplyr::select( id,
                   cedula,
                   liquidacion:=liquidacion_pagada ,
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

for (j in c(1:nrow(beneficiarios))) {
  
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


#3. Tabla resultados del calculo de la reserva matemática-------------------------------------------

aux <- beneficiarios %>% 
  dplyr::select( cedula,
                 f1_renta )

reserva_matematica <- reserva_matematica %>% 
  left_join( ., aux, by = 'cedula' )

for (j in c(1:nrow(reserva_matematica))) {
  
  aux <-  reserva_matematica %>%
    filter( id == j) %>%
    dplyr::select( id,
                   cedula,
                   f1_renta,
                   fecha_derecho_ivm,
                   x:=edad,
                   edad_derecho_ivm,
                   renta_concedida := pension_aumentos,
                   renta_ce,
                   renta_ivm,
                   n,
                   a_x_n,
                   res_mat_temporal,
                   k,
                   a_n_w,
                   res_mat_diferida,
                   a_x,
                   reserva_matematica ) %>%
    mutate( res_mat_temporal = format( res_mat_temporal,
                                  digits = 2, nsmall = 2, big.mark = '.',
                                  decimal.mark = ',', format = 'f' ),
            res_mat_diferida = format( res_mat_diferida,
                              digits = 2, nsmall = 2, big.mark = '.',
                              decimal.mark = ',', format = 'f' ),
            reserva_matematica = format( reserva_matematica,
                                         digits = 2, nsmall = 2, big.mark = '.',
                                         decimal.mark = ',', format = 'f' ),
            a_x_n = format( a_x_n,
                                         digits = 2, nsmall = 2, big.mark = '.',
                                         decimal.mark = ',', format = 'f' ),
            a_n_w  = format( a_n_w,
                                         digits = 2, nsmall = 2, big.mark = '.',
                                         decimal.mark = ',', format = 'f' ),
            a_x   = format( a_x,
                                         digits = 2, nsmall = 2, big.mark = '.',
                                         decimal.mark = ',', format = 'f' ),
            renta_ivm = format( renta_ivm,
                                  digits = 2, nsmall = 2, big.mark = '.',
                                  decimal.mark = ',', format = 'f' ),
            renta_concedida = format( renta_concedida,
                                  digits = 2, nsmall = 2, big.mark = '.',
                                  decimal.mark = ',', format = 'f' ),
            renta_ce = format( renta_ce,
                                      digits = 2, nsmall = 2, big.mark = '.',
                                      decimal.mark = ',', format = 'f' )
            ) 
  
  aux[c(1:ncol(aux))] <- lapply(aux[c(1:ncol(aux))], function(x) as.character(x))
  
  aux <- aux %>%
    pivot_longer(!c(id) ,names_to = "variable", values_to = "valor") %>%
    mutate( variable = c('Cédula ciudadanía',
                         'F1 Renta',
                         'Fecha de derecho IVM',
                         '$x$',
                         'Edad derecho IVM',
                         'Renta concedida al corte (USD)',
                         'Renta cemento al 2022 (USD)',
                         'Renta IVM al 2022 (USD)',
                         '$n$',
                         '$\\actsymb{\\ddot{a}}{\\nthtop{}{x}:\\angln}$',
                         'Renta matemática temporal (USD)',
                         '$k$',
                         '$k / \\ddot{a}_x$',
                         'Renta matemática diferida (USD)',
                         '$\\ddot{a}_x$',
                         'Reserva matemática (USD)') ) %>%
    dplyr::select(-id) %>%
    filter( !is.na( valor )) %>%
    filter(valor != 'NA')
  
  aux_xtab <- xtable( aux, digits = c( 0, rep(0,2) ) )
  
  aux_xtab <- tildes_a_latex(aux_xtab)
  
  ifelse( nrow(aux)==15,
          a <- c(paste(" \n \\hline \\multicolumn{2}{c}{\\textbf{Renta temporal}} \\\\ \n "), 
                 paste(" \n \\hline \\multicolumn{2}{c}{\\textbf{Renta diferida vitalicia}} \\\\ \n"),
                 paste(" \n \\hline \\multicolumn{2}{c}{\\textbf{Reserva Total}} \\\\ \n") ),
          a <- c(paste(" \n \\hline \\multicolumn{2}{c}{\\textbf{Renta Vitalicia}} \\\\ \n \\hline \n") ) )
  
  print( aux_xtab,
         file = paste0( parametros$resultado_tablas, 'iess_reserva_individual_', j, '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = if( nrow(aux)==15 )  c(8,10,11,13,14,nrow(aux)) else c(9,nrow(aux)) ,
         sanitize.text.function = identity,
         add.to.row = list( pos = if( nrow(aux)==15 ) list(8,11,14) else list(8),
                 command = a
           ) )
  
}

#4. Segundo grupo Tabla resumen resultados total----------------------------------------------------
for (j in c( tab_resultado_2$id ) ) {
  
  aux <- tab_resultado_2 %>%
    filter( id == j) %>%
    dplyr::select( id,
                   cedula,
                   nomina,
                   interes,
                   reserva_matematica,
                   montepio,
                   gastos_adm,
                   total ) %>%
    mutate( nomina = format( nomina,
                             digits = 2, nsmall = 2, big.mark = '.',
                             decimal.mark = ',', format = 'f' ),
            interes = "Valor pendiente",
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
                         'Nomina pagada desde F1 hasta octubre 2023',
                         'Peritaje de la corte',
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

#5. Segundo grupo Tabla resultados del calculo de la reserva matemática-----------------------------
aux <- beneficiarios_v2 %>% 
  dplyr::select( cedula,
                 f1_renta )

reserva_matematica_2 <- reserva_matematica_2 %>% 
  left_join( ., aux, by = 'cedula' )

for (j in c( reserva_matematica_2$id ) ) {
  
  aux <-  reserva_matematica_2 %>%
    filter( id == j) %>%
    dplyr::select( id,
                   cedula,
                   f1_renta,
                   fecha_derecho_ivm,
                   x:=edad,
                   edad_derecho_ivm,
                   renta_concedida:=pension_aumentos,
                   renta_ce,
                   renta_ivm,
                   n,
                   a_x_n,
                   res_mat_temporal,
                   k,
                   a_n_w,
                   res_mat_diferida,
                   a_x,
                   reserva_matematica ) %>%
    mutate( res_mat_temporal = format( res_mat_temporal,
                                       digits = 2, nsmall = 2, big.mark = '.',
                                       decimal.mark = ',', format = 'f' ),
            res_mat_diferida = format( res_mat_diferida,
                                       digits = 2, nsmall = 2, big.mark = '.',
                                       decimal.mark = ',', format = 'f' ),
            reserva_matematica = format( reserva_matematica,
                                         digits = 2, nsmall = 2, big.mark = '.',
                                         decimal.mark = ',', format = 'f' ),
            a_x_n = format( a_x_n,
                            digits = 2, nsmall = 2, big.mark = '.',
                            decimal.mark = ',', format = 'f' ),
            a_n_w  = format( a_n_w,
                             digits = 2, nsmall = 2, big.mark = '.',
                             decimal.mark = ',', format = 'f' ),
            a_x   = format( a_x,
                            digits = 2, nsmall = 2, big.mark = '.',
                            decimal.mark = ',', format = 'f' ),
            renta_ivm = format( renta_ivm,
                                digits = 2, nsmall = 2, big.mark = '.',
                                decimal.mark = ',', format = 'f' ),
            renta_concedida = format( renta_concedida,
                                      digits = 2, nsmall = 2, big.mark = '.',
                                      decimal.mark = ',', format = 'f' ),
            renta_ce = format( renta_ce,
                               digits = 2, nsmall = 2, big.mark = '.',
                               decimal.mark = ',', format = 'f' )
    ) 
  
  aux[c(1:ncol(aux))] <- lapply(aux[c(1:ncol(aux))], function(x) as.character(x))
  
  aux <- aux %>%
    pivot_longer(!c(id) ,names_to = "variable", values_to = "valor") %>%
    mutate( variable = c('Cédula ciudadanía',
                         'F1 Renta',
                         'Fecha de derecho IVM',
                         '$x$',
                         'Edad derecho IVM',
                         'Renta concedida al corte (USD)',
                         'Renta cemento al 2023 (USD)',
                         'Renta IVM al 2023 (USD)',
                         '$n$',
                         '$\\actsymb{\\ddot{a}}{\\nthtop{}{x}:\\angln}$',
                         'Renta matemática temporal (USD)',
                         '$k$',
                         '$k / \\ddot{a}_x$',
                         'Renta matemática diferida (USD)',
                         '$\\ddot{a}_x$',
                         'Reserva matemática (USD)') ) %>%
    dplyr::select(-id) %>%
    filter( !is.na( valor )) %>%
    filter(valor != 'NA')
  
  aux_xtab <- xtable( aux, digits = c( 0, rep(0,2) ) )
  
  aux_xtab <- tildes_a_latex(aux_xtab)
  
  ifelse( nrow(aux)==15,
          a <- c(paste(" \n \\hline \\multicolumn{2}{c}{\\textbf{Renta temporal}} \\\\ \n "), 
                 paste(" \n \\hline \\multicolumn{2}{c}{\\textbf{Renta diferida vitalicia}} \\\\ \n"),
                 paste(" \n \\hline \\multicolumn{2}{c}{\\textbf{Reserva Total}} \\\\ \n") ),
          a <- c(paste(" \n \\hline \\multicolumn{2}{c}{\\textbf{Renta Vitalicia}} \\\\ \n \\hline \n") ) )
  
  print( aux_xtab,
         file = paste0( parametros$resultado_tablas, 'iess_reserva_individual_', j, '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = if( nrow(aux)==15 )  c(8,10,11,13,14,nrow(aux)) else c(9,nrow(aux)) ,
         sanitize.text.function = identity,
         add.to.row = list( pos = if( nrow(aux)==15 ) list(8,11,14) else list(8),
                            command = a
         ) )
  
}
#---------------------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()