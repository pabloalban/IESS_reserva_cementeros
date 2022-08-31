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
for (j in c(1:nrow(beneficiarios))) {
  
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
    dplyr::select(-id)
  
  aux_xtab <- xtable( aux, digits = c( 0, 0, 0 ) )
  
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
                   interes)
  
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
         hline.after = nrow(aux) - 1,
         sanitize.text.function = identity )
  
}


#3. Tabla resultados del calculo de la reserva matemática-------------------------------------------

for (j in c(1:nrow(beneficiarios))) {
  
  aux <-  reserva_matematica %>%
    filter( id == j) %>%
    dplyr::select( id,
                   cedula,
                   f1_renta,
                   fecha_derecho_ivm,
                   x:=edad,
                   edad_derecho_ivm,
                   n,
                   a_x_n,
                   res_mat_temporal,
                   k,
                   k_a_x,
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
            k_a_x  = format( k_a_x,
                                         digits = 2, nsmall = 2, big.mark = '.',
                                         decimal.mark = ',', format = 'f' ),
            a_x   = format( a_x,
                                         digits = 2, nsmall = 2, big.mark = '.',
                                         decimal.mark = ',', format = 'f' )
            
            ) 
  
  aux[c(1:ncol(aux))] <- lapply(aux[c(1:ncol(aux))], function(x) as.character(x))
  
  aux <- aux %>%
    pivot_longer(!c(id) ,names_to = "variable", values_to = "valor") %>%
    dplyr::select(-id) %>%
    filter( !is.na( valor )) %>%
    filter(valor != 'NA')
  
  write.table(aux, file = paste0( parametros$resultado_tablas, 'iess_resultado_individual_',j,'.tex' ),
              sep = ",",
              col.names = FALSE,
              row.names = FALSE,
              quote = FALSE,
              qmethod = "double")
  
  
  aux_xtab <- xtable( aux, digits = c( 0, rep(0,2) ) )
  
  aux_xtab <- tildes_a_latex(aux_xtab)
  
  ifelse( nrow(aux)==12,
          a <- c(paste(" \n \\multicolumn{2}{c}{\\textbf{Renta temporal}} \\\\ \n \\hline \n"), 
                 paste(" \n \\hline \\multicolumn{2}{c}{\\textbf{Renta diferida vitalicia}} \\\\ \n"),
                 paste(" \n \\hline \\multicolumn{2}{c}{\\textbf{Reserva Total}} \\\\ \n") ),
          a <- c(paste(" \n \\hline \\multicolumn{2}{c}{\\textbf{Renta Vitalicia}} \\\\ \n") ) )
  
  print( aux_xtab,
         file = paste0( parametros$resultado_tablas, 'iess_resultado_individual_', j, '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = ifelse( nrow(aux)==12, c(5,8,11), c(6) ),
         sanitize.text.function = identity,
         add.to.row = list( pos = if( nrow(aux)==12 ) list(5,8,11) else list(6),
                 command = a
           ) )
  
}


#---------------------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()