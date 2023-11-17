message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tLectura de resultados' )

# Carga de datos -----------------------------------------------------------------------------------
load( paste0( parametros$RData, 'IESS_tab_resultado.RData' ) )
load( paste0( parametros$RData, "IESS_beneficiarios.RData" ) )

#Cargar función tíldes a latex----------------------------------------------------------------------
source( 'R/503_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )

message( '\tGenerando tabla de resultados' )
#1. Tabla resultados--------------------------------------------------------------------------------

aux <- tab_resultado %>%
  dplyr::select( -nombre,
                 -res_mat_temporal,
                 -res_mat_diferida,
                 -interes)
n <- nrow(aux)

aux <- rbind((aux), c("Total", NA, NA, NA, NA, as.character(colSums(aux[,6:ncol(aux)]) ) ) )
aux[c(6:ncol(aux))] <- lapply(aux[c(6:ncol(aux))], function(x) as.numeric(x))

aux <- aux %>%
  dplyr::select(-id) %>%
  mutate( fecha_derecho_ivm  = as.character( fecha_derecho_ivm  ) )

aux_xtab <- xtable( aux, digits = c( 0, rep(0, 5), rep(2,7) ) )
  
print( aux_xtab, 
         file = paste0( parametros$resultado_tablas, 'iess_tab_resultado', '.tex' ),
         type = 'latex',
         include.colnames = FALSE,
         include.rownames = FALSE,
         format.args = list( decimal.mark = ',', big.mark = '.' ),
         only.contents = TRUE,
         hline.after = c( n ),
         sanitize.text.function = identity)


#2. Tabla de resultados del segundo pedido----------------------------------------------------------
aux_a <- beneficiarios_v2 %>% 
  mutate( ocupacion_plani = toupper( ocupacion_plani ) ) %>% 
  dplyr::select( cedula, 
                 ocupacion_plani )

aux <- tab_resultado_2 %>%
  dplyr::select( -nombre,
                 -res_mat_temporal,
                 -res_mat_diferida,
                 -interes) %>% 
  left_join( ., aux_a, by ='cedula' ) %>% 
  dplyr::select(-id)

n <- nrow(aux)

aux <- rbind( ( aux ), c( "Total", NA, NA, NA, as.character( colSums( aux[,5: ( ncol(aux)-1 ) ] ) ), NA ) )
aux[ c( 5: ( ncol( aux ) - 1 ) ) ] <- lapply( aux[ c( 5: ( ncol( aux ) - 1 ) ) ], function(x) as.numeric(x) )

aux <- aux %>%
  mutate( fecha_derecho_ivm  = as.character( fecha_derecho_ivm  ) )

aux_xtab <- xtable( aux, digits = c( 0, rep( 0, 4 ), rep( 2, 8 ), 0 ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_tab_resultado_2', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( n ),
       sanitize.text.function = identity)

#---------------------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
