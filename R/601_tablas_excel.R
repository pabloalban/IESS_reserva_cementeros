message(paste(rep("-", 100), collapse = ""))
message( '\tExporta a excel' )

# Carga de datos -----------------------------------------------------------------------------------
load( paste0( parametros$RData, "IESS_tab_resultado.RData" ) )
load( paste0( parametros$RData, "IESS_presupuesto_2023.RData" ) )
load( paste0( parametros$RData, "IESS_beneficiarios.RData" ) )

#Cargar función tíldes a latex----------------------------------------------------------------------
source( 'R/503_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )

message( '\tGenerando fechas de cese de beneficiarios de liquidaciones' )
#1. Tabla resumen recaudación total-----------------------------------------------------------------


write.xlsx( tab_resultado,
           file = paste0( parametros$resultado_seguro , 'tab_resultados.xlsx' ),
           sheetName = "reserva", 
           col.names = TRUE,
           row.names = TRUE, 
           append = FALSE)

write.xlsx( reserva_matematica,
           file = paste0( parametros$resultado_seguro , 'IESS_presupuesto_2023.xlsx' ),
           sheetName = "presupuesto", 
           col.names = TRUE,
           row.names = TRUE, 
           append = FALSE)


aux <- beneficiarios_v2 %>% 
  mutate( ocupacion_plani = toupper( ocupacion_plani ) ) %>% 
  dplyr::select( cedula, 
                 ocupacion_plani )


aux <- tab_resultado_2 %>%
  dplyr::select( -nombre,
                 -res_mat_temporal,
                 -res_mat_diferida,
                 -interes) %>% 
  left_join( ., aux, by ='cedula' ) %>% 
  dplyr::select(-id)

n <- nrow(aux)

aux <- rbind( ( aux ), c( "Total", NA, NA, NA, as.character( colSums( aux[,5: ( ncol(aux)-1 ) ] ) ), NA ) )
aux[ c( 5: ( ncol( aux ) - 1 ) ) ] <- lapply( aux[ c( 5: ( ncol( aux ) - 1 ) ) ], function(x) as.numeric(x) )

aux <- aux %>%
  mutate( fecha_derecho_ivm  = as.character( fecha_derecho_ivm  ) )


write.xlsx( aux,
            file = paste0( parametros$resultado_seguro , 'tab_resultados_v2.xlsx' ),
            sheetName = "reserva", 
            col.names = TRUE,
            row.names = TRUE, 
            append = FALSE)

#---------------------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()