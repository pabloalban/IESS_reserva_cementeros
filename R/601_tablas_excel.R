message(paste(rep("-", 100), collapse = ""))
message( '\tExporta a excel' )

# Carga de datos -----------------------------------------------------------------------------------
load(paste0(parametros$RData, "IESS_tab_resultado.RData"))
load(paste0(parametros$RData, "IESS_presupuesto_2023.RData"))

#Cargar función tíldes a latex----------------------------------------------------------------------
source( 'R/503_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )

message( '\tGenerando fechas de cese de beneficiarios de liquidaciones' )
#1. Tabla resumen recaudación total-----------------------------------------------------------------


write.xlsx(tab_resultado,
           file = paste0( parametros$resultado_seguro , 'tab_resultados.xlsx' ),
           sheetName = "reserva", 
           col.names = TRUE,
           row.names = TRUE, 
           append = FALSE)

write.xlsx(reserva_matematica,
           file = paste0( parametros$resultado_seguro , 'IESS_presupuesto_2023.xlsx' ),
           sheetName = "presupuesto", 
           col.names = TRUE,
           row.names = TRUE, 
           append = FALSE)



#---------------------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()