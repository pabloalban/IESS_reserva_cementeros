message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tLectura de la proyección de precios' )

# Carga de datos -----------------------------------------------------------------------------------
load(paste0(parametros$RData, "IESS_beneficiarios.RData"))
load(paste0(parametros$RData, "IESS_liquidacion.RData"))


#Cargar función tíldes a latex----------------------------------------------------------------------
source( 'R/503_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )

message( '\tGenerando fechas de cese de beneficiarios de liquidaciones' )
#1. Tabla resumen recaudación total-----------------------------------------------------------------
for (j in c(1:nrow(beneficiarios))) {
  
  aux <- beneficiarios %>%
    filter( id == j) %>%
    dplyr::select( cedula,
                   sexo:=g,
                   fecha_de_nacimiento,
                   edad,
                   anios_imposiciones,
                   ultimo_sueldo,
                   ric_ivm,
                   fecha_derecho_ivm,
                   edad_derecho_ivm,
                   liquidacion_1,
                   fecha_liquidacion_1,
                   liquidacion_2,
                   fecha_liquidacion_2) %>%
    mutate( sexo = if_else( sexo == 'M', 'Masculino', 'Femenino'),
            ultimo_sueldo = format( ultimo_sueldo,
                                  digits = 2, nsmall = 2, big.mark = '.',
                                  decimal.mark = ',', format = 'f' ),
              ric_ivm = format( ric_ivm,
                                digits = 2, nsmall = 2, big.mark = '.',
                                decimal.mark = ',', format = 'f' ),
            liquidacion_1 = format( liquidacion_1,
                              digits = 2, nsmall = 2, big.mark = '.',
                              decimal.mark = ',', format = 'f' ),            
            liquidacion_2 = format( liquidacion_2,
                              digits = 2, nsmall = 2, big.mark = '.',
                              decimal.mark = ',', format = 'f' )
            
            ) 
  
  aux[c(1:ncol(aux))] <- lapply(aux[c(1:ncol(aux))], function(x) as.character(x))
  
  aux <- aux %>%
    pivot_longer(!cedula,names_to = "variable", values_to = "valor") %>%
    dplyr::select(-cedula)
  
  aux_xtab <- xtable( aux, digits = c( 0, 0, 0 ) )

#  xtb_aux <- tildes_a_latex(xtb_aux)
  
  print( aux_xtab, 
         file = paste0( parametros$resultado_tablas, 'iess_datos_id_',j,'.tex' ),
         type = 'latex',
         include.colnames = FALSE, 
         include.rownames = FALSE,
         format.args = list( decimal.mark = ',', big.mark = '.' ),
         only.contents = TRUE,
         sanitize.text.function = identity)
  
}
#---------------------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()