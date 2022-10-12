message(paste(rep("-", 100), collapse = ""))
message( '\tLectura' )

# Carga de datos -----------------------------------------------------------------------------------
file_reserva <- paste0( parametros$RData, 'IESS_reserva_matematica.RData' )
file_beneficiarios <- paste0( parametros$RData, 'IESS_beneficiarios.RData' )
file_liquidaciones <- paste0( parametros$RData, 'IESS_nomina_intereses.RData' )
load( file = file_reserva )
load( file = file_beneficiarios )
load( file = file_liquidaciones )
load(paste0( parametros$RData, 'IESS_fallecidos.RData'))


#Cargar función tíldes a latex----------------------------------------------------------------------
source( 'R/503_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )

message( '\tGenerando listado de liquidaciones' )

#1. Lista de beneficiarios--------------------------------------------------------------------------
aux_1 <- reserva_matematica %>%
  left_join(., beneficiarios %>% dplyr::select(id,cedula, nombre), by = c('id', 'cedula') ) %>%
  full_join(., fallecidos %>% dplyr::select(id,cedula, g, nombre, edad, fecha_derecho_ivm), by = c('id',
                                                                                                          'cedula',
                                                                                                          'nombre',
                                                                                                          'edad',
                                                                                                          'fecha_derecho_ivm') ) %>%
  dplyr::select( id,
                 cedula,
                 nombre,
                 edad, 
                 #f1_renta,
                 fecha_derecho_ivm,
                 edad_derecho_ivm,
                 res_mat_temporal,
                 res_mat_diferida,
                 reserva_matematica,
                 montepio)

aux_1 <- aux_1 %>%
  filter( !is.na(nombre))

#2. Tabla resumen de intereses----------------------------------------------------------------------



aux_2 <- interes %>%
  clean_names() %>%
  group_by( cedula ) %>%
  mutate(  liquidacion_pagada  = sum( total_pagado, na.rm = TRUE ),
           interes_liquidacion = sum( interes_liquidacion, na.rm = TRUE ) ) %>%
  ungroup() %>%
  distinct(cedula,.keep_all = TRUE) %>%
  dplyr::select( id,
                 cedula,
                 liquidacion_pagada,
                 interes_liquidacion )

#3. Tabla resumen de liquidaciones------------------------------------------------------------------

aux_3 <- nomina %>%
  group_by( cedula ) %>%
  mutate( nomina  = sum( parte_ce , na.rm = TRUE ) ) %>%
  mutate( interes_nomina  = sum( interes_nomina , na.rm = TRUE ) ) %>%
  ungroup() %>%
  distinct( cedula, .keep_all = TRUE) %>%
  dplyr::select( id, cedula,  nomina,  interes_nomina )


# Total---------------------------------------------------------------------------------------------
tab_resultado <- left_join( aux_1,
                            rbind(aux_2),
                            by = c('id','cedula') ) %>%
  left_join(., (aux_3 %>% filter(id %in% c(1:48))), by = c('id','cedula') ) %>%
  mutate( interes = interes_nomina + interes_liquidacion )
#5. Gatos administrativos---------------------------------------------------------------------------

tab_resultado <- tab_resultado %>%
  replace(is.na(.), 0) %>%
  mutate( gastos_adm = 0.03 * ( liquidacion_pagada + interes + reserva_matematica + montepio + nomina ) ) %>%
  mutate( total = liquidacion_pagada + interes + reserva_matematica + montepio + nomina + gastos_adm )

#Guardar en Rdata-----------------------------------------------------------------------------------
message( '\tGuardando reservas matemáticas' )

save( tab_resultado,
      file = paste0( parametros$RData, 'IESS_tab_resultado.RData' ) )

# Borrar elementos restantes -----------------------------------------------------------------------
message(paste(rep("-", 100), collapse = ""))
rm(list = ls()[!(ls() %in% c("parametros"))])
gc()

