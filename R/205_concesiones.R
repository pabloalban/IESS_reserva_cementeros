
message(paste(rep("-", 100), collapse = ""))

message("\tCargando datos para actualizar las pensiones a la fecha")
load(paste0(parametros$RData, "IESS_nomina_concesiones.RData"))

#Liquidaciones e intereses--------------------------------------------------------------------------

message("\tCalculando intereses de las liquidaciones")


interes <- liquidacion %>%
  dplyr::select( id,
                 cedula,
                 fecha_pago,
                 total_pagado) %>%
  mutate( fecha_fin = as.Date( "30/09/2022", "%d/%m/%Y")) %>%
  mutate( n = lubridate::interval(liquidacion$fecha_pago, fecha_fin ) %/% months(1) ) %>%
  mutate( tasa_interes = 8.21/100 ) %>%
  mutate( i = ( 1 + tasa_interes/12 )^n ) %>%
  mutate( interes =  total_pagado * (i) - total_pagado ) %>%
  group_by( cedula ) %>%
  mutate( i = 1:n() ) %>%
  ungroup() %>%
  mutate( pago = if_else( i==1, "Primer pago", "Segundo pago") ) %>%
  dplyr::select( id,
                 pago,
                 cedula,
                 fecha_pago,
                 n,
                 tasa_interes,
                 total_pagado,
                 interes    )

#nomina desde 2021----------------------------------------------------------------------------------


message("\tCalculando")
#Guardar en un RData--------------------------------------------------------------------------------
message( '\tGuardando liquidaciones a jubilados fallecidos' )

save( liquidacion,
      interes,
      file = paste0( parametros$RData, 'IESS_liquidacion_intereses_fallecidos.RData' ) )

# Borrar elementos restantes -----------------------------------------------------------------------
message(paste(rep("-", 100), collapse = ""))
rm(list = ls()[!(ls() %in% c("parametros"))])
gc()