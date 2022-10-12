
message(paste(rep("-", 100), collapse = ""))

message("\tCargando datos para actualizar las pensiones a la fecha")
load(paste0(parametros$RData, "IESS_nomina_concesiones.RData"))
load(paste0(parametros$RData, "IESS_beneficiarios.RData"))
load(paste0( parametros$RData, 'IESS_fallecidos.RData'))

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
  mutate( interes_liquidacion =  total_pagado * (i) - total_pagado ) %>%
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
                 interes_liquidacion    )

#nomina desde 2021----------------------------------------------------------------------------------
aux <- nomina  %>%
  filter( periodo == as.Date("2022-08-01", "%Y-%m-%d") ) %>%
  mutate( periodo = as.Date("2022-09-01", "%Y-%m-%d") )

aux_1 <- beneficiarios %>% select(id, cedula, fecha_derecho_ivm, ultimo_sueldo, ric_ivm)

aux_2 <- fallecidos %>% select(id, cedula, fecha_derecho_ivm, ultimo_sueldo, ric_ivm)

aux_3 <- rbind( aux_1, aux_2) %>%
  mutate( pension_concedida = max(ultimo_sueldo, ric_ivm)) %>%
  mutate( coef = ric_ivm / pension_concedida ) %>%
  dplyr::select(id, cedula, fecha_derecho_ivm, ultimo_sueldo, coef)

nomina <- rbind( nomina, aux ) %>%
  mutate( fecha_fin = as.Date( "30/09/2022", "%d/%m/%Y")) %>%
  mutate( n = lubridate::interval(nomina$periodo, fecha_fin ) %/% months(1) ) %>%
  mutate( n = ifelse( periodo == as.Date( "2022-09-01", "%Y-%m-%d"), 
                      n == 0,
                      n ) ) %>%
  mutate( tasa_interes = 8.21/100 ) %>%
  mutate( i = ( 1 + tasa_interes/12 )^n ) %>%
  left_join(., aux_3, by = c('id', 'cedula')) %>%
  #mutate( parte_ce =  (1 - coef) * ( pension_aumentos + decimo_tercera_pension ) ) %>%
  mutate( parte_ce = ifelse( periodo < fecha_derecho_ivm,  
                             total_pagado,
                             (1 - coef) * ( pension_aumentos + decimo_tercera_pension ) ) ) %>%
  mutate( interes_nomina =  parte_ce * (i) - parte_ce ) %>%
  arrange( cedula, periodo ) %>%
  group_by( cedula ) %>%
  mutate( i = 1:n() ) %>%
  ungroup() %>%
  dplyr::select( id,
                 cedula,
                 fecha_pago := periodo,
                 fecha_derecho_ivm,
                 ultimo_sueldo,
                 pension_aumentos,
                 decimo_tercera_pension,
                 decimo_cuarta_pension,
                 #n,
                 tasa_interes,
                 total_pagado,
                 parte_ce,
                 interes_nomina )


message("\tCalculando")
#Guardar en un RData--------------------------------------------------------------------------------
message( '\tGuardando liquidaciones a jubilados fallecidos' )

save( nomina,
      interes,
      file = paste0( parametros$RData, 'IESS_nomina_intereses.RData' ) )

# Borrar elementos restantes -----------------------------------------------------------------------
message(paste(rep("-", 100), collapse = ""))
rm(list = ls()[!(ls() %in% c("parametros"))])
gc()