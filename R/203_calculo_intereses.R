message(paste(rep("-", 100), collapse = ""))

message("\tCargando datos para cálculo liquidaciones de pensiones a la fecha")
load(paste0(parametros$RData, "IESS_beneficiarios.RData"))
load(paste0( parametros$RData, 'IESS_actualizacion_pensiones.RData'))
load(paste0( parametros$RData, 'IESS_liquidacion.RData'))

message("\tCalculando intereses")

#Calculo de intereses Primer pago-------------------------------------------------------------------

interes_1 <- beneficiarios %>%
  dplyr::select( cedula,
                 liquidacion_1,
                 fecha_liquidacion_1 ) %>%
  mutate( anio_f1 = year( fecha_liquidacion_1 ),
          mes_f1 =month( fecha_liquidacion_1 ) ) %>%
  mutate( anio_inicio = year( fecha_liquidacion_1 ),
          mes_inicio = month( fecha_liquidacion_1 ) ) %>%
  mutate( anio_fin = 2022,
          mes_fin = 8 ) %>%
  mutate( n = anio_fin - anio_inicio + 1 ) %>%
  slice(rep(1:n(), n )) %>%
  group_by( cedula ) %>%
  mutate(anio = anio_inicio:anio_fin) %>%
  ungroup() %>%
  mutate( mes_inicio = if_else( anio == anio_inicio, mes_inicio, 1 ),
          mes_fin = if_else( anio == anio_fin, mes_fin, 12 ) ) %>%
  mutate( mes_fin = ifelse( anio == 2022, 8, mes_fin ) ) %>%
  mutate( n = mes_fin - mes_inicio + 1 ) %>%
  group_by( cedula, anio ) %>%
  slice(rep(1:n(), n )) %>%
  mutate(mes = mes_inicio:mes_fin) %>%
  ungroup() %>%
  mutate( tasa_interes = if_else( anio == 2022, 0.0821, 0.0821 ) ) %>%
  mutate( i = ( 1 + tasa_interes/12 ) ) %>%
  mutate( i = if_else( anio == anio_fin & mes == mes_fin, 1, i ) ) %>%
  group_by( cedula ) %>%
  mutate( indice = cumprod( i ) ) %>%
  ungroup() %>%
  mutate( interes = liquidacion_1 * (indice) - liquidacion_1 ) %>%
  group_by( cedula ) %>%
  mutate( n_meses = n() - 1 ) %>%
  ungroup() %>%
  filter( anio == 2022, mes == 8 ) %>%
  dplyr::select( cedula,
                 liquidacion:=liquidacion_1,
                 fecha_liquidacion:=fecha_liquidacion_1,
                 interes,
                 n_meses ) %>%
  mutate( pago = 'liquidacion 1')

  
#Calculo de intereses Segundo pago------------------------------------------------------------------

interes_2 <- beneficiarios %>%
  dplyr::select( cedula,
                 liquidacion_2,
                 fecha_liquidacion_2 ) %>%
  filter( !is.na( fecha_liquidacion_2 ) ) %>%
  mutate( anio_f1 = year( fecha_liquidacion_2 ),
          mes_f1 =month( fecha_liquidacion_2 ) ) %>%
  mutate( anio_inicio = year( fecha_liquidacion_2 ),
          mes_inicio = month( fecha_liquidacion_2 ) ) %>%
  mutate( anio_fin = 2022,
          mes_fin = 8 ) %>%
  mutate( n = anio_fin - anio_inicio + 1 ) %>%
  slice(rep(1:n(), n )) %>%
  group_by( cedula ) %>%
  mutate(anio = anio_inicio:anio_fin) %>%
  ungroup() %>%
  mutate( mes_inicio = if_else( anio == anio_inicio, mes_inicio, 1 ),
          mes_fin = if_else( anio == anio_fin, mes_fin, 12 ) ) %>%
  mutate( mes_fin = ifelse( anio == 2022, 8, mes_fin ) ) %>%
  mutate( n = mes_fin - mes_inicio + 1 ) %>%
  group_by( cedula, anio ) %>%
  slice(rep(1:n(), n )) %>%
  mutate(mes = mes_inicio:mes_fin) %>%
  ungroup() %>%
  mutate( tasa_interes = if_else( anio == 2022, 0.0821, 0.0821 ) ) %>%
  mutate( i = ( 1 + tasa_interes/12 ) ) %>%
  mutate( i = if_else( anio == anio_fin & mes == mes_fin, 1, i ) ) %>%
  group_by( cedula ) %>%
  mutate( indice = cumprod( i ) ) %>%
  ungroup() %>%
  mutate( interes = liquidacion_2 * (indice) - liquidacion_2 ) %>%
  group_by( cedula ) %>%
  mutate( n_meses = n() - 1 ) %>%
  ungroup() %>%
  filter( anio == 2022, mes == 8 )  %>%
  dplyr::select( cedula,
                 liquidacion:=liquidacion_2,
                 fecha_liquidacion:=fecha_liquidacion_2,
                 interes,
                 n_meses ) %>%
  mutate( pago = 'liquidacion 2')
  
#Concatenar tablas----------------------------------------------------------------------------------
  
interes <- rbind( interes_1,
                      interes_2 )

#Guardar en un RData--------------------------------------------------------------------------------
message( '\tGuardando intereses para IVM' )

save( interes,
      file = paste0( parametros$RData, 'IESS_interes.RData' ) )

# Borrar elementos restantes -----------------------------------------------------------------------
message(paste(rep("-", 100), collapse = ""))
rm(list = ls()[!(ls() %in% c("parametros"))])
gc()