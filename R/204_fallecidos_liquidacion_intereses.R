
message(paste(rep("-", 100), collapse = ""))

message("\tCargando datos para actualizar las pensiones a la fecha")
load(paste0(parametros$RData, "IESS_fallecidos.RData"))
load(paste0( parametros$RData, 'IESS_sbu.RData'))
load(paste0( parametros$RData, 'IESS_pensiones_max_min.RData'))
load(paste0( parametros$RData, 'IESS_crecimiento_pensiones.RData'))

message("\tEstableciendo pensiones máximas y mínimas")
#Límites de las pensiones---------------------------------------------------------------------------

pensiones_limites <- expand.grid(anio = seq(2007, 2022, 1),imposiciones = pensiones_max_min$imposiciones) %>%
  left_join(., sbu, by='anio') %>%
  left_join(., pensiones_max_min, by='imposiciones') %>%
  mutate( pension_max = sbu * max_sbu,
          pension_min = sbu * min_sbu )

#Generación de la malla-----------------------------------------------------------------------------
pensiones <- fallecidos %>%
  mutate( anio_f1 = year(f1_renta)) %>%
  group_by( cedula ) %>%
  mutate( pension_concedida = max(ultimo_sueldo, ric_ivm)) %>%
  ungroup() %>%
  mutate( coef = ric_ivm / pension_concedida ) %>%
  dplyr::select( id,
                 cedula,
                 g,
                 pension_concedida,
                 anio_f1,
                 imposiciones:= (anios_imposiciones),
                 f1_renta,
                 edad,
                 edad_derecho_ivm,
                 fecha_derecho_ivm,
                 fecha_defuncion,
                 coef,
                 edad,
                 n,
                 k) %>%
  mutate( n = 2022 - anio_f1 + 1) %>%
  mutate( imposiciones = as.integer( imposiciones ) ) %>%
  slice(rep(1:n(),n)) %>%
  group_by(cedula) %>%
  mutate(contador = 1:n()) %>%
  mutate(anio = contador + anio_f1 - 1) %>%
  ungroup()

#Actualización de las pensiones---------------------------------------------------------------------
actualizacion_pensiones <- pensiones %>% 
  left_join(., pensiones_limites, by = c('anio', 'imposiciones') )  %>%
  left_join(., crecimiento_pensiones, by = 'anio') %>%
  mutate( i_s = if_else( contador == 1, 0, i_s ) ) %>%
  mutate( i_s = 1 + i_s ) %>%
  group_by(cedula) %>%
  mutate( indice_prod = cumprod(i_s) ) %>%
  mutate( renta_calculada = pension_concedida * indice_prod ) %>%
  ungroup() %>%
  group_by( cedula, contador ) %>%
  mutate( renta_concedida = max( min( renta_calculada, pension_max), pension_min) ) %>% 
  ungroup()




message("\tCalculando liquidaciones")

#Pensiones adeudadas--------------------------------------------------------------------------------

liquidacion <- actualizacion_pensiones %>%
  dplyr::select( id,
                 cedula,
                 f1_renta,
                 anio,
                 sbu,
                 renta_concedida,
                 fecha_defuncion,
                 fecha_derecho_ivm,
                 coef) %>%
  mutate( anio_f1 = year( f1_renta ),
          mes_f1 =month( f1_renta ),
          mes_inicio = 1,
          mes_fin = 12 ) %>%
  mutate( mes_inicio = ifelse(anio == anio_f1, mes_f1, mes_inicio  )) %>%
  mutate( mes_fin = ifelse( anio == 2022, 8, mes_fin ) ) %>%
  mutate( n = mes_fin - mes_inicio + 1 ) %>%
  slice(rep(1:n(), n )) %>%
  group_by( cedula, anio ) %>%
  mutate(mes = mes_inicio:mes_fin) %>%
  ungroup() %>%
  mutate( decima_tercera = if_else( mes == 8, sbu, 0 ),
          decima_cuarta = if_else( mes == 12, renta_concedida, 0 ) ) %>%
  mutate( total_a_pagar = renta_concedida + decima_tercera + decima_cuarta ) %>%
  mutate( parte_ivm = coef * total_a_pagar ) %>%
  mutate( periodo = as.Date( paste0(anio, '/', mes, '/01'), '%Y/%m/%d' ) ) %>%
  mutate( parte_ivm = if_else( periodo < fecha_derecho_ivm, 0, parte_ivm) ) %>%
  mutate( parte_ce = total_a_pagar - parte_ivm ) %>%
  dplyr::select(periodo,
                id,
                cedula,
                f1_renta,
                fecha_derecho_ivm,
                renta_concedida,
                decima_tercera,
                decima_cuarta,
                total_a_pagar,
                parte_ivm,
                parte_ce)



#Pensiones adeudadas--------------------------------------------------------------------------------

liquidacion <- actualizacion_pensiones %>%
  dplyr::select( id,
                 cedula,
                 f1_renta,
                 anio,
                 sbu,
                 renta_concedida,
                 fecha_derecho_ivm,
                 fecha_defuncion,
                 coef) %>%
  group_by( cedula ) %>%
  filter( anio <= year( fecha_defuncion ) ) %>%
  ungroup() %>%
  mutate( anio_f1 = year( f1_renta ),
          mes_f1 = month( f1_renta ),
          mes_inicio = 1,
          mes_fin = month( fecha_defuncion ),
          anio_fin = year( fecha_defuncion ) ) %>%
  mutate( mes_inicio = ifelse(anio == anio_f1, mes_f1, mes_inicio  )) %>%
  mutate( mes_fin = ifelse(anio == anio_fin, mes_fin, 12  )) %>%
  mutate( n = mes_fin - mes_inicio + 1 ) %>%
  slice(rep(1:n(), n )) %>%
  group_by( cedula, anio ) %>%
  mutate(mes = mes_inicio:mes_fin) %>%
  ungroup() %>%
  mutate( renta_concedida = ifelse( anio==anio_fin & mes==mes_fin, renta_concedida * day(fecha_defuncion)/30, renta_concedida ) ) %>%
  mutate( decima_tercera = if_else( mes == 8, sbu, 0 ),
          decima_cuarta = if_else( mes == 12, renta_concedida, 0 ) ) %>%
  mutate( total_a_pagar = renta_concedida + decima_tercera + decima_cuarta ) %>%
  mutate( parte_ivm = coef * total_a_pagar ) %>%
  mutate( periodo = as.Date( paste0(anio, '/', mes, '/01'), '%Y/%m/%d' ) ) %>%
  mutate( parte_ivm = if_else( periodo < fecha_derecho_ivm, 0, parte_ivm) ) %>%
  mutate( parte_ce = total_a_pagar - parte_ivm ) %>%
  dplyr::select(periodo,
                id,
                cedula,
                f1_renta,
                fecha_defuncion,
                fecha_derecho_ivm,
                renta_concedida,
                decima_tercera,
                decima_cuarta,
                total_a_pagar,
                parte_ivm,
                parte_ce)


#Calculo de intereses Primer pago-------------------------------------------------------------------

interes_1 <- fallecidos %>%
  dplyr::select( id,
                 cedula,
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
  dplyr::select( id,
                 cedula,
                 liquidacion:=liquidacion_1,
                 fecha_liquidacion:=fecha_liquidacion_1,
                 interes,
                 n_meses ) %>%
  mutate( pago = 'liquidacion 1')


#Concatenar tablas----------------------------------------------------------------------------------

interes <- rbind( interes_1 )


#Guardar en un RData--------------------------------------------------------------------------------
message( '\tGuardando liquidaciones a jubilados fallecidos' )

save( liquidacion,
      interes,
      file = paste0( parametros$RData, 'IESS_liquidacion_intereses_fallecidos.RData' ) )

# Borrar elementos restantes -----------------------------------------------------------------------
message(paste(rep("-", 100), collapse = ""))
rm(list = ls()[!(ls() %in% c("parametros"))])
gc()