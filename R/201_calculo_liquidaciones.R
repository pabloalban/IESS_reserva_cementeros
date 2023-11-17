message(paste(rep("-", 100), collapse = ""))

message( "\tCargando datos para cálculo liquidaciones de pensiones a la fecha" )
load(paste0( parametros$RData, "IESS_beneficiarios.RData" ) )
load(paste0( parametros$RData, 'IESS_actualizacion_pensiones.RData') )
load(paste0( parametros$RData, 'IESS_nomina_concesiones.RData' ) )

message("\tCalculando liquidaciones")

#Pensiones adeudadas--------------------------------------------------------------------------------

liquidacion <- actualizacion_pensiones %>%
  dplyr::select( id,
                 cedula,
                 f1_renta,
                anio,
                sbu,
                renta_concedida,
                fecha_derecho_ivm,
                pension_max,
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
                pension_max,
                renta_concedida,
                decima_tercera,
                decima_cuarta,
                total_a_pagar,
                parte_ivm,
                parte_ce)

#Listado de pensiones del cemento e IVM del segundo grupo-------------------------------------------

aux <- beneficiarios_v2 %>% 
  dplyr::select( id,
                 cedula, 
                 f1_renta,
                 fecha_derecho_ivm ) %>% 
  mutate( pension_max = NA )

liquidacion_v2 <- nomina_cem_v2 %>% 
  group_by( cedula ) %>% 
  mutate( renta_concedida = sum( valor_pension_y_aumentos , na.rm = TRUE ),
          decima_tercera = sum( valor_decimo_tercera_pension, na.rm = TRUE ),
          decima_cuarta = sum( valor_decimo_cuarta_pension, na.rm = TRUE ),
          total_a_pagar = sum( valor_total_pagado, na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  distinct( ., cedula, .keep_all = TRUE ) %>% 
  mutate( parte_ivm = 0,
          parte_ce = total_a_pagar ) %>% 
  left_join( ., aux, by = 'cedula' ) %>% 
  dplyr::select(periodo,
                id,
                cedula,
                f1_renta,
                fecha_derecho_ivm,
                pension_max,
                renta_concedida,
                decima_tercera,
                decima_cuarta,
                total_a_pagar,
                parte_ivm,
                parte_ce)
#Guardar en un RData--------------------------------------------------------------------------------
message( '\tGuardando liquidaciones a jubilados' )

save( liquidacion,
      liquidacion_v2,
      file = paste0( parametros$RData, 'IESS_liquidacion.RData' ) )

# Borrar elementos restantes -----------------------------------------------------------------------
message(paste(rep("-", 100), collapse = ""))
rm(list = ls()[!(ls() %in% c("parametros"))])
gc()