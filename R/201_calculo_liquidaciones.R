message(paste(rep("-", 100), collapse = ""))

message("\tCargando datos para cálculo liquidaciones de pensiones a la fecha")
load(paste0(parametros$RData, "IESS_beneficiarios.RData"))
load(paste0( parametros$RData, 'IESS_actualizacion_pensiones.RData'))

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

#Guardar en un RData--------------------------------------------------------------------------------
message( '\tGuardando liquidaciones a jubilados' )

save( liquidacion,
      file = paste0( parametros$RData, 'IESS_liquidacion.RData' ) )

# Borrar elementos restantes -----------------------------------------------------------------------
message(paste(rep("-", 100), collapse = ""))
rm(list = ls()[!(ls() %in% c("parametros"))])
gc()