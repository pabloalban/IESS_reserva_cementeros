message(paste(rep("-", 100), collapse = ""))

message("\tCargando datos para la reserva matmática")
load(paste0(parametros$RData, "IESS_beneficiarios.RData"))
load(paste0( parametros$RData, 'IESS_actualizacion_pensiones.RData'))
load(paste0( parametros$RData, 'IESS_tabla_mortalidad.RData'))

message("\tCalculando reserva matemática")

#Calculo de intereses Primer pago-------------------------------------------------------------------
pensiones <- actualizacion_pensiones %>%
  filter( anio == 2022) %>%
  dplyr::select( cedula,
                 renta_concedida )

reserva_matematica <- beneficiarios %>%
  dplyr::select( cedula,
                 fecha_de_nacimiento,
                 g ) %>%
  mutate(edad =round(age_calc(fecha_de_nacimiento,
                                       enddate = as.Date("31/08/2022","%d/%m/%Y"),
                                       units = "years"),0)) %>%
  left_join(., tabla_mortalidad, by=c('edad','g')) %>%
  left_join(., pensiones, by = 'cedula') %>%
  mutate( reserva_matematica = 13 * a_x * renta_concedida)

#Guardar en un RData--------------------------------------------------------------------------------
message( '\tGuardando intereses para IVM' )

save( interes,
      file = paste0( parametros$RData, 'IESS_reserva_matematica.RData' ) )

# Borrar elementos restantes -----------------------------------------------------------------------
message(paste(rep("-", 100), collapse = ""))
rm(list = ls()[!(ls() %in% c("parametros"))])
gc()