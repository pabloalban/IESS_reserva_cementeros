message(paste(rep("-", 100), collapse = ""))

message("\tCargando datos para actualizar las pensiones a la fecha")
load(paste0(parametros$RData, "IESS_beneficiarios.RData"))
load(paste0( parametros$RData, 'IESS_sbu.RData'))
load(paste0( parametros$RData, 'IESS_pensiones_max_min.RData'))
load(paste0( parametros$RData, 'IESS_crecimiento_pensiones.RData'))
load(paste0( parametros$RData, 'IESS_fallecidos.RData'))
load(paste0( parametros$RData, 'IESS_nomina_concesiones.RData'))


message("\tEstableciendo pensiones máximas y mínimas")
#Límites de las pensiones---------------------------------------------------------------------------

pensiones_limites <- expand.grid(anio = seq(2007, 2022, 1),imposiciones = pensiones_max_min$imposiciones) %>%
  left_join(., sbu, by='anio') %>%
  left_join(., pensiones_max_min, by='imposiciones') %>%
  mutate( pension_max = sbu * max_sbu,
          pension_min = sbu * min_sbu )

#Generación de la malla-----------------------------------------------------------------------------
pensiones <- rbind(beneficiarios,
                   fallecidos %>% dplyr::select(-fecha_defuncion))%>%
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
                 coef,
              edad,
              n,
              k) %>%
  mutate( i = 2022 - anio_f1 + 1) %>%
  mutate( imposiciones = as.integer( imposiciones ) ) %>%
  slice(rep(1:n(),i)) %>%
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
  ungroup() %>%
  dplyr::select(-i)

# Pensiones de nomina-------------------------------------------------------------------------------
actualizacion_pensiones <- actualizacion_pensiones %>%
  dplyr::select(-renta_concedida) %>%
  filter( anio == '2022')

aux <- nomina %>%
  filter( periodo == as.Date( "01/08/2022", "%d/%m/%Y" ) ) %>%
  dplyr::select(cedula, renta_concedida:=pension_aumentos)


actualizacion_pensiones <- actualizacion_pensiones %>%
  left_join(., aux, by = 'cedula')


#Guardar en un RData--------------------------------------------------------------------------------
message( '\tGuardando pensiones de los jubilados' )

save( actualizacion_pensiones,
      file = paste0( parametros$RData, 'IESS_actualizacion_pensiones.RData' ) )

# Borrar elementos restantes -----------------------------------------------------------------------
message(paste(rep("-", 100), collapse = ""))
rm(list = ls()[!(ls() %in% c("parametros"))])
gc()