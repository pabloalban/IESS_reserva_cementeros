message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLectura de lista de beneficarios' )

#Cargando información financiera--------------------------------------------------------------------
file<-paste0(parametros$Data, 'IESS_beneficiarios.xlsx' )

#Cargar función tíldes a latex----------------------------------------------------------------------
source( 'R/503_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )

#Listado de beneficiarios---------------------------------------------------------------------------
beneficiarios <- read_excel(file,
                            sheet = 3,
                            col_names = TRUE,
                            col_types = NULL,
                            na = "NA",
                            skip = 0) %>% clean_names() %>%
  mutate( fecha_de_nacimiento = as.Date(fecha_de_nacimiento, "%Y-%m-%d")) %>%
  mutate( f1_renta = as.Date(f1_renta, "%Y-%m-%d")) %>%  
  mutate( fecha_derecho_ivm = as.Date(fecha_derecho_ivm, "%Y-%m-%d")) %>%  
  mutate( fecha_liquidacion_1 = as.Date(fecha_liquidacion_1, "%Y-%m-%d")) %>%
  mutate( fecha_liquidacion_2 = as.Date(fecha_liquidacion_2, "%Y-%m-%d"))

beneficiarios <- beneficiarios %>%
  mutate( edad_derecho_ivm = round(age_calc(fecha_de_nacimiento,
                                            enddate = fecha_derecho_ivm,
                                            units = "years"),0) ) %>%
  mutate(edad =round(age_calc(fecha_de_nacimiento,
                              enddate = as.Date("31/08/2022","%d/%m/%Y"),
                              units = "years"),0)) %>%
  mutate( n = edad_derecho_ivm ) %>%
  mutate( k =  n - edad ) %>%
  mutate( k = ifelse( k <= 0, NA, k ) ) %>%
  mutate( anios_imposiciones = as.integer(anios_imposiciones) ) %>%
  filter( !is.na(fecha_defuncion)) %>%
  mutate( fecha_defuncion = as.Date(fecha_defuncion, "%Y-%m-%d"))

fallecidos <- beneficiarios
#Guardando en un Rdata------------------------------------------------------------------------------
message( '\tGuardando beneficiarios CE' )

save( fallecidos,
      file = paste0( parametros$RData, 'IESS_fallecidos.RData' ) )

#Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ]  )
gc()