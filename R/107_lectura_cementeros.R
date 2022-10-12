message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLectura de lista de cementeros' )

#Cargando información financiera--------------------------------------------------------------------
file<-paste0(parametros$Data, 'IESS_cementeros.xlsx' )

#Cargar función tíldes a latex----------------------------------------------------------------------
source( 'R/503_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )

#Listado de beneficiarios---------------------------------------------------------------------------
cementeros <- read_excel(file,
                            #sheet = 'CBF',
                            col_names = TRUE,
                            col_types = NULL,
                            na = "NA",
                            skip = 0) %>% clean_names() %>%
  mutate( fecha_nacimiento = as.Date(fecha_nacimiento, "%d/%m/%Y")) %>%
  mutate( fecha_jubilacion = as.Date(fecha_jubilacion, "%d/%m/%Y")) %>%
  mutate(edad =round(age_calc(fecha_nacimiento,
                              enddate = as.Date("31/12/2023","%d/%m/%Y"),
                              units = "years"),0)) %>%
  mutate( salario_jubilarce = as.double( salario_jubilarce ),
          renta_min = as.double( renta_min ),
          renta_max = as.double( renta_max ),
          ric = as.double( ric ),
          renta_ivm = as.double( renta_ivm ),
          sbu = as.double(sbu)) %>%
  mutate( g = ifelse( sexo == 'MASCULINO', 'M', 'F') )
  
  
#Guardando en un Rdata------------------------------------------------------------------------------
message( '\tGuardando beneficiarios CE' )

save( cementeros,
      file = paste0( parametros$RData, 'IESS_cementeros.RData' ) )

#Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ]  )
gc()