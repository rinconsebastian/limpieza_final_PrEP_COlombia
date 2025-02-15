if(!require(readxl)){
  install.packages("readxl")
  library(readxl)
}

if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}

if(!require(data.table)){
  install.packages("data.table")
  library(data.table)
}


#------------------------ LEYENDO ARCHIVOS -------------------------------------
encuestas <- read_excel("data/Encuestas_24-06-2021.xlsx")
orientaciones <- read_excel("data/Orientaciones_24-06-2021.xlsx")
personas <- read_excel("data/Persona_16-06-2021.xlsx")



# Edad de personas a entero
personas <- personas%>%mutate(Edad =as.integer(Edad))

# completa los nombres de las columnas de las encuestas
setnames(encuestas, "RelacionesConMas" , '�En los �ltimos 6 meses, ha tenido una o m�s relaciones sexuales sin cond�n?')
setnames(encuestas, "RelacionesConVih" , '�En los �ltimos 6 meses, ha tenido relaciones sexuales con alguna persona que viva con VIH?')
setnames(encuestas, "RelacionesConInyectado" , '�Ha tenido relaciones sexuales con personas que consumen drogas inyectables, en los �ltimos 6 meses?')
setnames(encuestas, "HaTenidoIts" , '�En los �ltimos 6 meses le han diagnosticado, o ha tenido s�ntomas recientemente de alguna infecci�n de transmisi�n sexual?')
setnames(encuestas, "HaUsadoPep" , '�Ha utilizado, en los �ltimos 6 meses, PEP o le han indicado la necesidad de utilizar antirretrovirales para prevenir el VIH?')
setnames(encuestas, "HaSidoDiagnosticadoVih" , '�Ha sido diagnosticado con infecci�n por el VIH o con Sida?')
setnames(encuestas, "HaUsadoPrep" , 'Ha utilizado (ha tomado o toma actualmente) o ha querido utilizar la PrEP?')
setnames(encuestas, "QueTantoSabePrep" , '�Qu� tanto sabe usted de PreP?')
setnames(encuestas, "HaSidoObligado" , '�Ha tenido relaciones sexuales contra su voluntad, en el �ltimo a�o?')
setnames(encuestas, "HaSufridoAgresiones" , '�Ha sido Ud. v�ctima de agresiones f�sicas o psicol�gicas, incluidas las agresiones por parte de una pareja sexual, en los �ltimos 6 meses?')
setnames(encuestas, "ParejaEstaConARV" , '�Sabe usted si su pareja est� tomando tratamiento antirretroviral (TAR) para la infecci�n por el VIH?')
setnames(encuestas, "ParejaHaUsadoARV" , '�Sabe si su pareja este tomado tratamiento antirretroviral (TAR), lo ha hecho durante los �ltimos 6 meses?')
setnames(encuestas, "ParejaCargaViral" , '�sabe si su pareja se ha realizado la Carga Viral en los �ltimos 6 meses?')
setnames(encuestas, "ParejaIndetectable" , '�Sabe si el resultado de ese examen fue indetectable?')

encuestas <- encuestas%>%mutate(Poblacion = if_else(Poblacion == "Hombre que tiene sexo con otros hombres","HSH",Poblacion)) #estandariza el  nombre de la poblaci�n



#1. PrEP uptake  
  #1.1 por edad
    #numerador N�mero de personas que iniciaron PrEP en los �ltimos 12 meses
    
      numerador11 <- personas%>%group_by(gr=cut(Edad, breaks= c(17,seq(20, 120, by = 10))) ) %>% 
                             summarise(num= n()) %>%
                             arrange(as.numeric(gr))
    
    # denominador N�mero de personas a los que se les ofreci� "de novo" el la PrEP en los �ltimos 12 meses (N�mero de personas que contestaron la encuesta)  
    
      denominador11 <- encuestas %>% filter(!(is.na(Doc) | Edad <18)) %>%             #filtra encuestas sin cedula 
                     filter(!(is.na(`Fec. Encuesta`) | `Enc. Aprobada` == "No")) %>% #filtra encuestas sin  fecha
                     arrange(desc(`Enc. Aprobada`))%>% #ordena encuestas  por aprobadas y no aprobadas
                     distinct(`Doc`, .keep_all= TRUE)%>% #elimina encuestas con cedula repetida
                     group_by(gr=cut(Edad, breaks= c(17,seq(20, 120, by = 10))) ) %>% 
                     summarise(den= n()) %>%
                     arrange(as.numeric(gr))
                                
    
      #solo encuestas aprobadas?  
    
     #resultado  
      prepUptake_Edad <-  merge(x= numerador11, y= denominador11, by.x = "gr", by.y = "gr")
      prepUptake_Edad <- prepUptake_Edad %>% mutate(resultado = 100* num/den)
      
  #1.2 por poblaci�n
      #numerador N�mero de personas que iniciaron PrEP en los �ltimos 12 meses
      
      numerador12 <- personas%>%group_by(gr=Poblacion ) %>% 
        summarise(num= n()) %>%
        arrange(as.numeric(gr))
      
      # denominador N�mero de personas a los que se les ofreci� "de novo" el la PrEP en los �ltimos 12 meses (N�mero de personas que contestaron la encuesta)  
      
      denominador12 <- encuestas %>% filter(!(is.na(Doc) | Edad <18)) %>%             #filtra encuestas sin cedula 
        filter(!(is.na(`Fec. Encuesta`) | `Enc. Aprobada` == "No")) %>% #filtra encuestas sin  fecha
        arrange(desc(`Enc. Aprobada`))%>% #ordena encuestas  por aprobadas y no aprobadas
        distinct(`Doc`, .keep_all= TRUE)%>% #elimina encuestas con cedula repetida
        group_by(gr=Poblacion ) %>% 
        summarise(den= n()) %>%
        arrange(as.numeric(gr))
      
      
      #solo encuestas aprobadas?  
      
      #resultado  
      prepUptake_Poblacion <-  merge(x= numerador12, y= denominador12, by.x = "gr", by.y = "gr")
      prepUptake_Poblacion <- prepUptake_Poblacion %>% mutate(resultado = 100* num/den)
      
            
  

#  1# de personas (y por desagregaci�n) que respondieron la encuesta y obtuvieron una cita con el educador comunitario?

orientaciones <- orientaciones %>% filter(!is.na(C�dula)) %>%            #filtra citas sin cedula
                 arrange(desc(`Fecha orientaci�n`))%>% #ordena citas sde mas reciente a mas antigua
                 distinct(`C�dula`, .keep_all= TRUE)%>%   #elimina citas con la misma cedula
                 mutate(Estado = as.factor(Estado))

encuestas <- encuestas %>% filter(!is.na(Doc)) %>%             #filtra encuestas sin cedula 
                           filter(!is.na(`Fec. Encuesta`)) %>% #filtra encuestas sin  fecha
                           arrange(desc(`Enc. Aprobada`))%>% #ordena encuestas  por aprobadas y no aprobadas
                           distinct(`Doc`, .keep_all= TRUE) #elimina encuestas con cedula repetida


orientaciones_encuestas <- merge(x= orientaciones, y= encuestas, by.x = "C�dula", by.y = "Doc")  # une orientaciones y encuestas, mantiene todas las citas


orientaciones_encuestas <- orientaciones_encuestas%>% select(-Id, -Estado, -`Hora orientaci�n`, -`F. Asignacion`, -Nombre, -Tel�fono, -`F. Atencion`, -Atiende, -`Nombre de pila`, -`Fec. Encuesta`, -`Enc. Aprobada`)  # elimina las columnas no relevantes

orientaciones_encuestas[,8] = as.factor(orientaciones_encuestas[,8])
orientaciones_encuestas[,9] = as.factor(orientaciones_encuestas[,9])
orientaciones_encuestas[,10] = as.factor(orientaciones_encuestas[,10])
orientaciones_encuestas[,11] = as.factor(orientaciones_encuestas[,11])
orientaciones_encuestas[,12] = as.factor(orientaciones_encuestas[,12])
orientaciones_encuestas[,13] = as.factor(orientaciones_encuestas[,13])
orientaciones_encuestas[,14] = as.factor(orientaciones_encuestas[,14])
orientaciones_encuestas[,15] = as.factor(orientaciones_encuestas[,15])
orientaciones_encuestas[,16] = as.factor(orientaciones_encuestas[,16])
orientaciones_encuestas[,17] = as.factor(orientaciones_encuestas[,17])
orientaciones_encuestas[,18] = as.factor(orientaciones_encuestas[,18])
orientaciones_encuestas[,19] = as.factor(orientaciones_encuestas[,19])
orientaciones_encuestas[,20] = as.factor(orientaciones_encuestas[,20])
orientaciones_encuestas[,21] = as.factor(orientaciones_encuestas[,21])


