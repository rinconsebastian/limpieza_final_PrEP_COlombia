#es necesario rpimero ejecutar todo hasta (incluido) el chunk #### 1.1 Tiempos promedio entre atenciones

numerador <- personas%>%select(Consecutivo,`Doc`,`Fec. Formulario 1`,`Fec. Formulario 2`,`Fec. Formulario 3`,`Fec. Formulario 3-2`,`Fec. Formulario 3-3`,`Fec. Form cierre`)%>%
  mutate(t1 = as.double(`Fec. Formulario 2`-`Fec. Formulario 1`))%>%
  mutate(t2 = as.double(`Fec. Formulario 3`-`Fec. Formulario 2`))%>%
  mutate(t3 = as.double(`Fec. Formulario 3-2`-`Fec. Formulario 3`))%>%
  mutate(t4 = as.double(`Fec. Formulario 3-3`-`Fec. Formulario 3-2`))%>%
  mutate(t5 = as.double(difftime( `Fec. Form cierre`,`Fec. Formulario 3-3`, units = "days") ))%>%
  mutate(t1 = if_else(t1<0,0,t1))%>%
  mutate(t2 = if_else(t2<0,0,t2))%>%
  mutate(t3 = if_else(t3<0,0,t3))%>%
  mutate(t4 = if_else(t4<0,0,t4))%>%
  mutate(t5 = if_else(t5<0,0,t5))

openxlsx::write.xlsx(numerador, "FechasyTiempos.xlsx")