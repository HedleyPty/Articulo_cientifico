#Utilice este script para crear funciones que generen texto

#Abajo se observa una de estas funciones
comentar<-defmacro(var, label, expr = {
  all.percent<-round(prop.table(svytable(~var,country.svy))[2]*100,1)
  all.ci<-round(attributes(svyciprop(~var,country.svy))$ci*100,1)
  
  comment<-paste("La ",
                 label,
                 " fue de ",
                 prettyNum(all.percent, nsmall=1),
                 "% (95% LIC: ",
                 prettyNum(all.ci[1]),
                 "%-",
                 prettyNum(all.ci[2]),
                 "%).", 
                 sep = "")
  p.value.sex<-svychisq(~var+CR2, country.svy)$p.value
  p.value.age<-svychisq(~var+CR1, country.svy)$p.value
  if(p.value.sex < 0.05){
    male.percent<-round(prop.table(svytable(~var+CR2,country.svy),2)[2,1]*100,1)
    female.percent<-round(prop.table(svytable(~var+CR2,country.svy),2)[2,2]*100,1)
    ci.male<-round(attributes(svyciprop(~var, subset(country.svy, CR2==1)))$ci*100,1)
    ci.female<-round(attributes(svyciprop(~var, subset(country.svy, CR2==2)))$ci*100, 1)
    if(male.percent > female.percent){
      comment<-paste(comment, 
                     " Los chicos tienen una ", 
                     label,
                     " m�s alta que las chicas [chicos: ",
                     prettyNum(male.percent, nsmall=1),
                     "%, 95% LIC (",
                     prettyNum(ci.male[1], nsmall=1),
                     "%-",
                     prettyNum(ci.male[2], nsmall=1),
                     "%), chicas: ",
                     prettyNum(female.percent, nsmall=1),
                     "%, 95% LIC (",
                     prettyNum(ci.female[1], nsmall=1),
                     "%-",
                     prettyNum(ci.female[2], nsmall=1),
                     "%)].",
                     sep="")    
    }else{
      comment<-paste(comment, 
                     " Los chicos tienen una ", 
                     label,
                     " m�s baja que las chicas [chicos: ",
                     prettyNum(male.percent, nsmall=1),
                     "%, 95% LIC (",
                     prettyNum(ci.male[1], nsmall=1),
                     "%-",
                     prettyNum(ci.male[2], nsmall=1),
                     "%), chicas: ",
                     prettyNum(female.percent, nsmall=1),
                     "%, 95% LIC (",
                     prettyNum(ci.female[1], nsmall=1),
                     "%-",
                     prettyNum(ci.female[2], nsmall=1),
                     "%)].",
                     sep="")    
    }
  }else{
    if(p.value.age > 0.05){
      comment<-paste(comment, "No hay diferencias estad�sticamente significativas por edad y sexo para esta proporci�n.")
      return(comment)
    }else{
      comment<-paste(comment, "No hay diferencias estad�sticamente significativas por sexo para esta proporci�n.")
    }}
  if(p.value.age > 0.05){
    comment<-paste(comment,"No hay diferencias estad�sticamente significativas por edad para esta proporci�n.")
    return(comment)
  }
  
  ages<-prop.table(svytable(~var+CR1, country.svy),2)[2,]
  
  p.value.13.14<-svychisq(~var+comp.13.14, country.svy)$p.value
  p.value.14.15<-svychisq(~var+comp.14.15, country.svy)$p.value
  p.value.13.15<-svychisq(~var+comp.13.15, country.svy)$p.value
  
  if(p.value.13.14 < 0.05 & p.value.14.15 < 0.05 & p.value.13.15 < 0.05 ){
    if(ages[1] > ages[2] & ages[2] > ages[3]){
      comment<-paste(comment, " Hay un aumento de la ",
                     label,
                     " al incrementar la edad (p value: ",
                     p.value.age,
                     ")",
                     sep="")
      return(comment)}
    if(ages[1] < ages[2] & ages[2] < ages[3]){
      comment<-paste(comment, " Hay una disminuci�n de la ",
                     label,
                     " al aumentar la edad (p value: ",
                     p.value.age,
                     ")",
                     sep="")
    }
    if(ages[1] > ages[2] & ages[2] < ages[3]){
      comment<-paste(comment,
                     "  Los adolescentes de trece y de quince a�os tienen la ",
                     label,
                     " m�s alta , aunque haya diferencias significativas entre ambos grupos et�reos (13 a�os: ",
                     prettyNum(round(ages[1]*100,1)),
                     "%, 15 a�os: ",
                     prettyNum(round(ages[3]*100,1)),
                     "%) que los que han que tienen 14 a�os (",
                     prettyNum(round(ages[2]*100,1)),
                     "%)",
                     sep="")
      return(comment)
    }
    if(ages[1] > ages[2] & ages[2] < ages[3]){
      comment<-paste(comment,
                     "  Los adolescentes de trece y de quince a�os tienen la ",
                     label,
                     " m�s baja , aunque haya diferencias significativas entre ambos grupos et�reos (13 a�os: ",
                     prettyNum(round(ages[1]*100,1)),
                     "%, 15 a�os: ",
                     prettyNum(round(ages[3]*100,1)),
                     "%) que los que han que tienen 14 a�os (",
                     prettyNum(round(ages[2]*100,1)),
                     "%)",
                     sep="")
      return(comment)
    }
  }
  comment
})