#########################################################

# Uniendo de Indices por zonas separadas segun la latitud

#########################################################
zone.div<-function(dir1,dir2,ilon,ilat,method=method,modelim=modelim,modelm=modelm,estadios=estadios,xi=xi,steps=steps,filtro=NULL,DL=NULL,hfeno){

  R<-8
	lats=p.area(ilat,R)
	IndxERI<-IndxGI<-IndxAI<-list()
	#############################################
	# Corriendo por cada area y almacenar su data
	#############################################
	
	for(j in R:1){

		TempsAll<-GenMatriz(dir1,dir2,ilon,lats[j,])

		coords=TempsAll$coords;nfil=nrow(coords)

		x1=TempsAll$x1

		y1=TempsAll$y1


		RFinal=matrix(NA,nfil,3)

		################################

		# Corriendo por punto del Area j

		################################

		#system.time(

				for(i in 1:nfil){

					RFinal[i,]=GenActIndex(i,TempsAll=TempsAll,coords=coords,x1=x1,y1=y1,method=method,modelim=modelim,modelm=modelm,estadios=estadios,xi=xi,steps=steps,filtro,DL,hfeno=hfeno)$indices
				}

		#)

		rm(TempsAll);rm(x1);rm(y1)

		# Inds=data.frame(coords[1:nfil,],RFinal)
		# coordinates(Inds)<-~x+y
		# gridded(Inds) = ~x+y ## Creando el objeto Grid
		IndxERI[[j]]<-rasterFromXYZ(data.frame(coords[1:nfil,],X3=RFinal[,3]))
		IndxGI[[j]]<-rasterFromXYZ(data.frame(coords[1:nfil,],X2=RFinal[,2]))
		IndxAI[[j]]<-rasterFromXYZ(data.frame(coords[1:nfil,],X1=RFinal[,1]))
		rm(RFinal)
		
		##################################################################

		# Generando los archivos con encabezado: Lon - Lat - GI - AI - ERI

		##################################################################
		# write.table(Inds,paste(dir.out,"file",j,".txt",sep=""),row.names = F)
		# 
		# rm(Inds)
	}

	###########################################

	# Corriendo por cada area y uniendo su data

	###########################################

	# Inds=read.table(paste(dir.out,"file",R,".txt",sep=""),header=T)
	ERI1 <- merge(IndxERI[[1]],IndxERI[[2]],IndxERI[[3]],IndxERI[[4]],IndxERI[[5]],IndxERI[[6]],IndxERI[[7]],IndxERI[[8]])
	GI1 <- merge(IndxGI[[1]],IndxGI[[2]],IndxGI[[3]],IndxGI[[4]],IndxGI[[5]],IndxGI[[6]],IndxGI[[7]],IndxGI[[8]])
	AI1 <- merge(IndxAI[[1]],IndxAI[[2]],IndxAI[[3]],IndxAI[[4]],IndxAI[[5]],IndxAI[[6]],IndxAI[[7]],IndxAI[[8]])
	rm(IndxERI);rm(IndxGI);rm(IndxAI)
	# if(R!=1)
	# {
	# 	for(j in (R-1):1){
	# 
	# 		# TempInds=read.table(paste(dir.out,"file",j,".txt",sep=""),header=T)
	# 	  TempInds<-IndxT[[j]]
	# 		Inds=rbind(Inds,TempInds)
	# 	}
	# }

	#mins<-apply(Inds[,-c(1,2)], 2,min, na.rm=T)
	#maxs<-apply(Inds[,-c(1,2)], 2,max, na.rm=T)

	# rm(TempInds)
	# gridded(Inds) = ~x+y ## Creando el objeto Grid

	# writeAsciiGrid(Inds["X1"], na.value = -9999,paste(dir.out,"AI.asc",sep=""))
	# writeAsciiGrid(Inds["X2"], na.value = -9999,paste(dir.out,"GI.asc",sep=""))
	# writeAsciiGrid(Inds["X3"], na.value = -9999,paste(dir.out,"ERI.asc",sep=""))
	
	return(list(ERI=ERI1,GI=GI1,AI=AI1))
}
#############################################
YearCompleted<-function(estacion)
{
  #estacion<-estaciones[1]
  # meses<-c("enero","febrero","marzo","abril","mayo","junio","julio","agosto","septiembre","octubre","noviembre","diciembre")
  meses<-1:12
  # urls<-paste("http://www.agrocabildo.org/tablas/",estacion,"-",meses,".htm",sep="")
  urls<-paste("https://www.agrocabildo.org/estaciones_mensual_historico.asp?id=",estacion,"&mes=",meses,sep="")
  dat1<-getClimateData(urls[1])
  for(i in 2:12)
  {
    tempC1<-getClimateData(urls[i])
    dat1<-rbind(dat1,tempC1)
  }
  datY<-dat1[order(dat1$Fecha),]
  
  #plot(datY$Fecha,datY$T,type="l",ylab="temperatura",xlab = "fecha")
  #write.csv(datY,"C:/Veronica/Enero_Prueba.csv",row.names = FALSE)
  return(datY)
}


# Obtencion de Data en 1 mes
getClimateData<-function(url)
{
  #url<-"https://www.agrocabildo.org/estaciones_mensual_historico.asp?id=90&mes=5"
  #url<-"https://www.agrocabildo.org/estaciones_mensual_historico.asp"
  #url<-urls[1]
  
  webpage <- read_html(url)

  dat0<-webpage %>% html_nodes("td") %>% html_text() %>% gsub("^\\s+|\\s+$", "", .)
  dat1<-dat0[-1]
  Nlbs<-(1:length(dat1))[dat1=="ET0 PM"]
  labI<-dat1[1:Nlbs]
  Ntxt<-(1:length(dat1))[dat1=="*Media\r\n\t\t\t\t\t\t\t\t\t\t\t\t**Suma"]
  dat2<-dat1[(Nlbs+1):(Ntxt-1)]
  if(length(dat2) < 13){return()}else{
    Fechas<-dat2[seq(1,length(dat2),Nlbs)]
    dat2<-gsub(",",".", dat2)
    dat2<-as.numeric(as.character(dat2))
    dat2<-t(matrix(dat2,Nlbs,length(Fechas)))
    dat2<-data.frame(Fechas,dat2[,-1])
    colnames(dat2)<-labI
    
    
    dat2$Fecha<-as.Date(dat2$Fecha,"%d/%m/%Y")
    dat2<-dat2[!is.na(dat2$Fecha),]
    #write.csv(dat1,"C:/Enero_Prueba.csv",row.names = FALSE)
    return(dat2)
  }
}

########################################


#########################################################
zone.div.web<-function(tmin,tmax,modelim=modelim,modelm=modelm,estadios=estadios,xi=xi,steps=steps,filtro=NULL,DL=NULL,hfeno){

  coords<-coordinates(raster(tmin[[1]]))
  nfil<-nrow(coords)
  
    RFinal=matrix(NA,nfil,3)
    
    for(i in 1:nfil){
      print(i)
      RFinal[i,]=GenActIndexWeb(i,coords=coords,tmin=tmin,tmax=tmax,modelim=modelim,modelm=modelm,estadios=estadios,xi=xi,steps=steps,hfeno=hfeno)$indices
    }
    
    ERI1<-rasterFromXYZ(data.frame(coords[1:nfil,],X3=RFinal[,3]))
    GI1<-rasterFromXYZ(data.frame(coords[1:nfil,],X2=RFinal[,2]))
    AI1<-rasterFromXYZ(data.frame(coords[1:nfil,],X1=RFinal[,1]))
    
    rm(RFinal)
  return(list(ERI=ERI1,GI=GI1,AI=AI1))
}



# Funcion generadora de Indices Mejorado
########################################
GenActIndexWeb<-function(posic,coords=coords,tmin=tmin,tmax=tmax,modelim=modelim,modelm=modelm,estadios=estadios,xi=xi,steps=steps,filtro=NULL,DL=NULL,hfeno)
{
  # d1<-1:length(x1);d2<-1:length(y1);
  filtroin=TRUE
  # plon=d1[x1==coords[posic,1]]
  # plat=d2[y1==coords[posic,2]]
  Table<-data.frame(mini=(unlist(terra::extract(tmin, vect(data.frame(lon=coords[posic,1],lat=coords[posic,2]), crs="+proj=longlat +datum=WGS84"))))[-1],
                    maxi=(unlist(terra::extract(tmax, vect(data.frame(lon=coords[posic,1],lat=coords[posic,2]), crs="+proj=longlat +datum=WGS84"))))[-1])
  row.names(Table)<-1:12
  # Table=data.frame(mini=c(TempsAll$zTmin1[plon,plat],TempsAll$zTmin2[plon,plat],TempsAll$zTmin3[plon,plat],TempsAll$zTmin4[plon,plat],TempsAll$zTmin5[plon,plat],TempsAll$zTmin6[plon,plat],TempsAll$zTmin7[plon,plat],TempsAll$zTmin8[plon,plat],TempsAll$zTmin9[plon,plat],TempsAll$zTmin10[plon,plat],TempsAll$zTmin11[plon,plat],TempsAll$zTmin12[plon,plat]),
  #                  maxi=c(TempsAll$zTmax1[plon,plat],TempsAll$zTmax2[plon,plat],TempsAll$zTmax3[plon,plat],TempsAll$zTmax4[plon,plat],TempsAll$zTmax5[plon,plat],TempsAll$zTmax6[plon,plat],TempsAll$zTmax7[plon,plat],TempsAll$zTmax8[plon,plat],TempsAll$zTmax9[plon,plat],TempsAll$zTmax10[plon,plat],TempsAll$zTmax11[plon,plat],TempsAll$zTmax12[plon,plat]))
  
  # Table=Table/10;Table2=cbind(id=1:nrow(Table),Table) ## filtrar en esta temperatura
  Table2=cbind(id=1:nrow(Table),Table) ## filtrar en esta temperatura

  #worldclim_tile(var, coords[posic,1], coords[posic,2], var="tmin",path=tempdir(), version="2.1", ...)
  
  if(length(Table[is.na(Table)])==0)
  {
    if(!is.null(filtro)){tmm=apply(Table,2,mean,na.rm=TRUE);if(tmm[1] > filtro[1] && tmm[2] < filtro[2]){filtroin=TRUE}else{filtroin=FALSE}}
    if(filtroin)
    {
      inmaduros <-  estadios[-(length(estadios)-1):-(length(estadios))]
      maduros   <-  estadios[(length(estadios)-1):(length(estadios))]
      nmax=nrow(Table)
      matriz<-matrix(0,ncol=2*(length(inmaduros)*2+length(maduros)+1),nrow=nrow(Table))
      
      for(K in 1:length(inmaduros))
      {
        #  Desarrollo  # extraccion de funciones y parametros
        
        parametrosc <- hfeno$pdv_dv[[K]]
        funciont <- as.expression(as.formula(hfeno$fdv_dv[[K]])[[3]])
        
        #   Mortalidad # extraccion de funciones y parametros
        
        parametrosm <- hfeno$pmortal[[K]]
        funcionm <- as.expression(as.formula(hfeno$mortal[[K]])[[3]])
        
        RM=apply(Table2,1,RateI,Table2,K,parametrosc,parametrosm,funciont,funcionm,nmax,steps,modelim=modelim) ## procesamiento de tasa de desarrollo y mortalidad por cada temperatura
        matriz[,4*K-3]=RM[1,];matriz[,4*K-2]=RM[2,]
        matriz[,4*K-1]=RM[3,];matriz[,4*K]=RM[4,]
      }
      #  Hembras
      
      parametrosc <- hfeno$pfh_h
      for (i in names(parametrosc)){temp <- parametrosc[i];storage.mode(temp) <- "double";assign(i, temp)}
      formulac <- as.formula(hfeno$fh_h)
      funciont <- as.expression(formulac[[3]])
      RM=apply(Table2,1,RateI,Table2,(K+1),parametrosc,funciont=funciont,nmax=nmax,steps=steps,J=NA,modelim=modelim)
      matriz[,4*(K+1)-3]=RM[1,];matriz[,4*(K+1)-2]=RM[2,]
      
      #  Machos
      
      parametrosc <- hfeno$pfm_m;
      for (i in names(parametrosc)){temp <- parametrosc[i];storage.mode(temp) <- "double";assign(i, temp)}
      formulac <- as.formula(hfeno$fm_m)
      funciont <- as.expression(formulac[[3]])
      RM=apply(Table2,1,RateI,Table2,(K+2),parametrosc,funciont=funciont,nmax=nmax,steps=steps,J=NA,modelim=modelim)
      matriz[,4*(K+1)-1]=RM[1,];matriz[,4*(K+1)]=RM[2,]
      matriz[matriz>1]<-1;matriz[matriz<0]<-0
      
      #  Fecundidad
      
      parametrosc <- hfeno$ptazaeh_h
      for (i in names(parametrosc)){temp <- parametrosc[i];storage.mode(temp) <- "double";assign(i, temp)}
      formulac <- as.formula(hfeno$ftazaeh_h)
      funciont <- as.expression(formulac[[3]])
      RM=apply(Table2,1,RateI,Table2,(K+3),parametrosc,funciont=funciont,nmax=nmax,steps=steps,J=3,modelim=modelim)
      matriz[,4*(K+1)+1]=RM[1,];matriz[,4*(K+1)+2]=RM[2,]
      
      # Oviposition Rate
      parametrosc <- hfeno$povih_h
      for (i in names(parametrosc)){temp <- parametrosc[i];storage.mode(temp) <- "double";assign(i, temp)}
      formulac <- as.formula(hfeno$fovih_h)
      funciont <- as.expression(formulac[[3]])
      model<-hfeno$idmodelOR 
      RMovi=t(apply(Table2,1,RateOR,Table2,model,parametrosc,funciont=funciont,nmax=nmax,steps=steps,J=NA,modelim=modelim))
      #matriz[,4*(K+1)-3]=RM[1,];matriz[,4*(K+1)-2]=RM[2,] # Wait
      
      TF=data.frame(Ind=1:nmax)
      for(i in 1:K){TF1=data.frame(DaiSurv1=(1-matriz[,4*i-1])^matriz[,4*i-3],DaiSurv2=(1-matriz[,4*i])^matriz[,4*i-2]);TF=cbind(TF,TF1)}
      for(i in 1:K){Gl0=data.frame(Gl01=1/matriz[,4*i-3],Gl02=1/matriz[,4*i-2]);TF=cbind(TF,Gl0)}
      #TF$Gl11=(1/matriz[,4*(K)+1])/(1/xi);TF$Gl22=(1/matriz[,4*(K)+2])/(1/xi);TF=TF[,-1]  ## aqui faltaba esta ultima linea pues no consideraba la tasa de desarrollo de la hembra en el ultimo mes
      TF$Gl11=1/RMovi[,1];TF$Gl22=1/RMovi[,2];TF=TF[,-1]
      temp1=TF[,c(2*K+seq(1,2*K,2),ncol(TF)-1)];temp2=TF[,c(2*K+seq(2,2*K,2),ncol(TF))]
      TF$Gl1=apply(temp1,1,sum);TF$Gl2=apply(temp2,1,sum)
      temp1<-temp2<-1;for(j in 1:K){temp1=temp1*(1-matriz[,4*j-1]);temp2=temp2*(1-matriz[,4*j])} ## aqui usaba como contador ala "i" en ves de la "j"
      TF$Is1=temp1;TF$Is2=temp2
      TF$Ro1=(TF$Is1*matriz[,4*K+5])/2;TF$Ro1[TF$Ro1<0]=0; TF$Ro2=(TF$Is2*matriz[,4*K+6])/2;TF$Ro2[TF$Ro2<0]=0
      TF$rm1=log(TF$Ro1)/TF$Gl1; TF$rm2=log(TF$Ro2)/TF$Gl2
      TF$Fr1=exp(TF$rm1); TF$Fr2=exp(TF$rm2)
      TF$Dt1=log(2)/TF$rm1; TF$Dt2=log(2)/TF$rm2
      #Ro=mean(c(TF[,"Ro1"],TF[,"Ro2"]))
      
      meses=c(31,28,31,30,31,30,31,31,30,31,30,31);r1=rep(1,nrow(TF))
      GI=sum((meses-1)/TF[,"Gl1"] + 1/TF[,"Gl2"])
      #temp.fr1=TF[,"Fr1"];ind1=temp.fr1>1;temp.fr1=temp.fr1[ind1];temp.meses=meses[ind1]  ## hay un objeto q no se utiliza
      #temp.fr2=TF[,"Fr2"];ind2=temp.fr2>1;temp.fr2=temp.fr2[ind2]
      #if(length(temp.fr2)==0){temp.fr2=1}
      #if(length(temp.fr1)==0){AI=NA}else{AI=log(prod(c(temp.fr1^(temp.meses-1),temp.fr2)),10)}
      temp.fr1=TF[,"Fr1"];ind1=temp.fr1<1;temp.fr1[ind1]=1
      temp.fr2=TF[,"Fr2"];ind2=temp.fr2<1;temp.fr2[ind2]=1
      AI=log(prod(c(temp.fr1^(meses-1),temp.fr2)),10) ## en este caso hubo el problema que no consideraba a los valores del ultimo mes si el resto valores eran menores a 1
      
      #ERI=1;for(s in 1:K){n0=(sum(meses[TF[,2*s-1]==0]-1) + sum(r1[TF[,2*s]==0]))/365;ERI=ERI*(1-n0)}
      #if(is.null(DL)){DL=0} ## La diferencia limite segun su origen siempre ha de ser positivo
      #ERI=1;for(s in 1:K){n0=(sum(meses[TF[,2*s-1]<=DL]-1) + sum(r1[TF[,2*s]<=DL]))/365;ERI=ERI*(1-n0)}
      
      #       ERI1=c(apply(TF[,seq(1,2*K,2)]*TF[,"Ro1"],1,mean,na.rm = TRUE), apply(TF[,seq(2,2*K,2)]*TF[,"Ro2"],1,mean,na.rm = TRUE))
      #       ERI=mean(ERI1)/max(ERI1);ERI[is.na(ERI)]=0;ERI[ERI>1]=1
      
      
      ERI=1;n0=(sum(meses[TF[,"Ro1"] <= 1]-1) + sum(r1[TF[,"Ro2"] <= 1]))/365;ERI=ERI*(1-n0)
      
      
      #indices=c(AI=AI,GI=GI,ERI=ERI,Ro=Ro)
      indices=c(AI=AI,GI=GI,ERI=ERI)
      return(list(indices=indices))
    }else{indices=c(AI=0,GI=0,ERI=0);return(list(indices=indices))}
  }else{
    
    #indices=c(AI=NA,GI=NA,ERI=NA,Ro=NA)
    indices=c(AI=NA,GI=NA,ERI=NA)
    return(list(indices=indices))
  }
  
}


getTemperCT<-function(CoordsI)
{
  estacionesLabels<-c("abona","aguamansa","ALCALA","anavi","ARAYA","arico01","badaj","BAILADERO","benij",
                      "bvista","CHAVAO","chio","cubo","ELDRAGO","ERES","GAITERO","galletas","guan1ha1",
                      "guia","guiais01","helecho","hoyos","ICOR","igueste","LAGUNA","llani","LOSSILOS",
                      "matan","mena","orotav01","palma","palob","PICACHO","pinal","pozo","ratil",
                      "ravelo01","REALETH_N","redon","SANTACRUZ","smig","suert","tacor","taganana","teges",
                      "tejina01","topo","TOPO_MA","trigo","ursubaja","ursul","ursumedi","victo","vilaflor",
                      "Imetos_Ravelo","Imetos_Tacoronte","Imetos_Mazape","Imetos_Trevejos","LaQuinta",
                      "ELVIENTO","ELROSARIO","GALECUBO_N","HOYA_GRANDE")
  estaciones<-c(55,87,95,61,96,1,80,105,70,102,89,74,91,98,181,88,2,12,75,5,9,64,85,101,190,57,103,67,81,7,65,71,90,77,76,58,10,106,73,92,78,69,66,97,56,11,54,94,62,83,63,82,68,13,198,199,204,201,194,60,86,91,93)
  meses<-1:12
  
  id_0<-(1:length(estaciones))[CoordsI$id==estacionesLabels]
  # urls<-paste("http://www.agrocabildo.org/tablas/",estacion,"-",meses,".htm",sep="")
  urls<-paste("https://www.agrocabildo.org/estaciones_mensual_historico.asp?id=",estaciones[id_0],"&mes=",meses,sep="")
  dat1<-getClimateData(urls[1])
  for(i in 2:12)
  {
    tempC1<-getClimateData(urls[i])
    dat1<-rbind(dat1,tempC1)
  }
  datY<-dat1[order(dat1$Fecha),]
  return(datY)
}

LPflucImp<-function(LPvec,nmeses){
  LPs<-c()
  sd1<-sd(LPvec)
  for(i in 1:12)
  {
    tempLP<-rnorm(nmeses[i],LPvec[i],sample(seq(sd1/6,sd1/2,length.out=4),1));LPs<-c(LPs,tempLP)
    LPs[LPs<0]<-0.001
  }
  return(LPs)
}

GenActIndex<-function(posic,TempsAll=TempsAll,coords=coords,x1=x1,y1=y1,method=method,modelim=modelim,modelm=modelm,estadios=estadios,xi=xi,steps=steps,filtro=NULL,DL=NULL,hfeno)
{
	d1<-1:length(x1);d2<-1:length(y1);filtroin=TRUE
	plon=d1[x1==coords[posic,1]]
	plat=d2[y1==coords[posic,2]]

	Table=data.frame(mini=c(TempsAll$zTmin1[plon,plat],TempsAll$zTmin2[plon,plat],TempsAll$zTmin3[plon,plat],TempsAll$zTmin4[plon,plat],TempsAll$zTmin5[plon,plat],TempsAll$zTmin6[plon,plat],TempsAll$zTmin7[plon,plat],TempsAll$zTmin8[plon,plat],TempsAll$zTmin9[plon,plat],TempsAll$zTmin10[plon,plat],TempsAll$zTmin11[plon,plat],TempsAll$zTmin12[plon,plat]),
			maxi=c(TempsAll$zTmax1[plon,plat],TempsAll$zTmax2[plon,plat],TempsAll$zTmax3[plon,plat],TempsAll$zTmax4[plon,plat],TempsAll$zTmax5[plon,plat],TempsAll$zTmax6[plon,plat],TempsAll$zTmax7[plon,plat],TempsAll$zTmax8[plon,plat],TempsAll$zTmax9[plon,plat],TempsAll$zTmax10[plon,plat],TempsAll$zTmax11[plon,plat],TempsAll$zTmax12[plon,plat]))

	Table=Table/10;Table2=cbind(id=1:nrow(Table),Table) ## filtrar en esta temperatura

  #worldclim_tile(var, coords[posic,1], coords[posic,2], var="tmin",path=tempdir(), version="2.1", ...)

	if(length(Table[is.na(Table)])==0)
	{
	 if(!is.null(filtro)){tmm=apply(Table,2,mean,na.rm=TRUE);if(tmm[1] > filtro[1] && tmm[2] < filtro[2]){filtroin=TRUE}else{filtroin=FALSE}}
	 if(filtroin)
	 {
		inmaduros <-  estadios[-(length(estadios)-1):-(length(estadios))]
		maduros   <-  estadios[(length(estadios)-1):(length(estadios))]
		nmax=nrow(Table)
		matriz<-matrix(0,ncol=2*(length(inmaduros)*2+length(maduros)+1),nrow=nrow(Table))

		for(K in 1:length(inmaduros))
		{
			#  Desarrollo  # extraccion de funciones y parametros

			parametrosc <- hfeno$pdv_dv[[K]]
			funciont <- as.expression(as.formula(hfeno$fdv_dv[[K]])[[3]])

			#   Mortalidad # extraccion de funciones y parametros

			parametrosm <- hfeno$pmortal[[K]]
			funcionm <- as.expression(as.formula(hfeno$mortal[[K]])[[3]])

			RM=apply(Table2,1,RateI,Table2,K,parametrosc,parametrosm,funciont,funcionm,nmax,steps,modelim=modelim) ## procesamiento de tasa de desarrollo y mortalidad por cada temperatura
			matriz[,4*K-3]=RM[1,];matriz[,4*K-2]=RM[2,]
			matriz[,4*K-1]=RM[3,];matriz[,4*K]=RM[4,]
		}
		#  Hembras

		parametrosc <- hfeno$pfh_h
		for (i in names(parametrosc)){temp <- parametrosc[i];storage.mode(temp) <- "double";assign(i, temp)}
		formulac <- as.formula(hfeno$fh_h)
		funciont <- as.expression(formulac[[3]])
		RM=apply(Table2,1,RateI,Table2,(K+1),parametrosc,funciont=funciont,nmax=nmax,steps=steps,J=NA,modelim=modelim)
		matriz[,4*(K+1)-3]=RM[1,];matriz[,4*(K+1)-2]=RM[2,]

		#  Machos

		parametrosc <- hfeno$pfm_m;
		for (i in names(parametrosc)){temp <- parametrosc[i];storage.mode(temp) <- "double";assign(i, temp)}
		formulac <- as.formula(hfeno$fm_m)
		funciont <- as.expression(formulac[[3]])
		RM=apply(Table2,1,RateI,Table2,(K+2),parametrosc,funciont=funciont,nmax=nmax,steps=steps,J=NA,modelim=modelim)
		matriz[,4*(K+1)-1]=RM[1,];matriz[,4*(K+1)]=RM[2,]
		matriz[matriz>1]<-1;matriz[matriz<0]<-0

		#  Fecundidad

		parametrosc <- hfeno$ptazaeh_h
		for (i in names(parametrosc)){temp <- parametrosc[i];storage.mode(temp) <- "double";assign(i, temp)}
		formulac <- as.formula(hfeno$ftazaeh_h)
		funciont <- as.expression(formulac[[3]])
		RM=apply(Table2,1,RateI,Table2,(K+3),parametrosc,funciont=funciont,nmax=nmax,steps=steps,J=3,modelim=modelim)
		matriz[,4*(K+1)+1]=RM[1,];matriz[,4*(K+1)+2]=RM[2,]

		# Oviposition Rate
		parametrosc <- hfeno$povih_h
		for (i in names(parametrosc)){temp <- parametrosc[i];storage.mode(temp) <- "double";assign(i, temp)}
		formulac <- as.formula(hfeno$fovih_h)
		funciont <- as.expression(formulac[[3]])
		model<-hfeno$idmodelOR 
		RMovi=t(apply(Table2,1,RateOR,Table2,model,parametrosc,funciont=funciont,nmax=nmax,steps=steps,J=NA,modelim=modelim))
		#matriz[,4*(K+1)-3]=RM[1,];matriz[,4*(K+1)-2]=RM[2,] # Wait
		
		TF=data.frame(Ind=1:nmax)
		for(i in 1:K){TF1=data.frame(DaiSurv1=(1-matriz[,4*i-1])^matriz[,4*i-3],DaiSurv2=(1-matriz[,4*i])^matriz[,4*i-2]);TF=cbind(TF,TF1)}
		for(i in 1:K){Gl0=data.frame(Gl01=1/matriz[,4*i-3],Gl02=1/matriz[,4*i-2]);TF=cbind(TF,Gl0)}
		#TF$Gl11=(1/matriz[,4*(K)+1])/(1/xi);TF$Gl22=(1/matriz[,4*(K)+2])/(1/xi);TF=TF[,-1]  ## aqui faltaba esta ultima linea pues no consideraba la tasa de desarrollo de la hembra en el ultimo mes
		TF$Gl11=1/RMovi[,1];TF$Gl22=1/RMovi[,2];TF=TF[,-1]
		temp1=TF[,c(2*K+seq(1,2*K,2),ncol(TF)-1)];temp2=TF[,c(2*K+seq(2,2*K,2),ncol(TF))]
		TF$Gl1=apply(temp1,1,sum);TF$Gl2=apply(temp2,1,sum)
		temp1<-temp2<-1;for(j in 1:K){temp1=temp1*(1-matriz[,4*j-1]);temp2=temp2*(1-matriz[,4*j])} ## aqui usaba como contador ala "i" en ves de la "j"
		TF$Is1=temp1;TF$Is2=temp2
		TF$Ro1=(TF$Is1*matriz[,4*K+5])/2;TF$Ro1[TF$Ro1<0]=0; TF$Ro2=(TF$Is2*matriz[,4*K+6])/2;TF$Ro2[TF$Ro2<0]=0
		TF$rm1=log(TF$Ro1)/TF$Gl1; TF$rm2=log(TF$Ro2)/TF$Gl2
		TF$Fr1=exp(TF$rm1); TF$Fr2=exp(TF$rm2)
		TF$Dt1=log(2)/TF$rm1; TF$Dt2=log(2)/TF$rm2
		#Ro=mean(c(TF[,"Ro1"],TF[,"Ro2"]))
		
		meses=c(31,28,31,30,31,30,31,31,30,31,30,31);r1=rep(1,nrow(TF))
		GI=sum((meses-1)/TF[,"Gl1"] + 1/TF[,"Gl2"])
		#temp.fr1=TF[,"Fr1"];ind1=temp.fr1>1;temp.fr1=temp.fr1[ind1];temp.meses=meses[ind1]  ## hay un objeto q no se utiliza
		#temp.fr2=TF[,"Fr2"];ind2=temp.fr2>1;temp.fr2=temp.fr2[ind2]
		#if(length(temp.fr2)==0){temp.fr2=1}
		#if(length(temp.fr1)==0){AI=NA}else{AI=log(prod(c(temp.fr1^(temp.meses-1),temp.fr2)),10)}
		temp.fr1=TF[,"Fr1"];ind1=temp.fr1<1;temp.fr1[ind1]=1
		temp.fr2=TF[,"Fr2"];ind2=temp.fr2<1;temp.fr2[ind2]=1
		AI=log(prod(c(temp.fr1^(meses-1),temp.fr2)),10) ## en este caso hubo el problema que no consideraba a los valores del ultimo mes si el resto valores eran menores a 1
		
		#ERI=1;for(s in 1:K){n0=(sum(meses[TF[,2*s-1]==0]-1) + sum(r1[TF[,2*s]==0]))/365;ERI=ERI*(1-n0)}
		#if(is.null(DL)){DL=0} ## La diferencia limite segun su origen siempre ha de ser positivo
		#ERI=1;for(s in 1:K){n0=(sum(meses[TF[,2*s-1]<=DL]-1) + sum(r1[TF[,2*s]<=DL]))/365;ERI=ERI*(1-n0)}
		
		#       ERI1=c(apply(TF[,seq(1,2*K,2)]*TF[,"Ro1"],1,mean,na.rm = TRUE), apply(TF[,seq(2,2*K,2)]*TF[,"Ro2"],1,mean,na.rm = TRUE))
		#       ERI=mean(ERI1)/max(ERI1);ERI[is.na(ERI)]=0;ERI[ERI>1]=1
		
		
		ERI=1;n0=(sum(meses[TF[,"Ro1"] <= 1]-1) + sum(r1[TF[,"Ro2"] <= 1]))/365;ERI=ERI*(1-n0)
		
		
		#indices=c(AI=AI,GI=GI,ERI=ERI,Ro=Ro)
		indices=c(AI=AI,GI=GI,ERI=ERI)
		return(list(indices=indices))
	   }else{indices=c(AI=0,GI=0,ERI=0);return(list(indices=indices))}
	}else{
	  
	  #indices=c(AI=NA,GI=NA,ERI=NA,Ro=NA)
	  indices=c(AI=NA,GI=NA,ERI=NA)
	  return(list(indices=indices))
	}

}
##########################################################
##########################################################
GenMatriz<-function(dir1,dir2,ilon,ilat){
	# archivos1=list.files(dir1,pattern="flt");archivos1=paste(dir1,"/",archivos1,sep="")
	# archivos2=list.files(dir2,pattern="flt");archivos2=paste(dir2,"/",archivos2,sep="")
	archivos1=dir1
	archivos2=dir2
	
	Tmin1=raster(archivos1[1])
	# para la extracion de los datos usamos la funcion:
	
	geodat=data.frame(coordinates(Tmin1))
	
	###########################################
	## Extraccion de las longitudes y latitudes
	
	x <- c(geodat[,1]);x <- unique(x)
	n1=length(x) ## tamao
	
	y <- c(geodat[,2]); y <- unique(y)
	n2=length(y)
	
	#rm(geodat)
	
	#######################################################################
	## Extraccion de las resoluciones tanto para longitud como para latitud
	
	v1=Tmin1@grid@cellsize[1]/2
	v2=Tmin1@grid@cellsize[2]/2
	
	
	##############################################################
	## Definiendo los rangos en la longitud y latitud con posicion
	
	r11=ilon[1];r12=ilon[2];r21=ilat[1];r22=ilat[2]
	
	k1=rownames(geodat[geodat[,1] >= (r11-v1) & geodat[,1] <= (r12+v1),])
	k2=rownames(geodat[geodat[,2] >= (r21-v2) & geodat[,2] <= (r22+v2),])
	
	sector=intersect(k1,k2)
	sector=as.numeric(sector)
	
	coords=geodat[sector,]
	
	rm(geodat)
	
	
	#################################################
	## Definiendo los rangos en la longitud y latitud 
	
	ind1<-1:n1
	d1=x>=(r11-v1) & x<=(r12+v1)
	x1=x[d1]
	ind1=ind1[d1]
	
	
	ind2<-1:n2
	d2=y>=(r21-v2) & y<=(r22+v2)
	y1=y[d2]
	ind2=ind2[d2]
	
	
	####################################################################################
	## Creacion de la matriz que contiene los valores de la variable por cada coordenada
	
	z <- c(Tmin1@data[,1]);zTmin1=matrix(z,n1,n2);rm(Tmin1)
	zTmin1=zTmin1[ind1,ind2]; rownames(zTmin1)=x1; colnames(zTmin1)=y1
	Tmin2=readGDAL(archivos1[2])
	z <- c(Tmin2@data[,1]);zTmin2=matrix(z,n1,n2);rm(Tmin2)
	zTmin2=zTmin2[ind1,ind2]; rownames(zTmin2)=x1; colnames(zTmin2)=y1
	Tmin3=readGDAL(archivos1[3])
	z <- c(Tmin3@data[,1]);zTmin3=matrix(z,n1,n2);rm(Tmin3)
	zTmin3=zTmin3[ind1,ind2]; rownames(zTmin3)=x1; colnames(zTmin3)=y1
	Tmin4=readGDAL(archivos1[4])
	z <- c(Tmin4@data[,1]);zTmin4=matrix(z,n1,n2);rm(Tmin4)
	zTmin4=zTmin4[ind1,ind2]; rownames(zTmin4)=x1; colnames(zTmin4)=y1
	Tmin5=readGDAL(archivos1[5])
	z <- c(Tmin5@data[,1]);zTmin5=matrix(z,n1,n2);rm(Tmin5)
	zTmin5=zTmin5[ind1,ind2]; rownames(zTmin5)=x1; colnames(zTmin5)=y1
	Tmin6=readGDAL(archivos1[6])
	z <- c(Tmin6@data[,1]);zTmin6=matrix(z,n1,n2);rm(Tmin6)
	zTmin6=zTmin6[ind1,ind2]; rownames(zTmin6)=x1; colnames(zTmin6)=y1
	Tmin7=readGDAL(archivos1[7])
	z <- c(Tmin7@data[,1]);zTmin7=matrix(z,n1,n2);rm(Tmin7)
	zTmin7=zTmin7[ind1,ind2]; rownames(zTmin7)=x1; colnames(zTmin7)=y1
	Tmin8=readGDAL(archivos1[8])
	z <- c(Tmin8@data[,1]);zTmin8=matrix(z,n1,n2);rm(Tmin8)
	zTmin8=zTmin8[ind1,ind2]; rownames(zTmin8)=x1; colnames(zTmin8)=y1
	Tmin9=readGDAL(archivos1[9])
	z <- c(Tmin9@data[,1]);zTmin9=matrix(z,n1,n2);rm(Tmin9)
	zTmin9=zTmin9[ind1,ind2]; rownames(zTmin9)=x1; colnames(zTmin9)=y1
	Tmin10=readGDAL(archivos1[10])
	z <- c(Tmin10@data[,1]);zTmin10=matrix(z,n1,n2);rm(Tmin10)
	zTmin10=zTmin10[ind1,ind2]; rownames(zTmin10)=x1; colnames(zTmin10)=y1
	Tmin11=readGDAL(archivos1[11])
	z <- c(Tmin11@data[,1]);zTmin11=matrix(z,n1,n2);rm(Tmin11)
	zTmin11=zTmin11[ind1,ind2]; rownames(zTmin11)=x1; colnames(zTmin11)=y1
	Tmin12=readGDAL(archivos1[12])
	z <- c(Tmin12@data[,1]);zTmin12=matrix(z,n1,n2);rm(Tmin12)
	zTmin12=zTmin12[ind1,ind2]; rownames(zTmin12)=x1; colnames(zTmin12)=y1
	rm(z)
	
	Tmax1=readGDAL(archivos2[1])
	z <- c(Tmax1@data[,1]);zTmax1=matrix(z,n1,n2);rm(Tmax1)
	zTmax1=zTmax1[ind1,ind2]; rownames(zTmax1)=x1; colnames(zTmax1)=y1
	Tmax2=readGDAL(archivos2[2])
	z <- c(Tmax2@data[,1]);zTmax2=matrix(z,n1,n2);rm(Tmax2)
	zTmax2=zTmax2[ind1,ind2]; rownames(zTmax2)=x1; colnames(zTmax2)=y1
	Tmax3=readGDAL(archivos2[3])
	z <- c(Tmax3@data[,1]);zTmax3=matrix(z,n1,n2);rm(Tmax3)
	zTmax3=zTmax3[ind1,ind2]; rownames(zTmax3)=x1; colnames(zTmax3)=y1
	Tmax4=readGDAL(archivos2[4])
	z <- c(Tmax4@data[,1]);zTmax4=matrix(z,n1,n2);rm(Tmax4)
	zTmax4=zTmax4[ind1,ind2]; rownames(zTmax4)=x1; colnames(zTmax4)=y1
	Tmax5=readGDAL(archivos2[5])
	z <- c(Tmax5@data[,1]);zTmax5=matrix(z,n1,n2);rm(Tmax5)
	zTmax5=zTmax5[ind1,ind2]; rownames(zTmax5)=x1; colnames(zTmax5)=y1
	Tmax6=readGDAL(archivos2[6])
	z <- c(Tmax6@data[,1]);zTmax6=matrix(z,n1,n2);rm(Tmax6)
	zTmax6=zTmax6[ind1,ind2]; rownames(zTmax6)=x1; colnames(zTmax6)=y1
	Tmax7=readGDAL(archivos2[7])
	z <- c(Tmax7@data[,1]);zTmax7=matrix(z,n1,n2);rm(Tmax7)
	zTmax7=zTmax7[ind1,ind2]; rownames(zTmax7)=x1; colnames(zTmax7)=y1
	Tmax8=readGDAL(archivos2[8])
	z <- c(Tmax8@data[,1]);zTmax8=matrix(z,n1,n2);rm(Tmax8)
	zTmax8=zTmax8[ind1,ind2]; rownames(zTmax8)=x1; colnames(zTmax8)=y1
	Tmax9=readGDAL(archivos2[9])
	z <- c(Tmax9@data[,1]);zTmax9=matrix(z,n1,n2);rm(Tmax9)
	zTmax9=zTmax9[ind1,ind2]; rownames(zTmax9)=x1; colnames(zTmax9)=y1
	Tmax10=readGDAL(archivos2[10])
	z <- c(Tmax10@data[,1]);zTmax10=matrix(z,n1,n2);rm(Tmax10)
	zTmax10=zTmax10[ind1,ind2]; rownames(zTmax10)=x1; colnames(zTmax10)=y1
	Tmax11=readGDAL(archivos2[11])
	z <- c(Tmax11@data[,1]);zTmax11=matrix(z,n1,n2);rm(Tmax11)
	zTmax11=zTmax11[ind1,ind2]; rownames(zTmax11)=x1; colnames(zTmax11)=y1
	Tmax12=readGDAL(archivos2[12])
	z <- c(Tmax12@data[,1]);zTmax12=matrix(z,n1,n2);rm(Tmax12)
	zTmax12=zTmax12[ind1,ind2]; rownames(zTmax12)=x1; colnames(zTmax12)=y1
	rm(z)
	
	return(list(zTmin1=zTmin1,zTmin2=zTmin2,zTmin3=zTmin3,zTmin4=zTmin4,zTmin5=zTmin5,zTmin6=zTmin6,zTmin7=zTmin7,zTmin8=zTmin8,zTmin9=zTmin9,zTmin10=zTmin10,zTmin11=zTmin11,zTmin12=zTmin12,zTmax1=zTmax1,zTmax2=zTmax2,zTmax3=zTmax3,zTmax4=zTmax4,zTmax5=zTmax5,zTmax6=zTmax6,zTmax7=zTmax7,zTmax8=zTmax8,zTmax9=zTmax9,zTmax10=zTmax10,zTmax11=zTmax11,zTmax12=zTmax12,coords=coords,x1=x1,y1=y1))
}

##########################################
# Funcion generadora de Tasas y mortalidad
##########################################
RateI<-function(vec,Table2,Ki,parametrosc,parametrosm=NULL,funciont,funcionm=NULL,nmax,steps,J=NA,modelim)
{
	for (i in names(parametrosc)){temp <- parametrosc[i];storage.mode(temp) <- "double";assign(i, temp)}
	i=0:(steps-1)
	T1=((vec[3]-vec[2])/2)*cos(pi*(i+0.5)/steps) + (vec[3]+vec[2])/2
	if(vec[1]!=nmax){T2<-((vec[3]-Table2[vec[1]+1,2])/2)*cos(pi*(i+0.5)/steps) + (vec[3]+Table2[vec[1]+1,2])/2}else{
		T2<-((vec[3]-Table2[1,2])/2)*cos(pi*(i+0.5)/steps) + (vec[3]+Table2[1,2])/2}

	if((modelim[Ki]==1 || modelim[Ki]==2 || modelim[Ki]==3 ||
				modelim[Ki]==4 || modelim[Ki]==5 || modelim[Ki]==6 ||
				modelim[Ki]==7 || modelim[Ki]==8 || modelim[Ki]==9 ||
				modelim[Ki]==10 || modelim[Ki]==11 || modelim[Ki]==12 || modelim[Ki]==13 ||
				modelim[Ki]==14) & is.na(J)){x = T1 + 273.15;x2 = T2 + 273.15}else{x = T1;x2 = T2}

	# Rat=eval(funciont);if(!is.na(J)){Rat[Rat<=0]=0}
	# Ratetot1<-(sum(Rat))/steps    ### aqui evalua la tasa de desarrollo de todas las divisiones
	# x=x2;Rat=eval(funciont);if(!is.na(J)){Rat[Rat<=0]=0}
	# Ratetot2<-(sum(Rat))/steps    ### aqui evalua la tasa de desarrollo de todas las divisiones del segundo dia
	# Rate=(Ratetot1+Ratetot2)/2
	
	if(!is.na(J)){Rat=exp(eval(funciont));Rat[Rat<=0]=0}else{Rat=1/exp(eval(funciont))}
	Ratetot1<-(sum(Rat))/steps    ### aqui evalua la tasa de desarrollo de todas las divisiones
	x=x2;if(!is.na(J)){Rat=exp(eval(funciont));Rat[Rat<=0]=0}else{Rat=1/exp(eval(funciont))}
	Ratetot2<-(sum(Rat))/steps    ### aqui evalua la tasa de desarrollo de todas las divisiones del segundo dia
	Rate=(Ratetot1+Ratetot2)/2

	if(!is.null(funcionm))
	{
		for (i in names(parametrosm)){temp <- parametrosm[i];storage.mode(temp) <- "double";assign(i, temp)}
		x=T1;M1=eval(funcionm);M1[M1>1]=1; Mortality1 <- (sum(M1))/steps    ### aqui evalua la Mortalidad de todas las divisiones
		x=T2;M2=eval(funcionm);M2[M2>1]=1; Mortality2 <- (sum(M2))/steps    ### aqui evalua la Mortalidad de todas las divisiones del segundo dia
		Mortality=(Mortality1+Mortality2)/2
		return(c(Rate1=Ratetot1,Rate2=Rate,Mortality1=Mortality1,Mortality2=Mortality))
	}else{return(c(Rate1=Ratetot1,Rate2=Rate))}
}
######################################
simultemp.dete.fluc<-function(N,sexratio,isFixed,Table,xi,steps, poli, params, ..){
  n1<-nrow(Table)
  hfeno=params$hfeno
  estadios<-params$estadios
  modelim<-params$modelim
  modelm<-params$modelm  
  modelim=c(modelim,modelm)
  pars=data.frame(Tmin=NA,Tmax=NA,r=NA,Ro=NA,GRR=NA,T=NA,lambda=NA,Dt=NA)
  
  for(i in 1:n1)
  {
    # i<-1
    Table0=data.frame(V1=rep(Table[i,1],365),V2=rep(Table[i,2],365))
    # Table$V1<-Table$V1-0.5
    if(isFixed){
      Rs=rep(sexratio,365)
    }else{
      paramR=params$paramR
      if(is.na(paramR)){
        sexratio <- params$ff
        Rs=rep(sexratio,365)
      }else{
        Table2=cbind(id=1:nrow(Table0),Table0)
        #paramR=params$paramR
        ff=params$ff
        Rs=apply(Table2,1,RateR,Table2,nmax=nrow(Table),steps=steps,ff,paramR)
      }
    }
    # for(j in 1:reps[i])
    # {
    #cuadro<-Ratios(Table,modelim,modelme,estadios, xi,steps)$MATRIZ
    # Table0 <- Table
    cuadro <- matrizA(estadios, hfeno, Table0, steps, modelim=modelim)
    #insectos<-simulacion(cuadro,estadios,N,M,hfeno,sexratio)
    
    ageclases <- AgeClases(cuadro, estadios, hfeno)
    estadiosAges <- ageclases$estadiosAges
    oviFreq <- ageclases$oviFreq
    ageClassMatriz <- ageclases$ageClassMatriz
    Day=1
    severalYear=FALSE
    Steps=365
    simu2<-simulacionUnaGeneracion(Day,estadiosAges, oviFreq, ageClassMatriz, hfeno, cuadro, estadios,N,sexratio,Rs,Steps)
    matrizOut<-simu2$matrizOut
    
    pravida<-parameters(N, estadios, matrizOut,poli )
    parametros<-pravida$parametro
    pars<-rbind(pars,c(Tmin=Table0[i,1],Tmax=Table0[i,2],t(parametros)))
    # }
  }
  pars=pars[-1,]
  return(pars)
}

######################################
# Pasado version hasta NOV2024
# matrizA <- function(estadios, hfeno, Table, steps){
# 	inmaduros <-  estadios[-(length(estadios)-1):-(length(estadios))]
# 	maduros   <-  estadios[(length(estadios)-1):(length(estadios))]
# 	nmax=nrow(Table)
# 	matriz<-matrix(0,ncol=2*(length(inmaduros)*2+length(maduros)+1),nrow=nrow(Table))
# 	Table2=cbind(id=1:nrow(Table),Table)
# 	
# 	for(K in 1:length(inmaduros))
# 	{
# 		#  Desarrollo  # extraccion de funciones y parametros
# 		
# 		parametrosc <- hfeno$pdv_dv[[K]]
# 		funciont <- as.expression(hfeno$fdv_dv[[K]][[3]])
# 		
# 		#   Mortalidad # extraccion de funciones y parametros
# 		
# 		parametrosm <- hfeno$pmortal[[K]]
# 		funcionm <- as.expression(hfeno$mortal[[K]][[3]])
# 		
# 		RM=apply(Table2,1,RateI,Table2,K,parametrosc,parametrosm,funciont,funcionm,nmax,steps) ## procesamiento de tasa de desarrollo y mortalidad por cada temperatura
# 		matriz[,4*K-3]=RM[1,];matriz[,4*K-2]=RM[2,]
# 		matriz[,4*K-1]=RM[3,];matriz[,4*K]=RM[4,]
# 	}
# 	#  Hembras
# 	
# 	parametrosc <- hfeno$pfh_h
# 	for (i in names(parametrosc)){temp <- parametrosc[i];storage.mode(temp) <- "double";assign(i, temp)}
# 	formulac <- hfeno$fh_h
# 	funciont <- as.expression(formulac[[3]])
# 	RM=apply(Table2,1,RateI,Table2,(K+1),parametrosc,funciont=funciont,nmax=nmax,steps=steps,J=NA)
# 	matriz[,4*(K+1)-3]=RM[1,];matriz[,4*(K+1)-2]=RM[2,]
# 	
# 	#  Machos
# 	
# 	parametrosc <- hfeno$pfm_m;
# 	for (i in names(parametrosc)){temp <- parametrosc[i];storage.mode(temp) <- "double";assign(i, temp)}
# 	formulac <- hfeno$fm_m
# 	funciont <- as.expression(formulac[[3]])
# 	RM=apply(Table2,1,RateI,Table2,(K+2),parametrosc,funciont=funciont,nmax=nmax,steps=steps,J=NA)
# 	matriz[,4*(K+1)-1]=RM[1,];matriz[,4*(K+1)]=RM[2,]
# 	matriz[matriz>1]<-1;matriz[matriz<0]<-0
# 	
# 	#  Fecundidad
# 	
# 	parametrosc <- hfeno$ptazaeh_h
# 	for (i in names(parametrosc)){temp <- parametrosc[i];storage.mode(temp) <- "double";assign(i, temp)}
# 	formulac <- hfeno$ftazaeh_h
# 	funciont <- as.expression(formulac[[3]])
# 	RM=apply(Table2,1,RateI,Table2,(K+3),parametrosc,funciont=funciont,nmax=nmax,steps=steps,J=3)
# 	matriz[,4*(K+1)+1]=RM[1,];matriz[,4*(K+1)+2]=RM[2,]
# 	
# 	vectorPares = (1:(ncol(matriz)/2))*2
# 	matrizA = matriz[,vectorPares]
# 	return(matrizA)
# }
matrizA <- function(estadios, hfeno, Table, steps, modelim){
  inmaduros <-  estadios[-(length(estadios)-1):-(length(estadios))]
  maduros   <-  estadios[(length(estadios)-1):(length(estadios))]
  nmax=nrow(Table)
  matriz<-matrix(0,ncol=2*(length(inmaduros)*2+length(maduros)+1),nrow=nrow(Table))
  Table2=cbind(id=1:nrow(Table),Table)
  
  for(K in 1:length(inmaduros))
  {
    #  Desarrollo  # extraccion de funciones y parametros
    
    parametrosc <- hfeno$pdv_dv[[K]]
    #funciont <- as.expression(hfeno$fdv_dv[[K]][[3]])
    funciont <- as.expression(as.formula(hfeno$fdv_dv[[K]])[[3]])
    
    #   Mortalidad # extraccion de funciones y parametros
    
    parametrosm <- hfeno$pmortal[[K]]
    funcionm <- as.expression(as.formula(hfeno$mortal[[K]])[[3]])
    
    RM=apply(Table2,1,RateI,Table2,K,parametrosc,parametrosm,funciont,funcionm,nmax,steps,modelim=modelim) ## procesamiento de tasa de desarrollo y mortalidad por cada temperatura
    matriz[,4*K-3]=RM[1,];matriz[,4*K-2]=RM[2,]
    matriz[,4*K-1]=RM[3,];matriz[,4*K]=RM[4,]
  }
  #  Hembras
  
  parametrosc <- hfeno$pfh_h
  for (i in names(parametrosc)){temp <- parametrosc[i];storage.mode(temp) <- "double";assign(i, temp)}
  formulac <- as.formula(hfeno$fh_h)
  funciont <- as.expression(formulac[[3]])
  RM=apply(Table2,1,RateI,Table2,(K+1),parametrosc,funciont=funciont,nmax=nmax,steps=steps,J=NA,modelim=modelim)
  matriz[,4*(K+1)-3]=RM[1,];matriz[,4*(K+1)-2]=RM[2,]
  
  #  Machos
  
  parametrosc <- hfeno$pfm_m;
  for (i in names(parametrosc)){temp <- parametrosc[i];storage.mode(temp) <- "double";assign(i, temp)}
  formulac <- as.formula(hfeno$fm_m)
  funciont <- as.expression(formulac[[3]])
  RM=apply(Table2,1,RateI,Table2,(K+2),parametrosc,funciont=funciont,nmax=nmax,steps=steps,J=NA,modelim=modelim)
  matriz[,4*(K+1)-1]=RM[1,];matriz[,4*(K+1)]=RM[2,]
  matriz[matriz>1]<-1;matriz[matriz<0]<-0
  
  #  Fecundidad
  
  parametrosc <- hfeno$ptazaeh_h
  for (i in names(parametrosc)){temp <- parametrosc[i];storage.mode(temp) <- "double";assign(i, temp)}
  formulac <- as.formula(hfeno$ftazaeh_h)
  funciont <- as.expression(formulac[[3]])
  RM=apply(Table2,1,RateI,Table2,(K+3),parametrosc,funciont=funciont,nmax=nmax,steps=steps,J=3,modelim=modelim)
  matriz[,4*(K+1)+1]=RM[1,];matriz[,4*(K+1)+2]=RM[2,]
  
  vectorPares = (1:(ncol(matriz)/2))*2
  matrizA = matriz[,vectorPares]
  return(matrizA)
}

AgeClases<- function(cuadro, estadios, hfeno){
  matrizA=cuadro
  sizeMatrizA = nrow(matrizA)
  inmad <-  estadios[-(length(estadios)-1):-(length(estadios))]
  numInmaduros = length(inmad)
  maduros   <-  estadios[(length(estadios)-1):(length(estadios))]
  numMaduros = length(maduros)
  estadiosAges<-list(1)
  ageClassMatriz = matrix(NA, nrow=sizeMatrizA, ncol=(numInmaduros+numMaduros))
  
  Day = 1
  Ageclass = 1
  age = 0
  
  #primera linea
  for(k in 1:(numInmaduros + numMaduros)){
    Ageclass = 1
    age = 0
    #Ageclass0 = c(rep(NA, 253))# porq 253??
    Ageclass0 = c(rep(NA, nrow(matrizA)))# porq 253??
    matrizAges = matrix(NA, nrow=sizeMatrizA, ncol=(length(Ageclass0)+1))
    #matrizAges = matrix(NA, nrow=366, ncol=(length(Ageclass0)+1))
    matrizAges[Day,1] = 0
    
    ## if(k==1){
    ##     Ageclass = Ageclass + 1
    ## }
    
    Ageclass = Ageclass + 1
    #for(Ii in 2:length(Ageclass0)){
    for(Ii in 1:length(Ageclass0)){
      #print(Ii)
      if(k == (numInmaduros + numMaduros)){
        Ageclass0[Ii] = matrizA[(sizeMatrizA+1)-Ii,k*2-2]
      }else{
        Ageclass0[Ii] = matrizA[(sizeMatrizA+1)-Ii,k*2-1]
      }
      
      age = age + Ageclass0[Ii]
      matrizAges[Day,Ii+1] = age
    }
    if(k <= numInmaduros){
      ageClassMatriz[Day,k] = length(matrizAges[1,][matrizAges[1,]<2])+1
    }else{
      ageClassMatriz[Day,k] = length(matrizAges[1,][matrizAges[1,]<hfeno$slope_snh])+1 # antes 1.8
    }
    estadiosAges[[k]] = matrizAges
  }
  
  # a partir de la 2da linea
  for(k in 1:(numInmaduros + numMaduros)){
    Day=2
    Ageclass = 1
    age = 0
    matrizAges = estadiosAges[[k]]
    while(Day <= sizeMatrizA){#porq 367 en excel?
      matrizAges[Day,1] = 0
      if(k == (numInmaduros + numMaduros)){
        Rate = matrizA[Day,k*2-2]
      }else{
        Rate = matrizA[Day,k*2-1]
      }
      
      if(k <= numInmaduros){
        while(age < 2){ # cambiar a valor del slop
          ageacc = matrizAges[Day-1, Ageclass]
          age = ageacc + Rate
          matrizAges[Day, Ageclass+1] = age
          Ageclass = Ageclass + 1
          
          if(Ageclass == nrow(matrizA)){ ## por que nrow(matrizA)=254?????
            age = 3
          }
        }
        ageClassMatriz[Day,k] = Ageclass
      }else{
        while(age < hfeno$slope_snh){ # cambiar a valor del slop (antes 1.8)
          ageacc = matrizAges[Day-1, Ageclass]
          age = ageacc + Rate
          matrizAges[Day, Ageclass+1] = age
          Ageclass = Ageclass + 1
          
          
          if(Ageclass == nrow(matrizA)){     ## antes : nrow(matrizA)=255){
            age = 3
          }
        }
        ageClassMatriz[Day,k] = Ageclass
      }
      age = 0
      Day = Day + 1
      Ageclass = 1
    }
    estadiosAges[[k]] = matrizAges
  }
  
  ## Ovifreq ##
  matrizAges = estadiosAges[[numInmaduros+1]]
  oviFreq = matrix(NA, nrow=sizeMatrizA, ncol=length(Ageclass0))
  Day = 1
  Ageclass = 1
  age1 = 0
  age2 = 0
  
  # parametrosc <- hfeno$povih_h
  # parametrosc<-as.list(parametrosc)
  # for (i in names(parametrosc)){
  #   temp <- parametrosc[[i]]
  #   storage.mode(temp) <- "double"
  #   assign(i, temp)
  # }
  # 
  # formulac <- as.formula(hfeno$fovih_h)
  # forexc <- formulac[[length(formulac)]]
  # funcionc <- as.expression(forexc)
  
  while(Day < sizeMatrizA){#porq 367 en excel?
    matrizAges[Day,1] = 0
    age1 = matrizAges[Day, Ageclass]
    x = age1
    #OviFrec1 = eval(funcionc)
    OviFrec1 = distriModelOvipTime(x,hfeno)
    
    while(age2 < hfeno$slope_snh & Ageclass<nrow(matrizA)){ # nrow(matrizA)=254?, otro antes 1.8
      age2 = matrizAges[Day, Ageclass]
      x= age2
      #OviFrec2 = eval(funcionc)
      OviFrec2 = distriModelOvipTime(x,hfeno)
      OviFrec = OviFrec2 - OviFrec1
      OviFrec1 = OviFrec2
      if(Ageclass == 1){
        OviFrec = 0
      }
      oviFreq[Day,Ageclass] = OviFrec
      Ageclass = Ageclass + 1
    }
    age2 = 0
    Day = Day + 1
    Ageclass = 1
  }
  
  
  return(list(estadiosAges=estadiosAges, oviFreq=oviFreq, ageClassMatriz=ageClassMatriz))
}

distriModelOvipTime<-function(vec,hfeno){
  # probit ###################################
  if(hfeno$distri_oviptime=="lognormal")
  {
    WW<-pnorm(log(vec)*hfeno$slope_oviptime)
  }
  # logit
  if(hfeno$distri_oviptime=="loglogistic")
  {
    WW<-1/(1+exp(-(log(vec)*hfeno$slope_oviptime)))
  }
  # cloglog
  if(hfeno$distri_oviptime=="weibull")
  {
    WW<-1-exp(-exp(log(vec)*hfeno$slope_oviptime))
  }
  return(WW)
}

simulacionUnaGeneracion <- function(Day, estadiosAges, oviFreq, ageClassMatriz, hfeno, cuadro, estadios,numIni,sexratio,Rs,Steps){
  Step=0
  #ageClassMatriz
  matrizA = cuadro
  
  sizeMatriz = nrow(matrizA)
  
  inmad <-  estadios[-(length(estadios)-1):-(length(estadios))]
  
  numInmaduros = length(inmad)
  
  maduros   <-  estadios[(length(estadios)-1):(length(estadios))]
  
  numMaduros = length(maduros)
  
  NEWIND = 0
  
  dias<-nrow(matrizA)#porq?
  
  
  
  #matrizOut = matrix(NA, nrow=(nrow(matrizA)-1), (numInmaduros+numMaduros+1))
  
  matrizOut = matrix(NA, nrow=nrow(matrizA), (numInmaduros + numMaduros + 1))
  
  stagesN = c(rep(0,dias))
  
  stagesN1 = c(numIni, rep(0,(dias-1)))
  
  femalesN = c(rep(NA,dias))
  
  eggsN = stagesN1
  
  stagesDev = c(rep(0,dias))
  
  
  
  ## vectorN <- list(1)
  
  ## vectorNini <- list(1)
  
  
  
  #if(!severalYear){
  
  vectorN <- list(1)
  
  vectorNini <- list(1)
  
  for(k in 1:(numInmaduros + numMaduros)){
    
    v1 = c(100,rep(0,(ageClassMatriz[1,k]-1)))
    
    v2 = c(0,rep(0,(ageClassMatriz[1,k]-1)))
    
    v3 = c(0,rep(0,(ageClassMatriz[1,k]-1)))#
    
    
    
    if(k == 1){
      
      vectorN[[k]] = list(v1,v2,v3)#
      
      vectorNini[[k]] = v1
      
    }else{
      
      vectorN[[k]] = list(v2,v2,v3)#
      
      vectorNini[[k]] = v2
      
    }
    
  }
  
  
  
  matrizOut[1,] = c(numIni,rep(0,(numInmaduros + numMaduros)))# numero inicial de individuos
  
  #}
  
  ## else{
  
  ##     vectorNini <- list(1)
  
  ##     for(k in 1:(numInmaduros + numMaduros)){
  
  ##         vectorNini[[k]] <- vectorN[[k]][[1]]
  
  ##         matrizOut[1,k] = round(numInd[[k]])
  
  ##     }
  
  ## }
  
  
  
  #while(Day < (sizeMatriz-1)){
  
  while(Step < Steps-1){
    #print(Day)
    for(k in 1:(numInmaduros + numMaduros)){
      
      #Ageclass = ageClassMatriz[Day,k]
      
      Ageclass = ageClassMatriz[Day+1,k]
      
      
      
      if(k <= numInmaduros){
        
        stagesN = c(rep(0,Ageclass))
        
      }
      
      
      
      if(k == (numInmaduros+1)){
        
        femalesN = c(rep(0,Ageclass))
        
      }
      
      
      
      matrizAges = estadiosAges[[k]]
      
      NEWIND = 0
      
      
      
      E <- c(rep(0, Ageclass))
      
      EL <- c(rep(0, Ageclass))
      
      Eage <- c(rep(0, Ageclass))
      
      pacc <- c(rep(0, Ageclass))
      
      p1 <- c(rep(0, Ageclass))
      
      p2 <- c(rep(0, Ageclass))
      
      
      
      if(k <= numInmaduros){
        
        s1 <- c(rep(NA, Ageclass))
        
      }
      
      
      
      if(k == numInmaduros+1){
        
        vectorN[[k]][[2]] = vectorN[[k]][[3]]#
        
      }
      
      if(k == numInmaduros+2){
        
        vectorN[[k]][[2]] = vectorN[[k]][[3]]#
        
      }
      
      
      
      for(Ii in 1:(Ageclass-1)){
        
        Eage[Ii] = matrizAges[Day+1, Ii+1]
        
        
        
        if(k <= numInmaduros){
          
          survivalk = (1 - matrizA[Day,k*2]) ^ matrizA[Day,k*2-1]
          
        }
        
        
        
        if(k <= numInmaduros){
          
          if(Eage[Ii] > 1){
            
            s1[Ii] = 1
            
          }else{
            
            s1[Ii] = survivalk
            
          }
          
        }
        
        
        
        if(k <= numInmaduros){
          
          pacc[Ii] = distrimodeldeim(Eage[Ii], k, hfeno)
          
        }else{
          
          pacc[Ii] = distrimodeldema(Eage[Ii], k - numInmaduros, hfeno)
          
        }
        
        
        
        if(Ii == 1){
          
          p1[Ii] = pacc[Ii] - 0
          
        }else{
          
          p1[Ii] = pacc[Ii] - pacc[Ii - 1]
          
        }
        
        
        
        if(p1[Ii] == 0){
          
          p2[Ii] = 0
          
        }else{
          
          if(Ii == 1){
            
            p2[Ii] = p1[Ii] / (1 - 0)
            
          }else{
            
            p2[Ii] = p1[Ii] / (1 - pacc[Ii - 1])
            
          }
          
        }
        
        
        
        if(Day==1){
          
          E[Ii] = vectorNini[[k]][Ii]
          
        }else{
          
          E[Ii] = vectorN[[k]][[1]][Ii]#modifique el 1 por el 2
          
        }
        
        
        
        
        
        if(k <= numInmaduros){
          
          EL[Ii] = E[Ii] * p2[Ii] * s1[Ii]
          
        }else{
          
          EL[Ii] = E[Ii] * p2[Ii]
          
        }
        
        
        
        if(k <= numInmaduros){
          
          E1 = E[Ii] * s1[Ii] - EL[Ii]
          
        }else{
          
          E1 = E[Ii] - EL[Ii]
          
        }
        
        
        
        stagesN[Ii+1] = E1
        
        
        
        if(k <= numInmaduros){
          
          NEWIND = NEWIND + EL[Ii]
          
        }
        
        
        
      } # fin for clases
      
      
      
      vectorN[[k]][[2]] = c(vectorN[[k]][[2]][1], stagesN[2:length(stagesN)])
      
      vectorN[[k]][[1]] = vectorN[[k]][[2]]
      
      
      
      if(k <= (numInmaduros-1)){
        
        vectorN[[k]][[3]][1]=NEWIND
        
        vectorN[[k+1]][[2]] = vectorN[[k]][[3]]
        
      }
      
      
      
      if(k == numInmaduros){
        
        vectorN[[k+1]][[3]][1] = NEWIND * Rs[Day]
        
        vectorN[[k+2]][[3]][1] = NEWIND * (1-Rs[Day])
        
        
        
        vectorN[[k+1]][[2]] = vectorN[[k]][[3]]#
        
      }
      
      
      
      if(k == (numInmaduros+1)){
        
        femalesN = E
        
      }
      
      
      
      matrizOut[Day+1,k] = round(sum(vectorN[[k]][[2]]))
      
      #matrizOut[1:3,]
      
      
      
    }#fin for estadios
    
    
    
    
    
    #Oviposition
    
    Ageclass = ageClassMatriz[Day,numInmaduros+1]
    
    matrizAges = estadiosAges[[numInmaduros+1]]
    
    Ovi = c(rep(0,Ageclass))
    
    
    
    E <- c(rep(0, Ageclass))
    
    EL <- c(rep(0, Ageclass))
    
    Eage <- c(rep(0, Ageclass))
    
    pacc <- c(rep(0, Ageclass))
    
    p1 <- c(rep(0, Ageclass))
    
    p2 <- c(rep(0, Ageclass))
    
    
    
    for(Ii in 1:(Ageclass-2)){
      
      E[Ii] = vectorN[[k-1]][[2]][Ii]
      
      Eage[Ii] = oviFreq[Day, Ii]
      
      E1 = matrizA[Day,k*2-1]
      
      E1 = E1 * Eage[Ii] * E[Ii]
      
      Ovi[Ii+1] = E1
      
      NEWIND = NEWIND + E1
      
    }
    
    
    
    Ovi = na.omit(Ovi)
    
    matrizOut[Day+1,(numInmaduros + numMaduros +1)] = round(sum(Ovi),2)
    
    
    
    #vectorN[[1]][[2]][1] = NEWIND
    
    matrizOut[Day+1,1] = round(sum(vectorN[[1]][[2]]))
    
    vectorN[[1]][[1]] = vectorN[[1]][[2]]
    
    
    
    #                      if(round(matrizOut[Day,(numInmaduros + numMaduros)]) > 0 && round(matrizOut[Day+1,(numInmaduros + numMaduros)]) == 0){
    
    #                                  break;
    
    #                      }
    
    if(matrizOut[Day,1]==0 && matrizOut[Day,2]==0 && matrizOut[Day,3]==0 && matrizOut[Day,4]==0 && matrizOut[Day,5]==0){
      
      #matrizOut[Day:nrow(matrizOut),]="Dead"
      break;
      
    }
    
    
    
    NEWIND=0
    
    Day=Day+1
    
    Step = Step + 1
    
  }#fin bucle steps
  
  
  
  
  
  namesMatriz = c(estadios, "New Egg");
  
  colnames(matrizOut) = namesMatriz
  
  matrizOutDead = matrizOut
  matrizOutDead[is.na(matrizOutDead)]="Dead"
  
  matrizOut[is.na(matrizOut)]=0
  
  return(list(matrizOut=matrizOut, vectorN=vectorN, matrizOutDead=matrizOutDead))
}

distrimodeldeim<-function(vec,sll,hfeno) ## this is GML function
{
  # probit
  if(hfeno$distri_dv[[sll]]=="lognormal")
  {
    pdd<-pnorm(log(vec)*hfeno$slope_dv[[sll]])
  }
  # logit
  if(hfeno$distri_dv[[sll]]=="loglogistic")
  {
    pdd<-1/(1+exp(-(log(vec)*hfeno$slope_dv[[sll]])))
  }
  # cloglog
  if(hfeno$distri_dv[[sll]]=="weibull")
  {
    pdd<-1-exp(-exp(log(vec)*hfeno$slope_dv[[sll]]))
  }
  return(pdd)
}

distrimodeldema<-function(vec,sll,hfeno)
{
  if(sll==1)
  {
    #hembras
    # probit
    if(hfeno$distri_snh=="lognormal")
    {
      pdd<-pnorm(log(vec)*hfeno$slope_snh)
    }
    # logit
    if(hfeno$distri_snh=="loglogistic")
    {
      pdd<-1/(1+exp(-(log(vec)*hfeno$slope_snh)))
    }
    # cloglog
    if(hfeno$distri_snh=="weibull")
    {
      pdd<-1-exp(-exp(log(vec)*hfeno$slope_snh))
    }
  }
  if(sll==2)
  {
    #machos
    # probit
    if(hfeno$distri_snm=="lognormal")
    {
      pdd<-pnorm(log(vec)*hfeno$slope_snm)
    }
    # logit
    if(hfeno$distri_snm=="loglogistic")
    {
      pdd<-1/(1+exp(-(log(vec)*hfeno$slope_snm)))
    }
    # cloglog
    if(hfeno$distri_snm=="weibull")
    {
      pdd<-1-exp(-exp(log(vec)*hfeno$slope_snm))
    }
  }
  return(pdd)
}

parameters <- function (N, estad, ltb,poli=1){
  ltb<-ltb[!apply(ltb,1,sum)==0,] ## considera solo los conteos del insecto vivo
  estadios <- estad
  lifetable <- ltb
  s.x <- m.x <- l.x <- rep(0, nrow(lifetable))
  for (i in 1:nrow(lifetable)) l.x[i] <- sum(lifetable[i, 1:(length(estadios))])/N  ## porcentaje de vivos por dias
  # for (i in 1:nrow(lifetable)) if(l.x[i]==0) m.x[i] <- 0 else m.x[i] <- lifetable[,(length(estad)+1)][i]/N/l.x[i]
  for (i in 1:nrow(lifetable)) if(l.x[i]==0) m.x[i] <- 0 else m.x[i] <- poli*lifetable[,(length(estad)+1)][i]/N/l.x[i]
  ## el objeto m.x no es afectado por el efecto poliembrionico pero asumimos la mortalidad del host es la unica que afecta ala mortalidad del parasitoide
  s.x1 <- c(l.x[1], l.x[2])
  s.x2 <- rep(0, nrow(lifetable) - 2)
  for (i in 1:(nrow(lifetable) - 2)) s.x2[i] <- l.x[i + 2]/l.x[i + 1]
  s.x <- c(s.x1, s.x2) ## es el porcentaje de vivos segun la cantidad del dia anterior
  Ro <- sum(l.x * m.x)  ## este parametro depende del procentaje de vivos de los inmaduros
  Day <- 0:(nrow(lifetable)-1)
  Tg <- sum(Day * l.x * m.x)/Ro
  GRR <- sum(m.x)
  
  r <- log(Ro)/Tg
  euler <- sum(l.x * m.x * exp(-r * (Day + 1)))
  
  r1 <- r - 1e-06 # ????
  for (k in 1:100) {
    #k<-1
    euler1 <- sum(l.x * m.x * exp(-r1 * (Day + 1)))
    r2 <- ((1 - euler) * (r1 - r)/(euler1 - euler)) + r
    r1 <- r2
  }
  rf <- r1
  Dt <- log(2)/rf
  lamda <- exp(rf)
  T <- log(Ro)/rf
  parameters <- t(data.frame(r = rf, Ro = Ro, GRR = GRR, T = T, lambda = lamda, Dt = Dt))  # Cambio 2019
  colnames(parameters) <- "Parameters"
  print(parameters)
  if(poli>1)
  {
    matrizOut=data.frame(ltb[,1:(length(estadios)-2)],poli*ltb[,(ncol(ltb)-2):ncol(ltb)])
    nombres=colnames(ltb);nombres2=c(paste(nombres[1:(length(estadios)-2)],"Host",sep="_"),paste(nombres[(ncol(ltb)-2):ncol(ltb)],"Paras",sep="_"))
    colnames(matrizOut)=nombres2
  }else{matrizOut=ltb}
  return(list(ltb=matrizOut,m.x=m.x,s.x=s.x,l.x=as.matrix(l.x),parametro=parameters))
}

####################################################################
####################################################################
p.area<-function(ilat,R){
	R=R+1
	lats=seq(ilat[1],ilat[2],length.out=R)+0.0000000001
	mat.lat=matrix(NA,R-1,2)
	for(i in 2:R) 
	{
		mat.lat[i-1,]=c(lats[i-1],lats[i])
	}
	return(mat.lat)
}
######################################
######################################
lifeTableParameters <- function(estadios, hfeno, Table, steps){
	inmaduros <-  estadios[-(length(estadios)-1):-(length(estadios))]
	maduros   <-  estadios[(length(estadios)-1):(length(estadios))]
	nmax=nrow(Table)
	matriz<-matrix(0,ncol=2*(length(inmaduros)*2+length(maduros)+1),nrow=nrow(Table))
	Table2=cbind(id=1:nrow(Table),Table)
	
	for(K in 1:length(inmaduros))
	{
		#  Desarrollo  # extraccion de funciones y parametros
		
		parametrosc <- hfeno$pdv_dv[[K]]
		funciont <- as.expression(hfeno$fdv_dv[[K]][[3]])
		
		#   Mortalidad # extraccion de funciones y parametros
		
		parametrosm <- hfeno$pmortal[[K]]
		funcionm <- as.expression(hfeno$mortal[[K]][[3]])
		
		RM=apply(Table2,1,RateI,Table2,K,parametrosc,parametrosm,funciont,funcionm,nmax,steps) ## procesamiento de tasa de desarrollo y mortalidad por cada temperatura
		matriz[,4*K-3]=RM[1,];matriz[,4*K-2]=RM[2,]
		matriz[,4*K-1]=RM[3,];matriz[,4*K]=RM[4,]
	}
	#  Hembras
	
	parametrosc <- hfeno$pfh_h
	for (i in names(parametrosc)){temp <- parametrosc[i];storage.mode(temp) <- "double";assign(i, temp)}
	formulac <- hfeno$fh_h
	funciont <- as.expression(formulac[[3]])
	RM=apply(Table2,1,RateI,Table2,(K+1),parametrosc,funciont=funciont,nmax=nmax,steps=steps,J=NA)
	matriz[,4*(K+1)-3]=RM[1,];matriz[,4*(K+1)-2]=RM[2,]
	
	#  Machos
	
	parametrosc <- hfeno$pfm_m;
	for (i in names(parametrosc)){temp <- parametrosc[i];storage.mode(temp) <- "double";assign(i, temp)}
	formulac <- hfeno$fm_m
	funciont <- as.expression(formulac[[3]])
	RM=apply(Table2,1,RateI,Table2,(K+2),parametrosc,funciont=funciont,nmax=nmax,steps=steps,J=NA)
	matriz[,4*(K+1)-1]=RM[1,];matriz[,4*(K+1)]=RM[2,]
	matriz[matriz>1]<-1;matriz[matriz<0]<-0
	
	#  Fecundidad
	
	parametrosc <- hfeno$ptazaeh_h
	for (i in names(parametrosc)){temp <- parametrosc[i];storage.mode(temp) <- "double";assign(i, temp)}
	formulac <- hfeno$ftazaeh_h
	funciont <- as.expression(formulac[[3]])
	RM=apply(Table2,1,RateI,Table2,(K+3),parametrosc,funciont=funciont,nmax=nmax,steps=steps,J=3)
	matriz[,4*(K+1)+1]=RM[1,];matriz[,4*(K+1)+2]=RM[2,]
	
	
	TF=data.frame(Ind=1:nmax)
	for(i in 1:K){TF1=data.frame(DaiSurv1=(1-matriz[,4*i-1])^matriz[,4*i-3],DaiSurv2=(1-matriz[,4*i])^matriz[,4*i-2]);TF=cbind(TF,TF1)}
	for(i in 1:K){Gl0=data.frame(Gl01=1/matriz[,4*i-3],Gl02=1/matriz[,4*i-2]);TF=cbind(TF,Gl0)}
	TF$Gl11=(1/matriz[,4*(K)+1])/(1/xi);TF=TF[,-1]
	temp1=TF[,c(2*K+seq(1,2*K,2),ncol(TF))];temp2=TF[,c(2*K+seq(2,2*K,2),ncol(TF))]
	TF$Gl1=apply(temp1,1,sum);TF$Gl2=apply(temp2,1,sum)
	temp1<-temp2<-1;for(j in 1:K){temp1=temp1*(1-matriz[,4*i-1]);temp2=temp2*(1-matriz[,4*i])}
	TF$Is1=temp1;TF$Is2=temp2
	TF$Ro1=(TF$Is1*matriz[,4*K+5])/2;TF$Ro1[TF$Ro1<0]=0; TF$Ro2=(TF$Is2*matriz[,4*K+6])/2;TF$Ro2[TF$Ro2<0]=0
	TF$rm1=log(TF$Ro1)/TF$Gl1; TF$rm2=log(TF$Ro2)/TF$Gl2
	TF$Fr1=exp(TF$rm1); TF$Fr2=exp(TF$rm2)
	TF$Dt1=log(2)/TF$rm1; TF$Dt2=log(2)/TF$rm2	
	
	meses=c(31,28,31,30,31,30,31,31,30,31,30,31);r1=rep(1,nrow(TF))
	GI=sum((meses-1)/TF[,"Gl1"] + 1/TF[,"Gl2"])
	#temp.fr1=TF[,"Fr1"];ind1=temp.fr1>1;temp.fr1=temp.fr1[ind1];temp.meses=meses[ind1];temp.fr2=TF[,"Fr2"][ind1]
	temp.fr1=TF[,"Fr1"];ind1=temp.fr1>1;temp.fr1=temp.fr1[ind1];temp.meses=meses[ind1]
	temp.fr2=TF[,"Fr2"];ind2=temp.fr2>1;temp.fr2=temp.fr2[ind2]
	if(length(temp.fr2)==0){temp.fr2=1}
	if(length(temp.fr1)==0){AI=NA}else{AI=log(prod(c(temp.fr1^(temp.meses-1),temp.fr2)),10)}
	#if(length(temp.fr1)==0){AI=NA}else{AI=log(prod(temp.fr1^(temp.meses-1)*temp.fr2),10)}
	ERI=1;for(s in 1:K){n0=(sum(meses[TF[,2*s-1]==0]-1) + sum(r1[TF[,2*s]==0]))/365;ERI=ERI*(1-n0)} 
	
	indices=c(AI=AI,GI=GI,ERI=ERI)
	return(list(indices=indices, TF=TF))
}
#############################################
#############################################

showParameterGraph <- function(TdV, parm, corry, grises){
	if(grises==TRUE){ccol="gray20"}else{ccol=2}	
	
	TdV[is.infinite(TdV)] = NA
	TdV[is.nan(TdV)] = NA
	laby = c("Generation length", "Net reproduction rate", "Intrinsic rate of increase", "Finite rate of increase", "Doubling time")
	plot(1:12,TdV,ylim=corry,xlim=c(1,12),type="l",col=ccol,frame=F,axes=F,ylab=list(laby[parm], cex=0.9),xlab="", lwd=2)
	axis(2, at=seq(round(corry[1],2), round(corry[2],2), length.out=5), las=2, cex.axis=0.7)
	
	par(new=TRUE)
	mes<-1:12
	plot(mes,TdV,xlab=list("Julian day",cex=0.8),ylab="",type="n",axes=F)
	dias <- c(0,"","",100,"","",200,"","",300,"","")
	axis(1,at=1:12, dias, cex.axis=0.7)
	par(new=TRUE)
} 
#############################################################################################################
graff_index<-function(TdV, grises)#,leg)
{
	TdV[is.infinite(TdV)] = NA#vefiricar a temperaturas bajas
	TdV[is.nan(TdV)] = NA
	if(grises==TRUE){ccol=c("gray40", "gray60", "gray20")}else{ccol=1:3}
	plot(1:12,TdV,ylim=c(0,max(TdV)),xlim=c(1,12),type="l",col=ccol[3],frame=F,axes=F,ylab="",xlab="")
	axis(4)
	#legend(10.5,min(TdV),leg,col="blue",text.col="blue",lty=1)
	par(new=TRUE)
	mes<-1:12
	plot(mes,TdV,xlab="month",ylab="",type="n",axes=F)
	meses<-c("J", "F", "M", "A", "M", "J", "J", "A" ,"S", "O" ,"N", "D")
	axis(1,at=1:12,lab=meses)
	par(new=TRUE)
	matplot(1:12,Table[,1:2],type="l",col=ccol[2],frame=F,axes=F,ylab="T C",xlab="",lwd=2)
	axis(2,seq(round(min(Table[,1])),max(round(Table[,2])),length=6))  
	text(2.5,max(Table[,1])+0.5,"min",col=ccol[1])
	text(2.5,max(Table[,2])+0.5,"max",col=ccol[1])
}

###########################################
###########################################
matriz<-function(data)
{
	mes<-c(31,28,31,30,31,30,31,31,30,31,30,31)
	mat<-cbind(round(seq(data[,1][1],data[,1][2],length=mes[1]),1),round(seq(data[,2][1],data[,2][2],length=mes[1]),1))
	for(i in 2:nrow(data))
	{
		if(i != nrow(data)) mat<-rbind(mat,cbind(round(seq(data[,1][i],data[,1][i+1],length=mes[i]),1),round(seq(data[,2][i],data[,2][i+1],length=mes[i]),1)))
		else mat<-rbind(mat,cbind(round(seq(data[,1][i],data[,1][i-11],length=mes[i]),1),round(seq(data[,2][i],data[,2][i-11],length=mes[i]),1)))
	}
	return(list(temperaturas=mat))
}

###########################################
###########################################
extractTempsFromDB<-function(long, lat, dir){
	archivos1=list.files(dir1,pattern="flt");archivos1=paste(dir1,"/",archivos1,sep="")
	archivos2=list.files(dir2,pattern="flt");archivos2=paste(dir2,"/",archivos2,sep="")
	
	Tmin1=readGDAL(archivos1[1])
	
	# exttrae resolucion
	v1=Tmin1@grid@cellsize[1]/2
	v2=Tmin1@grid@cellsize[2]/2
	
	## Extraccion de las longitudes y latitudes
	geodat=data.frame(coordinates(Tmin1))
	x <- c(geodat[,1]);x <- unique(x)
	n1=length(x) ## tama?o
	
	y <- c(geodat[,2]); y <- unique(y)
	n2=length(y)
	rm(geodat)
	
	
	#se indica coordenadas
	r11=long;r12=long;r21=lat;r22=lat
	
	#extrae posicion en la matriz
	ind1<-1:n1
	d1=x>=(r11-v1) & x<=(r12+v1)
	x1=x[d1]
	ind1=ind1[d1]
	
	ind2<-1:n2
	d2=y>=(r21-v2) & y<=(r22+v2)
	y1=y[d2]
	ind2=ind2[d2]
	
	z <- c(Tmin1@data[,1]); zTmin1=matrix(z,n1,n2); rm(Tmin1)
	valueT1=zTmin1[ind1,ind2] / 10
	Tmin2=readGDAL(archivos1[2])
	z <- c(Tmin2@data[,1]); zTmin2=matrix(z,n1,n2); rm(Tmin2)
	valueT2=zTmin2[ind1,ind2] / 10
	Tmin3=readGDAL(archivos1[3])
	z <- c(Tmin3@data[,1]); zTmin3=matrix(z,n1,n2); rm(Tmin3)
	valueT3=zTmin3[ind1,ind2] / 10
	Tmin4=readGDAL(archivos1[4])
	z <- c(Tmin4@data[,1]); zTmin4=matrix(z,n1,n2); rm(Tmin4)
	valueT4=zTmin4[ind1,ind2] / 10
	Tmin5=readGDAL(archivos1[5])
	z <- c(Tmin5@data[,1]); zTmin5=matrix(z,n1,n2); rm(Tmin5)
	valueT5=zTmin5[ind1,ind2] / 10
	Tmin6=readGDAL(archivos1[6])
	z <- c(Tmin6@data[,1]); zTmin6=matrix(z,n1,n2); rm(Tmin6)
	valueT6=zTmin6[ind1,ind2] / 10
	Tmin7=readGDAL(archivos1[7])
	z <- c(Tmin7@data[,1]); zTmin7=matrix(z,n1,n2); rm(Tmin7)
	valueT7=zTmin7[ind1,ind2] / 10
	Tmin8=readGDAL(archivos1[8])
	z <- c(Tmin8@data[,1]); zTmin8=matrix(z,n1,n2); rm(Tmin8)
	valueT8=zTmin8[ind1,ind2] / 10
	Tmin9=readGDAL(archivos1[9])
	z <- c(Tmin9@data[,1]); zTmin9=matrix(z,n1,n2); rm(Tmin9)
	valueT9=zTmin9[ind1,ind2] / 10
	Tmin10=readGDAL(archivos1[10])
	z <- c(Tmin10@data[,1]); zTmin10=matrix(z,n1,n2); rm(Tmin10)
	valueT10=zTmin10[ind1,ind2] / 10
	Tmin11=readGDAL(archivos1[11])
	z <- c(Tmin11@data[,1]); zTmin11=matrix(z,n1,n2); rm(Tmin11)
	valueT11=zTmin11[ind1,ind2] / 10
	Tmin12=readGDAL(archivos1[12])
	z <- c(Tmin12@data[,1]); zTmin12=matrix(z,n1,n2); rm(Tmin12)
	valueT12=zTmin12[ind1,ind2] / 10
	
	Tmax1=readGDAL(archivos2[1])
	z <- c(Tmax1@data[,1]); zTmax1=matrix(z,n1,n2); rm(Tmax1)
	valueTm1=zTmax1[ind1,ind2] / 10
	Tmax2=readGDAL(archivos2[2])
	z <- c(Tmax2@data[,1]); zTmax2=matrix(z,n1,n2); rm(Tmax2)
	valueTm2=zTmax2[ind1,ind2] / 10
	Tmax3=readGDAL(archivos2[3])
	z <- c(Tmax3@data[,1]); zTmax3=matrix(z,n1,n2); rm(Tmax3)
	valueTm3=zTmax3[ind1,ind2] / 10
	Tmax4=readGDAL(archivos2[4])
	z <- c(Tmax4@data[,1]); zTmax4=matrix(z,n1,n2); rm(Tmax4)
	valueTm4=zTmax4[ind1,ind2] / 10
	Tmax5=readGDAL(archivos2[5])
	z <- c(Tmax5@data[,1]); zTmax5=matrix(z,n1,n2); rm(Tmax5)
	valueTm5=zTmax5[ind1,ind2] / 10
	Tmax6=readGDAL(archivos2[6])
	z <- c(Tmax6@data[,1]); zTmax6=matrix(z,n1,n2); rm(Tmax6)
	valueTm6=zTmax6[ind1,ind2] / 10
	Tmax7=readGDAL(archivos2[7])
	z <- c(Tmax7@data[,1]); zTmax7=matrix(z,n1,n2); rm(Tmax7)
	valueTm7=zTmax7[ind1,ind2] / 10
	Tmax8=readGDAL(archivos2[8])
	z <- c(Tmax8@data[,1]); zTmax8=matrix(z,n1,n2); rm(Tmax8)
	valueTm8=zTmax8[ind1,ind2] / 10
	Tmax9=readGDAL(archivos2[9])
	z <- c(Tmax9@data[,1]); zTmax9=matrix(z,n1,n2); rm(Tmax9)
	valueTm9=zTmax9[ind1,ind2] / 10
	Tmax10=readGDAL(archivos2[10])
	z <- c(Tmax10@data[,1]); zTmax10=matrix(z,n1,n2); rm(Tmax10)
	valueTm10=zTmax10[ind1,ind2] / 10
	Tmax11=readGDAL(archivos2[11])
	z <- c(Tmax11@data[,1]); zTmax11=matrix(z,n1,n2); rm(Tmax11)
	valueTm11=zTmax11[ind1,ind2] / 10
	Tmax12=readGDAL(archivos2[12])
	z <- c(Tmax12@data[,1]); zTmax12=matrix(z,n1,n2); rm(Tmax12)
	valueTm12=zTmax12[ind1,ind2] / 10
	
	TMinVal <- c(valueT1,valueT2,valueT3,valueT4,valueT5,valueT6,valueT7,valueT8,valueT9,valueT10,valueT11,valueT12)
	TMaxVal <- c(valueTm1,valueTm2,valueTm3,valueTm4,valueTm5,valueTm6,valueTm7,valueTm8,valueTm9,valueTm10,valueTm11,valueTm12)
	
	return(list(Tmin=TMinVal, Tmax=TMaxVal))
}
############################################
overlay.valid<-function(coords,ndias,dir1,dir2)
{
 archivos1=list.files(dir1,pattern="flt");archivos1=paste(dir1,"/",archivos1,sep="")
 archivos2=list.files(dir2,pattern="flt");archivos2=paste(dir2,"/",archivos2,sep="")
 ptm2=data.frame(lon=NA,lat=NA,loc=NA,tmin=NA,tmax=NA,dia=NA)

 for(i in ndias)
 {
	Tmin1=readGDAL(archivos1[i])
	Tmax1=readGDAL(archivos2[i])

	Temp0=data.frame(coords)

	Temp1=Temp0
	coordinates(Temp1) <- colnames(coordinates(Tmin1))
	Tmin1.interp <- overlay(Tmin1, Temp1) ## es un "SpatialPointsDataFrame" en ambos imputs
	Tmax1.interp <- overlay(Tmax1, Temp1) ## es un "SpatialPointsDataFrame" en ambos imputs

	yl1=Tmin1.interp$band1
	yl2=Tmax1.interp$band1
	Temp0$tmin=yl1
	Temp0$tmax=yl2
	Temp0$dia=rep(i,length(yl1))

	colnames(Temp0)=colnames(ptm2)
	ptm2=rbind(ptm2,Temp0)
 }
 return(ptm2=ptm2[-1,])
}
##################################################
##################################################
##################################################
newOutputMap <- function(dir1, nombre){
#library(knitr)
library(maptools)
library(raster)
library(colorspace)

dir2= dir1
nombre.ERI=paste(nombre,"-ERI.asc",sep="")
nombre.GI=paste(nombre,"-GI.asc",sep="")
nombre.AI=paste(nombre,"-AI.asc",sep="")

colores=c("transparent","aquamarine4","chartreuse2","gold2","darkorange2","firebrick2")

#Establecimiento
ERI = readAsciiGrid(paste(dir2,"/",nombre.ERI,sep=""))  ### extraemos la variable de altura 
cap=raster(ERI)

landClass <- cut(cap, c(0, 0.3, 0.6, 0.8, 1.5))
classes <- c('0-0.3', '0.3-0.6', '0.6-0.8', '0.8-1')
nClasses <- length(classes)


rng <- c(minValue(landClass), maxValue(landClass))
my.at <- seq(rng[1]-1, rng[2])
my.labs.at <- seq(rng[1], rng[2])-0.5

p<-levelplot(landClass, at=my.at, margin=FALSE, col.regions=colores,colorkey=list(labels=list(labels=classes,at=my.labs.at)))
data(wrld_simpl)
p2 <-p + layer(sp.polygons(wrld_simpl, lwd=0.8, col='darkgray'))
png(file=paste(dir2,"/",nombre,"-ERI.png",sep=""))
print(p2)
dev.off()

rm(ERI);rm(cap);rm(p2)

#Numero de generaciones
GI = readAsciiGrid(paste(dir2,"/",nombre.GI,sep=""))  ### extraemos la variable de altura 
cap=raster(GI)

landClass <- cut(cap, c(0,1, 3, 6, 9, 12, 40))
classes <- c('0', '1-3', '3-6', '6-9', '9-12', '12-mas')
nClasses <- length(classes)

rng <- c(minValue(landClass), maxValue(landClass))
## define the breaks of the color key
my.at <- seq(rng[1]-1, rng[2])
## the labels will be placed vertically centered
my.labs.at <- seq(rng[1], rng[2])-0.5

p<-levelplot(landClass, at=my.at, margin=FALSE, col.regions=colores,
          colorkey=list(labels=list(
            labels=classes, ## classes names as labels
            at=my.labs.at)))
data(wrld_simpl)
p2 <-p + layer(sp.polygons(wrld_simpl, lwd=0.8, col='darkgray'))

png(file=paste(dir2,"/",nombre,"-GI.png",sep=""))
print(p2)
dev.off()

rm(GI);rm(cap);rm(p2)

#Actividad
AI = readAsciiGrid(paste(dir2,"/",nombre.AI,sep=""))  ### extraemos la variable de altura 
cap=raster(AI)
#plot(cap,col=terrain.colors(100))

landClass <- cut(cap, c(0, 2, 4, 6, 8, 10, 27))
classes <- c('0-2', '2-4', '4-6', '6-8', '8-10', '10-mas')
nClasses <- length(classes)


rng <- c(minValue(landClass), maxValue(landClass))
## define the breaks of the color key
my.at <- seq(rng[1]-1, rng[2])
## the labels will be placed vertically centered
my.labs.at <- seq(rng[1], rng[2])-0.5

p<-levelplot(landClass, at=my.at, margin=FALSE, col.regions=colores,
          colorkey=list(labels=list(
            labels=classes, ## classes names as labels
            at=my.labs.at)))
data(wrld_simpl)
p2 <-p + layer(sp.polygons(wrld_simpl, lwd=0.8, col='darkgray'))

png(file=paste(dir2,"/",nombre,"-AI.png",sep=""))
print(p2)
dev.off()

rm(AI);rm(cap);rm(p2)


}


RateOR<-function(vec,Table2,model,parametrosc,parametrosm=NULL,funciont,funcionm=NULL,nmax,steps,J=NA,modelim)
{
  for (i in names(parametrosc)){temp <- parametrosc[i];storage.mode(temp) <- "double";assign(i, temp)}
  i=0:(steps-1)
  T1=((vec[3]-vec[2])/2)*cos(pi*(i+0.5)/steps) + (vec[3]+vec[2])/2
  if(vec[1]!=nmax){T2<-((vec[3]-Table2[vec[1]+1,2])/2)*cos(pi*(i+0.5)/steps) + (vec[3]+Table2[vec[1]+1,2])/2}else{
    T2<-((vec[3]-Table2[1,2])/2)*cos(pi*(i+0.5)/steps) + (vec[3]+Table2[1,2])/2}
  
  if((model==1 || model==2 || model==3 ||
      model==4 || model==5 || model==6 ||
      model==7 || model==8 || model==9 ||
      model==10 || model==11 || model==12 || model==13 ||
      model==14) & is.na(J)){x = T1 + 273.15;x2 = T2 + 273.15}else{x = T1;x2 = T2}
  
  if(!is.na(J)){Rat=eval(funciont);Rat[Rat<=0]=0}else{Rat=1/exp(eval(funciont))}
  #Rat=eval(funciont);if(!is.na(J)){Rat[Rat<=0]=0}
  Ratetot1<-(sum(Rat))/steps    ### aqui evalua la tasa de desarrollo de todas las divisiones
  #x=x2;Rat=eval(funciont);if(!is.na(J)){Rat[Rat<=0]=0}
  x=x2;if(!is.na(J)){Rat=eval(funciont);Rat[Rat<=0]=0}else{Rat=1/exp(eval(funciont))}
  Ratetot2<-(sum(Rat))/steps    ### aqui evalua la tasa de desarrollo de todas las divisiones del segundo dia
  Rate=(Ratetot1+Ratetot2)/2
  
  if(!is.null(funcionm))
  {
    for (i in names(parametrosm)){temp <- parametrosm[i];storage.mode(temp) <- "double";assign(i, temp)}
    x=T1;M1=eval(funcionm);M1[M1>1]=1; Mortality1 <- (sum(M1))/steps    ### aqui evalua la Mortalidad de todas las divisiones
    x=T2;M2=eval(funcionm);M2[M2>1]=1; Mortality2 <- (sum(M2))/steps    ### aqui evalua la Mortalidad de todas las divisiones del segundo dia
    Mortality=(Mortality1+Mortality2)/2
    return(c(Rate1=Ratetot1,Rate2=Rate,Mortality1=Mortality1,Mortality2=Mortality))
  }else{return(c(Rate1=Ratetot1,Rate2=Rate))}
}

########################################################
# Getting the min and max monthly temperatures by point
TempGet<-function(CoordsI,archivos1,archivos2,modelim=modelim,modelm=modelm,estadios=estadios,steps=steps,hfeno)
{
  #veccord<-c(CoordsI[1,1],CoordsI[1,2])
  tempRmin1<-raster(archivos1[1]);tempRmin2<-raster(archivos1[2]);tempRmin3<-raster(archivos1[3])
  tempRmin4<-raster(archivos1[4]);tempRmin5<-raster(archivos1[5]);tempRmin6<-raster(archivos1[6])
  tempRmin7<-raster(archivos1[7]);tempRmin8<-raster(archivos1[7]);tempRmin9<-raster(archivos1[9])
  tempRmin10<-raster(archivos1[10]);tempRmin11<-raster(archivos1[11]);tempRmin12<-raster(archivos1[12])
  
  tempRmins<-stack(tempRmin1,tempRmin2,tempRmin3,tempRmin4,tempRmin5,tempRmin6,tempRmin7,tempRmin8,tempRmin9,tempRmin10,tempRmin11,tempRmin12)
  rm(tempRmin1,tempRmin2,tempRmin3,tempRmin4,tempRmin5,tempRmin6,tempRmin7,tempRmin8,tempRmin9,tempRmin10,tempRmin11,tempRmin12)
  TminsGot<-raster::extract(tempRmins, CoordsI)/10
  rm(tempRmins)
  
  tempRmax1<-raster(archivos2[1]);tempRmax2<-raster(archivos2[1]);tempRmax3<-raster(archivos2[3])
  tempRmax4<-raster(archivos2[4]);tempRmax5<-raster(archivos2[5]);tempRmax6<-raster(archivos2[6])
  tempRmax7<-raster(archivos2[7]);tempRmax8<-raster(archivos2[7]);tempRmax9<-raster(archivos2[9])
  tempRmax10<-raster(archivos2[10]);tempRmax11<-raster(archivos2[11]);tempRmax12<-raster(archivos2[12])
  
  tempRmaxs<-stack(tempRmax1,tempRmax2,tempRmax3,tempRmax4,tempRmax5,tempRmax6,tempRmax7,tempRmax8,tempRmax9,tempRmax10,tempRmax11,tempRmax12)
  rm(tempRmax1,tempRmax2,tempRmax3,tempRmax4,tempRmax5,tempRmax6,tempRmax7,tempRmax8,tempRmax9,tempRmax10,tempRmax11,tempRmax12)
  TmaxsGot<-raster::extract(tempRmaxs, CoordsI)/10
  
  TempsGot<-cbind(TminsGot,TmaxsGot)
  rm(tempRmaxs)
  
  
  #GenActIndex(TempsVec,modelim=modelim,modelm=modelm,estadios=estadios,xi=xi,steps=steps,filtro=NULL,hfeno)
  Indexes<-apply(TempsGot,1,GenActIndexByPoint,modelim=modelim,modelm=modelm,estadios=estadios,steps=steps,hfeno)
  Indexes<-t(Indexes)
  
  return(Indexes)
}

TempGetPlus<-function(CoordsI,archivos1,archivos2,modelim=modelim,modelm=modelm,estadios=estadios,steps=steps,hfeno,PLUS)
{
  #veccord<-c(CoordsI[1,1],CoordsI[1,2])
  tempRmin1<-raster(archivos1[1]);tempRmin2<-raster(archivos1[2]);tempRmin3<-raster(archivos1[3])
  tempRmin4<-raster(archivos1[4]);tempRmin5<-raster(archivos1[5]);tempRmin6<-raster(archivos1[6])
  tempRmin7<-raster(archivos1[7]);tempRmin8<-raster(archivos1[7]);tempRmin9<-raster(archivos1[9])
  tempRmin10<-raster(archivos1[10]);tempRmin11<-raster(archivos1[11]);tempRmin12<-raster(archivos1[12])
  
  tempRmins<-stack(tempRmin1,tempRmin2,tempRmin3,tempRmin4,tempRmin5,tempRmin6,tempRmin7,tempRmin8,tempRmin9,tempRmin10,tempRmin11,tempRmin12)
  rm(tempRmin1,tempRmin2,tempRmin3,tempRmin4,tempRmin5,tempRmin6,tempRmin7,tempRmin8,tempRmin9,tempRmin10,tempRmin11,tempRmin12)
  TminsGot<-raster::extract(tempRmins, CoordsI)/10
  rm(tempRmins)
  
  tempRmax1<-raster(archivos2[1]);tempRmax2<-raster(archivos2[1]);tempRmax3<-raster(archivos2[3])
  tempRmax4<-raster(archivos2[4]);tempRmax5<-raster(archivos2[5]);tempRmax6<-raster(archivos2[6])
  tempRmax7<-raster(archivos2[7]);tempRmax8<-raster(archivos2[7]);tempRmax9<-raster(archivos2[9])
  tempRmax10<-raster(archivos2[10]);tempRmax11<-raster(archivos2[11]);tempRmax12<-raster(archivos2[12])
  
  tempRmaxs<-stack(tempRmax1,tempRmax2,tempRmax3,tempRmax4,tempRmax5,tempRmax6,tempRmax7,tempRmax8,tempRmax9,tempRmax10,tempRmax11,tempRmax12)
  rm(tempRmax1,tempRmax2,tempRmax3,tempRmax4,tempRmax5,tempRmax6,tempRmax7,tempRmax8,tempRmax9,tempRmax10,tempRmax11,tempRmax12)
  TmaxsGot<-raster::extract(tempRmaxs, CoordsI)/10
  
  TempsGot<-cbind(TminsGot,TmaxsGot)+PLUS
  rm(tempRmaxs)
  
  
  #GenActIndex(TempsVec,modelim=modelim,modelm=modelm,estadios=estadios,xi=xi,steps=steps,filtro=NULL,hfeno)
  Indexes<-apply(TempsGot,1,GenActIndexByPoint,modelim=modelim,modelm=modelm,estadios=estadios,steps=steps,hfeno)
  Indexes<-t(Indexes)
  
  return(Indexes)
}

GetIndexesAlts<-function(pathclim,path,filedem,PLUS=0)
{
  load(paste(path,"/PhenologySims.RData",sep=""))
  modelim<-params$modelim
  modelm<-params$modelm
  estadios<-params$estadios
  hfeno<-params$hfeno
  xi<-params$xi
  steps<-4
  
  dir1 = paste(pathclim,"/Tmin/",sep="")
  dir2 = paste(pathclim,"/Tmax/",sep="")
  temp<-raster(list.files(dir1,pattern=".flt",full.names = TRUE)[1])
  
  captemp<-raster(filedem)
  bg <- randomPoints(captemp, 100)
  CoordsI<-data.frame(Long=bg[,1],Lat=bg[,2],Presence="Si")
  n0<-nrow(CoordsI)
  
  ilon<-c(bbox(temp)[1,1],bbox(temp)[1,2])
  ilat<-c(bbox(temp)[2,1],bbox(temp)[2,2])
  rm(temp)
  modelim=c(modelim,modelm)
  
  archivos1=list.files(dir1,pattern="flt");archivos1=paste(dir1,"/",archivos1,sep="")
  archivos2=list.files(dir2,pattern="flt");archivos2=paste(dir2,"/",archivos2,sep="")
  IndxByPoint<-TempGetPlus(CoordsI[,-3],archivos1,archivos2,modelim=modelim,modelm=modelm,estadios=estadios,steps=steps,hfeno,PLUS)
  AltByPoint<-raster::extract(captemp, CoordsI[,-3])
  dataF<-data.frame(CoordsI[,-3],alt=AltByPoint,loc=CoordsI[,3],IndxByPoint)
  colnames(dataF)[1:2]<-c("x","y")
  
  return(dataF)
}

########################################################
# Getting the min and max monthly temperatures by point
TempGetParms<-function(CoordsI,archivos1,archivos2,modelim=modelim,modelm=modelm,estadios=estadios,steps=steps,params,ID)
{
  #veccord<-c(CoordsI[1,1],CoordsI[1,2])
  #CoordsI<-CoordsI[,-3]
  tempRmin1<-raster(archivos1[1]);tempRmin2<-raster(archivos1[2]);tempRmin3<-raster(archivos1[3])
  tempRmin4<-raster(archivos1[4]);tempRmin5<-raster(archivos1[5]);tempRmin6<-raster(archivos1[6])
  tempRmin7<-raster(archivos1[7]);tempRmin8<-raster(archivos1[8]);tempRmin9<-raster(archivos1[9])
  tempRmin10<-raster(archivos1[10]);tempRmin11<-raster(archivos1[11]);tempRmin12<-raster(archivos1[12])
  
  tempRmins<-stack(tempRmin1,tempRmin2,tempRmin3,tempRmin4,tempRmin5,tempRmin6,tempRmin7,tempRmin8,tempRmin9,tempRmin10,tempRmin11,tempRmin12)
  rm(tempRmin1,tempRmin2,tempRmin3,tempRmin4,tempRmin5,tempRmin6,tempRmin7,tempRmin8,tempRmin9,tempRmin10,tempRmin11,tempRmin12)
  TminsGot<-raster::extract(tempRmins, CoordsI)/10
  rm(tempRmins)
  
  tempRmax1<-raster(archivos2[1]);tempRmax2<-raster(archivos2[1]);tempRmax3<-raster(archivos2[3])
  tempRmax4<-raster(archivos2[4]);tempRmax5<-raster(archivos2[5]);tempRmax6<-raster(archivos2[6])
  tempRmax7<-raster(archivos2[7]);tempRmax8<-raster(archivos2[8]);tempRmax9<-raster(archivos2[9])
  tempRmax10<-raster(archivos2[10]);tempRmax11<-raster(archivos2[11]);tempRmax12<-raster(archivos2[12])
  
  tempRmaxs<-stack(tempRmax1,tempRmax2,tempRmax3,tempRmax4,tempRmax5,tempRmax6,tempRmax7,tempRmax8,tempRmax9,tempRmax10,tempRmax11,tempRmax12)
  rm(tempRmax1,tempRmax2,tempRmax3,tempRmax4,tempRmax5,tempRmax6,tempRmax7,tempRmax8,tempRmax9,tempRmax10,tempRmax11,tempRmax12)
  TmaxsGot<-raster::extract(tempRmaxs, CoordsI)/10
  
  TempsGot<-cbind(TminsGot,TmaxsGot)
  # write.csv(TempsGot,"C:/Users/Entrevistas/Documents/CIP/Jurgen Kroschel/PhthorimaeaOperculella-AFT-2018/GeographicalSimulation/Temperatures.CSV",row.names=FALSE)
  
  rm(tempRmaxs)
  
  TempsVec<-c(TempsGot[ID,])
  Table<-data.frame(tmin=TempsVec[1:12],tmax=TempsVec[13:24])
  
  # ParamsTime<-GenActIndexByPointParms(TempsVec,modelim=modelim,modelm=modelm,estadios=estadios,steps=steps,hfeno)
  ParamsTime<-simultemp.dete.fluc(N=100,sexratio=0.5, isFixed=TRUE,Table=Table, xi=params$xi, steps=48,poli=1, params)
  # Indexes<-apply(TempsGot,1,GenActIndexByPoint,modelim=modelim,modelm=modelm,estadios=estadios,steps=steps,hfeno)
  # Indexes<-t(Indexes)
  
  return(ParamsTime)
}

########################################################
# Getting the indexes by point

GenActIndexByPoint<-function(TempsVec,modelim=modelim,modelm=modelm,estadios=estadios,steps=steps,hfeno)
{
  Table=cbind(TempsVec[1:12],TempsVec[13:24]);Table2=cbind(id=1:nrow(Table),Table)
  
  if(length(Table[is.na(Table)])==0)
  {
    filtroin<-TRUE
    if(filtroin)
    {
      inmaduros <-  estadios[-(length(estadios)-1):-(length(estadios))]
      maduros   <-  estadios[(length(estadios)-1):(length(estadios))]
      nmax=nrow(Table)
      matriz<-matrix(0,ncol=2*(length(inmaduros)*2+length(maduros)+1),nrow=nrow(Table))
      
      for(K in 1:length(inmaduros))
      {
        #  Desarrollo  # extraccion de funciones y parametros
        
        parametrosc <- hfeno$pdv_dv[[K]]
        funciont <- as.expression(as.formula(hfeno$fdv_dv[[K]])[[3]])
        
        #   Mortalidad # extraccion de funciones y parametros
        
        parametrosm <- hfeno$pmortal[[K]]
        funcionm <- as.expression(as.formula(hfeno$mortal[[K]])[[3]])
        
        RM=apply(Table2,1,RateI,Table2,K,parametrosc,parametrosm,funciont,funcionm,nmax,steps,modelim=modelim) ## procesamiento de tasa de desarrollo y mortalidad por cada temperatura
        matriz[,4*K-3]=RM[1,];matriz[,4*K-2]=RM[2,]
        matriz[,4*K-1]=RM[3,];matriz[,4*K]=RM[4,]
      }
      #  Hembras
      
      parametrosc <- hfeno$pfh_h
      for (i in names(parametrosc)){temp <- parametrosc[i];storage.mode(temp) <- "double";assign(i, temp)}
      formulac <- as.formula(hfeno$fh_h)
      funciont <- as.expression(formulac[[3]])
      RM=apply(Table2,1,RateI,Table2,(K+1),parametrosc,funciont=funciont,nmax=nmax,steps=steps,J=NA,modelim=modelim)
      matriz[,4*(K+1)-3]=RM[1,];matriz[,4*(K+1)-2]=RM[2,]
      
      #  Machos
      
      parametrosc <- hfeno$pfm_m;
      for (i in names(parametrosc)){temp <- parametrosc[i];storage.mode(temp) <- "double";assign(i, temp)}
      formulac <- as.formula(hfeno$fm_m)
      funciont <- as.expression(formulac[[3]])
      RM=apply(Table2,1,RateI,Table2,(K+2),parametrosc,funciont=funciont,nmax=nmax,steps=steps,J=NA,modelim=modelim)
      matriz[,4*(K+1)-1]=RM[1,];matriz[,4*(K+1)]=RM[2,]
      matriz[matriz>1]<-1;matriz[matriz<0]<-0
      
      #  Fecundidad
      
      parametrosc <- hfeno$ptazaeh_h
      for (i in names(parametrosc)){temp <- parametrosc[i];storage.mode(temp) <- "double";assign(i, temp)}
      formulac <- as.formula(hfeno$ftazaeh_h)
      funciont <- as.expression(formulac[[3]])
      RM=apply(Table2,1,RateI,Table2,(K+3),parametrosc,funciont=funciont,nmax=nmax,steps=steps,J=3,modelim=modelim)
      matriz[,4*(K+1)+1]=RM[1,];matriz[,4*(K+1)+2]=RM[2,]
      
      # Oviposition Rate
      parametrosc <- hfeno$povih_h
      for (i in names(parametrosc)){temp <- parametrosc[i];storage.mode(temp) <- "double";assign(i, temp)}
      formulac <- as.formula(hfeno$fovih_h)
      funciont <- as.expression(formulac[[3]])
      model<-hfeno$idmodelOR 
      RMovi=t(apply(Table2,1,RateOR,Table2,model,parametrosc,funciont=funciont,nmax=nmax,steps=steps,J=NA,modelim=modelim))
      #matriz[,4*(K+1)-3]=RM[1,];matriz[,4*(K+1)-2]=RM[2,] # Wait
      
      
      TF=data.frame(Ind=1:nmax)
      for(i in 1:K){TF1=data.frame(DaiSurv1=(1-matriz[,4*i-1])^matriz[,4*i-3],DaiSurv2=(1-matriz[,4*i])^matriz[,4*i-2]);TF=cbind(TF,TF1)}
      for(i in 1:K){Gl0=data.frame(Gl01=1/matriz[,4*i-3],Gl02=1/matriz[,4*i-2]);TF=cbind(TF,Gl0)}
      #TF$Gl11=(1/matriz[,4*(K)+1])/(1/xi);TF$Gl22=(1/matriz[,4*(K)+2])/(1/xi);TF=TF[,-1]  ## aqui faltaba esta ultima linea pues no consideraba la tasa de desarrollo de la hembra en el ultimo mes
      TF$Gl11=1/RMovi[,1];TF$Gl22=1/RMovi[,2];TF=TF[,-1]
      temp1=TF[,c(2*K+seq(1,2*K,2),ncol(TF)-1)];temp2=TF[,c(2*K+seq(2,2*K,2),ncol(TF))]
      TF$Gl1=apply(temp1,1,sum);TF$Gl2=apply(temp2,1,sum)
      temp1<-temp2<-1;for(j in 1:K){temp1=temp1*(1-matriz[,4*j-1]);temp2=temp2*(1-matriz[,4*j])} ## aqui usaba como contador ala "i" en ves de la "j"
      TF$Is1=temp1;TF$Is2=temp2
      TF$Ro1=(TF$Is1*matriz[,4*K+5])/2;TF$Ro1[TF$Ro1<0]=0; TF$Ro2=(TF$Is2*matriz[,4*K+6])/2;TF$Ro2[TF$Ro2<0]=0
      TF$rm1=log(TF$Ro1)/TF$Gl1; TF$rm2=log(TF$Ro2)/TF$Gl2
      TF$Fr1=exp(TF$rm1); TF$Fr2=exp(TF$rm2)
      TF$Dt1=log(2)/TF$rm1; TF$Dt2=log(2)/TF$rm2
      #Ro=mean(c(TF[,"Ro1"],TF[,"Ro2"]))
      
      meses=c(31,28,31,30,31,30,31,31,30,31,30,31);r1=rep(1,nrow(TF))
      GI=sum((meses-1)/TF[,"Gl1"] + 1/TF[,"Gl2"])
      temp.fr1=TF[,"Fr1"];ind1=temp.fr1<1;temp.fr1[ind1]=1
      temp.fr2=TF[,"Fr2"];ind2=temp.fr2<1;temp.fr2[ind2]=1
      AI=log(prod(c(temp.fr1^(meses-1),temp.fr2)),10) ## en este caso hubo el problema que no consideraba a los valores del ultimo mes si el resto valores eran menores a 1

      ERI=1;n0=(sum(meses[TF[,"Ro1"] <= 1]-1) + sum(r1[TF[,"Ro1"] <= 1]))/365;ERI=ERI*(1-n0)
      
      
      #indices=c(AI=AI,GI=GI,ERI=ERI,Ro=Ro)
      indices=c(AI=AI,GI=GI,ERI=ERI)
      return(indices)
    }else{indices=c(AI=0,GI=0,ERI=0);return(indices)}
  }else{
    
    #indices=c(AI=NA,GI=NA,ERI=NA,Ro=NA)
    indices=c(AI=NA,GI=NA,ERI=NA)
    return(indices)
  }
  
}

########################################################
# Getting the indexes by point

GenActIndexByPointParms<-function(TempsVec,modelim=modelim,modelm=modelm,estadios=estadios,steps=steps,hfeno,Daily=FALSE)
{
  if(Daily){
    Table=cbind(TempsVec$T2M_MIN,TempsVec$T2M_MAX);Table2=cbind(id=1:nrow(Table),Table)
  }else{  Table=cbind(TempsVec[1:12],TempsVec[13:24]);Table2=cbind(id=1:nrow(Table),Table)}
  
  if(length(Table[is.na(Table)])==0)
  {
    filtroin<-TRUE
    if(filtroin)
    {
      inmaduros <-  estadios[-(length(estadios)-1):-(length(estadios))]
      maduros   <-  estadios[(length(estadios)-1):(length(estadios))]
      nmax=nrow(Table)
      matriz<-matrix(0,ncol=2*(length(inmaduros)*2+length(maduros)+1),nrow=nrow(Table))
      
      for(K in 1:length(inmaduros))
      {
        #  Desarrollo  # extraccion de funciones y parametros
        
        parametrosc <- hfeno$pdv_dv[[K]]
        funciont <- as.expression(as.formula(hfeno$fdv_dv[[K]])[[3]])
        
        #   Mortalidad # extraccion de funciones y parametros
        
        parametrosm <- hfeno$pmortal[[K]]
        funcionm <- as.expression(as.formula(hfeno$mortal[[K]])[[3]])
        
        RM=apply(Table2,1,RateI,Table2,K,parametrosc,parametrosm,funciont,funcionm,nmax,steps,modelim=modelim) ## procesamiento de tasa de desarrollo y mortalidad por cada temperatura
        matriz[,4*K-3]=RM[1,];matriz[,4*K-2]=RM[2,]
        matriz[,4*K-1]=RM[3,];matriz[,4*K]=RM[4,]
      }
      #  Hembras
      
      parametrosc <- hfeno$pfh_h
      for (i in names(parametrosc)){temp <- parametrosc[i];storage.mode(temp) <- "double";assign(i, temp)}
      formulac <- as.formula(hfeno$fh_h)
      funciont <- as.expression(formulac[[3]])
      RM=apply(Table2,1,RateI,Table2,(K+1),parametrosc,funciont=funciont,nmax=nmax,steps=steps,J=NA,modelim=modelim)
      matriz[,4*(K+1)-3]=RM[1,];matriz[,4*(K+1)-2]=RM[2,]
      
      #  Machos
      
      parametrosc <- hfeno$pfm_m;
      for (i in names(parametrosc)){temp <- parametrosc[i];storage.mode(temp) <- "double";assign(i, temp)}
      formulac <- as.formula(hfeno$fm_m)
      funciont <- as.expression(formulac[[3]])
      RM=apply(Table2,1,RateI,Table2,(K+2),parametrosc,funciont=funciont,nmax=nmax,steps=steps,J=NA,modelim=modelim)
      matriz[,4*(K+1)-1]=RM[1,];matriz[,4*(K+1)]=RM[2,]
      matriz[matriz>1]<-1;matriz[matriz<0]<-0
      
      #  Fecundidad
      
      parametrosc <- hfeno$ptazaeh_h
      for (i in names(parametrosc)){temp <- parametrosc[i];storage.mode(temp) <- "double";assign(i, temp)}
      formulac <- as.formula(hfeno$ftazaeh_h)
      funciont <- as.expression(formulac[[3]])
      RM=apply(Table2,1,RateI,Table2,(K+3),parametrosc,funciont=funciont,nmax=nmax,steps=steps,J=3,modelim=modelim)
      matriz[,4*(K+1)+1]=RM[1,];matriz[,4*(K+1)+2]=RM[2,]
      
      # Oviposition Rate
      parametrosc <- hfeno$povih_h
      for (i in names(parametrosc)){temp <- parametrosc[i];storage.mode(temp) <- "double";assign(i, temp)}
      formulac <- as.formula(hfeno$fovih_h)
      funciont <- as.expression(formulac[[3]])
      model<-hfeno$idmodelOR 
      RMovi=t(apply(Table2,1,RateOR,Table2,model,parametrosc,funciont=funciont,nmax=nmax,steps=steps,J=NA,modelim=modelim))
      #matriz[,4*(K+1)-3]=RM[1,];matriz[,4*(K+1)-2]=RM[2,] # Wait
      
      
      TF=data.frame(Ind=1:nmax)
      for(i in 1:K){TF1=data.frame(DaiSurv1=(1-matriz[,4*i-1])^matriz[,4*i-3],DaiSurv2=(1-matriz[,4*i])^matriz[,4*i-2]);TF=cbind(TF,TF1)}
      for(i in 1:K){Gl0=data.frame(Gl01=1/matriz[,4*i-3],Gl02=1/matriz[,4*i-2]);TF=cbind(TF,Gl0)}
      #TF$Gl11=(1/matriz[,4*(K)+1])/(1/xi);TF$Gl22=(1/matriz[,4*(K)+2])/(1/xi);TF=TF[,-1]  ## aqui faltaba esta ultima linea pues no consideraba la tasa de desarrollo de la hembra en el ultimo mes
      TF$Gl11=1/RMovi[,1];TF$Gl22=1/RMovi[,2];TF=TF[,-1]
      temp1=TF[,c(2*K+seq(1,2*K,2),ncol(TF)-1)];temp2=TF[,c(2*K+seq(2,2*K,2),ncol(TF))]
      TF$Gl1=apply(temp1,1,sum);TF$Gl2=apply(temp2,1,sum)
      temp1<-temp2<-1;for(j in 1:K){temp1=temp1*(1-matriz[,4*j-1]);temp2=temp2*(1-matriz[,4*j])} ## aqui usaba como contador ala "i" en ves de la "j"
      TF$Is1=temp1;TF$Is2=temp2
      TF$Ro1=(TF$Is1*matriz[,4*K+5])/2;TF$Ro1[TF$Ro1<0]=0; TF$Ro2=(TF$Is2*matriz[,4*K+6])/2;TF$Ro2[TF$Ro2<0]=0
      TF$rm1=log(TF$Ro1)/TF$Gl1; TF$rm2=log(TF$Ro2)/TF$Gl2
      TF$Fr1=exp(TF$rm1); TF$Fr2=exp(TF$rm2)
      TF$Dt1=log(2)/TF$rm1; TF$Dt2=log(2)/TF$rm2
      #Ro=mean(c(TF[,"Ro1"],TF[,"Ro2"]))
      
      # meses=c(31,28,31,30,31,30,31,31,30,31,30,31);r1=rep(1,nrow(TF))
      # GI=sum((meses-1)/TF[,"Gl1"] + 1/TF[,"Gl2"])
      # temp.fr1=TF[,"Fr1"];ind1=temp.fr1<1;temp.fr1[ind1]=1
      # temp.fr2=TF[,"Fr2"];ind2=temp.fr2<1;temp.fr2[ind2]=1
      # AI=log(prod(c(temp.fr1^(meses-1),temp.fr2)),10) ## en este caso hubo el problema que no consideraba a los valores del ultimo mes si el resto valores eran menores a 1
      # 
      # ERI=1;n0=(sum(meses[TF[,"Ro1"] <= 1]-1) + sum(r1[TF[,"Ro1"] <= 1]))/365;ERI=ERI*(1-n0)
      
      
      #indices=c(AI=AI,GI=GI,ERI=ERI,Ro=Ro)
      params=TF
      return(params)
    }else{params=data.frame(AI=NA,GI=NA,ERI=NA);return(params)}
  }else{
    
    #indices=c(AI=NA,GI=NA,ERI=NA,Ro=NA)
    params=data.frame(AI=NA,GI=NA,ERI=NA)
    return(params)
  }
  
}

