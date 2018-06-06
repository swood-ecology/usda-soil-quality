##SMAF Analysis Within R
##The equations are taken from an excel file called 'SMAF-0-10cm(ver.2014-06)'
##The goal of this code was to match the columns and calculations used in the excel file.
##The following columns names are needed for this analysis (values in parentheses note the units): Sample,SOC (percent),AGG (percent),MBC (mg per kg),PMN (mg per kg),pH (standard units),SoilTestP (mg per kg),BD (g per cubic centimeter),EC (deciSiemens per meter),SoilTestK (mg per kg),SAR,BG (mg PNP per kg per hour),Slope(percent),Suborder (based on USA Soil Taxonomy),TextureClass,ClimateClass,SeasonCode,MineralClass,RegionCode,CropCode,CropLimitingCode,Pmethod,WeatheringClass,ECMethod
##You will need to provide the Soil Suborder name for each sample, Texture Class, Climate Class, Season Code, Mineral Class, Region Code, P Method, Weathering Class, and EC Method.
##The program can calculate Organic Matter class and Fe2O3 class by suborder.
##You also need the Crop Factors and the Suborder Classes csv files.

##Soil Surborder are assigned following the information in the Suborder Classes csv files. Taxonomic classes can be found using the Web Soil Survey or the Official Soil Series Descriptions.
##Texture Classes are assigned using US Soil Taxonomy Textural Classes as follows: 1 == SAND OR LOAMY SAND OR SANDY LOAM <8% CLAY; 2 == SANDY LOAM OR SANDY CLAY LOAM OR LOAM; 3 == SILT LOAM OR SILT; 4 == SANDY CLAY, CLAY LOAM, SILTY CLAY LOAM, SILTY CLAY, CLAY, 5 == CLAY >40% CLAY
##Climate Classes are assigned as follows: 1 == >=170 days above 0 degrees C AND average annual precipitation >= 550 mm (warm and wet); 2 == >=170 days above 0 degrees C AND average annual precipitation <550 mm (warm and dry); 3 == <170 days above 0 degrees C AND average annual precipitation >= 550 mm (cold and wet); 4 == <170 days above 0 degrees C AND average annual precipitation <550 mm (cold and dry)
##Season Code for when samples were collected are assigned as follows: 1 == Spring (pre-planting or at planting); 2 == summer (mid-growing season); 3 == fall (at or just after harvest); 4 == winter (fallow, cover or double)
##Mineral Classes are assigned as follows: 1 == Smectitic; 2 == Glassy; 3 == Other
##Region Code for Land Resource Regions are assigned as follows: 1 == Arid Land Resource Region (Regions B,C,D,E,F,G,H,I,J); 2 == Humid Land Resource Region (Regions A,K,L,M,N,O,P,Q,R,S,T,U,V)
##For more information on Land Resource Regions, see Land Resource Regions and Major Land Resource Areas of the United States, the Caribbean, and the Pacific Basin, USDA Handbook 296
##https://www.nrcs.usda.gov/Internet/FSE_DOCUMENTS/nrcs142p2_050898.pdf (access to pdf document of report)
##https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/home/?cid=nrcs142p2_053624 (access to mapping files)
##P Method Classes are assigned as follows: 1 == Mehlich 1; 2 == Mehlich 3; 3 == Bray; 4 == Olsen; 5 == Resin; 6 == Iron Oxide Strip
##Weathering Classes are assigned as follows: 1 == Calcareous (Soils With Free CaCO3) ; 2 == Highly weathered soils (Ultisols, Oxisols, acidic Orchepts, Quartzipsamments, Ultic Alfisols); 3 == Slightly Weathered Soils (All Others)
##Weathering Classes assigned following Sharpley 1991 SSSAJ 55:1038
#EC Method Classes are assigned as follows: 1 == Saturated paste; 2 == 1:1 soil:solution ratio

##SMAF dataset
SMAF<-read.csv("data/SMAFExample.csv",header=T)

##Adjust PMN values that are negative to 0.0001
SMAF$PMN<-ifelse(SMAF$PMN<0,0.0001,SMAF$PMN)

##Crop Factors Information
cropfactors<-read.csv("data/CropFactors.csv",header=T)

##Suborder Factors for OMClass and Fe2O3Class
suborder<-read.csv("data/SuborderClasses.csv",header=T)

##Calculate Slope Classes
SMAF$SlopeClass<-ifelse(SMAF$Slope<=2,1,ifelse(SMAF$Slope>2&SMAF$Slope<=5,2,ifelse(SMAF$Slope>5&SMAF$Slope<=9,3,ifelse(SMAF$Slope>9&SMAF$Slope<=15,4,ifelse(SMAF$Slope>15,5,NA)))))

##Add Organic Matter Class and Fe2O3 Class by Suborder
SMAF<-merge(SMAF,suborder,by.x="Suborder",by.y="SuborderList")
#If you are making other changes and then uploading again to R, make sure to detach the current SMAF file and attach the new one

attach(SMAF)
##Soil Organic Carbon Calculation
SMAF$SOCOMFP<-ifelse(OMClass==1,0.3,ifelse(OMClass==2,1.55,ifelse(OMClass==3,2.17,3.81)))
SMAF$SOCTextureFP<-ifelse(TextureClass==1,1.6,ifelse(TextureClass==2,1.25,ifelse(TextureClass==3,1.1,ifelse(TextureClass==4,1.05,1))))
SMAF$SOCClimateFP<-ifelse(ClimateClass==1,0.15,ifelse(ClimateClass==2,0.05,ifelse(ClimateClass==3,-0.05,-0.1)))
SMAF$SOCScore<-1/(1+50.1*exp(-((SMAF$SOCOMFP*SMAF$SOCTextureFP)+(SMAF$SOCOMFP*SMAF$SOCTextureFP*SMAF$SOCClimateFP))*SMAF$SOC))

##Macroaggregate Calculation
SMAF$AGGOMFP<-ifelse(OMClass==1,1.2211054,ifelse(OMClass==2,1.0715387,ifelse(OMClass==3,1.0199112,0.8999381)))
SMAF$AGGTextureFP<-ifelse(TextureClass==1,0.87,ifelse(TextureClass==2,1.06,ifelse(TextureClass==3,1,ifelse(TextureClass==4,1.16,1.25))))
SMAF$AGGFE2O3FP<-ifelse(Fe2O3Class==1,1.1,1)
SMAF$InitialAGGScore<-(-0.793+(1.7993*cos((0.0196*SMAF$AGG-(SMAF$AGGOMFP*SMAF$AGGTextureFP*SMAF$AGGFE2O3FP)))))
SMAF$AGGScore<-ifelse(SMAF$InitialAGGScore>50,1,ifelse(SMAF$InitialAGGScore>0,SMAF$InitialAGGScore,ifelse(SMAF$InitialAGGScore<0,0)))

##Microbial Biomass Carbon Calculation
SMAF$MBCOMFP<-ifelse(OMClass==1,0.0062097,ifelse(OMClass==2,0.012419,ifelse(OMClass==3,0.02,0.03)))
SMAF$MBCTextureFP<-ifelse(TextureClass==1,1.5,ifelse(TextureClass==2,1.35,ifelse(TextureClass==3,1.2,ifelse(TextureClass==4,1,0.9))))
SMAF$MBCValueSeason1Or3<-ifelse(SeasonCode==1,1,ifelse(SeasonCode==3&ClimateClass==1,1.02,ifelse(SeasonCode==3&ClimateClass==2,1.025,ifelse(SeasonCode==3&ClimateClass==3,1.03,ifelse(SeasonCode==3&ClimateClass==4,1.035,ifelse(SeasonCode==2|SeasonCode==4,0,9000))))))
SMAF$MBCSeasonClimate<-ifelse(SMAF$MBCValueSeason1Or3>0,SMAF$MBCValueSeason1Or3,ifelse(SMAF$MBCValueSeason4>0,SMAF$MBCValueSeason4,SMAF$MBCValueSeason2))
SMAF$MBCValueSeason4<-ifelse(SeasonCode==1,0,ifelse(SeasonCode==3,0,ifelse(SeasonCode==4,0,ifelse(SeasonCode==2&ClimateClass==1,1.08,ifelse(SeasonCode==2&ClimateClass==2,1.07,ifelse(SeasonCode==2&ClimateClass==3,1.06,ifelse(SeasonCode==2&ClimateClass==4,1.05,1000)))))))
SMAF$MBCValueSeason2<-ifelse(SeasonCode==1|SeasonCode==3,0,ifelse(SeasonCode==2,0,ifelse(SeasonCode==4&ClimateClass==1,1.12,ifelse(SeasonCode==4&ClimateClass==2,1.1,ifelse(SeasonCode==4&ClimateClass==3,1.08,ifelse(SeasonCode==4&ClimateClass==4,1.06,700))))))
SMAF$MBCScore<-1/(1+40.748*exp(-(SMAF$MBCOMFP*SMAF$MBCTextureFP*SMAF$MBCSeasonClimate)*SMAF$MBC))

##Potentially Mineralizable Nitrogen
SMAF$PMNOMFP<-ifelse(OMClass==1,0.254995,ifelse(OMClass==2,0.299255,ifelse(OMClass==3,0.35467,0.432275)))
SMAF$PMNTextureFP<-ifelse(TextureClass==1,1.15,ifelse(TextureClass==2|TextureClass==3,1,ifelse(TextureClass==4,0.9,0.85)))
SMAF$PMNClimateFP<-ifelse(ClimateClass==1,0.9,ifelse(ClimateClass==2,0.95,ifelse(CliamteClass==3,1,1.05)))
SMAF$PMNScore<-1/(1+161.32*exp(-((SMAF$PMNOMFP*SMAF$PMNTextureFP)+(SMAF$PMNOMFP*SMAF$PMNTextureFP*SMAF$PMNClimateFP))*SMAF$PMN))

##Bulk Density
SMAF$BDTextureFP1b<-ifelse(TextureClass==1,0.792,ifelse(TextureClass==2,0.794,ifelse(TextureClass==3,0.796,ifelse(TextureClass==4,0.796,0.799))))
SMAF$BDTextureFP2c<-ifelse(TextureClass==1,321.34,ifelse(TextureClass==2,88.025,ifelse(TextureClass==3,32.189,ifelse(TextureClass==4,16.945,7.47))))
SMAF$BDTextureFP3d<-ifelse(TextureClass==1,-12.99,ifelse(TextureClass==2,-12.061,ifelse(TextureClass==3,-11.297,ifelse(TextureClass==4,-10.79,-10.1))))
SMAF$BDMinFP1b<-ifelse(MineralClass==1,0.0005,ifelse(MineralClass==2,0.001,0))
SMAF$BDMinFP2c<-ifelse(MineralClass==1,5.23253292,ifelse(MineralClass==2,2.31849414,9.3587152))
SMAF$BDMinFP3d<-ifelse(MineralClass==1,-9.850864,ifelse(MineralClass==2,-9.25634,-10.275524))
SMAF$BDScore<-ifelse(TextureClass>=4,0.994-(SMAF$BDTextureFP1b+SMAF$BDMinFP1b)*exp(-SMAF$BDTextureFP2c*SMAF$BD^SMAF$BDTextureFP3d),0.994-SMAF$BDTextureFP1b*exp(-SMAF$BDTextureFP2c*SMAF$BD^SMAF$BDMinFP3d))

##Beta-1,4 Glucosidase Activity
SMAF$BGOMFP<-ifelse(OMClass==1,0.9,ifelse(OMClass==2,2.9,ifelse(OMClass==3,3.8,5.8)))
SMAF$BGTextureFP<-ifelse(TextureClass==1,4,ifelse(TextureClass==2,2.9,ifelse(TextureClass==3,2.8,ifelse(TextureClass==4,2.7,1.3))))
SMAF$BGClimateFP<-ifelse(ClimateClass==1,2.1,ifelse(ClimateClass==2,0.85,ifelse(ClimateClass==3,0.7,0.45)))
SMAF$BGCFactor<-(SMAF$BGOMFP*SMAF$BGTextureFP)+(SMAF$BGOMFP*SMAF$BGTextureFP*SMAF$BGClimateFP)
SMAF$InitialBGScore<-1.01/(1+48.4*exp(-SMAF$BGCFactor*(SMAF$BG/1000)))
SMAF$BGScore<-ifelse(SMAF$InitialBGScore>1,1,SMAF$InitialBGScore)

##Soil Test Potassium
SMAF$SoilTestKA<-ifelse(TextureClass==1,1.0541,ifelse(TextureClass==2,1.0541,ifelse(TextureClass==3,1.0745,ifelse(TextureClass==4,1.0745,ifelse(TextureClass==5,1.0745,NA)))))
SMAF$SoilTestKB<-ifelse(TextureClass==1,-0.00981,ifelse(TextureClass==2,-0.00981,ifelse(TextureClass==3,-0.01343,ifelse(TextureClass==4,-0.01343,ifelse(TextureClass==5,-0.01343,NA)))))
SMAF$SoilTestKScore<-ifelse(SMAF$SoilTestKA*(1-exp(SMAF$SoilTestKB*SMAF$SoilTestK))<1,SMAF$SoilTestKA*(1-exp(SMAF$SoilTestKB*SMAF$SoilTestK)),1)

##Soil pH
pHCropCodeb<-cropfactors$pHb[cropfactors$CropCode==SMAF$CropCode]
pHFPcdiff<-cropfactors$pHc[cropfactors$CropCode==SMAF$CropCode]
SMAF$pHScore<-1*exp(-(SMAF$pH-pHCropCodeb)^2/(2*pHFPcdiff^2))

##Electrical Conductivity
SMAF$ECRotationFP1T<-cropfactors$ECTsat[cropfactors$CropCode==SMAF$CropLimitingCode]
SMAF$ECRotationFP2dT<-cropfactors$ECdt[cropfactors$CropCode==SMAF$CropLimitingCode]
SMAF$ECRotationFP3m<-cropfactors$ECm[cropfactors$CropCode==SMAF$CropLimitingCode]
SMAF$ECTextureFP<-ifelse(TextureClass==5,1.25,ifelse(TextureClass==4,1.22,ifelse(TextureClass==3,1.18,ifelse(TextureClass==2,1.07,1))))
SMAF$InitialECScore<-ifelse(SMAF$ECMethod==1&SMAF$EC<0.3,3.33*SMAF$EC,ifelse(SMAF$ECMethod==1&SMAF$EC>SMAF$ECRotationFP1T,SMAF$ECRotationFP3m*SMAF$EC+(1-SMAF$ECRotationFP3m*SMAF$ECRotationFP1T),ifelse(SMAF$ECMethod==2&SMAF$EC<0.0017,5.88*SMAF$EC,ifelse(SMAF$ECMethod==2&SMAF$EC>(SMAF$ECRotationFP1T/1.77)*SMAF$ECTextureFP,SMAF$ECRotationFP3m*SMAF$EC+(1-SMAF$ECRotationFP3m*(SMAF$ECRotationFP1T/1.77)*SMAF$ECTextureFP),1))))
SMAF$ECScore<-ifelse(SMAF$InitialECScore<0,0,ifelse(SMAF$InitialECScore>1,1,SMAF$InitialECScore))

##Soil Test Phosphorus
SMAF$SoilTestPFPb<-cropfactors$SoilPop[cropfactors$CropCode==SMAF$CropCode]
SMAF$SoilTestPFPbinitial<-213.96744-39.579185*SMAF$SoilTestPFPb+2.3020512*SMAF$SoilTestPFPb*SMAF$SoilTestPFPb
SMAF$SoilTestPOMFP<-ifelse(OMClass==1,0.125,ifelse(OMClass==2,0.025,ifelse(OMClass==3,0.0175,0.01)))
SMAF$SoilTestPTextureFP<-ifelse(TextureClass==1,0.98,ifelse(TextureClass==2,0.99,ifelse(TextureClass==3,1,ifelse(TextureClass==4,1.01,1.03))))
SMAF$SoilTestPbSOCPara<-SMAF$SoilTestPFPbinitial+(SMAF$SoilTestPFPbinitial*SMAF$SOC/100)*SMAF$SoilTestPTextureFP
SMAF$SoilTestPbOMClass<-SMAF$SoilTestPFPbinitial+(SMAF$SoilTestPFPbinitial*SMAF$SoilTestPOMFP)*SMAF$SoilTestPTextureFP
SMAF$SoilTestPSlopeFPInflection<-ifelse(SlopeClass==1,160,ifelse(SlopeClass==2,140,ifelse(SlopeClass==3,115,ifelse(SlopeClass==4,85,60))))
SMAF$SoilTestPSlopeFPParameter<-ifelse(SlopeClass==1,110000,ifelse(SlopeClass==2,90000,ifelse(SlopeClass==3,70000,ifelse(SlopeClass==4,35000,20000))))
SMAF$SoilTestPTextureFP2<-ifelse(TextureClass==1,0.9,ifelse(TextureClass==2,1,ifelse(TextureClass==3,1.1,ifelse(TextureClass==4,1.4,1.6))))
SMAF$SoilTestPOMFP2<-ifelse(OMClass==1,0.25,ifelse(OMClass==2,0.05,ifelse(OMClass==3,0.035,0.02)))
SMAF$SoilTestPcSOC<-(SMAF$SoilTestPSlopeFPParameter+SMAF$SoilTestPSlopeFPParameter*SMAF$SOC/100)*SMAF$SoilTestPTextureFP2
SMAF$SoilTestPcOM<-(SMAF$SoilTestPSlopeFPParameter+SMAF$SoilTestPSlopeFPParameter*SMAF$SoilTestPOMFP2)*SMAF$SoilTestPTextureFP2
SMAF$SoilTestPMethodw1<-ifelse(Pmethod==1,2,ifelse(Pmethod==2,1,ifelse(Pmethod==3,1.8,ifelse(Pmethod==4,2.4,ifelse(Pmethod==5,2.1,2.3)))))
SMAF$SoilTestPMethodw2<-ifelse(Pmethod==1,1.4,ifelse(Pmethod==2,1,ifelse(Pmethod==3,0.66,ifelse(Pmethod==4,1.8,ifelse(Pmethod==5,1.25,3.66)))))
SMAF$SoilTestPMethodw3<-ifelse(Pmethod==1,1.35,ifelse(Pmethod==2,1,ifelse(Pmethod==3,1,ifelse(Pmethod==4,1.7,ifelse(Pmethod==5,1.25,1.5)))))
SMAF$SoilTestPMethodFP<-ifelse(WeatheringClass==1,SMAF$SoilTestPMethodw1,ifelse(WeatheringClass==2,SMAF$SoilTestPMethodw2,SMAF$SoilTestPMethodw3))
SMAF$InitialPsocScore<-ifelse(SMAF$SoilTestP*SMAF$SoilTestPMethodFP<=SMAF$SoilTestPFPb+6,0.00000925*SMAF$SoilTestPbSOCPara+(SMAF$SoilTestP*SMAF$SoilTestPMethodFP)^3.06/(SMAF$SoilTestPbSOCPara+(SMAF$SoilTestP*SMAF$SoilTestPMethodFP)^3.06),ifelse(SMAF$SoilTestP*SMAF$SoilTestPMethodFP>SMAF$SoilTestPSlopeFPInflection,(1-4.5*exp(-SMAF$SoilTestPcSOC*(SMAF$SoilTestP*SMAF$SoilTestPMethodFP)^-2)),1))
SMAF$SoilTestPsocScore<-ifelse(SMAF$InitialPsocScore>=0,SMAF$InitialPsocScore,0)
SMAF$InitialPomclassScore<-ifelse(SMAF$SoilTestP*SMAF$SoilTestPMethodFP<=SMAF$SoilTestPFPb+6,0.00000925*SMAF$SoilTestPbOMClass+(SMAF$SoilTestP*SMAF$SoilTestPMethodFP)^3.06/(SMAF$SoilTestPbOMClass+(SMAF$SoilTestP*SMAF$SoilTestPMethodFP)^3.06),ifelse(SMAF$SoilTestP*SMAF$SoilTestPMethodFP>SMAF$SoilTestPSlopeFPInflection,(1-4.5*exp(-SMAF$SoilTestPcOM*(SMAF$SoilTestP*SMAF$SoilTestPMethodFP)^-2)),1))
SMAF$SoilTestPomScore<-ifelse(SMAF$InitialPomclassScore>=0,SMAF$InitialPomclassScore,0)

##Sodium Adsorption Ratio
SMAF$SARScore<-ifelse(SMAF$EC<0.2,1/(4.056+0.793*SMAF$SAR^3.05),ifelse(SMAF$EC>=0.2&SMAF$EC<0.55,0.8+(0.01298857*SMAF$SAR)-0.067*(SMAF$SAR^2)+0.0257*(SMAF$SAR^3)-0.00536*(SMAF$SAR^4)+0.000547*(SMAF$SAR^5)-0.0000211*(SMAF$SAR^6),1-0.0702*SMAF$SAR+0.0105*(SMAF$SAR^2)-0.00068*(SMAF$SAR^3)-0.0000239*(SMAF$SAR^4)))

##Physical - BD and AGG
##Chemical - pH and EC, P, K, SAR
##Biological - SOC, MBC, PMN, BG
##Total - All indicators

SMAF$PhysicalSQI<-rowMeans(SMAF[c("BDScore","AGGScore")],na.rm=TRUE)
SMAF$ChemicalSQI<-rowMeans(SMAF[c("pHScore","ECScore","SoilTestPsocScore","SoilTestKScore","SARScore")],na.rm=TRUE)
SMAF$BiologicalSQI<-rowMeans(SMAF[c("SOCScore","MBCScore","PMNScore","BGScore")],na.rm=TRUE)
SMAF$TotalSQI<-rowMeans(SMAF[c("BDScore","AGGScore","pHScore","ECScore","SoilTestPsocScore","SoilTestKScore","SARScore","SOCScore","MBCScore","PMNScore","BGScore")],na.rm=TRUE)

detach(SMAF)

##Your data should now have all SQIs calculated for the available data and ready for additional analysis, graphing, and so forth.
