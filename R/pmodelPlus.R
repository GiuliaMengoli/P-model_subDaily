#  Function to apply the P model at sub daily timestep
#  param: file_setting name (or full path) of the setting file
#  param: showMsg (if T shows the function messages)
#  return: a data.frame with informations included in the setting file and model outputs
#  rdname: pmodelPlus


pmodelPlus <- function(data = dataIn,iabsMethod= 'fAPAR',fAPARMethod = 'fAPAR',
                               phi0Method = NA ) {
  #
  # INPUT:
  #   data.frame with columns: TIME, PPFD, Ta, CO2, fapar, VPD, LAI, z_v
  #                     where: fapar = fAPAR
  #                     where: z_v = elevation
  #
  # SETTING
  # this function is able to compute some inputs variables (such as LAI) by applying different approaches
  # Iabs computation (iabsMethod)
  # ---
  #   option: 'fAPAR';
  #       description: to compute Iabs from fAPAR and PPFD
  #       formula: Iabs = PPFD * fAPAR * 1               
  #
  #   option: 'LAI';
  #       description: to compute Iabs from PPFD and LAI, Lambert Beer relation
  #       formula: Iabs = ( PPFD * (1-exp(-0.5* LAI))) *0.845     
  #
  #
  # inverse computation of fAPAR (an attempt) (fAPARMethod)
  # ---
  #   option: 'fAPAR'
  #     description: fAPAR not calculated
  #     formula: NA
  #
  #   option: 'fAPAR1'
  #     description: fAPAR calculated with exponential from LAI (Beer-Lambert law)
  #     formula: fAPAR = (1- exp(-0.5* LAI))
  #
  #   option: 'fAPAR2'
  #     description: fAPAR calculated with exponential from LAI and 0.845
  #     formula: fAPAR = (1- exp(-0.5* LAI)) * 0.845
  #
  #   option: 'fAPAR3'
  #     description: fAPAR calculated with Iabs and 0.95 (# ref: Xiao et al 2005)
  #     formula: fAPAR = Iabs / (PPFD * 0.845)
  #
  # phi0 computation (phi0Method)
  # ---
  #   option: NA
  #     description: phi0 is calculated
  #     formula: phi0 = (1/8)*(0.352+0.022*Ta - 0.00034*Ta^(2) )     #  [mol/mol]
  #   option: <numericValue>
  #     description: phi0 is constant
  #     formula: phi0 = <numericValue>
  #
  #
  # OUTPUT:
  #   these columns are added to the input data.frame:
  #   - VPD              (VPDPA)
  #   - Ko               (koPa)
  #   - Kc               (kcPa)
  #   - Km               (kmPa)
  #   - GammaStar       (GammaStarM)
  #   - water viscosity  (viscosityH2oStar)
  #   - xi               (xiPaM)
  #   - ci               (CiM)
  #   - phi0             (phi0)
  #   - vcmax            (vcmaxPmodelM1)
  #   - Iabs
  #   #- Ac              (Ac1)
  #   #- Jmax            (JmaxM1)
  #   #- Jp              (Jp1)
  #   - AJ               (AJp1)
  #   - GPPp             (GPPp)

  # show the options used ----
  cat('\n')
  cat(sprintf('\tpmodelPlus start\n'))
  cat(sprintf('\tIabs method: %s\n',iabsMethod))
  cat(sprintf('\tfAPAR method: %s\n',fAPARMethod))
  if (is.na(phi0Method)) {
    cat(sprintf('\tphi0 method: %s\n','CALCULATED'))
  } else {
    cat(sprintf('\tphi0 method: %s\n','CONSTANT'))
  }
  cat('\n')

  # check the names of the columns ----
  nomiOk = c('YEAR','MONTH','DAY','PPFD', 'Ta', 'CO2', 'fapar', 'VPD','z_v') #, 'GPPDTCUTREF'

  # if necessary: add LAI column and check it
  if ( iabsMethod == 'LAI' ) nomiOk = c(nomiOk,'LAI')
  if ( fAPARMethod == 'fAPAR1' ) nomiOk = c(nomiOk,'LAI')
  if ( fAPARMethod == 'fAPAR2' ) nomiOk = c(nomiOk,'LAI')
  if ( fAPARMethod == 'fAPAR3' ) nomiOk = c(nomiOk,'LAI')

  nomiOk = unique(nomiOk)

  for ( cyNomi in nomiOk ) {
    posNomi = which(colnames(data) == cyNomi)
    if ( length(posNomi) == 0 ) {
      stop( sprintf('column: %s missing\n\n',cyNomi),call. = F)
    }
  }

  # replace the -9999 values with NA ----
  # since in fluxnet the missing value is indicated with -9999
  cat('\n')
  for ( col in colnames(data) ) {
    if ( !is.numeric(data[,col]) ) next
    posNa = which(data[,col] < -9990)
    if ( length(posNa) == 0 ) next
    data[posNa,col] = NA
    cat(sprintf('column: %s;%d values less than -9990 changed in NA\n',
                col,length(posNa)))
  }
  rm(col,posNa)

  # checK LAI column, if not present insert NA values
  if ( length(which(colnames(data) == 'LAI')) == 0) {
    data$LAI = NA
    cat(sprintf('add LAI column (all values are NA)\n'))
  }

  # replace with NA those values that cannot be negative ----
  for (colNoNeg in c('PPFD','VPD','CO2','fapar','LAI')) {
    ckCol = which(colnames(data) ==  colNoNeg)
    if ( length(ckCol) == 0 ) next
    cat('\n')
    posNeok=which(data[,colNoNeg] < 0)
    if(length(posNeok) == 0) next
    data[posNeok,colNoNeg] = NA
    cat(sprintf('column: %s; %d negative values changed in NA\n',
                colNoNeg,length(posNeok)))
  }
  rm(posNeok,colNoNeg,ckCol)

  # check if input variables have data ----
  for ( cyNomi in nomiOk ) {
    nrNa = sum( is.na(data[,cyNomi]) )
    if ( nrNa == length(data[,cyNomi]) ) {
      stop(sprintf('column: %s is empty\n',cyNomi),call. = F)
    }
  }

  # if there fAPAR column is missing then rename it with fAPAR
  if ( length(which(colnames(data) == 'fAPAR')) == 0 ) {
    cat(sprintf('\nfAPAR not found\n'))
    if ( length(which(colnames(data) == 'fapar')) > 0 ) {
      cat(sprintf('rename fapar in fAPAR\n\n'))
      data$fAPAR = data$fapar
    }
  }

  # conversion of VPD, VPDPa ----
  # VPD in Pa
  data$VPDPa = data$VPD * 100
  cat(sprintf('calculate VPDPa\n\n'))

  # Iabs computation ----
  data$Iabs = NA
  if ( iabsMethod == 'fAPAR' ) {
    # compute Iabs from fAPAR and PPFD
    data$Iabs = data$PPFD * data$fAPAR*1                 
    cat(sprintf('calculate Iabs from PPFD and fAPAR\n'))
  }
  if ( iabsMethod == 'LAI' ) {
    # compute Iabs from LAI, Beer-Lambert relation
    # data$Iabs = (data$PPFD*(1-exp(-0.5*data$LAI)))*0.95      # 1 formula: ref: Xiao et al.2005
    data$Iabs = (data$PPFD*(1-exp(-0.5*data$LAI)))             # 2 formula: Beer-Lambert law
    # k is an extinction coefficient equal to 0.5
    # according to the assumption of spherical angle distribution

    # data$Iabs = (data$PPFD*(1-exp(-0.3*data$LAI)))           # 3 formula: Beer-Lambert with the omega value=0.6
    cat(sprintf('calculate Iabs from PPFD and LAI\n'))
  }
  cat('\n')

  # inverse computation of fAPAR (an attempt) ----
  data$fAPAROriginal = data$fAPAR
  if ( fAPARMethod == 'fAPAR' ) {
    cat(sprintf('fAPAR not calculated\n'))
  }
  if ( fAPARMethod == 'fAPAR1' ) {
    # calcolo tre versioni di fAPAR in funzione del LAI
    data$fAPAR = (1- exp(-0.5*data$LAI))                   
    cat(sprintf('fAPAR calculated with exponential\n'))
  }
  if ( fAPARMethod == 'fAPAR2' ) {
    data$fAPAR = (1- exp(-0.5*data$LAI))*0.845             
    cat(sprintf('fAPAR calculated with exponential and 0.845\n'))
  }
  if ( fAPARMethod == 'fAPAR3' ) {
    
    data$fAPAR = data$Iabs / (data$PPFD * 0.845)           
    cat(sprintf('fAPAR calculated with Iabs and 0.845\n'))
  }

  YEAR = data[,'YEAR']
  MONTH = data[,'MONTH']
  DAY = data[,'DAY']
  PPFD = data[,'PPFD']
  Ta = data[,'Ta']
  CO2 = data[,'CO2']
  fapar = data[,'fAPAR']
  VPD = data[,'VPD']
  VPDPa= data[,'VPDPa']
  z_v = data[,'z_v']
  Iabs = data[,'Iabs']

  # Constants of photosynthesis ----
  # (kPa) Atmospheric pressure  (Po=101.325 in kPa instead of its value in Pa)
  # constants
  Rgas=8.314             #constant gas J/mol/K

  #Bernacchi et al. 2001
  Kc25         = 39.97;  # (Pa) Michaelis-Menten const carboxylase, 25 deg C
  EaKc         = 79430;  # (J mol-1) Activation energy for carboxylase
  Ko25         = 27480;  # (Pa) Michaelis-Menten const oxygenase, 25 deg
  EaKo         = 36380;  # (J mol-1) Activation energy for oxygenase

  gamma25      = 4.332;   # (Pa)
  EaGamma      = 37830;  # (J/mol)

  # ref: Wang et al 2017 (New Phytologist)
  kpo = 101.325    # (kPa) po: atmospheric pressure at the sea level
  kL= 0.0065       # mean adiabatic lapse rate (K/m2)
  kG= 9.8066       # gravitational acceleration (m/s2)
  kMa=0.028963     # molecular weight for dry air (kg/mol)
  kR= 8.314        # universal gas constant (J/mol*K)

  kco =2.09476e5  # ppm. US standard pressure. (Ref Bernacchi et al 2001)
  kPo =101325.0   # (Pa) Standard atmopsheric pressure at 0 m a.s.l., ref: Allen 1973

  # Define constants:
  # kPo = 101325    # standard atmosphere, Pa (Allen, 1973)
  kTo = 298.15      # base temperature, K     
  # kL  = 0.0065    # adiabiatic temperature lapse rate, K/m (Allen, 1973)
  # kG  = 9.80665   # gravitational acceleration, m/s^2 (Allen, 1973)
  # kR  = 8.3145    # universal gas constant, J/mol/K (Allen, 1973)
  # kMa = 0.028963  # molecular weight of dry air, kg/mol (Tsilingiris, 2008)


  # calculate atmosferic pressure as a function of eleveation [Pa] (ref: Berberan-Santos et al., 1997)----
  PPa = kPo*(1.0 - kL*z_v/kTo)^(kG*kMa/(kR*kL))     
  cat(sprintf('calculate Atmospheric pressure \n'))

  # compute K (effective Michaelis-Menten coefficient) ----
  # O2 partial pressure [Pa]
  O = kco  * (1e-6) * PPa                      

  # Effective Michaelis-Menten coefficient
  # Kc and Ko in Pascal
  kcPa = Kc25*exp(EaKc*(Ta - 25.0)/(298.15*Rgas*(Ta + 273.15)))      # [Pa]

  koPa = Ko25*exp(EaKo*(Ta-25.0)/(298.15*Rgas*(Ta+273.15)))          # [Pa]

  # compute K
  # K in Pascal is indicated here as kmPa
  kmPa = kcPa*(1.0+O/koPa)          # [Pa]
  cat(sprintf('calculate kmPa\n'))

  # compute GammaStar ----
  # Photorespiratory compensation point, gamma star in Pascal
  # GammaStar = GammaStarM
  
  # Adjust gammaStar25 for the pressure at given elevation
  # Pressure-dependent photorespiratory compensation point           
  z= 0
  PDeno = kPo*(1.0 - kL*z/kTo)^(kG*kMa/(kR*kL))           # standard atmospheric pressure at 0 m (= a kPo=101325 Pa)
  PRatio = PPa / PDeno
  rm(z)

  gamma25      = 4.332;   # [Pa]
  EaGamma      = 37830;  # [J/mol]

  gammastar25 = gamma25 * PRatio
  GammaStarM = gammastar25 * exp( EaGamma *(Ta-25)/(Rgas*298.15*(Ta+273.15)))             #[Pa]
  cat(sprintf('calculate GammaStarM\n'))

  rm(Rgas,Kc25,EaKc,Ko25,EaKo,gamma25,EaGamma,kpo,kL,kTo,kG,kMa,kR,kco)

  # compute water viscosity (star)----
  # which is the viscosity of the water relative to 25 C of temp
  # viscosity of water (star) = viscosityH2oStar
  # ref: Huber et al., 2009
  
  viscosityH2oStar = (exp(-3.719 + 580/((Ta+273) - 138)))/0.911          # unitless
  cat(sprintf('calculate viscosityH2oStar\n'))


  # compute 'xi' (sensitivity of ci/ca ratio to VPD) ----
  # 'xi' = 'xiPaM'
  
  # beta: the ratio of cost factors for carboxylation and transpiration capacities (at 25 C)
  
  betha=146.0                                                            # unitless; Sotcker et al., 2020

  # xi
  xiPaM= sqrt ( (betha * (kmPa+GammaStarM) ) / (1.6*viscosityH2oStar) )   # [Pa^1/2]
  
  rm(betha)
  cat(sprintf('calculate xiPaM\n'))


  # compute the internal CO2 ----
  # leaf-internal CO2 partial pressure (ci, Pa)
  # ci = ciM
  
  # Convert ambient CO2 from ppm (umol/mol) to Pa 
  ca = CO2 * ( 1.0e-6 ) *PPa                                          # [Pa]

  # compute ci 
  ciM = ( xiPaM * ca + GammaStarM*sqrt(VPDPa))/(xiPaM+sqrt(VPDPa))    # [Pa]
  cat(sprintf('calculate ciM\n'))

  # COMPUTATION OF VCMAX ----
  # Vcmax, maximum rate of carboxylation (ref: Wang et al., 2017)
  # Vcmax = vcmaxPmodelM1

  # the cost factor for electron-transport capacity (unitless)
  c=0.41  

  # Intrinsic quantum efficiency of photosynthesis (phi0, mol Co2/mol photons)
  if (is.na(phi0Method) ) {
    cat(sprintf('phi0 is calculated\n'))
    # Temperature dependence function of quantum efficiency (ref: Bernacchi et al., 2003)
    phi0 = (1/8)*(0.352+0.022*Ta - 0.00034*Ta^(2) )               #  [mol/mol]

  } else {
    cat(sprintf('phi0 is constant: %s\n',phi0Method))
    phi0 = phi0Method   # phi= 0.093
   
  }

  # Vcmax
  vcmaxPmodelM1  = phi0 * Iabs *
    ((ciM + kmPa) / (ciM + 2*GammaStarM)) *
    sqrt (1-(c * (ciM+2*GammaStarM)/(ciM-GammaStarM))^(2/3))     # [micromol CO2/m2s]
  
  #rm(c)
  cat(sprintf('calculate vcmaxPmodelM1\n'))


  # COMPUTATION OF THE FIRST LIMITATION OF THE ASSIMILAZION, Ac ----
  #   # Ac, Rubisco-limited assimilation rate       # [micromol CO2/m2s]
  #   # Ac = Ac1           
             
  Ac1 = vcmaxPmodelM1 * (ciM - GammaStarM) / (ciM+kmPa)             # [micromol CO2/m2s]

  cat(sprintf('calculate Ac1\n'))
  

  # COMPUTATION OF Jmax ----
  # Jmax, maximum rate of electron transport (ref: Wang et al., 2017)
  # Jmax = JmaxM1

  JmaxM1  = ( 4 *phi0 * Iabs ) / sqrt ( 1 / ( 1 - ( c * ( ciM + 2*GammaStarM ) /
             ( ciM - GammaStarM ) ) ^ (2.0/3.0) ) - 1 )  #micromol/m^2*s
  cat(sprintf('calculate JmaxM1\n'))


  # compute J ----
  # the electron transport rate (Smith et al, 1937)
  # J = Jp1
  
  Jp1 = (4 * phi0 * Iabs)/sqrt(1+((4*phi0*Iabs/JmaxM1)^(2)))             # [micromol electrons/m2s]
  cat(sprintf('calculate Jp1\n'))

  
  # COMPUTATION OF THE SECOND LIMITATION OF THE ASSIMILAZION, AJ ----
  #   # AJ, electron-transport limited assimilation    # [micromol CO2/m2s]
  #   # AJ = AJp1

  AJp1 = (Jp1/4)*(ciM-GammaStarM)/(ciM+2*GammaStarM)                    # [micromol CO2/m2s]
  cat(sprintf('calculate AJp1\n'))

  
  # COMPUTATION OF GPP
  # calcualte the predicted GPP as the minimum value between Ac and AJ----
  cat(sprintf('Calculate GPPp, please wait ....\n'))
  GPPp = c()
  for ( cy1 in seq(1,nrow(data)) ) {
    tmp = c(Ac1[cy1],AJp1[cy1])
    posNa = which(is.na(tmp) == 1)
    if ( length(posNa) > 0 ) tmp = tmp[-1*posNa]
    rm(posNa)
    if (length(tmp) == 2 ) {
      GPPp = c(GPPp,min(tmp))
    } else {
      GPPp = c(GPPp,NA)
    }
  }
  rm(cy1,tmp)

  cat(sprintf('Calculate GPPp, please wait ....OK\n'))

  for (cyv in c('koPa','kcPa','kmPa','GammaStarM','viscosityH2oStar',
               'xiPaM','ciM','ca','phi0','vcmaxPmodelM1','Ac1',
               'JmaxM1','Jp1','c','AJp1','GPPp')) {
    eval(parse(text = paste0("data[,'",cyv,"'] = ",cyv)))
    cat(sprintf('add column: %s\n',cyv))
  }
  rm(cyv)

  rm(koPa,kcPa,kmPa,GammaStarM,viscosityH2oStar,
      xiPaM,ciM,ca,phi0,vcmaxPmodelM1,Ac1,
      JmaxM1,Jp1,c,AJp1,GPPp)

  # add the columns for the method ----
  data$method = paste(iabsMethod,fAPARMethod,phi0Method)
  cat(sprintf('add column: %s\n','method'))
  data$iabsMethod = iabsMethod
  cat(sprintf('add column: %s; values: %s\n','iabsMethod',iabsMethod))
  data$fAPARMethod = fAPARMethod
  cat(sprintf('add column: %s; values: %s\n','fAPARMethod',fAPARMethod))

  if (is.na(phi0Method)) {
    data$phi0Method = 'phi0Calculated'
    cat(sprintf('add column: %s; values: %s\n','phi0Method','phi0Calculated'))
  } else {
    data$phi0Method = 'phi0Method'
    cat(sprintf('add column: %s; values: %s\n','phi0Method','phi0Constant'))
  }

  # change NaN with NA
  for (col in colnames(data)) {
    posNan = which(is.nan(data[,col]) == 1)
    if (length(posNan) > 0) {
      cat(sprintf('variable: %s; change %d NaN value%s in NA\n',
                  col,length(posNan),ifelse(length(posNan) == 1,'','s')))
      data[posNan,col] = NA
    }
    rm(posNan)
  }
  rm(col)

  return(data)
}
