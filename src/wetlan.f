      subroutine wetlan
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine simulates wetlands     

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bw1(:)      |none          |1st shape parameter for wetland surface area
!!                               |equation
!!    bw2(:)      |none          |2nd shape parameter for the wetland surface
!!                               |area equation
!!    chlaw(:)    |none          |chlorophyll-a production coefficient for 
!!                               |wetland
!!    hru_dafr(:) |none          |fraction of watershed area in HRU
!!    hru_ha(:)   |ha            |area of HRU in hectares
!!    ihru        |none          |HRU number
!!    ipnd1(:)    |none          |beginning month of 2nd "season" of nutrient
!!                               |settling
!!    ipnd2(:)    |none          |ending month of 2nd "season" of nutrient
!!                               |settling
!!    minpgw(:)   |kg P/ha       |soluble P loading to reach in groundwater
!!    no3gw(:)    |kg N/ha       |nitrate loading to reach in groundwater
!!    nsetlw(1,:) |m/day         |nitrogen settling rate for 1st season
!!    nsetlw(2,:) |m/day         |nitrogen settling rate for 2nd season
!!    pet_day     |mm H2O        |potential evapotranspiration for day in HRU
!!    psetlw(1,:) |m/day         |phosphorus settling rate for 1st season
!!    psetlw(2,:) |m/day         |phosphorus settling rate for 2nd season
!!    qdr(:)      |mm H2O        |net water loading from HRU to main channel
!!    secciw(:)   |none          |water clarity coefficient for wetland
!!    sed_stl(:)  |kg/kg         |fraction of sediment remaining suspended in
!!                               |impoundment after settling for one day
!!    sedminpa(:) |kg P/ha       |amount of active mineral phosphorus sorbed to
!!                               |sediment in surface runoff in HRU for day
!!    sedminps(:) |kg P/ha       |amount of stable mineral phosphorus sorbed to
!!                               |sediment in surface runoff in HRU for day
!!    sedorgn(:)  |kg N/ha       |amount of organic nitrogen in surface runoff
!!                               |in HRU for the day
!!    sedorgp(:)  |kg P/ha       |amount of organic phosphorus in surface runoff
!!                               |in HRU for the day
!!    sedyld(:)   |metric tons   |daily soil loss caused by water erosion in HRU
!!    shallst(:)  |mm H2O        |depth of water in shallow aquifer
!!    subp(:)     |mm H2O        |precipitation for the day in HRU
!!    surqno3(:)  |kg N/ha       |amount of NO3-N in surface runoff in HRU for
!!                               |the day
!!    surqsolp(:) |kg P/ha       |amount of soluble phosphorus in surface runoff
!!                               |in HRU for the day
!!    wet_fr(:)   |none          |fraction of HRU/subbasin area that drains
!!                               |into wetlands
!!    wet_k(:)    |mm/hr         |hydraulic conductivity of bottom of wetlands
!!    wet_mxvol(:)|m^3 H2O       |volume of water required to fill wetlands to
!!                               |maximum water level
!!    wet_no3(:)  |kg N          |amount of nitrate originating from surface
!!                               |runoff in wetland at beginning of day
!!    wet_no3g(:) |kg N          |amount of nitrate originating from
!!                               |groundwater in wetland at beginning of day
!!    wet_no3s(:) |kg N          |amount of nitrate originating from lateral
!!                               |flow in wetland at beginning of day
!!    wet_nsed(:) |kg/L          |normal sediment concentration in wetland water
!!    wet_nvol(:) |m^3 H2O       |volume of water needed to
!!                               |fill wetlands to normal water level
!!    wet_orgn(:) |kg N          |amount of organic N originating from
!!                               |surface runoff in wetland at beginning of day
!!    wet_orgp(:) |kg P          |amount of organic P originating from
!!                               |surface runoff in wetland at beginning of day
!!    wet_psed(:) |kg P          |amount of mineral P attached to sediment
!!                               |originating from surface runoff in wetland at
!!                               |beginning of day
!!    wet_sed(:)  |kg/L          |sediment concentration in wetland water
!!    wet_solp(:) |kg P          |amount of soluble P originating from surface
!!                               |runoff in wetland at beginning of day
!!    wet_solpg(:)|kg P          |amount of soluble P originating from
!!                               |groundwater in wetland at beginning of day
!!    wet_vol(:)  |m^3 H2O       |volume of water in wetland, apportioned to HRU(j)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    qdr(:)      |mm H2O        |net water loading from HRU to main channel
!!    sedyld(:)   |metric tons   |daily soil loss caused by water erosion in HRU
!!    shallst(:)  |mm H2O        |depth of water in shallow aquifer
!!    twlwet      |mm H2O        |water lost through seepage from wetlands on
!!                               |day in HRU
!!    wet_chla(:) |kg chla       |amount of chlorophyll-a in wetland at end
!!                               |of day
!!    wet_no3(:)  |kg N          |amount of nitrate originating from surface
!!                               |runoff in wetland at end of day
!!    wet_no3g(:) |kg N          |amount of nitrate originating from
!!                               |groundwater in wetland at end of day
!!    wet_no3s(:) |kg N          |amount of nitrate originating from lateral
!!                               |flow in wetland at end of day
!!    wet_orgn(:) |kg N          |amount of organic N originating from
!!                               |surface runoff in wetland at end of day
!!    wet_orgp(:) |kg P          |amount of organic P originating from
!!                               |surface runoff in wetland at end of day
!!    wet_psed(:) |kg P          |amount of mineral P attached to sediment
!!                               |originating from surface runoff in wetland at
!!                               |end of day
!!    wet_seci(:) |m             |secchi-disk depth in wetland at end of day
!!    wet_sed(:)  |kg/L          |sediment concentration in wetland water
!!    wet_solp(:) |kg P          |amount of soluble P originating from surface
!!                               |runoff in wetland at end of day
!!    wet_solpg(:)|kg P          |amount of soluble P originating from
!!                               |groundwater in wetland at end of day
!!    wet_vol(:)  |m^3 H2O       |volume of water in wetland, apportioned to HRU(j)
!!    wetev       |m^3 H2O       |evaporation from wetland for day
!!    wetflwi     |m^3 H2O       |volume of water flowing in wetland on day
!!    wetflwo     |m^3 H2O       |volume of water flowing out wetland on day
!!    wetpcp      |m^3 H2O       |precipitation on wetland for day
!!    wetsedc     |metric tons   |net change in sediment in wetland on day
!!    wetsedi     |metric tons   |sediment loading to wetland for day
!!    wetsedo     |metric tons   |sediment loading from wetland for day
!!    wetsep      |m^3 H2O       |seepage from wetland bottom for day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cnv         |none          |conversion factor (mm/ha => m^3)
!!    iseas       |none          |nutrient settling rate season
!!    j           |none          |HRU number
!!    nitrok      |none          |fraction of nitrogen in wetland removed by
!!                               |settling
!!    phosk       |none          |fraction of phosphorus in wetland removed by
!!                               |settling
!!    sed         |kg/kg         |sediment concentration in wetland at beginning
!!                               |of day
!!    tpco        |ppb (ug/L)    |concentration of phosphorus in pond water
!!                               |on day
!!    vol         |m^3 H2O       |volume of wetland at beginning of day
!!    wetsa       |ha            |surface area of wetland on current day
!!    xx          |none          |variable to hold intermediate calculation
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!   ************************************************************************ 
!!    Almendinger/Ulrich -- NEW VARIABLES; also see changes to existing variables above
!!
!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    alpha_bf(:) |days          |alpha factor for groundwater recession curve
!!    alpha_bfe(:)|none          |Exp(-alpha_bf(:))
!!    deepst(:)   |mm H2O        |depth of water in deep aquifer     
!!    gwminp(:)   |mg P/L        |soluble P concentration in groundwater
!!    qw_q_wet(:) |mm H2O        |groundwater contribution to streamflow from
!!                               |wetland seepage on current day
!!    gwqmn(:)    |mm H2O        |threshold depth of water in shallow aquifer
!!                               |required before groundwater flow will occur
!!    rchrg_dp(:) |none          |recharge to deep aquifer: the fraction of
!!                               |wetland seepage that reaches the deep aquifer
!!    rchrg_wet(:)|mm H2O        |amount of water recharging both aquifers on
!!                               |current day in HRU from wetland
!!    shallst(:)  |mm H2O        |depth of water in shallow aquifer
!!    latno3_wet(:)   |kg N/ha   |daily loading to reach from NO3-N entering 
!!                               |wetland via lateral flow and exiting
!!                               |via seepage gw return flow
!!    minpgw_wet(:)   |kg P/ha   |daily loading to reach from soluble P entering 
!!                               |wetland via gw return flow and exiting
!!                               |via seepage gw return flow
!!    no3gw_wet(:)    |kg N/ha   |daily loading to reach from NO3-N entering
!!                               |wetland via gw return flow and exiting
!!                               |via seepage gw return flow
!!    qdr_wet(:)      |mm H2O    |daily water entering wetland as surface/lat/gw flow  
!!                               |and exiting as surface/gw return flow
!!    sedminpa_wet(:) |kg P/ha   |daily loading to reach from active mineral P
!!                               |sorbed to sed. entering wetland via surface runoff
!!                               |and exiting via surface flow
!!    sedminps_wet(:) |kg P/ha   |daily loading to reach from stable mineral P
!!                               |sorbed to sed. entering wetland via surface runoff
!!                               |and exiting via surface flow
!!    sedorgn_wet(:)  |kg N/ha   |daily loading to reach from organic N
!!                               |entering wetland via surface runoff and 
!!                               |exiting via surface flow
!!    sedorgp_wet(:)  |kg P/ha   |daily loading to reach from organic P
!!                               |entering wetland via surface runoff and 
!!                               |exiting via surface flow
!!    sedyld_wet(:)   |met tons  |daily loading to reach from eroded sediment
!!                               |entering wetland via surface runoff and 
!!                               |exiting via surface flow
!!    surqno3_wet(:)  |kg N/ha   |daily loading to reach from NO3-N
!!                               |entering wetland via surface runoff and 
!!                               |exiting via surface flow
!!    surqsolp_wet(:) |kg P/ha   |daily loading to reach from soluble P
!!                               |entering wetland via surface runoff and 
!!                               |exiting via surface flow

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    deepst(:)   |mm H2O        |depth of water in deep aquifer     
!!    gwseep_wet  |mm H2O        |amount of water from wetland recharging deep 
!!                               |aquifer on current day in HRU
!!    gw_q_wet(:) |mm H2O        |groundwater contribution to streamflow from
!!                               |wetland seepage on current day
!!    rchrg_wet(:)|mm H2O        |amount of water recharging both aquifers on
!!                               |current day in HRU from wetland
!!    shallst(:)  |mm H2O        |depth of water in shallow aquifer
!!    latno3_wet(:)   |kg N/ha   |daily loading to reach from NO3-N entering 
!!                               |wetland via lateral flow and exiting
!!                               |via seepage gw return flow
!!    minpgw_wet(:)   |kg P/ha   |daily loading to reach from soluble P entering 
!!                               |wetland via gw return flow and exiting
!!                               |via seepage gw return flow
!!    no3gw_wet(:)    |kg N/ha   |daily loading to reach from NO3-N entering
!!                               |wetland via gw return flow and exiting
!!                               |via seepage gw return flow
!!    qdr_wet(:)      |mm H2O    |daily water entering wetland as surface/lat/gw flow  
!!                               |and exiting as surface/gw return flow
!!    sedminpa_wet(:) |kg P/ha   |daily loading to reach from active mineral P
!!                               |sorbed to sed. entering wetland via surface runoff
!!                               |and exiting via surface flow
!!    sedminps_wet(:) |kg P/ha   |daily loading to reach from stable mineral P
!!                               |sorbed to sed. entering wetland via surface runoff
!!                               |and exiting via surface flow
!!    sedorgn_wet(:)  |kg N/ha   |daily loading to reach from organic N
!!                               |entering wetland via surface runoff and 
!!                               |exiting via surface flow
!!    sedorgp_wet(:)  |kg P/ha   |daily loading to reach from organic P
!!                               |entering wetland via surface runoff and 
!!                               |exiting via surface flow
!!    sedyld_wet(:)   |met tons  |daily loading to reach from eroded sediment
!!                               |entering wetland via surface runoff and 
!!                               |exiting via surface flow
!!    surqno3_wet(:)  |kg N/ha   |daily loading to reach from NO3-N
!!                               |entering wetland via surface runoff and 
!!                               |exiting via surface flow
!!    surqsolp_wet(:) |kg P/ha   |daily loading to reach from soluble P
!!                               |entering wetland via surface runoff and 
!!                               |exiting via surface flow
!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    totno3_conc |kg/m^3        |NO3 concentration in water body
!!    zz          |none          |variable to hold intermediate calculation
!!
!!   Almendinger/Ulrich END NEW VAR's **********************************************************************
!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: j, iseas
      real :: vol, cnv, sed, wetsa, xx, phosk, nitrok, tpco
      real :: wetsani, wetsili, wetclai, wetsagi, wetlagi
	real :: san, sil, cla, sag, lag, inised, finsed,setsed,remsetsed
      real :: wetsano, wetsilo, wetclao, wetsago, wetlago
      !! Almendinger/Ulrich: new vars
      real :: zz, totno3_conc
            
      totno3_conc = 0 
      j = 0
      j = ihru

      if (wet_fr(j) > 0.) then
        cnv = 0.
        cnv = hru_ha(j) * 10.               !conversion factor
        
        !! store initial values
        vol = 0.
        sed = 0.

        san = 0.
        sil = 0.
        cla = 0.
        sag = 0.
        lag = 0.
        inised = 0.
        finsed = 0.
        setsed = 0.
        remsetsed = 0.

        vol = wet_vol(j)
        sed = wet_sed(j)

        san = wet_san(j)
        sil = wet_sil(j)
        cla = wet_cla(j)
        sag = wet_sag(j)
        lag = wet_lag(j)
        
        !! calculate wetland surface area apportioned to HRU(j),
        wetsa = 0.
        wetsa = bw1(j) * wet_vol(j) ** bw2(j)

        !! calculate water balance for day
        wetev = 10. * evwet(j) * pet_day * wetsa
        wetsep = wet_k(j) * wetsa * 240.
        wetpcp = subp(j) * wetsa * 10.
        
        !! calculate water flowing into wetland from HRU
        !! Almendinger/Ulrich: wet_fr() now accounted for in subbasin.f calc of qdr_wet()
        !!    But here we need to regenerate qdr() to match logic of old code, 
        !!    which was correct for calculating this part of the water balance:
        wetflwi = (qdr_wet(j)/wet_fr(j)) * 10. * (hru_ha(j) * wet_fr(j) &
     &                                                         - wetsa)
        !!OLD CODE
        !wetflwi = qdr(j) * 10. * (hru_ha(j) * wet_fr(j) - wetsa)
        
        !! Almendinger/Ulrich
        !! Re-initialize qdr_wet(), which will be adjusted for gains & losses
        !!   before being passed back to subbasin.f
        qdr_wet(j) = 0.
        
        !! OLD CODE not needed: replaced by qdr_wet
        !qdr(j) = qdr(j) - qdr(j) * wet_fr(j)
       
        !! sediment loading to wetland from HRU; subtract off area of pond so it's not double counted        
        !! Almendinger/Ulrich: wet_fr() already accounted for in subbasin.f calc of sedyld_wet()
        wetsedi = sedyld_wet(j) * (1 - (wetsa / hru_ha(j)))
        
        wetsani = sanyld_wet(j) * (1 - (wetsa / hru_ha(j)))
        wetsili = silyld_wet(j) * (1 - (wetsa / hru_ha(j)))
        wetclai = clayld_wet(j) * (1 - (wetsa / hru_ha(j)))
        wetsagi = sagyld_wet(j) * (1 - (wetsa / hru_ha(j)))
        wetlagi = lagyld_wet(j) * (1 - (wetsa / hru_ha(j)))

        !! OLD CODE: wet_fr() now accounted for in subbasin.f calc    
        !wetsedi = sedyld(j) * (wet_fr(j) - (wetsa / hru_ha(j)))
        
!        wetsani = sanyld(j) * (wet_fr(j) - (wetsa / hru_ha(j)))
!        wetsili = silyld(j) * (wet_fr(j) - (wetsa / hru_ha(j)))
!        wetclai = clayld(j) * (wet_fr(j) - (wetsa / hru_ha(j)))
!        wetsagi = sagyld(j) * (wet_fr(j) - (wetsa / hru_ha(j)))
!        wetlagi = lagyld(j) * (wet_fr(j) - (wetsa / hru_ha(j)))

        !! Almendinger/Ulrich NEW var name
        sedyld_wet(j) = 0.
        sanyld_wet(j) = 0.
        silyld_wet(j) = 0.
        clayld_wet(j) = 0.
        sagyld_wet(j) = 0.
        lagyld_wet(j) = 0.
 
         !OLD CODE   
!        sedyld(j) = sedyld(j) - sedyld(j) * wet_fr(j)
!
!        sanyld(j) = sanyld(j) - sanyld(j) * wet_fr(j)
!        silyld(j) = silyld(j) - silyld(j) * wet_fr(j)
!        clayld(j) = clayld(j) - clayld(j) * wet_fr(j)
!        sagyld(j) = sagyld(j) - sagyld(j) * wet_fr(j)
!        lagyld(j) = lagyld(j) - lagyld(j) * wet_fr(j)

        !! compute nitrogen and phosphorus levels in wetland at beginning
        !! of day
        !! equation 29.1.1 in SWAT manual
        xx = 0.
        !! Calculate area over which to apply sed and nutrient yields from HRU
        !! Almendinger/Ulrich: wet_fr() already accounted for in subbasin.f calc of xx        
        xx = hru_ha(j) - wetsa         
        !! OLD CODE
        !xx = wet_fr(j) * hru_ha(j)
        
        !! Almendinger/Ulrich: var names below changed to "_wet" form
        !! Results are masses (yield * area)
        wet_solp(j) = wet_solp(j) + (surqsolp_wet(j) + sedminpa_wet(j)) &
     &                                                              * xx
        wet_psed(j) = wet_psed(j) + sedminps_wet(j) * xx
        wet_orgp(j) = wet_orgp(j) + sedorgp_wet(j) * xx
        wet_solpg(j) = wet_solpg(j) + minpgw_wet(j) * xx
        wet_orgn(j) = wet_orgn(j) + sedorgn_wet(j) * xx
        wet_no3(j) = wet_no3(j) + surqno3_wet(j) * xx
        wet_no3s(j) = wet_no3s(j) + latno3_wet(j) * xx
        wet_no3g(j) = wet_no3g(j) + no3gw_wet(j) * xx
        
        !!OLD CODE
!        wet_solp(j) = wet_solp(j) + (surqsolp(j) + sedminpa(j)) * xx
!        wet_psed(j) = wet_psed(j) + sedminps(j) * xx
!        wet_orgp(j) = wet_orgp(j) + sedorgp(j) * xx
!        wet_solpg(j) = wet_solpg(j) + minpgw(j) * xx
!        wet_orgn(j) = wet_orgn(j) + sedorgn(j) * xx
!        wet_no3(j) = wet_no3(j) + surqno3(j) * xx
!        wet_no3s(j) = wet_no3s(j) + latno3(j) * xx
!        wet_no3g(j) = wet_no3g(j) + no3gw(j) * xx

        !! Almendinger/Ulrich: zero-out _wet vars
        sedorgn_wet(j) = 0.
        surqno3_wet(j) = 0.
        latno3_wet(j) = 0.
        no3gw_wet(j) = 0.
!        sedorgp_wet(j) = 0.
        sedminps_wet(j) = 0.
        sedminpa_wet(j) = 0.
        surqsolp_wet(j) = 0.
        minpgw_wet(j) = 0.
        
        !! Almendinger/Ulrich -- compute total NO3 concentration (kg/m^3) in water body
        !!  prior to any settling, outflow, or atmospheric exchanges.
        !!  This concentration will be used to characterize NO3 for 
        !!  seepage when wetland volume approaches zero.
        If ((vol + wetflwi) > 0.001) then
          totno3_conc = (wet_no3(j) + wet_no3s(j) + wet_no3g(j)) /      &
     &                                                (vol + wetflwi)
        else
          totno3_conc = 0
        end if

!        !! remove nutrients entering wetlands from HRU loadings
!        xx = 0.
!        xx = 1. - wet_fr(j)
!        sedorgn(j) = sedorgn(j) * xx
!        surqno3(j) = surqno3(j) * xx
!        latno3(j) = latno3(j) * xx
!        no3gw(j) = no3gw(j) * xx
!        sedorgp(j) = sedorgp(j) * xx
!        sedminpa(j) = sedminpa(j) * xx
!        sedminps(j) = sedminps(j) * xx
!        surqsolp(j) = surqsolp(j) * xx
!        minpgw(j) = minpgw(j) * xx

        !! new water volume for day 
        wet_vol(j) = wet_vol(j) - wetsep - wetev + wetpcp + wetflwi

        if (wet_vol(j) < 0.001) then

          !! check for volume deficit in wetland
          !! reduce seepage so that the wetland volume is zero
          wetsep = wetsep + wet_vol(j)
          wet_vol(j) = 0.
          !! if seepage is less than the volume deficit, take the 
          !! remainder from evaporation
          if (wetsep < 0.) then
            wetev = wetev + wetsep
            wetsep = 0.
          end if
          !! Assume all nutrients are lost from aquatic system when  
          !!  volume goes to zero. 
          !!  (But leave totno3_conc at inflowing value)
          wet_sed(j) = 0.

          wet_san(j) = 0.
          wet_sil(j) = 0.
          wet_cla(j) = 0.
          wet_sag(j) = 0.
          wet_lag(j) = 0.

          wet_solp(j) = 0.
          wet_psed(j) = 0.
          wet_orgp(j) = 0.
          wet_solpg(j) = 0.
          wet_orgn(j) = 0.
          wet_no3(j) = 0.
          wet_no3s(j) = 0.
          wet_no3g(j) = 0.
          wet_chla(j) = 0.
          wet_seci(j) = 0.
          
        !! For all cases where wetland has water (vol > 0):   
        else
 
          !! compute new sediment concentration
          wet_sed(j) = (sed * vol + wetsedi) / wet_vol(j)

          wet_san(j) = (san * vol + wetsani) / wet_vol(j)
          wet_sil(j) = (sil * vol + wetsili) / wet_vol(j)
          wet_cla(j) = (cla * vol + wetclai) / wet_vol(j)
          wet_sag(j) = (sag * vol + wetsagi) / wet_vol(j)
          wet_lag(j) = (lag * vol + wetlagi) / wet_vol(j)

          !! compute outflow if wetland water volume > 0
          if (wet_vol(j) <= wet_nvol(j)) then
            wetflwo = 0.
          else
            if (wet_vol(j) <= wet_mxvol(j)) then
              wetflwo = (wet_vol(j) - wet_nvol(j)) / 10.
              wet_vol(j) = wet_vol(j) - wetflwo
            else
              wetflwo = wet_vol(j) - wet_mxvol(j)
              wet_vol(j) = wet_mxvol(j)
            end if
          end if
          
          !! Almendinger/Ulrich: compute water leaving impoundment via surface outlet
          qdr_wet(j) = wetflwo / cnv
          !! OLD CODE: qdr replaced by qdr_wet here
          !!qdr(j) = qdr(j) + wetflwo / cnv

          !! compute sediment settling
	    if (sed_stl(j) < 1.e-6) sed_stl(j) = 0.0
          inised = wet_sed(j)
          if (wet_sed(j) > wet_nsed(j)) then
            wet_sed(j) = (wet_sed(j) - wet_nsed(j)) * sed_stl(j) +      &
     &                                                       wet_nsed(j)
          end if
          finsed = wet_sed(j)
	    setsed = inised - finsed
          setsed = Max(0.,setsed)

	    if (wet_lag(j) >= setsed) then
	      wet_lag(j) = wet_lag(j) - setsed
	      remsetsed = 0.
	    else
	      remsetsed = setsed - wet_lag(j)
	      wet_lag(j) = 0.
	      if (wet_san(j) >= remsetsed) then
	        wet_san(j) = wet_san(j) - remsetsed
	        remsetsed = 0.
	      else
	        remsetsed = remsetsed - wet_san(j)
	        wet_san(j) = 0.
              if (wet_sag(j) >= remsetsed) then
	          wet_sag(j) = wet_sag(j) - remsetsed
	          remsetsed = 0.
	        else
	          remsetsed = remsetsed - wet_sag(j)
	          wet_sag(j) = 0.
                if (wet_sil(j) >= remsetsed) then
  	            wet_sil(j) = wet_sil(j) - remsetsed
	            remsetsed = 0.
	          else
	            remsetsed = remsetsed - wet_sil(j)
	            wet_sil(j) = 0.
                  if (wet_cla(j) >= remsetsed) then
	              wet_cla(j) = wet_cla(j) - remsetsed
	              remsetsed = 0.
	            else
	              remsetsed = remsetsed - wet_cla(j)
	              wet_cla(j) = 0.
	            end if
                end if
	        end if
	      end if
	    end if

          !! compute sediment leaving wetland
          wetsedo = wet_sed(j) * wetflwo

          wetsano = wet_san(j) * wetflwo
          wetsilo = wet_sil(j) * wetflwo
          wetclao = wet_cla(j) * wetflwo
          wetsago = wet_sag(j) * wetflwo
          wetlago = wet_lag(j) * wetflwo

          !! Almendinger/Ulrich: var name change _wet
          sedyld_wet(j) = wetsedo   
                 
          sanyld_wet(j) = wetsano
          silyld_wet(j) = wetsilo
          clayld_wet(j) = wetclao
          sagyld_wet(j) = wetsago
          lagyld_wet(j) = wetlago

          !! OLD CODE: vars replaced by _wet form above
!          sedyld(j) = sedyld(j) + wetsedo
!          sanyld(j) = sanyld(j) + wetsano
!          silyld(j) = silyld(j) + wetsilo
!          clayld(j) = clayld(j) + wetclao
!          sagyld(j) = sagyld(j) + wetsago
!          lagyld(j) = lagyld(j) + wetlago

          !! net change in amount of sediment in wetland for day
          wetsedc = vol * sed + wetsedi - wetsedo - wet_sed(j) *        &
     &                                                        wet_vol(j)
          !! determine settling rate for nutrients
          !! part of equation 29.1.3 in SWAT manual
          if (i_mo >= ipnd1(j) .and. i_mo <= ipnd2(j)) then
            iseas = 1
          else
            iseas = 2
          endif
          phosk = 0.
          nitrok = 0.
          phosk = psetlw(iseas,j) * wetsa * 10000. / wet_vol(j)
          phosk = Min(phosk, 1.)
          nitrok = nsetlw(iseas,j) * wetsa * 10000. / wet_vol(j)
          nitrok = Min(nitrok, 1.)

          !! remove nutrients by settling
          !! other part of equation 29.1.3 in SWAT manual
          wet_solp(j) = wet_solp(j) * (1. - phosk)
          wet_psed(j) = wet_psed(j) * (1. - phosk)
          wet_orgp(j) = wet_orgp(j) * (1. - phosk)
          wet_solpg(j) = wet_solpg(j) * (1. - phosk)
          wet_orgn(j) = wet_orgn(j) * (1. - nitrok)
          wet_no3(j) = wet_no3(j) * (1. - nitrok)
          wet_no3s(j) = wet_no3s(j) * (1. - nitrok)
          wet_no3g(j) = wet_no3g(j) * (1. - nitrok)

          if (wet_vol(j) < 1.e-6) wet_vol(j) = 0.0
          if (wet_orgn(j) < 1.e-6) wet_orgn(j) = 0.0
          if (wet_no3(j) < 1.e-6) wet_no3(j) = 0.0
          if (wet_no3s(j) < 1.e-6) wet_no3s(j) = 0.0
          if (wet_no3g(j) < 1.e-6) wet_no3g(j) = 0.0
          if (wet_orgp(j) < 1.e-6) wet_orgp(j) = 0.0
          if (wet_psed(j) < 1.e-6) wet_psed(j) = 0.0
          if (wet_solp(j) < 1.e-6) wet_solp(j) = 0.0
          if (wet_solpg(j) < 1.e-6) wet_solpg(j) = 0.0

          !! Almendinger/Ulrich: Calculate resulting nutrient concentrations,
          !!   accounting for losses of particulate species to outflow, and
          !!   losses of soluble species to outflow and seepage.
          !! Calculate resulting no3 concentration, as kg/m^3
          totno3_conc = 0.
          totno3_conc = (wet_no3(j) + wet_no3s(j) + wet_no3g(j)) /      &
     &                           (wet_vol(j) + wetflwo + wetsep)
          !! Calculate resulting TP concentration   
          tpco = 0.
          tpco = 1.e+6 *                                                &
     &        ((wet_orgp(j)+wet_psed(j)) / (wet_vol(j)+wetflwo) +       &
     &         (wet_solp(j)+wet_solpg(j)) / (wet_vol(j)+wetflwo+wetsep))
     
          chlaco = 0.
          wet_chla(j) = 0.
          wet_seci(j) = 0.
          if (tpco > 1.e-4) then
            !! equation 29.1.6 in SWAT manual
            chlaco = chlaw(j) * 0.551 * (tpco**0.76)
            wet_chla(j) = chlaco * (wet_vol(j) + wetflwo) * 1.e-6
          endif
          if (chlaco > 1.e-4) then
            !! equation 29.1.8 in SWAT manual
            wet_seci(j) = secciw(j) * 6.35 * (chlaco**(-0.473))
          endif

          !! compute nutrients leaving wetland
          !! fraction leaving in outflow
          yy = 0.
          if ((wet_vol(j) + wetflwo) > 0.) then
            yy = wetflwo / (wet_vol(j) + wetflwo)
          else
            yy = 0.
          endif
          
          !! Almendinger/Ulrich : fraction leaving in outflow AND seepage
          zz = 0.
          if ((wet_vol(j) + wetflwo + wetsep) > 0.) then
            zz = (wetflwo + wetsep) / (wet_vol(j) + wetflwo + wetsep)
          else
            zz = 0.
          endif
          
          !! Almendinger/Ulrich: var name change _wet
          !! Quantities leaving in surface outflow, going directly 
          !!   to the reach, as yields (kg/ha)
          !!  Note yields here are apportioned over entire HRU area (hru_ha(j)),
          !!    rather than just the land portion (hru_ha(j) - wetsa)
          sedorgn_wet(j) = wet_orgn(j) * yy / hru_ha(j)          
          surqno3_wet(j) = wet_no3(j) * yy / hru_ha(j)
          latno3_wet(j) = wet_no3s(j) * yy / hru_ha(j)
          no3gw_wet(j) = wet_no3g(j) * yy / hru_ha(j)          
          sedorgp_wet(j) = wet_orgp(j) * yy / hru_ha(j)
          sedminps_wet(j) = wet_psed(j) * yy / hru_ha(j)
          surqsolp_wet(j) = wet_solp(j) * yy / hru_ha(j)
          minpgw_wet(j) = wet_solpg(j) * yy / hru_ha(j)          

          !! OLD CODE -- not needed          
!          sedorgn(j) = sedorgn(j) + wet_orgn(j) * yy / hru_ha(j)
!          surqno3(j) = surqno3(j) + wet_no3(j) * yy / hru_ha(j)
!          latno3(j) = latno3(j) + wet_no3s(j) * yy / hru_ha(j)
!          no3gw(j) = no3gw(j) + wet_no3g(j) * yy / hru_ha(j)
!          sedorgp(j) = sedorgp(j) + wet_orgp(j) * yy / hru_ha(j)
!          sedminps(j) = sedminps(j) + wet_psed(j) * yy / hru_ha(j)
!          surqsolp(j) = surqsolp(j) + wet_solp(j) * yy / hru_ha(j)
!          minpgw(j) = minpgw(j) + wet_solpg(j) * yy / hru_ha(j)

          !!update nutrient pools in wetlands
          !! For particulates, that leave only in outflow
          wet_orgn(j) = wet_orgn(j) * (1. - yy)
          wet_orgp(j) = wet_orgp(j) * (1. - yy)
          wet_psed(j) = wet_psed(j) * (1. - yy)
          wet_chla(j) = wet_chla(j) * (1. - yy)          
          !! Almendinger/Ulrich:
          !! For soluble fractions, that leave in outflow AND seepage 
          wet_no3(j) = wet_no3(j) * (1. - zz)
          wet_no3s(j) = wet_no3s(j) * (1. - zz)
          wet_no3g(j) = wet_no3g(j) * (1. - zz)
          wet_solp(j) = wet_solp(j) * (1. - zz)
          wet_solpg(j) = wet_solpg(j) * (1. - zz) 

          !! OLD CODE -- for when seepage wasn't considered        
!          wet_no3(j) = wet_no3(j) * (1. - yy)
!          wet_no3s(j) = wet_no3s(j) * (1. - yy)
!          wet_no3g(j) = wet_no3g(j) * (1. - yy)
!          wet_solp(j) = wet_solp(j) * (1. - yy)
!          wet_solpg(j) = wet_solpg(j) * (1. - yy)          

        end if

        !! compute seepage depth for HRU water balance
        twlwet = wetsep / cnv

!! **************************************************************************************************
        !! Almendinger/Ulrich: compute groundwater recharge and return flow from bottom seepage
        !! Code is based on gwmod.f: same form as hrupond.f and routres.f
        
        !! Set recharge = seepage: assume 100% of seepage immediately becomes recharge     
        rchrg_wet(j) = twlwet
        
        !! var name change _wet
        if (rchrg_wet(j) < 1.e-6) rchrg_wet(j) = 0.

        !! compute deep aquifer level for day, adding component from ponds and wetlands to that calc'd prev in gwmod.f
        gwseep_wet = rchrg_wet(j) * rchrg_dp(j)
        !! add wetland deep seepage to that from HRU
        gwseep = gwseep + gwseep_wet
        deepst(j) = deepst(j) + gwseep_wet
        
        !! adjust shallow aquifer storage
        shallst(j) = shallst(j) + (rchrg_wet(j) - gwseep_wet)
        
        !! compute groundwater contribution to streamflow for day and adjust shallow storage
        if (shallst(j) > gwqmn(j)) then
           gw_q_wet(j) = gw_q_wet(j) * alpha_bfe(j) + (rchrg_wet(j) -   &
     &                              gwseep_wet ) * (1. - alpha_bfe(j))
           shallst(j) = shallst(j) - gw_q_wet(j)
           if (shallst(j) < gwqmn(j)) then
             gw_q_wet(j) = gw_q_wet(j) - (gwqmn(j) - shallst(j))
             shallst(j) = gwqmn(j)
           end if
        else
           gw_q_wet(j) = 0.
        end if

             
        !! Almendinger/Ulrich -- calc sol-P concentration out of impoundment via seepage
        !!   Here, simply assumed to = ambient aq conc: gwminp(j)
        
        !! Almendinger/Ulrich -- calc sol-P contribution to reach from groundwater, as --
        !!   -- sol-P originally reaching wetland from incoming gw flow leaving as surface outflow, plus
        !!   -- sol-P as groundwater originating as seepage from wetland at ambient HRU gw P concentration
        !! all calculated as yield (kg/ha) over entire HRU.
        !!   Note 1/100 factor to convert (mg/L * mm) to (kg/ha)
        minpgw_wet(j) = minpgw_wet(j) + gwminp(j) * gw_q_wet(j) / 100
        
        !! Almendinger/Ulrich -- estimate gw delivery of NO3 to reach as -- 
        !!   -- NO3gw originally reaching wetland from incoming gw flow leaving as surface outflow, plus
        !!   -- NO3gw as groundwater originating as seepage from wetland at NO3 concentration in wetland at end of day
        !! all calculated as yield (kg/ha) over entire HRU.
        !!   Note 10 factor to convert (kg/m^3 * mm) to (kg/ha)
        no3gw_wet(j) = no3gw_wet(j) + totno3_conc *  gw_q_wet(j) * 10
                      
        !! compute final adjusted HRU water yield, add GW return flow from wetland seepage
        qdr_wet(j) = qdr_wet(j) + gw_q_wet(j)
            
        !! END Almendinger/Ulrich NEW CODE 
        !! *******************************************************************************************  
        
        
        !! Almendinger/Ulrich -- commented -- moved above
        !! add impoundment seepage to shallow aquifer 
        !!shallst(j) = shallst(j) + wetsep / cnv

        !! Almendinger/Ulrich -- commented -- moved above
        !! compute seepage depth for HRU water balance
        !!twlwet = wetsep / cnv

      end if
      
      !! Almendinger/Ulrich -- var names changes: "_wet"  
      if (qdr_wet(j) < 0.) qdr_wet(j) = 0.
      if (sedyld_wet(j) < 0.) then
        sedyld_wet(j) = 0.
        sanyld_wet(j) = 0.
        silyld_wet(j) = 0.
        clayld_wet(j) = 0.
        sagyld_wet(j) = 0.
        lagyld_wet(j) = 0.
	end if

      return
      end
