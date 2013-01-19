      subroutine hrupond
      
!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bp1(:)      |none          |1st shape parameter for pond surface area
!!                               |equation
!!    bp2(:)      |none          |2nd shape parameter for the pond surface area
!!                               |equation
!!    hru_ha(:)   |ha            |area of HRU in hectares
!!    ihru        |none          |HRU number
!!    latno3(:)   |kg N/ha       |amount of NO3-N in lateral flow in HRU for !! ** Almendinger/Ulrich: var name and description change below**
!!                               |the day
!!    minpgw(:)   |kg P/ha       |soluble P loading to reach in groundwater !! ** Almendinger/Ulrich: var name and description change below**
!!    no3gw(:)    |kg N/ha       |nitrate loading to reach in groundwater !! ** Almendinger/Ulrich: var name and description change below**
!!    pnd_fr(:)   |none          |fraction of HRU/subbasin area that drains
!!                               |into ponds
!!    pnd_no3(:)  |kg N          |amount of nitrate originating from surface
!!                               |runoff in pond at beginning of day
!!    pnd_no3g(:) |kg N          |amount of nitrate originating from
!!                               |groundwater in pond at beginning of day
!!    pnd_no3s(:) |kg N          |amount of nitrate originating from lateral
!!                               |flow in pond at beginning of day
!!    pnd_orgn(:) |kg N          |amount of organic N originating from
!!                               |surface runoff in pond at beginning of day
!!    pnd_orgp(:) |kg P          |amount of organic P originating from
!!                               |surface runoff in pond at beginning of day
!!    pnd_psed(:) |kg P          |amount of mineral P attached to sediment
!!                               |originating from surface runoff in pond at
!!                               |beginning of day
!!    pnd_solp(:) |kg P          |amount of soluble P originating from surface
!!                               |runoff in pond at beginning of day
!!    pnd_solpg(:)|kg P          |amount of soluble P originating from
!!                               |groundwater in pond at beginning of day
!!    pnd_vol(:)  |m^3 H2O       |volume of water in pond
!!    pndflwo     |m^3 H2O       |volume of water flowing out of pond on day
!!    pndsedo     |metric tons   |sediment leaving pond during day
!!    pndsep      |m^3 H2O       |seepage from pond on day
!!    qdr(:)      |mm H2O        |net water loading from HRU to main channel !! ** Almendinger/Ulrich: var name and description change below**
!!    sedminpa(:) |kg P/ha       |amount of active mineral phosphorus sorbed to !! ** Almendinger/Ulrich: var name and description change below**
!!                               |sediment in surface runoff in HRU for day
!!    sedminps(:) |kg P/ha       |amount of stable mineral phosphorus sorbed to !! ** Almendinger/Ulrich: var name and description change below**
!!                               |sediment in surface runoff in HRU for day
!!    sedorgn(:)  |kg N/ha       |amount of organic nitrogen in surface runoff !! ** Almendinger/Ulrich: var name and description change below**
!!                               |in HRU for the day
!!    sedorgp(:)  |kg P/ha       |amount of organic phosphorus in surface runoff !! ** Almendinger/Ulrich: var name and description change below**
!!                               |in HRU for the day
!!    sedyld(:)   |metric tons   |daily soil loss caused by water erosion in HRU !! ** Almendinger/Ulrich: var name and description change below**
!!    shallst(:)  |mm H2O        |depth of water in shallow aquifer
!!    surqno3(:)  |kg N/ha       |amount of NO3-N in surface runoff in HRU for !! ** Almendinger/Ulrich: var name and description change below**
!!                               |the day
!!    surqsolp(:) |kg P/ha       |amount of soluble phosphorus in surface runoff !! ** Almendinger/Ulrich: var name and description change below**
!!                               |in HRU for the day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    latno3(:)   |kg N/ha       |amount of NO3-N in lateral flow in HRU for the !! ** Almendinger/Ulrich: var name and description change below**
!!                               |day
!!    minpgw(:)   |kg P/ha       |soluble P loading to reach in groundwater !! ** Almendinger/Ulrich: var name and description change below**
!!    no3gw(:)    |kg N/ha       |nitrate loading to reach in groundwater !! ** Almendinger/Ulrich: var name and description change below**
!!    pndflwi     |m^3 H2O       |volume of water flowing into pond on day
!!    pndsedin    |metric tons   |sediment entering pond during day
!!    qdr(:)      |mm H2O        |net water loading from HRU to main channel !! ** Almendinger/Ulrich: var name and description change below**
!!    sedminpa(:) |kg P/ha       |amount of active mineral phosphorus sorbed to !! ** Almendinger/Ulrich: var name and description change below**
!!                               |sediment in surface runoff in HRU for day
!!    sedminps(:) |kg P/ha       |amount of stable mineral phosphorus sorbed to !! ** Almendinger/Ulrich: var name and description change below**
!!                               |sediment in surface runoff in HRU for day
!!    sedorgn(:)  |kg N/ha       |amount of organic nitrogen in surface runoff !! ** Almendinger/Ulrich: var name and description change below**
!!                               |in HRU for the day
!!    sedorgp(:)  |kg P/ha       |amount of organic phosphorus in surface runoff !! ** Almendinger/Ulrich: var name and description change below**
!!                               |in HRU for the day
!!    sedyld(:)   |metric tons   |daily soil loss caused by water erosion in HRU !! ** Almendinger/Ulrich: var name and description change below**
!!    shallst(:)  |mm H2O        |depth of water in shallow aquifer
!!    surqno3(:)  |kg N/ha       |amount of NO3-N in surface runoff in HRU for !! ** Almendinger/Ulrich: var name and description change below**
!!                               |the day
!!    surqsolp(:) |kg P/ha       |amount of soluble phosphorus in surface runoff !! ** Almendinger/Ulrich: var name and description change below**
!!                               |in HRU for the day
!!    twlpnd      |mm H2O        |water lost through seepage from ponds on
!!                               |day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cnv         |none          |conversion factor (mm/ha => m^3)
!!    j           |none          |HRU number
!!    pndsa       |ha            |surface area of pond on current day
!!    xx          |none          |fraction of HRU not draining into ponds
!!    yy          |none          |fraction of water leaving pond on day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: pond

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
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
!!    qw_q_pnd(:) |mm H2O        |groundwater contribution to streamflow from
!!                               |wetland seepage on current day
!!    gwqmn(:)    |mm H2O        |threshold depth of water in shallow aquifer
!!                               |required before groundwater flow will occur
!!    rchrg_dp(:) |none          |recharge to deep aquifer: the fraction of
!!                               |wetland seepage that reaches the deep aquifer
!!    rchrg_pnd(:)|mm H2O        |amount of water recharging both aquifers on
!!                               |current day in HRU from wetland
!!    shallst(:)  |mm H2O        |depth of water in shallow aquifer
!!~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    latno3_pnd(:)   |kg N/ha   |daily loading to reach from NO3-N entering 
!!                               |pond via lateral flow and exiting
!!                               |via seepage gw return flow
!!    minpgw_pnd(:)   |kg P/ha   |daily loading to reach from soluble P entering 
!!                               |pond via gw return flow and exiting
!!                               |via seepage gw return flow
!!    no3gw_pnd(:)    |kg N/ha   |daily loading to reach from NO3-N entering
!!                               |pond via gw return flow and exiting
!!                               |via seepage gw return flow
!!    qdr_pnd(:)      |mm H2O    |daily water entering pond as surface/lat/gw flow  
!!                               |and exiting as surface/gw return flow
!!    sedminpa_pnd(:) |kg P/ha   |daily loading to reach from active mineral P
!!                               |sorbed to sed. entering pond via surface runoff
!!                               |and exiting via surface flow
!!    sedminps_pnd(:) |kg P/ha   |daily loading to reach from stable mineral P
!!                               |sorbed to sed. entering pond via surface runoff
!!                               |and exiting via surface flow
!!    sedorgn_pnd(:)  |kg N/ha   |daily loading to reach from organic N
!!                               |entering pond via surface runoff and 
!!                               |exiting via surface flow
!!    sedorgp_pnd(:)  |kg P/ha   |daily loading to reach from organic P
!!                               |entering pond via surface runoff and 
!!                               |exiting via surface flow
!!    sedyld_pnd(:)   |met tons  |daily loading to reach from eroded sediment
!!                               |entering pond via surface runoff and 
!!                               |exiting via surface flow
!!    surqno3_pnd(:)  |kg N/ha   |daily loading to reach from NO3-N
!!                               |entering pond via surface runoff and 
!!                               |exiting via surface flow
!!    surqsolp_pnd(:) |kg P/ha   |daily loading to reach from soluble P
!!                               |entering pond via surface runoff and 
!!                               |exiting via surface flow

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    deepst(:)   |mm H2O        |depth of water in deep aquifer     
!!    gwseep_pnd  |mm H2O        |amount of water from wetland recharging deep 
!!                               |aquifer on current day in HRU
!!    qw_q_pnd(:) |mm H2O        |groundwater contribution to streamflow from
!!                               |wetland seepage on current day
!!    rchrg_pnd(:)|mm H2O        |amount of water recharging both aquifers on
!!                               |current day in HRU from wetland
!!    shallst(:)  |mm H2O        |depth of water in shallow aquifer
!!~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    latno3_pnd(:)   |kg N/ha   |daily loading to reach from NO3-N entering 
!!                               |pond via lateral flow and exiting
!!                               |via seepage gw return flow
!!    minpgw_pnd(:)   |kg P/ha   |daily loading to reach from soluble P entering 
!!                               |pond via gw return flow and exiting
!!                               |via seepage gw return flow
!!    no3gw_pnd(:)    |kg N/ha   |daily loading to reach from NO3-N entering
!!                               |pond via gw return flow and exiting
!!                               |via seepage gw return flow
!!    qdr_pnd(:)      |mm H2O    |daily water entering pond as surface/lat/gw flow  
!!                               |and exiting as surface/gw return flow
!!    sedminpa_pnd(:) |kg P/ha   |daily loading to reach from active mineral P
!!                               |sorbed to sed. entering pond via surface runoff
!!                               |and exiting via surface flow
!!    sedminps_pnd(:) |kg P/ha   |daily loading to reach from stable mineral P
!!                               |sorbed to sed. entering pond via surface runoff
!!                               |and exiting via surface flow
!!    sedorgn_pnd(:)  |kg N/ha   |daily loading to reach from organic N
!!                               |entering pond via surface runoff and 
!!                               |exiting via surface flow
!!    sedorgp_pnd(:)  |kg P/ha   |daily loading to reach from organic P
!!                               |entering pond via surface runoff and 
!!                               |exiting via surface flow
!!    sedyld_pnd(:)   |met tons  |daily loading to reach from eroded sediment
!!                               |entering pond via surface runoff and 
!!                               |exiting via surface flow
!!    surqno3_pnd(:)  |kg N/ha   |daily loading to reach from NO3-N
!!                               |entering pond via surface runoff and 
!!                               |exiting via surface flow
!!    surqsolp_pnd(:) |kg P/ha   |daily loading to reach from soluble P
!!                               |entering pond via surface runoff and 
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

      integer :: j
      real :: cnv, pndsa, xx, yy
      !! Almendinger/Ulrich: new vars
      real :: zz, totno3_conc      
      totno3_conc = 0
      
      j = 0
      j = ihru

      if (pnd_fr(j) > 0.01) then
        cnv = 0.
        cnv = hru_ha(j) * 10.

        !! calculate area of HRU covered by pond
        pndsa = 0.
        pndsa = bp1(j) * pnd_vol(j) ** bp2(j)

        !! calculate water flowing into pond for day                
        !!Almendinger/Ulrich: pnd_fr() now accounted for in subbasin.f calc of qdr_pnd()
        !!    But here we need to regenerate qdr() to match logic of old code, 
        !!    plus include the area of the pond itself:
        pndflwi = (qdr_pnd(j)/pnd_fr(j)) * 10. * (hru_ha(j) * pnd_fr(j) &
     &                                                         - pndsa)
        !!OLD CODE
        !!pndflwi = qdr_pnd(j) * 10. * hru_ha(j) * pnd_fr(j)
        
        !! Almendinger/Ulrich
        !! Re-initialize qdr_pnd(), which will be adjusted for gains & losses
        !!   before being passed back to subbasin.f
        qdr_pnd(j) = 0. 
        
        !! OLD CODE not needed: replaced by qdr_pnd
        !!qdr(j) = qdr(j) - qdr(j) * pnd_fr(j)
        
        !! calculate sediment loading to pond for day
        pndsedin = sedyld_pnd(j) * (1 - (pndsa / hru_ha(j)))
        
        pndsanin = sanyld_pnd(j) * (1 - (pndsa / hru_ha(j)))
        pndsilin = silyld_pnd(j) * (1 - (pndsa / hru_ha(j)))
        pndclain = clayld_pnd(j) * (1 - (pndsa / hru_ha(j)))
        pndsagin = sagyld_pnd(j) * (1 - (pndsa / hru_ha(j)))
        pndlagin = lagyld_pnd(j) * (1 - (pndsa / hru_ha(j)))
           
        !! OLD CODE: wet_fr() now accounted for in subbasin.f calc  
!        pndsedin = sedyld(j) * (pnd_fr(j) - pndsa / hru_ha(j))
!        pndsanin = sanyld(j) * (pnd_fr(j) - pndsa / hru_ha(j))
!        pndsilin = silyld(j) * (pnd_fr(j) - pndsa / hru_ha(j))
!        pndclain = clayld(j) * (pnd_fr(j) - pndsa / hru_ha(j))
!        pndsagin = sagyld(j) * (pnd_fr(j) - pndsa / hru_ha(j))
!        pndlagin = lagyld(j) * (pnd_fr(j) - pndsa / hru_ha(j))
        
        !! Almendinger/Ulrich NEW var name
        sedyld_pnd(j) = 0.
        sanyld_pnd(j) = 0.
        silyld_pnd(j) = 0.
        clayld_pnd(j) = 0.
        sagyld_pnd(j) = 0.
        lagyld_pnd(j) = 0.

        !OLD CODE -- not needed
!        sedyld(j) = sedyld(j) - sedyld(j) * pnd_fr(j)
!        sanyld(j) = sanyld(j) - sanyld(j) * pnd_fr(j)
!        silyld(j) = silyld(j) - silyld(j) * pnd_fr(j)
!        clayld(j) = clayld(j) - clayld(j) * pnd_fr(j)
!        sagyld(j) = sagyld(j) - sagyld(j) * pnd_fr(j)
!        lagyld(j) = lagyld(j) - lagyld(j) * pnd_fr(j)

        !! compute nitrogen and phosphorus levels in pond at beginning
        !! of day: equation 29.1.1 in SWAT manual
        if (pnd_solp(j) < 1.e-6) pnd_solp(j) = 0.0
        if (pnd_psed(j) < 1.e-6) pnd_psed(j) = 0.0
        if (pnd_orgp(j) < 1.e-6) pnd_orgp(j) = 0.0
        if (pnd_solpg(j) < 1.e-6) pnd_solpg(j) = 0.0
        if (pnd_orgn(j) < 1.e-6) pnd_orgn(j) = 0.0
        if (pnd_no3(j) < 1.e-6) pnd_no3(j) = 0.0
        if (pnd_no3s(j) < 1.e-6) pnd_no3s(j) = 0.0
        if (pnd_no3g(j) < 1.e-6) pnd_no3g(j) = 0.0
        
        xx = 0.
        !! Calculate area over which to apply sed and nutrient yields from HRU
        !! Almendinger/Ulrich: pnd_fr() already accounted for in subbasin.f calc of xx  
        xx = hru_ha(j) - pndsa 
        !! OLD CODE
        !xx = pnd_fr(j) * hru_ha(j)
        
        pnd_solp(j) = pnd_solp(j) + (surqsolp_pnd(j) + sedminpa_pnd(j)) &
     &                                                              * xx
        pnd_psed(j) = pnd_psed(j) + sedminps_pnd(j) * xx
        pnd_orgp(j) = pnd_orgp(j) + sedorgp_pnd(j) * xx
        pnd_solpg(j) = pnd_solpg(j) + minpgw_pnd(j) * xx
        pnd_orgn(j) = pnd_orgn(j) + sedorgn_pnd(j) * xx
        pnd_no3(j) = pnd_no3(j) + surqno3_pnd(j) * xx
        pnd_no3s(j) = pnd_no3s(j) + latno3_pnd(j) * xx
        pnd_no3g(j) = pnd_no3g(j) + no3gw_pnd(j) * xx

        !! Almendinger/Ulrich: zero-out _pnd vars
        sedorgn_pnd(j) = 0.
        surqno3_pnd(j) = 0.
        latno3_pnd(j) = 0.
        no3gw_pnd(j) = 0.
!        sedorgp_pnd(j) = 0.
        sedminps_pnd(j) = 0.
        sedminpa_pnd(j) = 0.
        surqsolp_pnd(j) = 0.
        minpgw_pnd(j) = 0.
        
        !! Almendinger/Ulrich: commented OLD CODE: pond and wetlands now executed in parallel rather than series  
        !! compute amount of nutrients not passing through ponds
!        xx = 0.
!        xx = 1. - pnd_fr(j)
!        sedorgn(j) = sedorgn(j) * xx
!        surqno3(j) = surqno3(j) * xx
!        latno3(j) = latno3(j) * xx
!        no3gw(j) = no3gw(j) * xx
!        sedorgp(j) = sedorgp(j) * xx
!        sedminpa(j) = sedminpa(j) * xx
!        sedminps(j) = sedminps(j) * xx
!        surqsolp(j) = surqsolp(j) * xx
!        minpgw(j) = minpgw(j) * xx

        !! Almendinger/Ulrich: add 2nd var to pond() subroutine: totno3_conc = return value to be used below
        call pond(j, totno3_conc)

        !! compute water leaving pond via surface outlet
        qdr_pnd(j) = pndflwo / cnv
        !! OLD CODE: qdr replaced by qdr_pnd here
        !!qdr(j) = qdr(j) + pndflwo / cnv

        !! compute sediment leaving pond
        sedyld_pnd(j) = pndsedo
        sanyld_pnd(j) = pndsano
        silyld_pnd(j) = pndsilo
        clayld_pnd(j) = pndclao
        sagyld_pnd(j) = pndsago
        lagyld_pnd(j) = pndlago        
        
        !! OLD CODE: sedyld replaced by sedyld_pnd here
!        sedyld(j) = sedyld(j) + pndsedo
!        sanyld(j) = sanyld(j) + pndsano
!        silyld(j) = silyld(j) + pndsilo
!        clayld(j) = clayld(j) + pndclao
!        sagyld(j) = sagyld(j) + pndsago
!        lagyld(j) = lagyld(j) + pndlago

        !! compute nutrients leaving pond
        !! fraction leaving in outflow
        yy = 0.
        if ((pnd_vol(j) + pndflwo) > 0.) then
          yy = pndflwo / (pnd_vol(j) + pndflwo)
        else
          yy = 0.
        endif
        !! Almendinger/Ulrich - fraction leaving in outflow AND seepage
        zz = 0.
        if ((pnd_vol(j) + pndflwo + pndsep) > 0.) then
          zz = (pndflwo + pndsep) / (pnd_vol(j) + pndflwo + pndsep)
        else
          zz = 0.
        endif

        !! Quantities leaving in outflow
        !! Almendinger/Ulrich: var name change _pnd
        !! Quantities leaving in surface outflow, going directly 
        !!   to the reach, as yields (kg/ha)
        !!  Note yields here are apportioned over entire HRU area (hru_ha(j)),
        !!    rather than just the land portion (hru_ha(j) - wetsa)
        sedorgn_pnd(j) = pnd_orgn(j) * yy / hru_ha(j)
        surqno3_pnd(j) = pnd_no3(j) * yy / hru_ha(j)
        latno3_pnd(j) = pnd_no3s(j) * yy / hru_ha(j)
        no3gw_pnd(j) = pnd_no3g(j) * yy / hru_ha(j)
        sedorgp_pnd(j) = pnd_orgp(j) * yy / hru_ha(j)
        sedminps_pnd(j) = pnd_psed(j) * yy / hru_ha(j)
        surqsolp_pnd(j) = pnd_solp(j) * yy / hru_ha(j)
        minpgw_pnd(j) = pnd_solpg(j) * yy / hru_ha(j)

        !! COMMENTED -- not needed
  !          sedorgn(j) = sedorgn(j) + pnd_orgn(j) * yy / hru_ha(j)
  !          surqno3(j) = surqno3(j) + pnd_no3(j) * yy / hru_ha(j)
  !          latno3(j) = latno3(j) + pnd_no3s(j) * yy / hru_ha(j)
  !          no3gw(j) = no3gw(j) + pnd_no3g(j) * yy / hru_ha(j)
  !          sedorgp(j) = sedorgp(j) + pnd_orgp(j) * yy / hru_ha(j)
  !          sedminps(j) = sedminps(j) + pnd_psed(j) * yy / hru_ha(j)
  !          surqsolp(j) = surqsolp(j) + pnd_solp(j) * yy / hru_ha(j)
  !          minpgw(j) = minpgw(j) + pnd_solpg(j) * yy / hru_ha(j)

        !!update nutrient pools in ponds
        !! For particulates, that leave only in outflow
        pnd_orgn(j) = pnd_orgn(j) * (1. - yy)
        pnd_orgp(j) = pnd_orgp(j) * (1. - yy)
        pnd_psed(j) = pnd_psed(j) * (1. - yy)
        pnd_chla(j) = pnd_chla(j) * (1. - yy)
        !! Almendinger/Ulrich:
        !! For soluble fractions, that leave in outflow AND seepage 
        pnd_no3(j) = pnd_no3(j) * (1. - zz)
        pnd_no3g(j) = pnd_no3g(j) * (1. - zz)
        pnd_no3s(j) = pnd_no3s(j) * (1. - zz)          
        pnd_solp(j) = pnd_solp(j) * (1. - zz)
        pnd_solpg(j) = pnd_solpg(j) * (1. - zz)
        
        !! OLD CODE -- for when seepage wasn't considered        
  !          pnd_no3(j) = pnd_no3(j) * (1. - yy)
  !          pnd_no3s(j) = pnd_no3s(j) * (1. - yy)
  !          pnd_no3g(j) = pnd_no3g(j) * (1. - yy)
  !          pnd_solp(j) = pnd_solp(j) * (1. - yy)
  !          pnd_solpg(j) = pnd_solpg(j) * (1. - yy) 

        
        !! compute seepage depth for HRU water balance
        twlpnd = pndsep / cnv
        
        ! **************************************************************************************************
        !! Almendinger/Ulrich: compute groundwater recharge and return flow from bottom seepage
        !! Code is based on gwmod.f: same form as hrupond.f and routres.f        
        
        !! Set recharge = seepage: assume 100% of seepage immediately becomes recharge 
        rchrg_pnd(j) = twlpnd

        if (rchrg_pnd(j) < 1.e-6) rchrg_pnd(j) = 0.

        !! compute deep aquifer level for day, adding component from ponds and wetlands to that calc'd prev in gwmod.f
        gwseep_pnd = rchrg_pnd(j) * rchrg_dp(j)
        !! add pond deep seepage to that from HRU
        gwseep = gwseep + gwseep_pnd
        deepst(j) = deepst(j) + gwseep_pnd
        
        !! adjust shallow aquifer storage
        shallst(j) = shallst(j) + (rchrg_pnd(j) - gwseep_pnd)
                             
        !! compute groundwater contribution to streamflow for day and 
        !!   adjust shallow storage
        if (shallst(j) > gwqmn(j)) then
            gw_q_pnd(j) = gw_q_pnd(j) * alpha_bfe(j) + (rchrg_pnd(j) -  &
     &                              gwseep_pnd ) * (1. - alpha_bfe(j))
            shallst(j) = shallst(j) - gw_q_pnd(j)
            if (shallst(j) < gwqmn(j)) then
              gw_q_pnd(j) = gw_q_pnd(j) - (gwqmn(j) - shallst(j))
              shallst(j) = gwqmn(j)
            end if
        else
            gw_q_pnd(j) = 0.
        end if
        
        !! Almendinger/Ulrich -- calc sol-P out of impoundment via seepage
        !!   Here, simply assumed to = ambient aq conc: gwminp(j)
        
        !! Almendinger/Ulrich -- calc sol-P contribution to reach from groundwater, as --
        !!   -- sol-P originally reaching wetland from incoming gw flow leaving as surface outflow, plus
        !!   -- sol-P as groundwater originating as seepage from wetland at ambient HRU gw P concentration
        !! all calculated as yield (kg/ha) over entire HRU.
        !!   Note 1/100 factor to convert (mg/L * mm) to (kg/ha)
        minpgw_pnd(j) = minpgw_pnd(j) + gwminp(j) * gw_q_pnd(j) / 100
        
        !! Almendinger/Ulrich -- estimate gw delivery of NO3 to reach as -- 
        !!   -- NO3gw originally reaching wetland from incoming gw flow leaving as surface outflow, plus
        !!   -- NO3gw as groundwater originating as seepage from wetland at NO3 concentration in wetland at end of day
        !! all calculated as yield (kg/ha) over entire HRU.
        !!   Note 10 factor to convert (kg/m^3 * mm) to (kg/ha)
        no3gw_pnd(j) = no3gw_pnd(j) + totno3_conc * gw_q_pnd(j)* 10
              
        !! compute final adjusted HRU yield, add GW return flow from pond seepage
        qdr_pnd(j) = qdr_pnd(j) + gw_q_pnd(j)
            
        !! END Almendinger/Ulrich NEW CODE
        !! *******************************************************************************************  

        !! Almendinger/Ulrich  -- moved above 
        !! add pond seepage to shallow aquifer convert from m^3 to mm
        !!shallst(j) = shallst(j) + pndsep / cnv
        
        !! Almendinger/Ulrich  -- moved above
        !! compute seepage depth for HRU water balance
        !!twlpnd = pndsep / cnv

      end if
      
      !! Almendinger/Ulrich -- var names changes: "_pnd" 
      if (qdr_pnd(j) < 0.) qdr_pnd(j) = 0.
      if (sedyld_pnd(j) < 0.) then
        sedyld_pnd(j) = 0.0
        sanyld_pnd(j) = 0.0
        silyld_pnd(j) = 0.0
        clayld_pnd(j) = 0.0
        sagyld_pnd(j) = 0.0
        lagyld_pnd(j) = 0.0
	end if

      return
      end
