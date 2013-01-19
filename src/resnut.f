      subroutine resnut
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine routes solluble nitrogen and soluble phosphorus through reservoirs

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    chlar(:)    |none          |chlorophyll-a production coefficient for
!!                               |reservoir
!!    inum1       |none          |reservoir number
!!    inum2       |none          |inflow hydrograph storage location number
!!    ires1(:)    |none          |beginning of mid-year nutrient settling
!!                               |"season"
!!    ires2(:)    |none          |end of mid-year nutrient settling "season"
!!    i_mo        |none          |current month of simulation
!!    nsetlr(1,:) |m/day         |nitrogen settling rate for 1st season
!!    nsetlr(2,:) |m/day         |nitrogen settling rate for 2nd season
!!    psetlr(1,:) |m/day         |phosphorus settling rate for 1st season
!!    psetlr(2,:) |m/day         |phosphorus settling rate for 2nd season
!!    res_nh3(:)  |kg N          |amount of ammonia in reservoir
!!    res_no2(:)  |kg N          |amount of nitrite in reservoir
!!    res_no3(:)  |kg N          |amount of nitrate in reservoir
!!    res_orgn(:) |kg N          |amount of organic N in reservoir
!!    res_orgp(:) |kg P          |amount of organic P in reservoir
!!    res_solp(:) |kg P          |amount of soluble P in reservoir
!!    res_vol(:)  |m^3 H2O       |reservoir volume
!!    resflwo     |m^3 H2O       |water leaving reservoir on day
!!    ressep      |m^3 H2O       |water seeping out of reservoir on day
!!    ressa       |ha            |surface area of reservoir on day
!!    seccir(:)   |none          |water clarity coefficient for reservoir
!!    varoute(4,:)|kg N          |organic nitrogen
!!    varoute(5,:)|kg P          |organic posphorus
!!    varoute(6,:)|kg N          |nitrate
!!    varoute(7,:)|kg P          |soluble phosphorus
!!    varoute(14,:)|kg N         |ammonia
!!    varoute(15,:)|kg N         |nitrite
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    res_chla(:) |kg chl-a      |amount of chlorophyll-a in reservoir
!!    res_nh3(:)  |kg N          |amount of ammonia in reservoir
!!    res_no2(:)  |kg N          |amount of nitrite in reservoir
!!    res_no3(:)  |kg N          |amount of nitrate in reservoir
!!    res_orgn(:) |kg N          |amount of organic N in reservoir
!!    res_orgp(:) |kg P          |amount of organic P in reservoir
!!    res_seci(:) |m             |secchi-disk depth
!!    res_solp(:) |kg P          |amount of soluble P in reservior
!!    reschlao    |kg chl-a      |amount of chlorophyll-a leaving reaservoir
!!                               |on day
!!    resnh3o     |kg N          |amount of ammonia leaving reservoir on day
!!    resno2o     |kg N          |amount of nitrite leaving reservoir on day
!!    resno3o     |kg N          |amount of nitrate leaving reservoir on day
!!    resorgno    |kg N          |amount of organic N leaving reservoir on day
!!    resorgpo    |kg P          |amount of organic P leaving reservoir on day
!!    ressolpo    |kg P          |amount of soluble P leaving reservoir on day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    chlaco      |ppb (ug/L)    |chlorophyll-a concentration
!!    iseas       |none          |nutrient settling rate season
!!    jres        |none          |reservior number
!!    nitrok      |none          |fraction of nitrogen in reservoir removed by
!!                               |settling
!!    phosk       |none          |fraction of phosphorus in reservoir removed
!!                               |by settling
!!    tpco        |ppb (ug/L)    |concentration of phosphorus in water
!!                               |on day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!   ************************************************************************ 
!!    Almendinger/Ulrich -- New Variables
!!
!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    gwminp(:)   |mg P/L        |soluble P concentration in groundwater
!!    resgwflwo   |m^3 H2O       |water originally from reservoir as gw return flow for day
!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    res_vol_wq  |m^3 H2O       |reservoir volume for water-quality purposes,
!!                               |= ending volume with surface & groundwater losses added back in
!!    resno3o_gw  |kg N          |mass of NO3 as N lost to gw seepage that day
!!    resno2o_gw  |kg N          |mass of NO2 as N lost to gw seepage that day
!!    resnh3o_gw  |kg N          |mass of NH3 as N lost to gw seepage that day
!!    resorgno_gw |kg N          |mass of orgN as N lost to gw seepage that day
!!    resorgpo_gw |kg P          |mass of orgP as P lost to gw seepage that day
!!    ressolpo_gw |kg P          |mass of solP as P lost to gw seepage that day     
!!    gwminp_sub  |mg P/L        |Subbasin weighted mean of all HRU soluble P 
!!                               |concentrations (gwminp) in return flow
!!    no2_conc    |mg N/L        |NO2 concentration in reservoir and gw
!!    no3_conc    |mg N/L        |NO3 concentration in reservoir and gw
!!    nh3_conc    |mg N/L        |NH3 concentration in reservoir and gw  
!!   Almendinger/Ulrich END NEW VAR's ********************************************************************** 
!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: jres, iseas
      real :: nitrok, phosk, tpco, chlaco
      !! Almendinger/Ulrich: New vars
      real :: res_vol_wq
      real :: resno3o_gw, resno2o_gw, resnh3o_gw, resorgno_gw
      real :: resorgpo_gw, ressolpo_gw 
      real :: gwminp_sub, no2_conc, no3_conc 
      
      jres = 0
      jres = inum1

!!    Almendinger/Ulrich -- new in 2011
!! Reconstitute total res volume that day for water-quality purposes, 
!!   before flow losses that affect nutrient masses
      res_vol_wq = 0.
      res_vol_wq = res_vol(jres) + resflwo + ressep

!! if reservoir volume less than 1 m^3, set all nutrient levels to
!! zero and perform no nutrient calculations
      if (res_vol_wq < 1.) then
        res_orgn(jres) = 0.
        res_orgp(jres) = 0.
        res_no3(jres) = 0.
        res_nh3(jres) = 0.
        res_no2(jres) = 0.
        res_solp(jres) = 0.
        res_chla(jres) = 0.
        res_seci(jres) = 0.
      end if
      if (res_vol_wq < 1.) return

!! if reservoir volume greater than 1 m^3, perform nutrient calculations
      if (i_mo >= ires1(jres) .and. i_mo <= ires2(jres)) then
        iseas = 1
      else
        iseas = 2
      endif

      !! add incoming nutrients to those in reservoir
      !! equation 29.1.1 in SWAT manual
      res_orgn(jres) = res_orgn(jres) + varoute(4,inum2)
      res_orgp(jres) = res_orgp(jres) + varoute(5,inum2)
      res_no3(jres) = res_no3(jres) + varoute(6,inum2)
      res_nh3(jres) = res_nh3(jres) + varoute(14,inum2)
      res_no2(jres) = res_no2(jres) + varoute(15,inum2)
      res_solp(jres) = res_solp(jres) + varoute(7,inum2)

      !! settling rate/mean depth
      !! part of equation 29.1.3 in SWAT manual
      phosk = 0.
      nitrok = 0.
      phosk = psetlr(iseas,jres) * ressa * 10000. / res_vol_wq
      phosk = Min(phosk, 1.)
      nitrok = nsetlr(iseas,jres) * ressa * 10000. / res_vol_wq
      nitrok = Min(nitrok, 1.)

      !! remove nutrients from reservoir by settling
      !! other part of equation 29.1.3 in SWAT manual
      res_solp(jres) = res_solp(jres) * (1. - phosk)
      res_orgp(jres) = res_orgp(jres) * (1. - phosk)
      res_orgn(jres) = res_orgn(jres) * (1. - nitrok)
      res_no3(jres) = res_no3(jres) * (1. - nitrok)
      res_nh3(jres) = res_nh3(jres) * (1. - nitrok)
      res_no2(jres) = res_no2(jres) * (1. - nitrok)

      !! calculate chlorophyll-a and water clarity
      tpco = 0.
      chlaco = 0.
      res_chla(jres) = 0.
      res_seci(jres) = 0.
      tpco = 1.e+6 * (res_solp(jres) + res_orgp(jres)) / res_vol_wq
      if (tpco > 1.e-4) then
        !! equation 29.1.6 in SWAT manual
        chlaco = chlar(jres) * 0.551 * (tpco**0.76)
        res_chla(jres) = chlaco * res_vol_wq * 1.e-6
      endif
      if (chlaco > 1.e-4) then
        !! equation 29.1.8 in SWAT manual
        res_seci(jres) = seccir(jres) * 6.35 * (chlaco**(-0.473))
      endif

      !! calculate amount of nutrients leaving reservoir 
      !!   first zero out any small or negative amounts
      if (res_no3(jres) < 1.e-4) res_no3(jres) = 0.0
      if (res_orgn(jres) < 1.e-4) res_orgn(jres) = 0.0
      if (res_orgp(jres) < 1.e-4) res_orgp(jres) = 0.0
      if (res_solp(jres) < 1.e-4) res_solp(jres) = 0.0
      if (res_chla(jres) < 1.e-4) res_chla(jres) = 0.0
      if (res_nh3(jres) < 1.e-4) res_nh3(jres) = 0.0
      if (res_no2(jres) < 1.e-4) res_no2(jres) = 0.0
      
!!****************************************************************************************************     
!!Almendinger/Ulrich: ALTERED CODE for adjusting nutrient mass remaining in reservoir
 
      !! chem masses out of reservoir via surface flow, 
      !!  as a proportion of total volume, including surface water and groundwater outflow.
      !!  N.B. res_vol() has already been reduced by sw and gr outflows at this point.
      !!  Presumes outflow and seepage occurred at max vol = min concentrations
      !!  Initialize
      resno3o = 0.
      resorgno = 0.
      resorgpo = 0.
      ressolpo = 0.
      reschlao = 0.
      resnh3o = 0.
      resno2o = 0.
      !!  Calculate
      resno3o = res_no3(jres) * resflwo / res_vol_wq
      resorgno = res_orgn(jres) * resflwo / res_vol_wq
      resorgpo = res_orgp(jres) * resflwo / res_vol_wq
      ressolpo = res_solp(jres) * resflwo / res_vol_wq
      reschlao = res_chla(jres) * resflwo / res_vol_wq
      resnh3o = res_nh3(jres) * resflwo / res_vol_wq
      resno2o = res_no2(jres) * resflwo / res_vol_wq
      
      !! chem masses out of reservoir via groundwater flow (seepage), same logic as above
      !!  Initialize
      resno3o_gw = 0.
      resorgno_gw = 0.
      resorgpo_gw = 0.
      ressolpo_gw = 0.
      reschlao_gw = 0. 
      resnh3o_gw = 0.
      resno2o_gw = 0.
      !!  Calculate
      resno3o_gw = res_no3(jres) * ressep / res_vol_wq
!      resorgno_gw = res_orgn(jres) * ressep / res_vol_wq   !Assume orgN is particulate, not lost to groundwater -- commented out for now
!      resorgpo_gw = res_orgp(jres) * ressep / res_vol_wq   !Assume orgP is particulate, not lost to groundwater -- commented out for now
      ressolpo_gw = res_solp(jres) * ressep / res_vol_wq
!      reschlao_gw = res_chla(jres) * ressep / res_vol_wq   !Assume chla is particulate, not lost to groundwater -- commented out for now 
      resnh3o_gw = res_nh3(jres) * ressep / res_vol_wq
      resno2o_gw = res_no2(jres) * ressep / res_vol_wq
 
       !! update chem masses in res.     
      res_orgn(jres) = res_orgn(jres) - resorgno
      res_orgp(jres) = res_orgp(jres) - resorgpo
      res_no3(jres) = res_no3(jres) - resno3o - resno3o_gw
      res_nh3(jres) = res_nh3(jres) - resnh3o - resnh3o_gw
      res_no2(jres) = res_no2(jres) - resno2o - resno2o_gw
      res_solp(jres) = res_solp(jres) - ressolpo - ressolpo_gw
      res_chla(jres) = res_chla(jres) - reschlao


      !! Estimate N and P delivered by seepage water to outflow
      !! Assume N concentration same as that seeping out of reservoir that day
      if (ressep > 0.) then
        no3_conc = 1000 * resno3o_gw / ressep
        no2_conc = 1000 * resno2o_gw / ressep
        nh3_conc = 1000 * resnh3o_gw / ressep
      else
        no3_conc = 0.
        no2_conc = 0.
        nh3_conc = 0.
      endif
      
      !! Assume P concentration is same as set in gw file for component HRUs in subbasin      
      !! calc mean by adding area-weighted gw P conc's for HRU's in subbasin
      gwminp_sub = 0.
      !! loop thru all HRUs to find those associated with the subbasin associated with reservoir
      do j = 1, nhru
        if (hru_sub(j) == res_sub(jres)) then    
            !! calc mean of sol-P conc's from all HRUs in subbasin       
            gwminp_sub = gwminp_sub + gwminp(j) * hru_fr(j)    
        end if    
      end do
                  
      !! Ideally we'd add N and P from seepage to the groundwater inputs to the next Reach
      !!  downstream from the Reservoir.  Instead, here we add the masses simply to the
      !!  outflow from the Reservoir, as though it were part of the surficial outflow: 
      !! Nitrogen:
      resno3o = resno3o + no3_conc * resgwflwo / 1000.
      resno2o = resno2o + no2_conc * resgwflwo / 1000.
      resnh3o = resnh3o + nh3_conc * resgwflwo / 1000.
      !! Phosphorus:
      ressolpo = ressolpo + gwminp_sub * resgwflwo / 1000.
            
!! END Almendinger/Ulrich: NEW CODE      
!!****************************************************************************************************      

      return
      end
