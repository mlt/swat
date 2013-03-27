cmake_minimum_required(VERSION 2.8)

set( SWAT_SRCS
  NCsed_leach.f90
  addh.f
  albedo.f
  allocate_parms.f
  alph.f
  anfert.f
  apex_day.f
  apply.f
  ascrv.f
  atri.f
  aunif.f
  autoirr.f
  aveval.f
  bacteria.f
  biofilm.f
  biozone.f
  bmp_det_pond.f
  bmp_ri_pond.f
  bmp_sand_filter.f
  bmp_sed_pond.f
  bmp_wet_pond.f
  bmpfixed.f
  bmpinit.f
  buffer.f
  burnop.f
  canopyint.f
  caps.f
  carbon_new.f
  carbon_zhang2.f90
  cfactor.f
  chkcst.f
  clgen.f
  clicon.f
  command.f
  conapply.f
  confert.f
  crackflow.f
  crackvol.f
  curno.f
  dailycn.f
  decay.f
  depstor.f
  distrib_bmps.f
  dormant.f
  drains.f
  dstn1.f
  ee.f
  eiusle.f
  enrsb.f
  estimate_ksat.f
  etact.f
  etpot.f
  expo.f
  fert.f
  filter.f
  filtw.f
  finalbal.f
  gcycl.f
  getallo.f
  grass_wway.f
  graze.f
  grow.f
  gw_no3.f
  gwmod.f
  gwmod_deep.f
  gwnutr.f
  h2omgt_init.f
  harvestop.f
  harvgrainop.f
  harvkillop.f
  header.f
  headout.f
  hhnoqual.f
  hhwatqual.f
  hmeas.f
  hruaa.f
  hruallo.f
  hruday.f90
  hrumon.f
  hrupond.f
  hrupondhr.f
  hruyr.f
  hydroinit.f
  icl.f
  impnd_init.f
  impndaa.f
  impndday.f
  impndmon.f
  impndyr.f
  irr_rch.f
  irr_res.f
  irrigate.f
  irrsub.f
  jdt.f
  killop.f
  lakeq.f
  latsed.f
  layersplit.f
  lwqdef.f
  main.f
  modparm.f
  ndenit.f
  newtillmix.f
  nfix.f
  nitvol.f
  nlch.f
  nminrl.f
  noqual.f
  npup.f
  nrain.f
  nup.f
  nuts.f
  openwth.f
  operatn.f
  orgn.f
  orgncswat.f
  origtile.f
  ovr_sed.f
  percmacro.f
  percmain.f
  percmicro.f
  pestlch.f
  pestw.f
  pesty.f
  pgen.f
  pgenhr.f
  pkq.f
  plantmod.f
  plantop.f
  pmeas.f
  pminrl.f
  pminrl2.f
  pond.f
  pondhr.f
  pothole.f
  potholehr.f
  print_hyd.f
  psed.f
  qman.f
  rchaa.f
  rchday.f
  rchinit.f
  rchmon.f
  rchuse.f
  rchyr.f
  readatmodep.f
  readbsn.f
  readchm.f
  readcnst.f
  readfcst.f
  readfert.f
  readfig.f
  readfile.f
  readgw.f
  readhru.f
  readinpt.f
  readlup.f
  readlwq.f
  readmgt.f
  readmon.f
  readops.f
  readpest.f
  readplant.f
  readpnd.f
  readres.f
  readrte.f
  readru.f
  readsdr.f
  readsepticbz.f
  readseptwq.f
  readsno.f
  readsol.f
  readsub.f
  readswq.f
  readtill.f
  readurban.f
  readwgn.f
  readwus.f
  readwwq.f
  readyr.f
  reccnst.f
  recday.f
  rechour.f
  recmon.f
  recyear.f
  regres.f
  res.f
  resbact.f
  resetlu.f
  reshr.f
  resinit.f
  resnut.f
  rewind_init.f
  rhgen.f
  rootfr.f
  route.f
  routels.f
  routeunit.f
  routres.f
  rsedaa.f
  rseday.f
  rsedmon.f
  rsedyr.f
  rtbact.f
  rtday.f
  rteinit.f
  rthmusk.f
  rthpest.f
  rthr.f
  rthsed.f
  rtmusk.f
  rtout.f
  rtpest.f
  rtsed.f
  rtsed_Molinas_Wu.f
  rtsed_bagnold.f
  rtsed_kodatie.f
  rtsed_yangsand.f
  sat_excess.f
  save.f
  saveconc.f
  sched_mgt.f
  schedule_ops.f
  sim_initday.f
  sim_inityr.f
  simulate.f
  slrgen.f
  smeas.f
  snom.f
  soil_chem.f
  soil_par.f
  soil_phys.f
  soil_write.f
  solp.f
  solt.f
  std1.f
  std2.f
  std3.f
  stdaa.f
  storeinitial.f
  structure.f
  sub_subbasin.f
  subaa.f
  subbasin.f
  subday.f
  submon.f
  substor.f
  subwq.f
  subyr.f
  sumhyd.f
  sumv.f
  surface.f
  surfst_h2o.f
  surfstor.f
  surq_daycn.f
  surq_greenampt.f
  swbl.f
  sweep.f
  swu.f
  tair.f
  tgen.f
  theta.f
  tillfactor.f
  tillmix.f
  tmeas.f
  tran.f
  transfer.f
  tstr.f
  ttcoef.f
  ttcoef_wway.f
  urb_bmp.f
  urban.f
  urbanhr.f
  varinit.f
  vbl.f
  virtual.f
  volq.f
  washp.f
  watbal.f
  water_hru.f
  watqual.f
  watqual2.f
  wattable.f
  watuse.f
  weatgn.f
  wetlan.f
  wmeas.f
  wndgen.f
  writea.f
  writeaa.f
  writed.f
  writem.f
  xmon.f
  ysed.f
  zero0.f
  zero1.f
  zero2.f
  zero_urbn.f
  zeroini.f
)

# -e80 for ifort???
set_source_files_properties(
  bmp_det_pond.f
  bmpinit.f
  modparm.f
  ovr_sed.f
  percmain.f
  readbsn.f
  rthsed.f
  PROPERTIES COMPILE_FLAGS -ffixed-line-length-79)

set_source_files_properties(
  carbon_zhang2.f90
  PROPERTIES COMPILE_FLAGS -ffree-line-length-none)

add_executable(swat ${SWAT_SRCS})
set(PATCH ${CPACK_PACKAGE_VERSION_PATCH})
if(CPACK_PACKAGE_VERSION_PATCH MATCHES "^[0-9]")
  set(PATCH ".${PATCH}")
endif()

set_target_properties(swat PROPERTIES
  RELEASE_POSTFIX "${CPACK_PACKAGE_VERSION_MAJOR}Rev${CPACK_PACKAGE_VERSION_MINOR}${PATCH}Rel"
  DEBUG_POSTFIX "${CPACK_PACKAGE_VERSION_MAJOR}Rev${CPACK_PACKAGE_VERSION_MINOR}${PATCH}Dbg"
  PGO_POSTFIX "${CPACK_PACKAGE_VERSION_MAJOR}Rev${CPACK_PACKAGE_VERSION_MINOR}${PATCH}RelPGO"
  IPGO_POSTFIX "${CPACK_PACKAGE_VERSION_MAJOR}Rev${CPACK_PACKAGE_VERSION_MINOR}${PATCH}RelPGO"
)

install(TARGETS swat RUNTIME DESTINATION ".")

# Profile guided optimization

if(CMAKE_BUILD_TYPE MATCHES "PGO")

  if(CMAKE_BUILD_TYPE MATCHES "I")
    set(PROF_DIR "-prof-dir")
    set(PROF_GEN "-prof-gen")
    set(PROF_USE "-prof-use")
  else()			# GCC
    set(PROF_DIR "-fprofile-dir")
    set(PROF_GEN "-fprofile-generate")
    set(PROF_USE "-fprofile-use")
  endif()

  set(EXTRA_LINK_FLAGS "${PROF_DIR}=${CMAKE_BINARY_DIR}/profile")
  set(EXTRA_COMPILE_FLAGS "${EXTRA_LINK_FLAGS}")

  file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/profile)

  add_executable(swat0 ${SWAT_SRCS})
  set_target_properties(swat0 PROPERTIES
    COMPILE_FLAGS "${EXTRA_COMPILE_FLAGS} ${PROF_GEN}"
    LINK_FLAGS "${EXTRA_LINK_FLAGS} ${PROF_GEN}")

  add_custom_target(swat0_run
    COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_BINARY_DIR}/profile/CMakeFiles/swat0.dir
    COMMAND ${CMAKE_CURRENT_BINARY_DIR}/swat0
    COMMAND ${CMAKE_COMMAND} -E remove_directory ${CMAKE_BINARY_DIR}/profile/CMakeFiles/swat.dir
    COMMAND ${CMAKE_COMMAND} -E rename ${CMAKE_BINARY_DIR}/profile/CMakeFiles/swat0.dir ${CMAKE_BINARY_DIR}/profile/CMakeFiles/swat.dir
    DEPENDS swat0
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/TxtInOut)

  set_target_properties(swat PROPERTIES
    COMPILE_FLAGS "${EXTRA_COMPILE_FLAGS} ${PROF_USE}"
    LINK_FLAGS "${EXTRA_LINK_FLAGS} ${PROF_USE}")
  add_dependencies(swat swat0_run)

endif()
