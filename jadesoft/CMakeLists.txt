CMAKE_MINIMUM_REQUIRED(VERSION 3.5.0) #Because of ROOT
PROJECT(JADESOFT Fortran C)
set(VERSION 2022.07)
set(JADESOFT_VERSION_MAJOR 2022)
set(JADESOFT_VERSION_MINOR 07)
set(JADESOFT_VERSION_PATCH 1)
set(PROJECT_VERSION ${JADESOFT_VERSION_MAJOR}.${JADESOFT_VERSION_MINOR}.${JADESOFT_VERSION_PATCH})
include("GNUInstallDirs")
set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/../cmake/Modules/")
set(CMAKE_VERBOSE_MAKEFILE OFF)
########################################################################
option(JADE_OLD_MC                     "Compile old MC executables" ON)
option(JADE_FPE                        "Trap floating point exceptions" OFF)
option(JADE_USE_CERNLIB                "Attempt to use CERNLIB instead of buildin code (picocernlib)"     ON)
option(JADE_FORCE_32                   "Force build of 32 bit libraries." OFF)
if (JADE_FORCE_32)
  list(PREPEND CMAKE_SYSTEM_LIBRARY_PATH "/usr/lib")
  message(STATUS "${CMAKE_SYSTEM_LIBRARY_PATH}")
  set( FIND_LIBRARY_USE_LIB32_PATHS  ON)
  set( FIND_LIBRARY_USE_LIB64_PATHS  OFF)
  set(CMAKE_SIZEOF_VOID_P 4)
  set(LIB_SUFFIX )
endif()
########################################################################
include(JADE_compiler_setup)
set(CMAKE_POSITION_INDEPENDENT_CODE FALSE)
########################################################################
find_package(BLAS REQUIRED)
message(STATUS "JADE: BLAS_LIBRARIES=${BLAS_LIBRARIES}")
find_package(LAPACK REQUIRED)
message(STATUS "JADE: LAPACK_LIBRARIES=${LAPACK_LIBRARIES}")
########################################################################
find_package(X11  REQUIRED)
message(STATUS "JADE: X11_LIBRARIES=${X11_LIBRARIES}")
message(STATUS "JADE: X11_xcb_LIB=${X11_xcb_LIB}")
message(STATUS "JADE: X11_Xau_LIB=${X11_Xau_LIB}")
message(STATUS "JADE: X11_Xdmcp_LIB=${X11_Xdmcp_LIB}")
########################################################################
if (JADE_USE_CERNLIB)
  find_package(CERNLIB  REQUIRED)
  message(STATUS "JADE: CERNLIB_KERNLIB_STATIC=${CERNLIB_KERNLIB_STATIC}")
  message(STATUS "JADE: CERNLIB_PACKLIB_STATIC=${CERNLIB_PACKLIB_STATIC}")
  message(STATUS "JADE: CERNLIB_MATHLIB_STATIC=${CERNLIB_MATHLIB_STATIC}")
  if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
    set(Motif_DIR /usr/local/Cellar/openmotif/2.3.8_1)
  endif()
  find_package(Motif REQUIRED)
  message(STATUS "JADE: MOTIF_LIBRARIES=${MOTIF_LIBRARIES}")
  set(PICOCERNLIB "${MOTIF_LIBRARIES};${CERNLIB_PACKLIB_STATIC};${CERNLIB_KERNLIB_STATIC};${CERNLIB_MATHLIB_STATIC};${CERNLIB_GRAFLIB_STATIC};${CERNLIB_GRAFX11_STATIC}")
else()
  include(JADE_ROOT_setup)
  find_library(LIBPICOKERNLIB REQUIRED NAMES libpicokernlib.a  PATHS ${PICOCERNLIBPREFIX}/lib ${PICOCERNLIBPREFIX}/lib64 NO_DEFAULT_PATH)
  find_library(LIBPICOGRAFLIB REQUIRED NAMES libpicograflib.a  PATHS ${PICOCERNLIBPREFIX}/lib ${PICOCERNLIBPREFIX}/lib64 NO_DEFAULT_PATH)
  set(PICOCERNLIB ${LIBPICOKERNLIB} ${LIBPICOGRAFLIB} ${ROOT_LIBRARIES})
endif()
set(JEXTERN_LIBRARIES  ${PICOCERNLIB} )
message(STATUS "JADE: JEXTERN_LIBRARIES    : ${JEXTERN_LIBRARIES}")
########################################################################
set(MCJADEEXE    mcjade)
set(SUPERVEXE    superv)
set(JADEZEXE     jadez)
set(JADEZROOTEXE jadezroot)
set(ZE4VEXE      ze4v)

# Define the library name
set(BOSLIB          boslib)
set(GRAFIX          grafix)
set(INTERFACE       interface)
set(JADEGS          jadegs)
set(JADEMUS         jademus)
set(JADESR         jadesr)
set(JADE56             jade56)
set(JMC             jmc)
set(PATRECSR        patrecsr)
set(SOURCE          source)
set(TAGG            tagg)
set(TOFLIB          toflib)
set(VERTEX          vertex)
set(WERTEX          wertex)
set(ZLIB            zlib)

# Define some directories
set(SRC ${CMAKE_SOURCE_DIR}/src2016)
set(UTIL ${CMAKE_SOURCE_DIR}/util2016)
set(SRCMAIN ${CMAKE_SOURCE_DIR}/main2016)
set(LIB ${CMAKE_SOURCE_DIR}/lib)
set(BIN ${CMAKE_SOURCE_DIR}/bin)

include_directories( ${SRC}/boslib 
                     ${SRC}/grafix 
                     ${SRC}/interface 
                     ${SRC}/jadegs 
                     ${SRC}/jademus 
                     ${SRC}/jadesr 
                     ${SRC}/jmc 
                     ${SRC}/patrecsr 
                     ${SRC}/source 
                     ${SRC}/tagg  
                     ${SRC}/toflib 
                     ${SRC}/vertex 
                     ${SRC}/wertex 
                     ${SRC}/zlib  )

set(SRCMCJADE ${SRCMAIN}/mcjade)
set(SRCSUPERV ${SRCMAIN}/superv)
set(SRCJADEZ ${SRCMAIN}/jadez)
set(SRCZE4V ${SRCMAIN}/ze4v)

set(SRCBOSLIB ${SRC}/boslib)
set(SRCGRAFIX          ${SRC}/grafix)
set(SRCINTERFACE         ${SRC}/interface)
set(SRCJADEGS         ${SRC}/jadegs)
set(SRCJADEMUS         ${SRC}/jademus)
set(SRCJMC         ${SRC}/jmc)
set(SRCPATRECSR         ${SRC}/patrecsr)
set(SRCSOURCE         ${SRC}/source)
set(SRCTAGG         ${SRC}/tagg)
set(SRCTOFLIB         ${SRC}/toflib)
set(SRCVERTEX         ${SRC}/vertex)
set(SRCWERTEX         ${SRC}/wertex)
set(SRCZLIB         ${SRC}/zlib)
########################################################################
set(BOSLIB_src
         asrd.f bchl.F bchm.F bcre.F bdar.F bdlg.F bdlm.F bdls.F bdmp.F bfmt.for bfrd.for
         bgac.for bgar.F binlda.F binp.F bint.F bloada.F blocda.F bloc.F bmlt.F
         bnxt.F boutp.F bpasda.F bpos.F brds.F breadc.for bread.for brnm.F
         bsaw.F bseq.F bsls.F bsta.F bwrite.for bwrs.F cchl.F ccor.for
         ccre.F cloc.F cnxt.F cread.for dada.F defst.for histpr.f iaf.for
         ibln.F indar.F itoda.for msgpr.for pall.f pcond.for pcorr.for phist.for
         poda.F pstor.for ptabl.for ptext1.f pvert.for rdda.F readhl.for
         sort4.for ucorr.for uhist.for uwp.for vall.for  vecsub.f
       #  rdvar.for 
)
if (${CMAKE_Fortran_COMPILER_ID}  MATCHES  "XL")
  file(GLOB MyFiles ${SRCBOSLIB}/*.for)
  set_property(SOURCE ${MyFiles} PROPERTY COMPILE_FLAGS -qsuffix=f=for)
endif()
list(TRANSFORM BOSLIB_src PREPEND "${SRCBOSLIB}/")
ADD_LIBRARY(${BOSLIB} STATIC ${BOSLIB_src})
if (${CMAKE_Fortran_COMPILER_ID}  MATCHES  "GNU")
 target_compile_options(${BOSLIB} PRIVATE -Wno-surprising)
endif()



########################################################################
set(JADE56_src 
amscat.f    bound2.f    cerfunh.f   convef.f    enels.f     fnpecr6.f   
#initja.f
#   jeosum3.f
     lgcr56y.f   
lgecorzt.f  lscctl40.f  mcin56mx.f  mctagmge.f  npecero.f   ptcprt.f    shmax.f     trkgmv.f
avabs5.f    bound.f     cergnr6.f   curfit5.f   ff.f        functn.f    intshw9.f  
#jeosum4.f  
lgecorwt.f  
lgmc560.f   lscctl.f    mcin56my.f  mctagmpl.f  npecr6.f    ranazi.f    statagpl.f  usunigam.f
avabs6.f    boundj.f    cernkv55.f  dendel.f    fnpecer8.f  geantdum.f  intshw.f   
#lgclvar.f
  lgecorx.f   
lgmc56.f    lscctlnl.f  
#mcjade.f
    mctagrpl.f  pairc1.f    sekbn1.f    sthead.f    wrtmcb.f
bhabha.f    brems1.f    cernkv66.f  enelel.f    fnpecer9.f  guout.f     iprdiv.f   
#lgclwar.f  
lgecory.f   
lgst56.f    m0ller.f    
#mcjadege.f  
newdir.f    pairc2.f    setect.f    tjock.f
bk7abs.f    brems2.f    cernkv67.f  enelmx.f    fnpecer.f   gustep.f    
#jeosum1.f  
#lgclxar.f  
lgecoryt.f  
lscctl0.f   
#mass2g.f    
#mcjadepl.f  
npecer5.f   parset.f    setscv.f    tjocki.f
bondec.f    brvect.f    compt.f     enelpo.f    fnpecero.f  inital.f    
#jeosum2.f
  lgcr56.f   lgecorz.f   
lscctl20.f  mcin56.f    mctagepl.f  npecer.f    parst2.f    sf5fun.f    trkgam.f
)
list(TRANSFORM JADE56_src PREPEND "${CMAKE_SOURCE_DIR}/src2016/jade56/")
ADD_LIBRARY(${JADE56} STATIC ${JADE56_src})

########################################################################


########################################################################
set(JADESR_src 
addon.F     
#cutsr1.for 
 evconv1.F   evt2.f    kaladdfx.f  kalrev.f  lgbrps.F  lgct82.F  lgkosm.F  reshuf.f   tagf82.f   trgchk.F   usr1ana.F   usreptil.F
breadkw.f   dbasedum.f  evconv.F    evt.f     kalcop.f    korder.f  lgcctk.F  lgcut.F   lgnbck.f  stat820.F  tagflg.F   uscopy.F   
#usred82.F   
xxtest.f
bwritekw.f  difflb.f    evfoldkw.F  gbook.f   kalib.f     lganak.F  lgclbk.F  lgdcpk.F  mewt3.F   stat82.F   trck82.F   userspi.F  
#usreduc1.F
  zvtrck.F
copylb.F    distri.f    evpk.f      kaladd.f  kalm1s.f    lgbrcs.F  lgclsk.F  lgeigk.f  r1mode.F  supvlb.F   trck82x.F  #userx.F
    usrepat.F
)
list(TRANSFORM JADESR_src PREPEND "${CMAKE_SOURCE_DIR}/src2016/jadesr/")
ADD_LIBRARY(${JADESR} STATIC ${JADESR_src})

########################################################################



########################################################################
set(GRAFIX_src axshow.F  bankls.F bankpr.F barsec.f blnumb.F blshow.F calloc.F capmrk.F cdst.F   cirdis.F  
               clrcon.f  coment.f comwrd.F convrt.f credit.F cricro.f croax.F  ctdist.F decide.f dedxds.F 
               dedxv1.F  dedxvw.F defcmd.F defmac.f defwrd.f delmac.f diaghn.f diagin.f disaxi.f disply.F  
               dispvc.F  dispzc.F dnum.f   dracap.f dramov.f drawbp.F drawer.F drawfd.F drawid.F drawlg.F 
               drawmu.f  drawpi.f drawvc.F drawzc.F drftim.f drhatc.F drvcrd.F drwhit.F dsfadc.F dsvac.F 
               dthcv1.f  ecapcy.F ecap.F   ecaps.F  edimac.f edvp.F   efmass.F ellips.f endmes.F evdisp.F 
               evtax.F   expmac.f fadcax.F faddrw.f fampds.F fampft.F ficoor.F ficoov.F finabr.f fincmd.f 
               finwrd.f  ftdlho.F fw83.F   fwc83.F  fwcap.F  fwchev.F fwdisp.F fwmucn.F fwmuht.F fwnew.F fyrint.f
               gamdis.F  gmtext.F gvtxft.F heldis.F helpli.f helpus.f hemsym.f hitdsp.F hitmrk.F hitpnt.F 
               hitscl.f  hlpmac.f hodrfi.F idfun.f  j68kds.F jadarg.f jadcmd.f jadisp.F joys.F   komman.F 
               koord.F   lgclst.F lindis.F locmac.f mcgjet.f menu.f   more.F   muldsp.F mur2ds.F n50sds.F 
               newcal.F  newcap.F news.f   notice.f numbwr.F pardis.F pbgcyl.F pbgrfi.F pbgrz.F  pbgsum.F
               phnumb.F  pick.F   plygon.f pratof.F predit.F prj68k.f prmprs.F prn50s.F profil.f projec.F
               projvw.F  prompt.f prtpev.F prtptr.F prtpvx.F przvtx.F qplots.F ralloc.F realnm.f rectan.f
               renmac.f  resmac.f retten.F rsdisp.F rutrck.F rzdraw.F rzhits.F rzroll.F rztrck.F saege.F 
               scannr.F  sepcmd.f setcmd.f setdef.F setscl.F setupg.F spare.f  spthak.F stplst.F tbgdis.F 
               ternum.f  tofds.F  tonum.f  tr3dsp.F trig2.F  trkhit.F trkrol.F trn4v.F  trnumb.F trpvtx.F 
               trtext.F  truth.F  vcdo.F   vrxcir.F vtx.F    vtx.F    vxdisp.F walloc.F welcme.f xcalcor.F xsafchk.F 
               xtofvw.F  xxxyyy.F xytag.F  yamada.F zfitds.F zfit.F   zinter.F ztrgvw.F grblock.F
)
list(TRANSFORM GRAFIX_src PREPEND "${SRCGRAFIX}/")
ADD_LIBRARY(${GRAFIX} STATIC ${GRAFIX_src})
########################################################################
set(INTERFACE_src
         hfix.f biby.f  dlib.f igwlcm.F ips.f plot10.F 
         #plot2016.F 
         #ranmar.f 
         setcol.F
)
list(TRANSFORM INTERFACE_src PREPEND "${SRCINTERFACE}/")
ADD_LIBRARY(${INTERFACE} STATIC ${INTERFACE_src})
if (${CMAKE_Fortran_COMPILER_ID}  MATCHES  "GNU")
 target_compile_options(${INTERFACE} PRIVATE -Wno-intrinsic-shadow)
endif()

########################################################################
set(JADEGS_src
akop.F alphan.F amps2z.F change.F cl2prc.f cnewid.F dedxbk.F dedxbn.F distxy.F drpipe.F drtrck.F 
dserr.f dxmcsp.f dxmcss.f ebeam.F eigen.f ergtot.F errmom.F errprt.F evbkpr.F evrea1.f evread.F 
evwrit9.F exjhtl.F fampco.F getds.f getint.f hprs.f idattm.f izt2rn.f jadebd.F jegain.f jetcal.F 
jfetch.F jftnew.F jrecal.F kalibr.f klread.F ktmcon.F ldatyp.f linkds.f locfld.f loc.f mchtrb.F 
mctr4v.F mctrcb.F messag.F mofrci.f momenp.F moment.F momgam.F n50sco.F numfun.f nvert.f opunit.F 
parmin.f pragai.f prstat.F rbpipe.F rdalgn.F rddate.F rdddcl.f rddoub.f rddouv.F rdhead.f rdiefv.F 
rdinef.f rdmerg.f rdmodn.F rdmodv.F rdmrgv.f rdmtco9.F rdpatr.F rdpoin.f rdpoiv.f rdrdmh.f rdreso.F 
rdresv.F rdrnhv.F rdtrg1.f rdtrg2.f rdtrig.F runfix.F safchk.f shell9.F sminvd.f sphrcy.f spinnr.f 
sqtval.F superv.F symin.f thrunb.F thrust.F tofbk.f track.F treltr.F vertex.f vrtpos.f vtm.f vtxaft.f 
vtxbnk.F vtxee.f vtxini.f vtxpnt.f vtxpre.F vtxsrc.f xbpipe.F xyblk.F zcass.f zcdata.f zcftnw.F zchcal.f 
zcoor.f zextra.f zsfit.F zsline.F zspdbk.f zspur.F zsrft1.F zsrftv.F zsxy.F zvertf.F
#NOTE, The old version from PMF contains a bug:  zrecal and zbug symbols are taken from zebra 
zrecal.F zbug.f
)
list(TRANSFORM JADEGS_src PREPEND "${SRCJADEGS}/")
ADD_LIBRARY(${JADEGS} STATIC ${JADEGS_src})
########################################################################
set(JADEMUS_src
 mu2pul.f muanab.F muanac.F muanaf.F muana.f  mucon.F  muconw.F mucoor.F mucuts.F 
 mudchk.F mudump.F muendp.F muerry.f mufflb.F muffle.F mufflr.F muffls.F mufflt.F 
 mufflx.F muffly.F mufflz.F mufini.F mufmat.F mufsta.F muhdsp.F muhit.F  muhits.F
 muloch.F mumess.f muons.F  mupic.F  muregm.f mureg.F  muregs.f muregy.F musafe.f 
 muwire.F tanthm.F xymud.F
)
list(TRANSFORM JADEMUS_src PREPEND "${SRCJADEMUS}/")
if (${CMAKE_Fortran_COMPILER_ID}  MATCHES  "PGI")
  LIST(APPEND JADEMUS_src ${SRCJADEMUS}/muininohash.F)
else()
  LIST(APPEND JADEMUS_src ${SRCJADEMUS}/muini.F)
endif()
ADD_LIBRARY(${JADEMUS} STATIC ${JADEMUS_src})
########################################################################
set(JMC_src 
actof.f  brvect.F datemc.f eepair.f endclg.f errmsg.F evtini.f fcrang.f 
hdecls.f inifwd.f inttof.f jchamb.f jdtime.f jeloss.f jftest.f jgetev.f jgetvc.f 
jhitin.f jinit.f  jipatr.f jmulsc.f jprabs.f jprfre.f jprthi.f jprtlo.f jrchck.f 
jring.f  jspoin.f jstep.f  jstore.f jtrapz.f jtrkgm.f jtrntr.f jvdtim.f jvtxch.f 
limint.F mchist.f mcnews.f mctagb.f mctagc.f mctage.f mctagf.f mctagi.F mctagm.F 
mctagr.F mctags.F mcvali.F mcverr.f mcvsum.f muconm.f muordr.f murtne.f ordlg.f 
pikmuf.f plligd.f radiat.f rncont.f rnexp0.f rngau.f  rtmi.f   setbpc.f settof.f 
showr.f  stalgn.f statag.F statbp.f sthead.f stlatc.f stpall.F stvect.F svect1.f 
tags2h.F tags2h.F tofmak.f trcdet.f trcdtv.f trcoil.f trkadc.f trkgam.f trkgmv.f 
trlgl.f  trlgs6.F trlgsh.F wegas.f  wrtmcb.f
)
list(TRANSFORM JMC_src PREPEND "${SRCJMC}/")
ADD_LIBRARY(${JMC} STATIC ${JMC_src})
########################################################################
set(PATRECSR_src
         adpatr.F backtr.F bakfit.F bakpak.F bside.F bstore.F crtrel.F deadcl.f errorm.F
         flinel.F ftrkel.F fxyz.F inpatc.F inpatr.F kntrel.F patrc1.F patrec.F
         patrol.F pcwork.F prhtar.F rincon.F rstbtr.F srtrel.F trlord.F xyfit.F
         zrfit.F
)
list(TRANSFORM PATRECSR_src PREPEND "${SRCPATRECSR}/")
ADD_LIBRARY(${PATRECSR} STATIC ${PATRECSR_src})
########################################################################
set(SOURCE_src
         angbar.f bbcorr.F bbleak.F brlgn6.f brlgn.F calcor.F elgobs.f encorr.F
         englos.f enlosg.f ilctrc.f intolg.F lganal9.F lgavde.f lgavdp.f lgbadc.F
         lgbfil.F lgbhit.f lgbtdc.F lgcalb.F lgcctl.F lgcdir9.F lgchc2.F lgclpb9.F
         lgclpc.F lgclus.F lgcrn2.f lgcrnr.f lgdcpy.F lgdead.f lgecor9.F lgeerr.f
         lgefil.F lgehit.f lgeign.F lgerse.F lgesmr.f lginit.f lgmesg.F lgnbls.f
         lgnmec.f lgrein.F lgsrth.F lgsubc.f lgthcr.f lkcorr.F posend.f shmax.f
         thcofa.F thcorr.F tpcnst.F
)
list(TRANSFORM SOURCE_src PREPEND "${SRCSOURCE}/")
ADD_LIBRARY(${SOURCE} STATIC ${SOURCE_src})
########################################################################
set(TAGG_src
         amctag.f anatag.F conver.f ggcctl.f ggclpc.f ggclus.f ggfac2.f ggresq.f
         ggsort.f ggsrth.f gscrub.f lumons.f rcoord.f repack.f sort.f tagadc.F
         tagan.F tagchk.F tagcls.F tagcol.F tagdir.f tagfit.F tagfty.f taggtp.F
         tagh2s.F tagint.F tagkal.F tagmrk.F tagneb.F tagpd1.F tagpd2.F tagped.F
         tagphi.f tagpos.F tagps1.F tagps2.f tagraw.F tagrty.f tags2h.F tagsr1.F
         tagsr2.F tagsto.for tagsum.F tracks.f umcpak.f
)
if (${CMAKE_Fortran_COMPILER_ID}  MATCHES  "XL")
  file(GLOB myfortag ${SRCTAGG}/*.for)
  set_property(SOURCE ${myfortag} PROPERTY COMPILE_FLAGS -qsuffix=f=for)
endif()
list(TRANSFORM TAGG_src PREPEND "${SRCTAGG}/")
ADD_LIBRARY(${TAGG} STATIC ${TAGG_src})
########################################################################
set(TOFLIB_src
         agrpr.f atfugr.f atofrw.f atofun.F cmass.f corcst.f mocard.F rd0880.F
         tfarnw.F tfctd1.F tfstor.f tofar2.F tofard.F tofbra.f tofcl2.f tofclc.F
         tofcor.f tofin2.F tofint.F tofma2.f tofmas.F tofsm2.f tofsm3.F tofsmp.F
)
list(TRANSFORM TOFLIB_src PREPEND "${SRCTOFLIB}/")
ADD_LIBRARY(${TOFLIB} STATIC ${TOFLIB_src})
########################################################################
set(VERTEX_src
         vertex.f vtxaft.f vtxbnk.F vtxee.f vtxini.f vtxpnt.f vtxpre.F vtxsrc.f
)
list(TRANSFORM VERTEX_src PREPEND "${SRCVERTEX}/")
ADD_LIBRARY(${VERTEX} STATIC ${VERTEX_src})
########################################################################
set(WERTEX_src
         parkor.F wertex.f wtxaft.f wtxbnk.F wtxcrv.f wtxee.f wtxeer.F wtxerr.f
         wtxini.f wtxpnt.f wtxpre.F wtxsrc.f xfwid.F xfwvxc.F
)
list(TRANSFORM WERTEX_src PREPEND "${SRCWERTEX}/")
ADD_LIBRARY(${WERTEX} STATIC ${WERTEX_src})
########################################################################
set(ZLIB_src
     chien52.F chien5.F crdoca.f dxmcgn.f ealgcl.F eamcde.f eaunin.f eazero.f
     ekand.f expect.F fehler.f irnevt.f jobfin.f prallb.f prlist.f prsinb.f
     prtoci.f shado9.F shwcpr.f trkbl9.F trkbse.F ze4vdp.f ze4vea.f ze4vet2.f
     ze4vet.f ze4vet.f ze4vmc.f ze4vpk.for ze4vtp.f ze4vun.f ze4vvx.F zmcredu.f
     zsphrcy.f zspthak.f
)
list(TRANSFORM ZLIB_src PREPEND "${SRCZLIB}/")
if (${CMAKE_Fortran_COMPILER_ID}  MATCHES  "XL")
  file(GLOB myforzlib ${SRCZLIB}/*.for)
  set_property(SOURCE ${myforzlib} PROPERTY COMPILE_FLAGS -qsuffix=f=for)
endif()
ADD_LIBRARY(${ZLIB} STATIC ${ZLIB_src})
########################################################################
#JADEZ
set(JADEZ_src ${SRCJADEZ}/gphmain.F  ${SRCJADEZ}/xuser.F)
add_executable(${JADEZEXE} ${JADEZ_src})
#JADEZROOT
if (NOT JADE_USE_CERNLIB)
  set(JADEZROOT_src ${SRCJADEZ}/gphmain2.F  ${SRCJADEZ}/xuser.F  ${SRCJADEZ}/gphmain.cxx)
  add_executable(${JADEZROOTEXE} ${JADEZROOT_src})
  target_include_directories(${JADEZROOTEXE} PUBLIC ${ROOT_INCLUDE_DIRS})
endif()
#MCJADE
set(MCJADE_src ${SRCMCJADE}/mcjade.f ${SRCMCJADE}/mcmain.f  ${SRCMCJADE}/mcrand.f  ${SRCMCJADE}/mcuser.f ${SRCSUPERV}/showb.f)
add_executable(${MCJADEEXE} ${MCJADE_src})
#SUPERV
set(SUPERV_src ${SRCSUPERV}/ana.F    ${SRCSUPERV}/jdmain.f  ${SRCSUPERV}/showb.f  ${SRCSUPERV}/user.f ${SRCMCJADE}/mcrand.f)
add_executable(${SUPERVEXE} ${SUPERV_src})
#ZE4V
set(ZE4V_src ${SRCZE4V}/ze4vjb.f ${SRCMCJADE}/mcrand.f ${SRCSUPERV}/showb.f )
add_executable(${ZE4VEXE} ${ZE4V_src})

if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
  target_link_libraries(${JADEZEXE}   ${GRAFIX} ${BOSLIB} ${TOFLIB} ${TAGG} ${JADEGS} ${INTERFACE} ${JADEMUS} ${SOURCE} ${WERTEX} ${PATRECSR}
    ${JEXTERN_LIBRARIES} ${X11_LIBRARIES} ${X11_xcb_LIB} ${X11_Xau_LIB} ${X11_Xdmcp_LIB})
  if (NOT JADE_USE_CERNLIB)
  target_link_libraries(${JADEZROOTEXE}   ${GRAFIX} ${BOSLIB} ${TOFLIB} ${TAGG} ${JADEGS} ${INTERFACE} ${JADEMUS} ${SOURCE} ${WERTEX} ${PATRECSR}
    ${JEXTERN_LIBRARIES} ${X11_LIBRARIES} )
  endif()
  target_link_libraries(${MCJADEEXE}    ${JMC} ${BOSLIB} ${JADEGS} ${INTERFACE} ${JADEMUS} ${SOURCE} ${WERTEX} ${PATRECSR} 
    ${JEXTERN_LIBRARIES}  )

  target_link_libraries(${SUPERVEXE}    ${JMC} ${BOSLIB} ${JADEGS} ${INTERFACE} ${JADEMUS} ${SOURCE} ${WERTEX} ${PATRECSR}
    ${JEXTERN_LIBRARIES}   )

  target_link_libraries(${ZE4VEXE}    ${ZLIB} ${JMC} ${BOSLIB} ${JADEGS} ${INTERFACE} ${JADEMUS} ${SOURCE} ${WERTEX} ${PATRECSR}
    ${JEXTERN_LIBRARIES} )
else()
  target_link_libraries(${JADEZEXE}  -Wl,--start-group ${GRAFIX} ${BOSLIB} ${TOFLIB} ${TAGG} ${JADEGS} ${INTERFACE} ${JADEMUS} ${SOURCE} ${WERTEX} ${PATRECSR}
    ${JEXTERN_LIBRARIES} ${X11_LIBRARIES} -Wl,--end-group)
  if (NOT JADE_USE_CERNLIB)
  target_link_libraries(${JADEZROOTEXE}  -Wl,--start-group ${GRAFIX} ${BOSLIB} ${TOFLIB} ${TAGG} ${JADEGS} ${INTERFACE} ${JADEMUS} ${SOURCE} ${WERTEX} ${PATRECSR}
    ${JEXTERN_LIBRARIES} ${X11_LIBRARIES} -Wl,--end-group)
  endif()
  target_link_libraries(${MCJADEEXE} -Wl,--start-group ${JMC} ${BOSLIB} ${JADEGS} ${INTERFACE} ${JADEMUS} ${SOURCE} 
    ${WERTEX} ${PATRECSR} 
    ${JEXTERN_LIBRARIES}  -Wl,--end-group)

  target_link_libraries(${SUPERVEXE} -Wl,--start-group ${JMC} ${BOSLIB} ${JADEGS} ${INTERFACE} ${JADEMUS} ${SOURCE} ${WERTEX} ${PATRECSR}
    ${JEXTERN_LIBRARIES}   -Wl,--end-group)

  target_link_libraries(${ZE4VEXE} -Wl,--start-group ${ZLIB} ${JMC} ${BOSLIB} ${JADEGS} ${INTERFACE} ${JADEMUS} ${SOURCE} ${WERTEX} ${PATRECSR}
    ${JEXTERN_LIBRARIES} -Wl,--end-group)
endif()

########################################################################

set(RCONVEXE  rconv)
set(CONVEXE   conv)
add_executable(${CONVEXE} ${UTIL}/ccal/conv.f)
target_link_libraries(${CONVEXE}  ${JEXTERN_LIBRARIES})

add_executable(${RCONVEXE} ${UTIL}/ccal/rconv.f)
target_link_libraries(${RCONVEXE}  ${JEXTERN_LIBRARIES})

INSTALL(TARGETS ${CONVEXE} ${RCONVEXE} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})


set(ZREADEXE      zread)
add_executable(${ZREADEXE} ${UTIL}/zread/zread.f)
INSTALL(TARGETS ${ZREADEXE}  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
target_link_libraries(${ZREADEXE}  ${JEXTERN_LIBRARIES})

#####################################################################

if (JADE_OLD_MC)

set( MCLIBS    ariadne408 coj623d herwig58d herwig59 jt63jade jt74opal pythia5722 ckern104 ckern105  px114 )
foreach ( ml ${MCLIBS} )  
 ADD_LIBRARY(${ml} STATIC ${UTIL}/mcgen/src/${ml}.f)
endforeach (  ml ${MCLIBS} )
ADD_LIBRARY(mc105j STATIC                        ${UTIL}/mcgen/mc/mc105j.f  )

if (${CMAKE_Fortran_COMPILER_ID}  MATCHES  "GNU")
 target_compile_options(jt74opal PRIVATE -Wno-integer-division)
 target_compile_options(jt63jade PRIVATE -Wno-integer-division)
 target_compile_options(pythia5722 PRIVATE -Wno-integer-division)
endif()


if (${CMAKE_Fortran_COMPILER_ID}  MATCHES "GNU")
  target_compile_options(coj623d  PRIVATE -fdefault-real-8 -fdefault-double-8)
endif()

add_executable(ar48p   ${UTIL}/mcgen/ariadne/mcar48p_105j.f  ${UTIL}/mcgen/src/pymaxi.f)
target_link_libraries(ar48p  PRIVATE 
                       jt74opal 
                       mc105j
                       pythia5722
                       ariadne408   
                       ckern105   
                       px114  
                       ${JEXTERN_LIBRARIES}  ${BLAS_LIBRARIES}  ${LAPACK_lapack_LIBRARY} 
                       )

add_executable(cj623   ${UTIL}/mcgen/cojets/mccj623_105j.f ${UTIL}/mcgen/src/pystfu.f)
target_link_libraries(cj623   PRIVATE  
                       mc105j 
                       pythia5722 
                       coj623d   
                       ckern105   
                       jt74opal   
                       px114  
                       ${JEXTERN_LIBRARIES} ${BLAS_LIBRARIES}  ${LAPACK_lapack_LIBRARY} 
                       )

add_executable(hw59   ${UTIL}/mcgen/herwig/mchw59_105j.f )
target_link_libraries(hw59    PRIVATE                       
                       mc105j 
                       herwig59 
                       ckern105   
                       jt74opal  
                       px114  
                       ${JEXTERN_LIBRARIES} ${BLAS_LIBRARIES}  ${LAPACK_lapack_LIBRARY} 
                       )

add_executable(hw58d   ${UTIL}/mcgen/herwig/mchw58d_105j.f )
target_link_libraries(hw58d   PRIVATE                        
                       mc105j 
                       herwig58d  
                       ckern105   
                       jt74opal  
                       px114  
                       ${JEXTERN_LIBRARIES} ${BLAS_LIBRARIES}  ${LAPACK_lapack_LIBRARY} 
                       ) 


add_executable(jt63   ${UTIL}/mcgen/jetset/mcjt63_105j.f ${UTIL}/mcgen/src/lugive.f)
target_link_libraries(jt63   PRIVATE                    
                       mc105j 
                       jt63jade   
                       ckern105   
                       px114 
                       ${JEXTERN_LIBRARIES} ${BLAS_LIBRARIES}  ${LAPACK_lapack_LIBRARY} 
                       )

add_executable(jt74   ${UTIL}/mcgen/jetset/mcjt74_105j.f )
target_link_libraries(jt74    PRIVATE                 
                       mc105j  
                       ckern105   
                       jt74opal   
                       px114  
                       ${JEXTERN_LIBRARIES} ${BLAS_LIBRARIES} ${LAPACK_lapack_LIBRARY} 
                       )

add_executable(lepton   ${UTIL}/mcgen/lepton/lepton.f )
target_link_libraries(lepton    PRIVATE                
                       mc105j 
                       ckern105  
                       jt63jade  
                       px114  
                       ${JEXTERN_LIBRARIES} ${BLAS_LIBRARIES} ${LAPACK_lapack_LIBRARY} 
                       ${INTERFACE} #ranmar
                       )
                       
add_executable(py57   ${UTIL}/mcgen/pythia/mcpy57_105j.f ${UTIL}/mcgen/src/pymaxi.f ${UTIL}/mcgen/src/pystfu.f)
target_link_libraries(py57    PRIVATE                
                       mc105j 
                       ckern105  
                       pythia5722
                       jt74opal
                       px114  
                       ${JEXTERN_LIBRARIES} ${BLAS_LIBRARIES}  ${LAPACK_lapack_LIBRARY} 
                       )
                       
endif()
########################################################################

INSTALL(TARGETS ${MCJADEEXE} ${JADEZEXE} ${SUPERVEXE}  ${ZE4VEXE} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
if (NOT JADE_USE_CERNLIB)
  INSTALL(TARGETS ${JADEZROOTEXE} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
endif()
INSTALL( TARGETS ${BOSLIB} 
         ${GRAFIX} 
         ${INTERFACE} 
         ${JADEGS} 
         ${JADEMUS} 
         #${JADESR}
         ${JMC} 
         ${PATRECSR} 
         ${SOURCE} 
         ${TAGG} 
         ${TOFLIB} 
         ${VERTEX} 
         ${WERTEX} 
         ${ZLIB} 
         ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR} LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR} COMPONENT library )
########################################################################


include(CMakePackageConfigHelpers)

set(CONFIG_INSTALL_DIR ${CMAKE_INSTALL_DATADIR}/JADESOFT/cmake)

configure_package_config_file(../cmake/Templates/JADESOFTConfig.cmake.in ${CMAKE_CURRENT_BINARY_DIR}/cmake/JADESOFTConfig.cmake
                              INSTALL_DESTINATION ${CONFIG_INSTALL_DIR}
                              PATH_VARS 
                              CMAKE_INSTALL_BINDIR 
                              CMAKE_INSTALL_LIBDIR 
                              CMAKE_INSTALL_DATADIR)
            
write_basic_package_version_file(${CMAKE_CURRENT_BINARY_DIR}/cmake/JADESOFTConfig-version.cmake COMPATIBILITY SameMajorVersion VERSION ${PROJECT_VERSION})

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/cmake/JADESOFTConfig.cmake
              ${CMAKE_CURRENT_BINARY_DIR}/cmake/JADESOFTConfig-version.cmake
              DESTINATION ${CONFIG_INSTALL_DIR} COMPONENT devel)


install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/cal/aupdat1 
              ${CMAKE_CURRENT_SOURCE_DIR}/cal/aupdat1.b 
              ${CMAKE_CURRENT_SOURCE_DIR}/cal/bupdat0 
              ${CMAKE_CURRENT_SOURCE_DIR}/cal/bupdat0.b 
              ${CMAKE_CURRENT_SOURCE_DIR}/cal/bupdat1 
              ${CMAKE_CURRENT_SOURCE_DIR}/cal/bupdat1.b  DESTINATION ${CMAKE_INSTALL_DATADIR}/JADESOFT/cal )





