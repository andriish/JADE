#https://github.com/SethMMorton/cmake_fortran_template
##################################################
# Define the project and the depencies that it has
##################################################

CMAKE_MINIMUM_REQUIRED(VERSION 2.8.5)
PROJECT(JADESOFT Fortran CXX)

SET(VERSION 2.99)

# Add our local modlues to the module path
SET(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/../cmake/Modules/")

set(CMAKE_VERBOSE_MAKEFILE ON)


find_package(CERNLIB)

########################################################################

set (ROOT_FIND_COMPONENTS Gui Core Cint RIO Net Hist Graf Graf3d Gpad Tree Rint Postscript Matrix Physics MathCore Thread)
  find_file(ROOT_CONFIG ROOTConfig.cmake HINTS $ENV{ROOTSYS} ${ROOT_DIR} NO_DEFAULT_PATH PATH_SUFFIXES cmake)
  if (ROOT_CONFIG)
     message(STATUS "ROOTConfig.cmake found and will be used: ${ROOT_CONFIG}")
     include(${ROOT_CONFIG})
     message(STATUS "ROOT use file: ${ROOT_USE_FILE}")
     include(${ROOT_USE_FILE})
     if(ROOT_LIBRARIES AND ROOT_INCLUDE_DIRS)
       set(ROOT_FOUND TRUE)
     endif()
  else()
     message(STATUS "ROOTConfig.cmake not found, trying to use FindROOT.cmake...")
     find_package(ROOT QUIET REQUIRED COMPONENTS Gui Core Cint RIO Net Hist Graf Graf3d Gpad Tree Rint Postscript Matrix Physics MathCore Thread)
  endif()

  message(STATUS "ROOT includes : ${ROOT_INCLUDE_DIRS}")
  message(STATUS "ROOT libraries: ${ROOT_LIBRARIES}")
  message(STATUS "ROOT_FOUND    : ${ROOT_FOUND}")

  if(ROOT_FOUND)
    include_directories(${ROOT_INCLUDE_DIRS})
#    add_definitions(-DHEPMC_ROOTIO)
  else()
    message(STATUS "JADESOFT warning: ROOT not found")
  endif()



set(JEXTERN_LIBRARIES  ${CMAKE_SOURCE_DIR}/../picocernlib/libpicocernlib.a ${ROOT_LIBRARIES} -lstdc++)
set(CMAKE_CXX_FLAGS     "${CMAKE_CXX_FLAGS}     -DJEXTERNISPICO")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -DJEXTERNISPICO")

#############USE IT WITH CERNLIB->########################################
#set(JEXTERN_LIBRARIES   ${CERN_STATIC_LIBRARIES} )
#set(JEXTERN_LIBRARIES   ${CERN_DYNAMIC_LIBRARIES} ) 
#set(CMAKE_CXX_FLAGS     "${CMAKE_CXX_FLAGS}     -DJEXTERNISCERNLIB")
#set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -DJEXTERNISCERNLIB")
#############<-USE IT WITH CERNLIB########################################


set(JEXTERN_GRAPH_LIBRARIES X11)
########################################################################


get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)
if (Fortran_COMPILER_NAME MATCHES "gfortran.*")
   set(CMAKE_Fortran_FLAGS            "${CMAKE_Fortran_FLAGS}  -std=legacy -g -ffixed-line-length-none -finit-local-zero   -fno-automatic -O0   -fno-align-commons ")
endif()

if (Fortran_COMPILER_NAME MATCHES "ifort.*")
#NOT USED
   set(CMAKE_Fortran_FLAGS            "${CMAKE_Fortran_FLAGS}  -fno-automatic -fno-backslash -extend-source 132")	
endif()

if (Fortran_COMPILER_NAME MATCHES "xlf.*")
#PPC64 only
   set(CMAKE_Fortran_FLAGS           "${CMAKE_Fortran_FLAGS}  -qfixed=256  -qsigtrap -g -qsave -qrndsngl -qmaxmem=-1 -qextname -qfloat=fltint:hsflt:hssngl:nans:rndsngl -qcharlen=32767 -qxlf77=leadzero -qfullpath -qctyplss -qintlog ")	
#-q32  -melf32ppc
endif()

if (Fortran_COMPILER_NAME MATCHES "sun.*")
#NOT WELL TESTED
#Local variables are static by default. Alternate is STATIC statement. 
   set(CMAKE_Fortran_FLAGS           "${CMAKE_Fortran_FLAGS}   -ftrap=%none -f77 -e  -g  ")
#-m32 
endif()


message(STATUS "Fortran compiler        : ${Fortran_COMPILER_NAME}")
message(STATUS "Fortran compiler flags  : ${CMAKE_Fortran_FLAGS}")
message(STATUS "C++ compiler            : ${CXX_COMPILER_NAME}")
message(STATUS "C++ compiler flags      : ${CMAKE_CXX_FLAGS}")
message(STATUS "JEXTERN libraries       : ${JEXTERN_LIBRARIES}")
message(STATUS "JEXTERN_GRAPH libraries : ${JEXTERN_GRAPH_LIBRARIES}")


set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/lib)


############################################################
# Define the actual files and folders that make up the build
############################################################

# Define the executable name
#SET(FOOEXE foo)
SET(MCJADEEXE    bin/mcjade)
SET(SUPERVEXE    bin/superv)
SET(JADEZEXE     bin/jadez)
SET(JADEZROOTEXE bin/jadezroot)
SET(ZE4VEXE      bin/ze4v)

# Define the library name
SET(BOSLIB boslib)
SET(GRAFIX          grafix)
SET(INTERFACE       interface)
SET(JADEGS          jadegs)
SET(JADEMUS         jademus)
#SET(JADESR         jadesr)
SET(JMC             jmc)
SET(PATRECSR        patrecsr)
SET(SOURCE          source)
SET(TAGG            tagg)
SET(TOFLIB          toflib)
SET(VERTEX          vertex)
SET(WERTEX          wertex)
SET(ZLIB            zlib)



# Define some directories
SET(SRC ${CMAKE_SOURCE_DIR}/src2016)
SET(SRCMAIN ${CMAKE_SOURCE_DIR}/main2016)
SET(LIB ${CMAKE_SOURCE_DIR}/lib)
SET(BIN ${CMAKE_SOURCE_DIR}/bin)

include_directories( ${SRC}/boslib ${SRC}/grafix ${SRC}/interface ${SRC}/jadegs ${SRC}/jademus ${SRC}/jadesr ${SRC}/jmc ${SRC}/patrecsr ${SRC}/source ${SRC}/tagg  ${SRC}/toflib ${SRC}/vertex ${SRC}/wertex ${SRC}/zlib  )

SET(SRCMCJADE ${SRCMAIN}/mcjade)
SET(SRCSUPERV ${SRCMAIN}/superv)
SET(SRCJADEZ ${SRCMAIN}/jadez)
SET(SRCZE4V ${SRCMAIN}/ze4v)



SET(SRCBOSLIB ${SRC}/boslib)
SET(SRCGRAFIX          ${SRC}/grafix)
SET(SRCINTERFACE         ${SRC}/interface)
SET(SRCJADEGS         ${SRC}/jadegs)
SET(SRCJADEMUS         ${SRC}/jademus)
SET(SRCJMC         ${SRC}/jmc)
SET(SRCPATRECSR         ${SRC}/patrecsr)
SET(SRCSOURCE         ${SRC}/source)
SET(SRCTAGG         ${SRC}/tagg)
SET(SRCTOFLIB         ${SRC}/toflib)
SET(SRCVERTEX         ${SRC}/vertex)
SET(SRCWERTEX         ${SRC}/wertex)
SET(SRCZLIB         ${SRC}/zlib)
# Have the .mod files placed in the lib folder
SET(CMAKE_Fortran_MODULE_DIRECTORY ${LIB})


if (Fortran_COMPILER_NAME MATCHES "xlf.*")
   file(GLOB myfortag ${SRCTAGG}/*.for)
   set_property(SOURCE ${myfortag} PROPERTY COMPILE_FLAGS -qsuffix=f=for)
endif()


SET(BOSLIB_src
${SRCBOSLIB}/asrd.f ${SRCBOSLIB}/bchl.F ${SRCBOSLIB}/bchm.F ${SRCBOSLIB}/bcre.F ${SRCBOSLIB}/bdar.F ${SRCBOSLIB}/bdlg.F
${SRCBOSLIB}/bdlm.F ${SRCBOSLIB}/bdls.F ${SRCBOSLIB}/bdmp.F ${SRCBOSLIB}/bfmt.for ${SRCBOSLIB}/bfrd.for
${SRCBOSLIB}/bgac.for ${SRCBOSLIB}/bgar.F ${SRCBOSLIB}/binlda.F ${SRCBOSLIB}/binp.F ${SRCBOSLIB}/bint.F
${SRCBOSLIB}/bloada.F ${SRCBOSLIB}/blocda.F ${SRCBOSLIB}/bloc.F ${SRCBOSLIB}/bmlt.F
${SRCBOSLIB}/bnxt.F ${SRCBOSLIB}/boutp.F ${SRCBOSLIB}/bpasda.F ${SRCBOSLIB}/bpos.F
${SRCBOSLIB}/brds.F ${SRCBOSLIB}/breadc.for ${SRCBOSLIB}/bread.for ${SRCBOSLIB}/brnm.F
${SRCBOSLIB}/bsaw.F ${SRCBOSLIB}/bseq.F ${SRCBOSLIB}/bsls.F ${SRCBOSLIB}/bsta.F
${SRCBOSLIB}/bwrite.for ${SRCBOSLIB}/bwrs.F ${SRCBOSLIB}/cchl.F ${SRCBOSLIB}/ccor.for
${SRCBOSLIB}/ccre.F ${SRCBOSLIB}/cloc.F ${SRCBOSLIB}/cnxt.F ${SRCBOSLIB}/cread.for
${SRCBOSLIB}/dada.F ${SRCBOSLIB}/defst.for ${SRCBOSLIB}/histpr.f ${SRCBOSLIB}/iaf.for
${SRCBOSLIB}/ibln.F ${SRCBOSLIB}/indar.F ${SRCBOSLIB}/itoda.for ${SRCBOSLIB}/msgpr.for
${SRCBOSLIB}/pall.f ${SRCBOSLIB}/pcond.for ${SRCBOSLIB}/pcorr.for ${SRCBOSLIB}/phist.for
${SRCBOSLIB}/poda.F ${SRCBOSLIB}/pstor.for ${SRCBOSLIB}/ptabl.for ${SRCBOSLIB}/ptext1.f
${SRCBOSLIB}/pvert.for ${SRCBOSLIB}/rdda.F ${SRCBOSLIB}/rdvar.for ${SRCBOSLIB}/readhl.for
${SRCBOSLIB}/sort4.for ${SRCBOSLIB}/ucorr.for ${SRCBOSLIB}/uhist.for ${SRCBOSLIB}/uwp.for
${SRCBOSLIB}/vall.for  ${SRCBOSLIB}/vecsub.f
)

if (Fortran_COMPILER_NAME MATCHES "xlf.*")
   file(GLOB MyFiles ${SRCBOSLIB}/*.for)
   set_property(SOURCE ${MyFiles} PROPERTY COMPILE_FLAGS -qsuffix=f=for)
 endif()
   


ADD_LIBRARY(${BOSLIBLIB} boslib ${BOSLIB_src})
SET(GRAFIX_src
${SRCGRAFIX}/axshow.F ${SRCGRAFIX}/bankls.F ${SRCGRAFIX}/bankpr.F ${SRCGRAFIX}/barsec.f
${SRCGRAFIX}/blnumb.F ${SRCGRAFIX}/blshow.F ${SRCGRAFIX}/calloc.F ${SRCGRAFIX}/capmrk.F
${SRCGRAFIX}/cdst.F ${SRCGRAFIX}/cirdis.F ${SRCGRAFIX}/clrcon.f ${SRCGRAFIX}/coment.f
${SRCGRAFIX}/comwrd.F ${SRCGRAFIX}/convrt.f ${SRCGRAFIX}/credit.F ${SRCGRAFIX}/cricro.f
${SRCGRAFIX}/croax.F ${SRCGRAFIX}/ctdist.F ${SRCGRAFIX}/decide.f ${SRCGRAFIX}/dedxds.F
${SRCGRAFIX}/dedxv1.F ${SRCGRAFIX}/dedxvw.F ${SRCGRAFIX}/defcmd.F ${SRCGRAFIX}/defmac.f
${SRCGRAFIX}/defwrd.f ${SRCGRAFIX}/delmac.f ${SRCGRAFIX}/diaghn.f ${SRCGRAFIX}/diagin.f
${SRCGRAFIX}/disaxi.f ${SRCGRAFIX}/disply.F ${SRCGRAFIX}/dispvc.F ${SRCGRAFIX}/dispzc.F
${SRCGRAFIX}/dnum.f ${SRCGRAFIX}/dracap.f ${SRCGRAFIX}/dramov.f ${SRCGRAFIX}/drawbp.F
${SRCGRAFIX}/drawer.F ${SRCGRAFIX}/drawfd.F ${SRCGRAFIX}/drawid.F ${SRCGRAFIX}/drawlg.F
${SRCGRAFIX}/drawmu.f ${SRCGRAFIX}/drawpi.f ${SRCGRAFIX}/drawvc.F ${SRCGRAFIX}/drawzc.F
${SRCGRAFIX}/drftim.f ${SRCGRAFIX}/drhatc.F ${SRCGRAFIX}/drvcrd.F ${SRCGRAFIX}/drwhit.F
${SRCGRAFIX}/dsfadc.F ${SRCGRAFIX}/dsvac.F ${SRCGRAFIX}/dthcv1.f ${SRCGRAFIX}/ecapcy.F
${SRCGRAFIX}/ecap.F ${SRCGRAFIX}/ecaps.F ${SRCGRAFIX}/edimac.f ${SRCGRAFIX}/edvp.F
${SRCGRAFIX}/efmass.F ${SRCGRAFIX}/ellips.f ${SRCGRAFIX}/endmes.F ${SRCGRAFIX}/evdisp.F
${SRCGRAFIX}/evtax.F
${SRCGRAFIX}/expmac.f
${SRCGRAFIX}/fadcax.F
${SRCGRAFIX}/faddrw.f
${SRCGRAFIX}/fampds.F
${SRCGRAFIX}/fampft.F
${SRCGRAFIX}/ficoor.F
${SRCGRAFIX}/ficoov.F
${SRCGRAFIX}/finabr.f
${SRCGRAFIX}/fincmd.f
${SRCGRAFIX}/finwrd.f
${SRCGRAFIX}/ftdlho.F
${SRCGRAFIX}/fw83.F
${SRCGRAFIX}/fwc83.F
${SRCGRAFIX}/fwcap.F
${SRCGRAFIX}/fwchev.F
${SRCGRAFIX}/fwdisp.F
${SRCGRAFIX}/fwmucn.F
${SRCGRAFIX}/fwmuht.F
${SRCGRAFIX}/fwnew.F
${SRCGRAFIX}/fyrint.f
${SRCGRAFIX}/gamdis.F
${SRCGRAFIX}/gmtext.F
${SRCGRAFIX}/gvtxft.F
${SRCGRAFIX}/heldis.F
${SRCGRAFIX}/helpli.f
${SRCGRAFIX}/helpus.f
${SRCGRAFIX}/hemsym.f
${SRCGRAFIX}/hitdsp.F
${SRCGRAFIX}/hitmrk.F
${SRCGRAFIX}/hitpnt.F
${SRCGRAFIX}/hitscl.f
${SRCGRAFIX}/hlpmac.f
${SRCGRAFIX}/hodrfi.F
${SRCGRAFIX}/idfun.f
${SRCGRAFIX}/j68kds.F
${SRCGRAFIX}/jadarg.f
${SRCGRAFIX}/jadcmd.f
${SRCGRAFIX}/jadisp.F
${SRCGRAFIX}/joys.F
${SRCGRAFIX}/komman.F
${SRCGRAFIX}/koord.F
${SRCGRAFIX}/lgclst.F
${SRCGRAFIX}/lindis.F
${SRCGRAFIX}/locmac.f
${SRCGRAFIX}/mcgjet.f
${SRCGRAFIX}/menu.f
${SRCGRAFIX}/more.F
${SRCGRAFIX}/muldsp.F
${SRCGRAFIX}/mur2ds.F
${SRCGRAFIX}/n50sds.F
${SRCGRAFIX}/newcal.F
${SRCGRAFIX}/newcap.F
${SRCGRAFIX}/news.f
${SRCGRAFIX}/notice.f
${SRCGRAFIX}/numbwr.F
${SRCGRAFIX}/pardis.F
${SRCGRAFIX}/pbgcyl.F
${SRCGRAFIX}/pbgrfi.F
${SRCGRAFIX}/pbgrz.F
${SRCGRAFIX}/pbgsum.F
${SRCGRAFIX}/phnumb.F
${SRCGRAFIX}/pick.F
${SRCGRAFIX}/plygon.f
${SRCGRAFIX}/pratof.F
${SRCGRAFIX}/predit.F
${SRCGRAFIX}/prj68k.f
${SRCGRAFIX}/prmprs.F
${SRCGRAFIX}/prn50s.F
${SRCGRAFIX}/profil.f
${SRCGRAFIX}/projec.F
${SRCGRAFIX}/projvw.F
${SRCGRAFIX}/prompt.f
${SRCGRAFIX}/prtpev.F
${SRCGRAFIX}/prtptr.F
${SRCGRAFIX}/prtpvx.F
${SRCGRAFIX}/przvtx.F
${SRCGRAFIX}/qplots.F
${SRCGRAFIX}/ralloc.F
${SRCGRAFIX}/realnm.f
${SRCGRAFIX}/rectan.f
${SRCGRAFIX}/renmac.f
${SRCGRAFIX}/resmac.f
${SRCGRAFIX}/retten.F
${SRCGRAFIX}/rsdisp.F
${SRCGRAFIX}/rutrck.F
${SRCGRAFIX}/rzdraw.F
${SRCGRAFIX}/rzhits.F
${SRCGRAFIX}/rzroll.F
${SRCGRAFIX}/rztrck.F
${SRCGRAFIX}/saege.F
${SRCGRAFIX}/scannr.F
${SRCGRAFIX}/sepcmd.f
${SRCGRAFIX}/setcmd.f
${SRCGRAFIX}/setdef.F
${SRCGRAFIX}/setscl.F
${SRCGRAFIX}/setupg.F
${SRCGRAFIX}/spare.f
${SRCGRAFIX}/spthak.F
${SRCGRAFIX}/stplst.F
${SRCGRAFIX}/tbgdis.F
${SRCGRAFIX}/ternum.f
${SRCGRAFIX}/tofds.F
${SRCGRAFIX}/tonum.f
${SRCGRAFIX}/tr3dsp.F
${SRCGRAFIX}/trig2.F
${SRCGRAFIX}/trkhit.F
${SRCGRAFIX}/trkrol.F
${SRCGRAFIX}/trn4v.F
${SRCGRAFIX}/trnumb.F
${SRCGRAFIX}/trpvtx.F
${SRCGRAFIX}/trtext.F
${SRCGRAFIX}/truth.F
${SRCGRAFIX}/vcdo.F
${SRCGRAFIX}/vrxcir.F
${SRCGRAFIX}/vtx.F
${SRCGRAFIX}/vtx.F
${SRCGRAFIX}/vxdisp.F
${SRCGRAFIX}/walloc.F
${SRCGRAFIX}/welcme.f
${SRCGRAFIX}/xcalcor.F
${SRCGRAFIX}/xsafchk.F
${SRCGRAFIX}/xtofvw.F
${SRCGRAFIX}/xxxyyy.F
${SRCGRAFIX}/xytag.F
${SRCGRAFIX}/yamada.F
${SRCGRAFIX}/zfitds.F
${SRCGRAFIX}/zfit.F
${SRCGRAFIX}/zinter.F
${SRCGRAFIX}/ztrgvw.F
${SRCGRAFIX}/grblock.F

)
ADD_LIBRARY(${GRAFIXLIB} grafix ${GRAFIX_src})
SET(INTERFACE_src
${SRCINTERFACE}/hfix.f ${SRCINTERFACE}/biby.f ${SRCINTERFACE}/dlib.f ${SRCINTERFACE}/igwlcm.F
${SRCINTERFACE}/ips.f ${SRCINTERFACE}/plot10.F   ${SRCINTERFACE}/plot2016.F ${SRCINTERFACE}/ranmar.f ${SRCINTERFACE}/setcol.F
)
ADD_LIBRARY(${INTERFACELIB} interface ${INTERFACE_src})
SET(JADEGS_src
${SRCJADEGS}/akop.F
${SRCJADEGS}/alphan.F
${SRCJADEGS}/amps2z.F
${SRCJADEGS}/change.F
${SRCJADEGS}/cl2prc.f
${SRCJADEGS}/cnewid.F
${SRCJADEGS}/dedxbk.F
${SRCJADEGS}/dedxbn.F
${SRCJADEGS}/distxy.F
${SRCJADEGS}/drpipe.F
${SRCJADEGS}/drtrck.F
${SRCJADEGS}/dserr.f
${SRCJADEGS}/dxmcsp.f
${SRCJADEGS}/dxmcss.f
${SRCJADEGS}/ebeam.F
${SRCJADEGS}/eigen.f
${SRCJADEGS}/ergtot.F
${SRCJADEGS}/errmom.F
${SRCJADEGS}/errprt.F
${SRCJADEGS}/evbkpr.F
${SRCJADEGS}/evrea1.f
${SRCJADEGS}/evread.F
${SRCJADEGS}/evwrit9.F
${SRCJADEGS}/exjhtl.F
${SRCJADEGS}/fampco.F
${SRCJADEGS}/getds.f
${SRCJADEGS}/getint.f
${SRCJADEGS}/hprs.f
${SRCJADEGS}/idattm.f
${SRCJADEGS}/izt2rn.f
${SRCJADEGS}/jadebd.F
${SRCJADEGS}/jegain.f
${SRCJADEGS}/jetcal.F
${SRCJADEGS}/jfetch.F
${SRCJADEGS}/jftnew.F
${SRCJADEGS}/jrecal.F
${SRCJADEGS}/kalibr.f
${SRCJADEGS}/klread.F
${SRCJADEGS}/ktmcon.F
${SRCJADEGS}/ldatyp.f
${SRCJADEGS}/linkds.f
${SRCJADEGS}/locfld.f
${SRCJADEGS}/loc.f
${SRCJADEGS}/mchtrb.F
${SRCJADEGS}/mctr4v.F
${SRCJADEGS}/mctrcb.F
${SRCJADEGS}/messag.F
${SRCJADEGS}/mofrci.f
${SRCJADEGS}/momenp.F
${SRCJADEGS}/moment.F
${SRCJADEGS}/momgam.F
${SRCJADEGS}/n50sco.F
${SRCJADEGS}/numfun.f
${SRCJADEGS}/nvert.f
${SRCJADEGS}/opunit.F
${SRCJADEGS}/parmin.f
${SRCJADEGS}/pragai.f
${SRCJADEGS}/prstat.F
${SRCJADEGS}/rbpipe.F
${SRCJADEGS}/rdalgn.F
${SRCJADEGS}/rddate.F
${SRCJADEGS}/rdddcl.f
${SRCJADEGS}/rddoub.f
${SRCJADEGS}/rddouv.F
${SRCJADEGS}/rdhead.f
${SRCJADEGS}/rdiefv.F
${SRCJADEGS}/rdinef.f
${SRCJADEGS}/rdmerg.f
${SRCJADEGS}/rdmodn.F
${SRCJADEGS}/rdmodv.F
${SRCJADEGS}/rdmrgv.f
${SRCJADEGS}/rdmtco9.F
${SRCJADEGS}/rdpatr.F
${SRCJADEGS}/rdpoin.f
${SRCJADEGS}/rdpoiv.f
${SRCJADEGS}/rdrdmh.f
${SRCJADEGS}/rdreso.F
${SRCJADEGS}/rdresv.F
${SRCJADEGS}/rdrnhv.F
${SRCJADEGS}/rdtrg1.f
${SRCJADEGS}/rdtrg2.f
${SRCJADEGS}/rdtrig.F
${SRCJADEGS}/runfix.F
${SRCJADEGS}/safchk.f
${SRCJADEGS}/shell9.F
${SRCJADEGS}/sminvd.f
${SRCJADEGS}/sphrcy.f
${SRCJADEGS}/spinnr.f
${SRCJADEGS}/sqtval.F
${SRCJADEGS}/superv.F
${SRCJADEGS}/symin.f
${SRCJADEGS}/thrunb.F
${SRCJADEGS}/thrust.F
${SRCJADEGS}/tofbk.f
${SRCJADEGS}/track.F
${SRCJADEGS}/treltr.F
${SRCJADEGS}/vertex.f
${SRCJADEGS}/vrtpos.f
${SRCJADEGS}/vtm.f
${SRCJADEGS}/vtxaft.f
${SRCJADEGS}/vtxbnk.F
${SRCJADEGS}/vtxee.f
${SRCJADEGS}/vtxini.f
${SRCJADEGS}/vtxpnt.f
${SRCJADEGS}/vtxpre.F
${SRCJADEGS}/vtxsrc.f
${SRCJADEGS}/xbpipe.F
${SRCJADEGS}/xyblk.F
${SRCJADEGS}/zcass.f
${SRCJADEGS}/zcdata.f
${SRCJADEGS}/zcftnw.F
${SRCJADEGS}/zchcal.f
${SRCJADEGS}/zcoor.f
${SRCJADEGS}/zextra.f
${SRCJADEGS}/zsfit.F
${SRCJADEGS}/zsline.F
${SRCJADEGS}/zspdbk.f
${SRCJADEGS}/zspur.F
${SRCJADEGS}/zsrft1.F
${SRCJADEGS}/zsrftv.F
${SRCJADEGS}/zsxy.F
${SRCJADEGS}/zvertf.F
#NOTE, The old version from PMF contains a bug:  zrecal and zbug symbols are taken from zebra
${SRCJADEGS}/zrecal.F
${SRCJADEGS}/zbug.f
)


ADD_LIBRARY(${JADEGSLIB} jadegs ${JADEGS_src})
SET(JADEMUS_src
${SRCJADEMUS}/mu2pul.f
${SRCJADEMUS}/muanab.F
${SRCJADEMUS}/muanac.F
${SRCJADEMUS}/muanaf.F
${SRCJADEMUS}/muana.f
${SRCJADEMUS}/mucon.F
${SRCJADEMUS}/muconw.F
${SRCJADEMUS}/mucoor.F
${SRCJADEMUS}/mucuts.F
${SRCJADEMUS}/mudchk.F
${SRCJADEMUS}/mudump.F
${SRCJADEMUS}/muendp.F
${SRCJADEMUS}/muerry.f
${SRCJADEMUS}/mufflb.F
${SRCJADEMUS}/muffle.F
${SRCJADEMUS}/mufflr.F
${SRCJADEMUS}/muffls.F
${SRCJADEMUS}/mufflt.F
${SRCJADEMUS}/mufflx.F
${SRCJADEMUS}/muffly.F
${SRCJADEMUS}/mufflz.F
${SRCJADEMUS}/mufini.F
${SRCJADEMUS}/mufmat.F
${SRCJADEMUS}/mufsta.F
${SRCJADEMUS}/muhdsp.F
${SRCJADEMUS}/muhit.F
${SRCJADEMUS}/muhits.F
${SRCJADEMUS}/muini.F
${SRCJADEMUS}/muloch.F
${SRCJADEMUS}/mumess.f
${SRCJADEMUS}/muons.F
${SRCJADEMUS}/mupic.F
${SRCJADEMUS}/muregm.f
${SRCJADEMUS}/mureg.F
${SRCJADEMUS}/muregs.f
${SRCJADEMUS}/muregy.F
${SRCJADEMUS}/musafe.f
${SRCJADEMUS}/muwire.F
${SRCJADEMUS}/tanthm.F
${SRCJADEMUS}/xymud.F
)
ADD_LIBRARY(${JADEMUSLIB} jademus ${JADEMUS_src})
#SET(JADESR_src
#)
#ADD_LIBRARY(${JADESRLIB} jadesr ${JADESR_src})

SET(JMC_src
${SRCJMC}/actof.f
${SRCJMC}/brvect.F
${SRCJMC}/datemc.f
${SRCJMC}/eepair.f
${SRCJMC}/endclg.f
${SRCJMC}/errmsg.F
${SRCJMC}/evtini.f
${SRCJMC}/fcrang.f
${SRCJMC}/hdecls.f
${SRCJMC}/inifwd.f
${SRCJMC}/inttof.f
${SRCJMC}/jchamb.f
${SRCJMC}/jdtime.f
${SRCJMC}/jeloss.f
${SRCJMC}/jftest.f
${SRCJMC}/jgetev.f
${SRCJMC}/jgetvc.f
${SRCJMC}/jhitin.f
${SRCJMC}/jinit.f
${SRCJMC}/jipatr.f
${SRCJMC}/jmulsc.f
${SRCJMC}/jprabs.f
${SRCJMC}/jprfre.f
${SRCJMC}/jprthi.f
${SRCJMC}/jprtlo.f
${SRCJMC}/jrchck.f
${SRCJMC}/jring.f
${SRCJMC}/jspoin.f
${SRCJMC}/jstep.f
${SRCJMC}/jstore.f
${SRCJMC}/jtrapz.f
${SRCJMC}/jtrkgm.f
${SRCJMC}/jtrntr.f
${SRCJMC}/jvdtim.f
${SRCJMC}/jvtxch.f
${SRCJMC}/limint.F
${SRCJMC}/mchist.f
${SRCJMC}/mcnews.f
${SRCJMC}/mctagb.f
${SRCJMC}/mctagc.f
${SRCJMC}/mctage.f
${SRCJMC}/mctagf.f
${SRCJMC}/mctagi.F
${SRCJMC}/mctagm.F
${SRCJMC}/mctagr.F
${SRCJMC}/mctags.F
${SRCJMC}/mcvali.F
${SRCJMC}/mcverr.f
${SRCJMC}/mcvsum.f
${SRCJMC}/muconm.f
${SRCJMC}/muordr.f
${SRCJMC}/murtne.f
${SRCJMC}/ordlg.f
${SRCJMC}/pikmuf.f
${SRCJMC}/plligd.f
${SRCJMC}/radiat.f
${SRCJMC}/rncont.f
${SRCJMC}/rnexp0.f
${SRCJMC}/rngau.f
${SRCJMC}/rtmi.f
${SRCJMC}/setbpc.f
${SRCJMC}/settof.f
${SRCJMC}/showr.f
${SRCJMC}/stalgn.f
${SRCJMC}/statag.F
${SRCJMC}/statbp.f
${SRCJMC}/sthead.f
${SRCJMC}/stlatc.f
${SRCJMC}/stpall.F
${SRCJMC}/stvect.F
${SRCJMC}/svect1.f
${SRCJMC}/tags2h.F
${SRCJMC}/tags2h.F
${SRCJMC}/tofmak.f
${SRCJMC}/trcdet.f
${SRCJMC}/trcdtv.f
${SRCJMC}/trcoil.f
${SRCJMC}/trkadc.f
${SRCJMC}/trkgam.f
${SRCJMC}/trkgmv.f
${SRCJMC}/trlgl.f
${SRCJMC}/trlgs6.F
${SRCJMC}/trlgsh.F
${SRCJMC}/wegas.f
${SRCJMC}/wrtmcb.f
)
ADD_LIBRARY(${JMCLIB} jmc ${JMC_src})
SET(PATRECSR_src
${SRCPATRECSR}/adpatr.F ${SRCPATRECSR}/backtr.F ${SRCPATRECSR}/bakfit.F ${SRCPATRECSR}/bakpak.F ${SRCPATRECSR}/bside.F
${SRCPATRECSR}/bstore.F ${SRCPATRECSR}/crtrel.F ${SRCPATRECSR}/deadcl.f ${SRCPATRECSR}/errorm.F
${SRCPATRECSR}/flinel.F ${SRCPATRECSR}/ftrkel.F ${SRCPATRECSR}/fxyz.F ${SRCPATRECSR}/inpatc.F
${SRCPATRECSR}/inpatr.F ${SRCPATRECSR}/kntrel.F ${SRCPATRECSR}/patrc1.F ${SRCPATRECSR}/patrec.F
${SRCPATRECSR}/patrol.F ${SRCPATRECSR}/pcwork.F ${SRCPATRECSR}/prhtar.F ${SRCPATRECSR}/rincon.F
${SRCPATRECSR}/rstbtr.F ${SRCPATRECSR}/srtrel.F ${SRCPATRECSR}/trlord.F ${SRCPATRECSR}/xyfit.F
${SRCPATRECSR}/zrfit.F
)
ADD_LIBRARY(${PATRECSRLIB} patrecsr ${PATRECSR_src})
SET(SOURCE_src
${SRCSOURCE}/angbar.f ${SRCSOURCE}/bbcorr.F ${SRCSOURCE}/bbleak.F ${SRCSOURCE}/brlgn6.f
${SRCSOURCE}/brlgn.F ${SRCSOURCE}/calcor.F ${SRCSOURCE}/elgobs.f ${SRCSOURCE}/encorr.F
${SRCSOURCE}/englos.f ${SRCSOURCE}/enlosg.f ${SRCSOURCE}/ilctrc.f ${SRCSOURCE}/intolg.F
${SRCSOURCE}/lganal9.F ${SRCSOURCE}/lgavde.f ${SRCSOURCE}/lgavdp.f ${SRCSOURCE}/lgbadc.F
${SRCSOURCE}/lgbfil.F ${SRCSOURCE}/lgbhit.f ${SRCSOURCE}/lgbtdc.F ${SRCSOURCE}/lgcalb.F
${SRCSOURCE}/lgcctl.F ${SRCSOURCE}/lgcdir9.F ${SRCSOURCE}/lgchc2.F ${SRCSOURCE}/lgclpb9.F
${SRCSOURCE}/lgclpc.F ${SRCSOURCE}/lgclus.F ${SRCSOURCE}/lgcrn2.f ${SRCSOURCE}/lgcrnr.f
${SRCSOURCE}/lgdcpy.F ${SRCSOURCE}/lgdead.f ${SRCSOURCE}/lgecor9.F ${SRCSOURCE}/lgeerr.f
${SRCSOURCE}/lgefil.F ${SRCSOURCE}/lgehit.f ${SRCSOURCE}/lgeign.F ${SRCSOURCE}/lgerse.F
${SRCSOURCE}/lgesmr.f ${SRCSOURCE}/lginit.f ${SRCSOURCE}/lgmesg.F ${SRCSOURCE}/lgnbls.f
${SRCSOURCE}/lgnmec.f ${SRCSOURCE}/lgrein.F ${SRCSOURCE}/lgsrth.F ${SRCSOURCE}/lgsubc.f
${SRCSOURCE}/lgthcr.f ${SRCSOURCE}/lkcorr.F ${SRCSOURCE}/posend.f ${SRCSOURCE}/shmax.f
${SRCSOURCE}/thcofa.F ${SRCSOURCE}/thcorr.F ${SRCSOURCE}/tpcnst.F
)
ADD_LIBRARY(${SOURCELIB} source ${SOURCE_src})
SET(TAGG_src
${SRCTAGG}/amctag.f ${SRCTAGG}/anatag.F ${SRCTAGG}/conver.f ${SRCTAGG}/ggcctl.f
${SRCTAGG}/ggclpc.f ${SRCTAGG}/ggclus.f ${SRCTAGG}/ggfac2.f ${SRCTAGG}/ggresq.f
${SRCTAGG}/ggsort.f ${SRCTAGG}/ggsrth.f ${SRCTAGG}/gscrub.f ${SRCTAGG}/lumons.f
${SRCTAGG}/rcoord.f ${SRCTAGG}/repack.f ${SRCTAGG}/sort.f ${SRCTAGG}/tagadc.F
${SRCTAGG}/tagan.F ${SRCTAGG}/tagchk.F ${SRCTAGG}/tagcls.F ${SRCTAGG}/tagcol.F
${SRCTAGG}/tagdir.f ${SRCTAGG}/tagfit.F ${SRCTAGG}/tagfty.f ${SRCTAGG}/taggtp.F
${SRCTAGG}/tagh2s.F ${SRCTAGG}/tagint.F ${SRCTAGG}/tagkal.F ${SRCTAGG}/tagmrk.F
${SRCTAGG}/tagneb.F ${SRCTAGG}/tagpd1.F ${SRCTAGG}/tagpd2.F ${SRCTAGG}/tagped.F
${SRCTAGG}/tagphi.f ${SRCTAGG}/tagpos.F ${SRCTAGG}/tagps1.F ${SRCTAGG}/tagps2.f
${SRCTAGG}/tagraw.F ${SRCTAGG}/tagrty.f ${SRCTAGG}/tags2h.F ${SRCTAGG}/tagsr1.F
${SRCTAGG}/tagsr2.F ${SRCTAGG}/tagsto.for ${SRCTAGG}/tagsum.F ${SRCTAGG}/tracks.f
${SRCTAGG}/umcpak.f
)
if (Fortran_COMPILER_NAME MATCHES "xlf.*")
   file(GLOB myfortag ${SRCTAGG}/*.for)
      set_property(SOURCE ${myfortag} PROPERTY COMPILE_FLAGS -qsuffix=f=for)
       endif()
      
ADD_LIBRARY(${TAGGLIB} tagg ${TAGG_src})
SET(TOFLIB_src
${SRCTOFLIB}/agrpr.f ${SRCTOFLIB}/atfugr.f ${SRCTOFLIB}/atofrw.f ${SRCTOFLIB}/atofun.F
${SRCTOFLIB}/cmass.f ${SRCTOFLIB}/corcst.f ${SRCTOFLIB}/mocard.F ${SRCTOFLIB}/rd0880.F
${SRCTOFLIB}/tfarnw.F ${SRCTOFLIB}/tfctd1.F ${SRCTOFLIB}/tfstor.f ${SRCTOFLIB}/tofar2.F
${SRCTOFLIB}/tofard.F ${SRCTOFLIB}/tofbra.f ${SRCTOFLIB}/tofcl2.f ${SRCTOFLIB}/tofclc.F
${SRCTOFLIB}/tofcor.f ${SRCTOFLIB}/tofin2.F ${SRCTOFLIB}/tofint.F ${SRCTOFLIB}/tofma2.f
${SRCTOFLIB}/tofmas.F ${SRCTOFLIB}/tofsm2.f ${SRCTOFLIB}/tofsm3.F ${SRCTOFLIB}/tofsmp.F
)
ADD_LIBRARY(${TOFLIBLIB} toflib ${TOFLIB_src})
SET(VERTEX_src
${SRCVERTEX}/vertex.f ${SRCVERTEX}/vtxaft.f ${SRCVERTEX}/vtxbnk.F ${SRCVERTEX}/vtxee.f
${SRCVERTEX}/vtxini.f ${SRCVERTEX}/vtxpnt.f ${SRCVERTEX}/vtxpre.F ${SRCVERTEX}/vtxsrc.f
)
ADD_LIBRARY(${VERTEXLIB} vertex ${VERTEX_src})
SET(WERTEX_src
${SRCWERTEX}/parkor.F ${SRCWERTEX}/wertex.f ${SRCWERTEX}/wtxaft.f ${SRCWERTEX}/wtxbnk.F
${SRCWERTEX}/wtxcrv.f ${SRCWERTEX}/wtxee.f ${SRCWERTEX}/wtxeer.F ${SRCWERTEX}/wtxerr.f
${SRCWERTEX}/wtxini.f ${SRCWERTEX}/wtxpnt.f ${SRCWERTEX}/wtxpre.F ${SRCWERTEX}/wtxsrc.f
${SRCWERTEX}/xfwid.F ${SRCWERTEX}/xfwvxc.F
)
ADD_LIBRARY(${WERTEXLIB} wertex ${WERTEX_src})
SET(ZLIB_src
${SRCZLIB}/chien52.F ${SRCZLIB}/chien5.F ${SRCZLIB}/crdoca.f ${SRCZLIB}/dxmcgn.f
${SRCZLIB}/ealgcl.F ${SRCZLIB}/eamcde.f ${SRCZLIB}/eaunin.f ${SRCZLIB}/eazero.f
${SRCZLIB}/ekand.f ${SRCZLIB}/expect.F ${SRCZLIB}/fehler.f ${SRCZLIB}/irnevt.f
${SRCZLIB}/jobfin.f ${SRCZLIB}/prallb.f ${SRCZLIB}/prlist.f ${SRCZLIB}/prsinb.f
${SRCZLIB}/prtoci.f ${SRCZLIB}/shado9.F ${SRCZLIB}/shwcpr.f ${SRCZLIB}/trkbl9.F
${SRCZLIB}/trkbse.F ${SRCZLIB}/ze4vdp.f ${SRCZLIB}/ze4vea.f ${SRCZLIB}/ze4vet2.f
${SRCZLIB}/ze4vet.f ${SRCZLIB}/ze4vet.f ${SRCZLIB}/ze4vmc.f ${SRCZLIB}/ze4vpk.for
${SRCZLIB}/ze4vtp.f ${SRCZLIB}/ze4vun.f ${SRCZLIB}/ze4vvx.F ${SRCZLIB}/zmcredu.f
${SRCZLIB}/zsphrcy.f ${SRCZLIB}/zspthak.f
)
if (Fortran_COMPILER_NAME MATCHES "xlf.*")
   file(GLOB myforzlib ${SRCZLIB}/*.for)
      set_property(SOURCE ${myforzlib} PROPERTY COMPILE_FLAGS -qsuffix=f=for)
       endif()
      
ADD_LIBRARY(${ZLIBLIB} zlib ${ZLIB_src})


# Add the source files
SET(JADEZ_src ${SRCJADEZ}/gphmain.F  ${SRCJADEZ}/xuser.F
)

# Define the executable in terms of the source files
ADD_EXECUTABLE(${JADEZEXE} ${JADEZ_src})


#-lpatrecsr -ltoflib -ltagg -lboslib -ljadegs -linterface -ljademus -lsource -lwertex -lpatrecsr
TARGET_LINK_LIBRARIES(${JADEZEXE}  -Wl,--start-group ${GRAFIX} ${BOSLIB} ${TOFLIB} ${TAGG} ${JADEGS} ${INTERFACE} ${JADEMUS} ${SOURCE} ${WERTEX} ${PATRECSR}
 ${JEXTERN_LIBRARIES} ${JEXTERN_GRAPH_LIBRARIES} -Wl,--end-group)

INSTALL(TARGETS ${JADEZEXE} RUNTIME DESTINATION bin)


########################################################################################################################3


SET(JADEZROOT_src ${SRCJADEZ}/gphmain2.F  ${SRCJADEZ}/xuser.F  ${SRCJADEZ}/gphmain.cxx)
ADD_EXECUTABLE(${JADEZROOTEXE} ${JADEZROOT_src})
TARGET_LINK_LIBRARIES(${JADEZROOTEXE}  -Wl,--start-group ${GRAFIX} ${BOSLIB} ${TOFLIB} ${TAGG} ${JADEGS} ${INTERFACE} ${JADEMUS} ${SOURCE} ${WERTEX} ${PATRECSR}
 ${JEXTERN_LIBRARIES} ${JEXTERN_GRAPH_LIBRARIES} -Wl,--end-group)
target_include_directories(${JADEZROOTEXE} PUBLIC /usr/include/root)
INSTALL(TARGETS ${JADEZROOTEXE} RUNTIME DESTINATION bin)


########################################################################################################################3


# Add the source files
SET(MCJADE_src ${SRCMCJADE}/mcjade.f ${SRCMCJADE}/mcmain.f  ${SRCMCJADE}/mcrand.f  ${SRCMCJADE}/mcuser.f ${SRCSUPERV}/showb.f)

ADD_EXECUTABLE(${MCJADEEXE} ${MCJADE_src})
TARGET_LINK_LIBRARIES(${MCJADEEXE} -Wl,--start-group ${JMC} ${BOSLIB} ${JADEGS} ${INTERFACE} ${JADEMUS} ${SOURCE} ${WERTEX} ${PATRECSR} 
${JEXTERN_LIBRARIES} ${JEXTERN_GRAPH_LIBRARIES} -Wl,--end-group)
INSTALL(TARGETS ${MCJADEEXE} RUNTIME DESTINATION bin)





# Add the source files
SET(SUPERV_src ${SRCSUPERV}/ana.F    ${SRCSUPERV}/jdmain.f  ${SRCSUPERV}/showb.f  ${SRCSUPERV}/user.f ${SRCMCJADE}/mcrand.f)

# Define the executable in terms of the source files
ADD_EXECUTABLE(${SUPERVEXE} ${SUPERV_src})


TARGET_LINK_LIBRARIES(${SUPERVEXE} -Wl,--start-group ${JMC} ${BOSLIB} ${JADEGS} ${INTERFACE} ${JADEMUS} ${SOURCE} ${WERTEX} ${PATRECSR}
 ${JEXTERN_LIBRARIES}  ${JEXTERN_GRAPH_LIBRARIES} -Wl,--end-group)




INSTALL(TARGETS ${SUPERVEXE} RUNTIME DESTINATION bin)


# Add the source files
SET(ZE4V_src ${SRCZE4V}/ze4vjb.f ${SRCMCJADE}/mcrand.f ${SRCSUPERV}/showb.f )

# Define the executable in terms of the source files
ADD_EXECUTABLE(${ZE4VEXE} ${ZE4V_src})


TARGET_LINK_LIBRARIES(${ZE4VEXE} -Wl,--start-group ${ZLIB} ${JMC} ${BOSLIB} ${JADEGS} ${INTERFACE} ${JADEMUS} ${SOURCE} ${WERTEX} ${PATRECSR}
 ${JEXTERN_LIBRARIES} -Wl,--end-group)




INSTALL(TARGETS ${ZE4VEXE} RUNTIME DESTINATION bin)


IF(WIN32)
    SET(CMAKE_INSTALL_PREFIX "C:\\Program Files")
ELSE()
    SET(CMAKE_INSTALL_PREFIX /usr/local)
ENDIF(WIN32)

# Add a distclean target to the Makefile
ADD_CUSTOM_TARGET(distclean 
    COMMAND ${CMAKE_COMMAND} -P ${CMAKE_SOURCE_DIR}/distclean.cmake
)
