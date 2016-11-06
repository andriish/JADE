SET(TAGG_src
${SRCTAGG}/amctag.f
${SRCTAGG}/anatag.F
${SRCTAGG}/conver.f
${SRCTAGG}/ggcctl.f
${SRCTAGG}/ggclpc.f
${SRCTAGG}/ggclus.f
${SRCTAGG}/ggfac2.f
${SRCTAGG}/ggresq.f
${SRCTAGG}/ggsort.f
${SRCTAGG}/ggsrth.f
${SRCTAGG}/gscrub.f
${SRCTAGG}/lumons.f
${SRCTAGG}/rcoord.f
${SRCTAGG}/repack.f
${SRCTAGG}/sort.f
${SRCTAGG}/tagadc.F
${SRCTAGG}/tagan.F
${SRCTAGG}/tagchk.F
${SRCTAGG}/tagcls.F
${SRCTAGG}/tagcol.F
${SRCTAGG}/tagdir.f
${SRCTAGG}/tagfit.F
${SRCTAGG}/tagfty.f
${SRCTAGG}/taggtp.F
${SRCTAGG}/tagh2s.F
${SRCTAGG}/tagint.F
${SRCTAGG}/tagkal.F
${SRCTAGG}/tagmrk.F
${SRCTAGG}/tagneb.F
${SRCTAGG}/tagpd1.F
${SRCTAGG}/tagpd2.F
${SRCTAGG}/tagped.F
${SRCTAGG}/tagphi.f
${SRCTAGG}/tagpos.F
${SRCTAGG}/tagps1.F
${SRCTAGG}/tagps2.f
${SRCTAGG}/tagraw.F
${SRCTAGG}/tagrty.f
${SRCTAGG}/tags2h.F
${SRCTAGG}/tagsr1.F
${SRCTAGG}/tagsr2.F
${SRCTAGG}/tagsto.for
${SRCTAGG}/tagsum.F
${SRCTAGG}/tracks.f
${SRCTAGG}/umcpak.f
)
if (Fortran_COMPILER_NAME MATCHES "xlf.*")
   file(GLOB myfortag ${SRCTAGG}/*.for)
      set_property(SOURCE ${myfortag} PROPERTY COMPILE_FLAGS -qsuffix=f=for)
       endif()
      
ADD_LIBRARY(${TAGGLIB} tagg ${TAGG_src})
