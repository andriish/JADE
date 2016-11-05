SET(WERTEX_src
${SRCWERTEX}/parkor.F
${SRCWERTEX}/wertex.f
${SRCWERTEX}/wtxaft.f
${SRCWERTEX}/wtxbnk.F
${SRCWERTEX}/wtxcrv.f
${SRCWERTEX}/wtxee.f
${SRCWERTEX}/wtxeer.F
${SRCWERTEX}/wtxerr.f
${SRCWERTEX}/wtxini.f
${SRCWERTEX}/wtxpnt.f
${SRCWERTEX}/wtxpre.F
${SRCWERTEX}/wtxsrc.f
${SRCWERTEX}/xfwid.F
${SRCWERTEX}/xfwvxc.F
)
ADD_LIBRARY(${WERTEXLIB} STATIC ${WERTEX_src})
