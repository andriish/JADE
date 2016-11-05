SET(VERTEX_src
${SRCVERTEX}/vertex.f
${SRCVERTEX}/vtxaft.f
${SRCVERTEX}/vtxbnk.F
${SRCVERTEX}/vtxee.f
${SRCVERTEX}/vtxini.f
${SRCVERTEX}/vtxpnt.f
${SRCVERTEX}/vtxpre.F
${SRCVERTEX}/vtxsrc.f
)
ADD_LIBRARY(${VERTEXLIB} STATIC ${VERTEX_src})
