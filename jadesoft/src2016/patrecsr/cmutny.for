C ============= MACRO CMUTNY =================================
C   /CMUTNY/
C
C CONDENSED MU-FILTER PAARAMETERS FOR USE BY APPROXIMATE SIGNAL TO
C   COORDINATE CONVERSION SUBROUTINE MUTINY.
C
C HPLANS      NO. OF CHAMBER PLANES.
C HVDRAV      AVERAGE DRIFT VELOCITY.
C HDTPAV      AVERAGE DRIFT TIME PEDESTAL.
C HLTPAV      AVERAGE LONGITUDINAL TIME DIFFERENCE PEDESTAL.
C HLSFAV      AVERAGE LONGITUDINAL SCALING FACTOR.
C
C FOR EACH CHAMBER PLANE...
C
C HLY         LAYER NUMBER.
C HOR         ORIENTATION PARAMETER:
C             =1, WIRES PARALLEL TO BEAM, AND NORMAL PARALLEL TO
C                   X-AXIS - FACES 1(-X) AND 2(+X).
C             =2, WIRES PARALLEL TO BEAM, AND NORMAL PARALLEL TO
C                   Y-AXIS - FACES 3(-Y) AND 4(+Y).
C             =3, WIRES VERTICAL, AND NORMAL PARALLEL TO Z-AXIS -
C                   FACES 5(-Z) AND 6(+Z).
C HC1         FIRST CHAMBER NUMBER.
C HCN0        NORMAL       )
C HCL0        LONGITUDINAL )  COORDINATE OF 'ORIGIN' OF CHAMBER PLANE.
C HCT0        TRANSVERSE   )
C HSP         AVERAGE SPACING OF CHAMBERS.
C
C   (THE 'ORIGIN' IS AT ONE END OF THE WIRE OF THE FIRST CHAMBER IN THE
C PLANE.  THE END IS THAT WITH THE LOWEST LONGITUDINAL COORDINATE.)
C
      COMMON /CMUTNY/HPLANS,HVDRAV,HDTPAV,HLTPAV,HLSFAV,
     + HLY(48),HOR(48),HC1(48),HCN0(48),HCL0(48),HCT0(48),HSP(48)
C  ========= ENDMACRO CMUTNY =======================================