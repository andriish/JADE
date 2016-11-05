      SUBROUTINE VTXINI 
C*800623*DITTMANN***************************************************
C*  COPIED FROM F22KLE.VERTEX.S(VTXINI)   18.9.86
C*                                                                 *
C* I N I T I A L I S A T I O N   O F   X Y Z   V E R T E X   F I T *
C*                                                                 *
C*       TO BE CALLED ONCE AND BEFORE FIRST CALL TO VTXPRE, VERTEX *
C*                                                                 *
C*    MODIFIED FOR VERTEX CHAMBER HARDWARE     860211 C.K.         *
C*       ADDITIONAL CONSTANTS ARE TAKEN FROM COMMON /CGEOV/        *
C*       JADEBD MUST BE DECLARED AS EXTERNAL                       *
C*                                                                 *
C*    MODES AND STATISTICS                        860527 C.K.      *
C*                                                                 *
C*******************************************************************
      IMPLICIT INTEGER*2 (H)
C PMF 03.11.98 
      LOGICAL TBIT
      COMMON /CVTXC/ XB,YB,ZB,RTANK,DTANK,X0INN,SIGX0,SIGZ0,PNTMIN,
     +               DISTB,COLL2,MITER,DSCONV,PRCUT,IREJTR,EEDPMN,
     +               EEDPMX,EEDTMX,EEDRMX,SEMAX,SIMAX,SIGFAC,EEXYMN,
     +               EEXYMX,PHEMAX,SIG1,SIG2,SIG3,CSECV
     +               ,ITDLEN,IVDLEN       ! PMF 03/12/99
*** PMF 03/12/99: ITDLEN,IVDLEN needed for graphics ( commands VFIT, VX in JADEZ)
C
C%MACRO MVERTEX2
C     MACRO FOR VERTEX-FIT ROUTINES ( AXIS AND STATISTICS )
      COMMON /CVTX2/ MODE,TAXIS(12),SVR,HVTXST(120)
C
      DIMENSION IVTXST(1)
**** PMF 03/12/99: Tell graphics routines GVTXFT and VXDISP
*     the number of words for each track ( "ITDLEN" ) and for each
*     vertex ( "IVDLEN" ).
*     NB.: This communication via COMMON /CVTXC/ is NOT needed
*     for the functionality of the 'wertex' routines themselves
*     in which these parameters are fixed.
      ITDLEN = 40
      IVDLEN = 10
*** PMF(end)
C
C
C        RESET MODE AND CLEAR STATISTICS
      MODE = 0
      DO 10 I = 1, 120
         HVTXST(I) = 0
   10 CONTINUE
C
C        MEAN VERTEX COORDINATES
      XB = 0.
      YB = 0.
      ZB = 0.
C        OUTER RADIUS OF INNER TANK WALL
      RTANK = 174.
C        OUTER DISTANCE BEAM PIPE TO TANK WALL
      DTANK = 81.
C
C THE VARIABLES DTANK AND X0INN ARE UPDATED IN VTXPRE ACC. TO JADE
C STATUS, I.E. OLD OR NEW BEAMPIPE AND BP-COUNTERS OR VTX CHAMBER
C
C        RADIATION LENGTH BETWEEN BEAM AND FIRST WIRE
      X0INN = 0.1312
C        MEAN TRACK RESIDUAL IN XY AND ZR PLANE
      SIGX0 = 0.55
      SIGZ0 = 30.
C        ARTIFICIAL FACTOR TO ACCOUNT FOR SYSTEMATIC ERRORS
      SIGFAC = 1.0
      SIGX0 = SIGX0 * SIGFAC
      SIGZ0 = SIGZ0 * SIGFAC
C        MINIMUM NUMBER OF POINTS IN XY AND Z TRACK
      PNTMIN = 5.
C        MAXIMUM TRACK RESIDUAL IN XY AND Z (SIG<SIG1*SIGX0)
      SIG1 = 4.
C        MAXIMUM DISTANCE OF TRACKS TO AVERAGE BEAM
C        USED IN PRIMARY VERTEX SEARCH
      DISTB = 20.
C        MAXIMUM OPENING ANGLE OF COLLINEAR 2-PRONGS
      COLL2 = 0.99
C        MAXIMUM NUMBER OF ITERATIONS IN VERTEX FIT
      MITER = 12
C        CONVERGENCE PARAMETER
      DSCONV = 0.05
C        MINIMUM PROBABILITY FOR GOOD VERTEX
      PRCUT = 0.001
C        REJECT BAD TRACKS DURING VERTEX FIT (0=NO, 1=YES)
      IREJTR = 1
C        EE PAIRS: MINIMUM AND MAXIMUM MEASURED PHI DIFFERENCE (RADIAN)
      EEDPMN = -0.07
      EEDPMX =  0.8
C        EE PAIRS: MAXIMUM THETA DIFFERENCE (STD.DEV.)
      EEDTMX = 3.0
C        EE PAIRS: MAXIMUM DISTANCE WHERE TRACKS ARE PARALLEL (STD.DEV.)
      EEDRMX = 3.0
C        EE PAIRS: MAXIMUM POSITIVE XY DISTANCE, NO ST.DEV. CUT (MM)
      EEXYMN = 4.0
C        EE PAIRS: MAXIMUM XY DISTANCE WHERE TRACKS ARE PARALLEL (MM)
      EEXYMX = 20.
C        MAXIMUM TRACK EXTRAPOLATION AND INTRAPOLATION(ARC LENGTH)
      SEMAX = -300.
      SIMAX = 25.
C        MAXIMUM TRACK EXTRAPOLATION (PHI)
      PHEMAX = 1.0
C        MAXIMUM TRACK DISTANCE TO VERTEX DURING FIT (STD.DEV.)
      SIG2 = 2.
C        MAXIMUM TRACK DISTANCE TO VERTEX AFTER FIT (STD.DEV.)
      SIG3 = 4.
C        SEC. VERTICES: MINIMUM COS OF TRACK TO MAIN VERTEX (OR XB,YB)
      CSECV = -0.15
C
      RETURN
      END
