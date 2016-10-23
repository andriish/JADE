C   15/03/80 003281450  MEMBER NAME  MUREGY   (JADEMUS)     FORTRAN
C=======================<< MUREGY >>====================================
C THIS ROUTINE FINDS WHICH REGIONS OF THE MUON FILTER LIE ALONG
C A GIVEN STRAIGHT LINE AND CALCULATES THE INTERCEPTS OF THE LINE
C WITH THE PLANES BOUNDING THE REGIONS.
C=======================================================================
C  HARRISON B. PROSPER   APRIL 1979    J A D E
C=======================================================================
C
C
C
C
C
C====================<< I N T E R C E P T S >>==========================
C
C  THE EQUATION OF A PLANE CAN BE WRITTEN AS   L1.X = P  WHERE P IS THE
C  PERPENDICULAR DISTANCE OF THE PLANE FROM THE ORIGIN,AND X = (X,Y,Z)
C  IS ANY POINT IN THE PLANE. L1 = (L1,M1,N1) IS THE DIRECTION COSINE
C  VECTOR OF THE NORMAL TO THE GIVEN PLANE.
C
C  THE EQUATION OF A LINE CAN BE WRITTEN AS  R = R2 + A*L2
C
C  WHERE  L2=(L2,M2,N2) , R2=(X2,Y2,Z2) (A GIVEN POINT ON THE LINE),AND
C
C  R=(X,Y,Z) IS ANY POINT ON THE LINE.
C
C  LET T = ( P - L1.R2 )/( L1.L2)    ( HERE SCALAR PRODUCT IS IMPLIED)
C
C  LET W = (X3,Y3,Z3)  BE THE RADIUS VECTOR OF THE INTERSECTION OF THE
C
C  LINE WITH THE PLANE. W IS GIVEN BY:
C                    -----------------
C
C                      W = T*L2 + R2
C
C                    -----------------
C
C T IS THE DISTANCE OF A POINT (ALONG THE LINE) FROM THE GIVEN POINT
C
C=======================================================================
C
C
C
C
C
C===================<< I N F O R M A T I O N >>=========================
C
C        BELOW (1,2,3)=(X,Y,Z)
C
C        DCS(1) = X2
C        DCS(2) = Y2    POINT ON LINE ( E.G. 'FIRST' POINT )
C        DCS(3) = Z2
C
C        DCS(4) = L2
C        DCS(5) = M2    DIRECTION COSINES OF LINE
C        DCS(6) = N2
C
C        A REGION 'I' IS DEFINED BY THE BOUNDARIES OF A RECTANGULAR
C        PRISM. XRLO(I),XRHI(I),...ETC. ARE THE LOWER AND UPPER
C        LIMITS RESPECTIVELY.
C
C   B)   HRTYPE(I) INDICATES THE NATURE OF THE REGION.I.E:
C
C        1...MUON CHAMBERS
C
C        2...CONCRETE REGION
C
C        3...IRON REGION
C
C
C   C)   HRORI(I) (=1,2,OR,3) GIVES THE DIRECTION OF THE PRINCIPAL
C        NORMAL TO THE REGION (I.E. THE NORMAL TO THE LARGEST FACE)
C   D)   NFLAG(J) (J=1,N) GIVES THE ORIGINAL POSITION OF THE  J TH
C        ELEMENT IN THE ORDERED LIST
C   E)   N IS THE TOTAL NUMBER OF INTERCEPTS
C
C NOTE:  MUREGS OUTPUTS THE SMALLEST ELEMENT FIRST
C
C
C   F)   IF N1 = 0  MUREGY WILL CONSIDER   A L L   REGIONS
C         ( N1 = 1  FOR CHAMBER REGIONS   O N L Y  )
C         ( N1 = 2  FOR CONCRETE REGIONS  O N L Y  )
C         ( N1 = 3  FOR IRON   REGIONS    O N L Y  )
C
C   G)   IF N2 = 0  MUREGY WILL FIND REGIONS IN BOTH DIRECTIONS
C        IF N2 = 1  MUREGY WILL FIND REGIONS WHOSE ENTRY FACES LIE
C                   AHEAD OF THE GIVEN POINT
C        IF N2 = 2  MUREGY WILL FIND REGIONS WHOSE EXIT FACES LIE
C                   AHEAD OF THE GIVEN POINT
C
C
C=====<< ERROR  CODES >>================================================
C
C   H) 1) NER = -1 IF ALL OK
C      2) NER =  0 IF DIRECTION COSINES ARE INVALID
C      3) NER =  1 IF TRACK IS OUTSIDE DETECTOR
C      4) NER =  2 IF NO REGIONS INTERCEPTED ( BUT TRACK NONTHELESS
C                  PASSES THROUGH THE DETECTOR )
C      5) NER =  3 IF THE INPUTTED MASK IS INVALID
C=======================================================================
C     FOR A DESCRIPTION OF HMASK PLEASE SEE MUREGM( ON CALIB.S)
C=======================================================================
      SUBROUTINE MUREGY(N1,N2,NER,DCS,HMASK,N)
      IMPLICIT INTEGER*2 (H)
      DIMENSION DCS(6),TH(3),TL(3),T(100,2,3)
#include "cmtemp.for"
#include "cmureg.for"
      EQUIVALENCE(T(1,1,1),XRLO(1))
C=======================================================================
              MER = 0
                N = 0
              NER = 0
C=======================================================================
C==STEP 1 =========== CHECK DIRECTION COSINES ==========================
C=======================================================================
      IF(ABS(DCS(4)).GT.1.01)                    GOTO 97
      IF(ABS(DCS(5)).GT.1.01)                    GOTO 97
      IF(ABS(DCS(6)).GT.1.01)                    GOTO 97
      SUM = ABS(DCS(4)) + ABS(DCS(5)) + ABS(DCS(6))
      IF(SUM.LT.0.99.OR.SUM.GT.1.75)             GOTO 97
C=======================================================================
C
C
C=======================================================================
C==STEP 2 ==== CHECK IF TRACK PASSES THROUGH THE DETECTOR ==============
C=======================================================================
      DO 81 K = 1,3
      GOTO(76,77,78),K
  76  CONTINUE
      L1 = 5
      L2 = 10
      GOTO 79
  77  CONTINUE
      L1 = 16
      L2 = 32
      GOTO 79
  78  CONTINUE
      L1 = 40
      L2 = 48
  79  CONTINUE
      IF(ABS(DCS(K+3)).LT.1.E-07)      GOTO 80
           T1 = ( T(L1,1,K) - DCS(K) )/DCS(K+3)
           T2 = ( T(L2,2,K) - DCS(K) )/DCS(K+3)
        TH(K) = AMAX1( T1,T2 )
        TL(K) = AMIN1( T1,T2 )
                                       GOTO 81
  80  CONTINUE
      IF(DCS(K).LT.T(L1,1,K).OR.DCS(K).GT.T(L2,2,K)) GOTO 84
        TH(K) = 1.E07
        TL(K) =-1.E07
  81  CONTINUE
C
                   KH = 1
                   KL = 1
                   SL = TL(1)
                   SH = TH(1)
      DO 83 J=2,3
                   IF(TL(J).LE.SL)     GOTO 82
                   KL = J
                   SL = TL(J)
  82  CONTINUE
                   IF(TH(J).GE.SH)     GOTO 83
                   KH = J
                   SH = TH(J)
  83  CONTINUE
                   IF(TL(KL).LT.TH(KH))GOTO 85
  84  CONTINUE
                   NER = 1
                                       GOTO 97
C=======================================================================
C.....STEP 3
C           LOOP OVER ALL REGIONS
C           AND WHERE APPROPRIATE CALCULATE INTERCEPTS
  85  CONTINUE
       DO 93 I = 1,NREGS
C
C PERFORM A BIT BY BIT .AND. OF THE CORRESPONDING BITS OF HRMASK(I) *
C HMASK,AND THEN .OR. THE RESULT. IF THE 'TRUTH' VALUE IS 0 THEN IGNORE
C THIS REGION (SINCE IT DOES NOT BELONG TO A FACE SPECIFIED BY THE MASK)
C
      IF(hLAND(HRMASK(I),HMASK).NE.0)  GOTO 86 ! PMF 10/06/99: hland
C**** HARRY , WAS THIS INTENDED ?---> GOTO 86   **********
C.... ERROR CODE (3)
      MER = MER + 1
                                      GOTO 93
C IF N1.EQ.0 THEN  REGIONS OF ALL TYPES ARE TO BE LOOPED OVER
C
  86  CONTINUE
      IF(N1.EQ.0)                     GOTO 87
C
C IF N1.NE.0 IT MAY BE EQUAL TO (1,2,3,OR 4) IN WHICH CASE CONSIDER
C ONLY THE APPROPRIATE REGION
C
      JTYPE = HRTYPE(I)
      IF(JTYPE.NE.N1)                 GOTO 93
  87  CONTINUE
C
C
C=======================================================================
C=========== C A L C U L A T E       I N T E R C E P T S===============
C=======================================================================
C CALCULATE THE DISTANCES FROM THE GIVEN POINT ON THE LINE TO THE POINTS
C OF INTERSECTION OF THE LINE WITH THE 6 PLANES BOUNDING THE REGION.
C FOR EACH PAIR OF PLANES DETERMINE WHICH INTERCEPT IS THE GREATER AND
C WHICH THE SMALLER.CALL THE GREATER ONE TH(K) I.E. AN EXIT POINT,
C AND THE SMALLER TL(K) I.E. AN ENTRY POINT.
C
      DO 89 K = 1,3
      IF(ABS(DCS(K+3)).LT.1.E-07)      GOTO 88
           T1 = ( T(I,1,K) - DCS(K) )/DCS(K+3)
           T2 = ( T(I,2,K) - DCS(K) )/DCS(K+3)
        TH(K) = AMAX1( T1,T2 )
        TL(K) = AMIN1( T1,T2 )
                                       GOTO 89
  88  CONTINUE
      IF(DCS(K).LT.T(I,1,K).OR.DCS(K).GT.T(I,2,K)) GOTO 93
        TH(K) = 1.E07
        TL(K) =-1.E07
  89  CONTINUE
C=======================================================================
C====== DETERMINE   'E N T R Y'   AND   'E X I T'    P O I N T S =======
C=======================================================================
C FIND THE MINIMUM OF THE THREE TH(K)'S AND THE MAXIMUM OF THE THREE
C TL(K)'S.THE ENTRY POINT FURTHEST AWAY FROM THE GIVEN POINT ON THE LINE
C IS POTENTIALLY THE ENTRY POINT INTO THE REGION.THE EXIT POINT NEAREST
C THE GIVEN POINT ON THE LINE IS POTENTIALLY THE EXIT POINT OUT OF THE
C REGION.
                   KH = 1
                   KL = 1
                   SL = TL(1)
                   SH = TH(1)
      DO 91 J=2,3
                   IF(TL(J).LE.SL) GOTO 90
                   KL = J
                   SL = TL(J)
  90  CONTINUE
                   IF(TH(J).GE.SH) GOTO 91
                   KH = J
                   SH = TH(J)
  91  CONTINUE
C=======================================================================
C============== TEST IF LINE PASSES THRU' REGION =======================
C=======================================================================
C  IF THE DISTANCE FROM THE GIVEN POINT ON THE LINE TO THE POTENTIAL
C  ENTRY POINT IS GREATER THAN THAT OF THE POTENTIAL EXIT POINT THEN THE
C  LINE FAILS TO INTERCEPT THE CURRENT REGION.(THIS IS NOTHING MORE THAN
C  THE TOPOLOGICAL REQUIREMENT THAT TO GET OUT OF A REGION WITH AN
C  INTERIOR DISCONECTED FROM THE EXTERIOR ONE MUST HAVE AT SOME STAGE
C  ENTERED THROUGH THE BOUNDARY.NOTE ALSO THAT A CONVEX REGION (ROUGHLY
C  SPEAKING ONE WITHOUT DIMPLES) HAS ONLY ONE ENTRY POINT AND ALSO ONLY
C  ONE EXIT POINT.
      IF(TL(KL).GT.TH(KH)) GOTO 93
C.....IF N2.EQ.0 FIND REGIONS IN BOTH DIRECTIONS
      IF(N2.EQ.0)          GOTO 92
C.....IF N2.EQ.2 FIND REGIONS WHOSE EXIT FACES LIE AHEAD
C.....OF THE GIVEN POINT ON THE LINE
      IF(TH(KH).LT.0.0.AND.N2.EQ.2)    GOTO 93
C
C.....IF N2.EQ.1 FIND REGIONS WHOSE ENTRY FACES LIE AHEAD
C.....OF THE GIVEN POINT ON THE LINE
      IF(TL(KL).LT.0.0.AND.N2.EQ.1)    GOTO 93
C=======================================================================
C     ACCUMULATE  RESULTS IN OUTPUT ARRAYS
  92  CONTINUE
             N = N + 1
      RDOTD(N) = TL(KL)
       ITEM(N) = I
             N = N + 1
      RDOTD(N) = TH(KH)
       ITEM(N) = I
  93  CONTINUE
C=======================================================================
      IF(N.LT.1) GOTO 96
C=======================================================================
C==STEP 4 ============== ORDER THE INTERCEPTS ==========================
C=======================================================================
      CALL MUREGS(RDOTD,N,NFLAG)
      DO 95 K=1,N
      DO 94 L=1,3
      CINT(L,K) = RDOTD(K)*DCS(L+3) + DCS(L)
  94  CONTINUE
      IREG(K) = ITEM(NFLAG(K))
  95  CONTINUE
C=======================================================================
C     SET ERROR   CODES
      NER =-1
      GOTO 97
  96  CONTINUE
      NER = 2
      IF(MER.EQ.NREGS)    NER = 3
C=======================================================================
  97  CONTINUE
      RETURN
      END
