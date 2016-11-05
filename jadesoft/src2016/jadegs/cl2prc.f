C   04/08/86 611261031  MEMBER NAME  CL2PRC   (JADEGS)      FORTRAN77
      INTEGER FUNCTION CL2PRC( CELL )
C-----------------------------------------------------------
C  VERSION OF 04/08/86    LAST MOD 04/08/86    E ELSEN
C  CONVERT JETC CELL NUMBER ( 0-95 ) TO PROCESSOR NUMBER
C-----------------------------------------------------------
C
      INTEGER CELL
      INTEGER PRMAP(0:31) /
     +  26, 27,  8,  9, 10, 11, 24, 25,
     +  30, 31, 12, 13, 14, 15, 28, 29,
     +  20, 21, 22, 23,  0,  1,  2,  3,
     +   4,  5,  6,  7, 16, 17, 18, 19 /
C
      IF( CELL .LT. 0 .OR. CELL .GT. 95 ) THEN
        CL2PRC = -1
      ELSE
        CL2PRC = PRMAP(CELL/3)
      ENDIF
      END
      INTEGER FUNCTION PRC2CL( PROC )
C-----------------------------------------------------------
C  VERSION OF 04/08/86    LAST MOD 04/08/86    E ELSEN
C  CONVERT PROCESSOR NUMBER ( 0-31 ) TO NUMBER OF FIRST
C  CELL IN THAT PROC
C-----------------------------------------------------------
C
      INTEGER PROC, CL2PRC
      INTEGER CLMAP(0:31)
      INTEGER ICALL / 0 /
      IF( ICALL .EQ. 0 ) THEN
        ICALL = 1
        DO 10 I=0,95,3
          CLMAP(CL2PRC(I)) = I
   10   CONTINUE
      ENDIF
C
      IF( PROC .LT. 0 .OR. PROC .GT. 31 ) THEN
        PRC2CL = -1
      ELSE
        PRC2CL = CLMAP(PROC)
      ENDIF
      END
      REAL FUNCTION CL2HEU( CELL )
C-----------------------------------------------------------
C   VERSION OF 19/08/86     LAST MOD 19/08/86    E ELSEN
C   CONVERT CELL NUMBER INTO HEUER FORMAT
C-----------------------------------------------------------
      INTEGER CELL
      IF( CELL .LT. 48 ) THEN
        K = (MOD(CELL,24)+1)*10 + CELL/24 + 1
      ELSE
        K = (MOD(CELL/2,24)+1)*10 + 3+MOD(CELL,2)
      ENDIF
      CL2HEU = .1*K
      END
      FUNCTION IT0GLB( IRUN )
C-----------------------------------------------------------
C  VERSION OF 14/08/86          LAST MOD 05/11/86  E ELSEN
C  GLOBAL T0 THAT WAS SUBTRACTED FROM RAW JETC TIMES
C-----------------------------------------------------------
      INTEGER IRUN
C                                           PRE DL300 DATA
      IF( IRUN.LT. 24201 ) THEN
        IT0GLB = 0
C                                           MATSUS T0 BOX FROM HERE ON
      ELSEIF( IRUN.GT. 24698 ) THEN
        IT0GLB = 1088
C                                           HERE IS THE INITIAL TRYING
C                                           FIRST WEEK
      ELSEIF( IRUN.LT. 24227 ) THEN
        IT0GLB = 1283
C                                           BY MISTAKE TO TIGHT
      ELSEIF( IRUN.LT. 24246 ) THEN
        IT0GLB = 1430
C                                           PRE MATSU
      ELSE
        IT0GLB = 1130
      ENDIF
      RETURN
      END
      FUNCTION T0EFFC( IRUN )
C-----------------------------------------------------------
C  VERSION OF 05/11/86          LAST MOD 05/11/86  E ELSEN
C  EFFECTIVE T0 TO BE APPLIED TO JETC DATA TAKING INTO
C  ACCOUNT THE SOFTWARE CHANGES OF IT0GLB AND THE EFFECT
C  OF HARDWARE CHANGES DUE TO CHANGED GATES ETC.
C  UNITS ARE OLD DL8 COUNTS
C-----------------------------------------------------------
      INTEGER IRUN
C                                           PRE DL300 DATA
      IF( IRUN.LT. 24200 ) THEN
        T0EFFC = 0.
C                                           MATSUS T0 BOX FROM HERE ON
C                                           THIS IS THE REFERENCE
      ELSEIF( IRUN.GT. 24698 ) THEN
        T0EFFC = 0.
C                                           HERE IS THE INITIAL TRYING
C                                           FIRST WEEKS
      ELSEIF( IRUN.LT. 24227 ) THEN
        T0EFFC =  2.3125
      ELSEIF( IRUN.LT. 24233 ) THEN
        T0EFFC =  4.6094
      ELSEIF( IRUN.LT. 24246 ) THEN
        T0EFFC =  6.9531
      ELSEIF( IRUN.LT. 24405 ) THEN
        T0EFFC =  2.3125
      ELSE
        T0EFFC = -0.0781
      ENDIF
      RETURN
      END
