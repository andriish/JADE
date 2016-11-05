C   01/11/84 807241744  MEMBER NAME  DRWHIT   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE DRWHIT( IPW, LSEP, LDIF, IWR )
C-----------------------------------------------------------------------
C
C    AUTHOR:   R. RAMCKE     23/10/84 :  DRAWS AMPLITUDES FOR VERTEX
C                                        CHAMBER RAW DATA ON SCREEN
C       MOD:   J. HAGEMANN   24/07/85 :  FOR DIFFENCIATED PULSES
C  LAST MOD:   J. HAGEMANN   16/01/86 :  FOR VCVW BANK
C
C      ABSOLUTE SCREEN COORDINATES ARE USED
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL LSEP, LP, LDIF
C
C   23/03/97 703231941  MEMBER NAME  MVCCAL   (PATRECSR)    SHELTRAN
C**HEADER*** MEMBER  MVCCAL         SAVED BY F22KLE  ON 87/02/09  AT 17:52
C   27/06/85 702091751  MEMBER NAME  MVCCAL   (S)           FORTRAN
C
C         THIS MACRO CONTAINS THE DECLARATION FOR THE
C         VTXC-CALIBRATION-COMMON / CVCCAL / :
C
C------------------------------------------------- 03.03.86 J.H. -------
C
      REAL*8 VCDATE
      COMMON / CVCCAL / NVFREE, VFREE(50), VCDATE,
     &      T0, VD, CSLOR, SNLOR, VROT, VDX, VZX, VDY, VZY,
     &      S0R(2,168), CVD(2,168),
     &      VIHCRR(7,2,25)
C
C     NVFREE  : NUMBER OF FREE REAL WORDS IN VFREE
C     VFREE   : REAL ARRAY FOR CONSTANTS
C     T0      : GLOBAL T0                              (. 0.1 NS .)
C     VD      : GLOBAL DRIFTVELOCITY                   (. MM/(0.1 NS) .)
C     CSLOR   : COS( LORENTZ-ANGLE )
C     SNLOR   : SIN( LORENTZ-ANGLE )
C     VROT    : ROTATION VTXC - ID                     (. RADIAN .)
C     VDX     : DISPLACEMENT VTXC - ID IN X            (. MM .)
C     VZX     : SLOPE OF " IN Z
C     VDY     : DISPLACEMENT VTXC - ID IN Y            (. MM .)
C     VZY     : SLOPE OF " IN Z
C     S0R     : DISPLACEMENT FOR EACH WIRE (#)         (. MM .)
C     CVD     : DRIFTVEL.-CORRECTURE FOR EACH WIRE (#)
C
C     VIHCRR  : LAYER-DEPENDENT CORRECTIONS FOR INHOMOGENITIES
C                                                      (. MM . )
C
C     VFREE(50) : D(VROT)/D(Z)                         (. RADIAN/MM .)
C
C     VFREE(48,49) JOBNAME OF JOB GENERATING THIS CALIBRATION
C
C     (#)     : FOR EACH DRIFTSPACESIDE
C
C
      COMMON / CWORK1 / HAMP(2,60), HSTORE(2,420), ISCLOW, ISCHIG
      COMMON / CORDER / KWR(8), KSIDE, KSTART, KEND, KTIMB, KEV
      COMMON / CKORD  / IX1, IX2,
     +                  IY1, IY2
C
      DIMENSION IWRADE(6)
C
C
      DATA IWRADE / 87, 105, 114, 101, 96, 96 /
      DATA ICALL  / 0 /
C
C-----------------  C O D E  -------------------------------------------
C
C                                                FETCH FADC-DATA
      IPREL =(IPW - 1)*60
      DO 60 JS = 1,2
          DO 50 JJ = 1,60
              HAMP(JS,JJ) = HSTORE(JS, JJ + IPREL)
  50      CONTINUE
  60  CONTINUE
C
      IF( .NOT. LDIF ) GO TO 100
         DO 90 JS = 1, 2
            IPD2 = 0
            IF( HAMP(JS,60) .GE. 300 ) IPD2 = 300
            HAMP(JS,60) = IPD2
C
            DO 80 JJ = 1, 58
               H1 = HAMP(JS,59-JJ)
               H2 = HAMP(JS,60-JJ)
               IPD1 = 0
               IF( H1 .GE. 300 ) IPD1 = 300
               IPD2 = 0
               IF( H2 .GE. 300 ) IPD2 = 300
               H1 = H1 - IPD1
               H2 = H2 - IPD2
               HAMP(JS,60-JJ) = H2 - H1 + IPD2
  80        CONTINUE
C
            IPD2 = 0
            IF( HAMP(JS,1) .GE. 300 ) IPD2 = 300
            HAMP(JS,1) = IPD2
C
  90  CONTINUE
C
 100  IF( .NOT. LSEP ) GOTO 120
         KSIDE = 1
         IX1 = 300
         IX2 = 2100
 120  IF( ICALL .GT. 0 ) GOTO 130
         KRANGE = KEND - KSTART
         IYMAX  = (IY2 - IY1)/KWR(8) - 50
         ISCRCH = IX2 - IX1
C
 130  IYO = IY1  + (IYMAX + 50)*ICALL
      IF( LDIF ) IYO = IYO  + (IYMAX*3)/8
C
C
      IT0OFS = IFIX((0.5/VD*(S0R(1,IWR)+S0R(2,IWR))+T0)*.01
     *                                  *FLOAT(ISCRCH)/FLOAT(KRANGE))
      IXO    = IX1 - IT0OFS
      IX     = IXO
      IY     = IYO
C
C                                               DRAW FADC-DATA
      LSIDE = KSIDE
 150  IF( LSIDE .EQ. 3 ) LSIDE = 2
      CALL MOVABS(IX,IY)
      KSTR1 = KSTART + 1
      DO 200 IREP = KSTR1,KEND
         LP = .FALSE.
         IF( HAMP(LSIDE,IREP) .LT. 200 ) GOTO 155
            HAMP(LSIDE,IREP) = HAMP(LSIDE,IREP) - 300
            LP = .TRUE.
 155     IDY = IYMAX*HAMP(LSIDE, IREP)/(ISCHIG-ISCLOW)
         IF( IDY .GE. IYMAX ) IDY = IYMAX
         IYY = IY + IDY
         IF( KSIDE .NE. 3  .OR.  LSIDE .NE. 2 ) GOTO 180
            CALL DSHABS(IX, IYY, 34)
            IX = IXO + ISCRCH*(IREP-KSTART)/KRANGE
            IF( IDY .LT. IYMAX ) GOTO 170
               CALL DSHABS(IX, IYY, 1)
               GOTO 200
 170        CALL DSHABS(IX, IYY, 34)
            GOTO 200
 180     CONTINUE
         CALL DRWABS(IX, IYY)
         IX = IXO + ISCRCH*(IREP-KSTART)/KRANGE
         IF( IDY .LT. IYMAX ) GOTO 190
            CALL DSHABS(IX, IYY, 1)
            GOTO 200
 190     IF( LP ) CALL DSHABS(IX,IYY,1)
         IF( .NOT. LP ) CALL DRWABS(IX, IYY)
 200  CONTINUE
C
C                                                REPEAT IF KSIDE = BOTH
      IF( KSIDE .NE. 3  .OR.  LSIDE .NE. 2 ) GOTO 210
      LSIDE = 1
      IX = IXO
      GOTO 150
 210  CONTINUE
C                                                WRITE WIRE NUMBER
      IYWR = 100
      IF( LDIF ) IYWR = 190
      CALL MOVABS(IX1 + 50, IYO + IYMAX - IYWR)
      CALL CHRSIZ(4)
      IWRADE(6) = IPW + 48
      CALL HLABEL(6,IWRADE)
C
      IF( (.NOT. LSEP)  .OR.  KSIDE .NE. 1 ) GOTO 220
         KSIDE = 2
         IX1 = 2200
         IX2 = 4000
         GOTO 120
C
 220  ICALL = ICALL + 1
      IF( KWR(8) .NE. ICALL ) RETURN
      ICALL = 0
C
      RETURN
      END
