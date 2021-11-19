C   15/02/84 709012313  MEMBER NAME  DTHCV1   (S)        M  FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE DTHCV1( FRAC1 , LARGE , SMALL )
C-----------------------------------------------------------------------
C
C   AUTHOR:   J. OLSSON    05/05/80 :  DRAW THEORETICAL DEDX CURVES
C
C        MOD: C. BOWDERY   15/02/84 :  MOVE CODE TO THIS SUBROUTINE
C                                   :  ADD LABELS TO CURVES
C
C
C   LAST MOD: J. OLSSON    01/09/87 :  AMBRUS VERSION OF DEDX THEORY
C     DRAW "THEORETICAL" DE/DX CURVES FROM K. AMBRUS
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL  SMALL, LARGE, DSPDTM
C
      COMMON / CGRAP2 / BCMD, DSPDTM(30)
C
      DIMENSION  ARMAS(5) , PLABEL(6)
C
      DATA ARMAS / 0.000511, 0.105659, 0.139567, 0.49367, 0.93828 /
C
C                       LOWER CASE 'e', BLANK, BLANK, K, LOWER CASE p
C
      CHARACTER PLABEL*2
      DATA PLABEL / 'e ', '  ', '  '  , 'K ', 'p ', '  '  /
C
C------------------  C O D E  ------------------------------------------
C
C     DMINAW = 7.2
C     IF( BCMD .GT. 4.0 ) DMINAW = BCMD
C
       DO  1  IMS = 1,5
         FMASS  = ARMAS(IMS)
C        FMASS2 = FMASS**2
         PX     = 0.0
         IMMM   = 0
         PADD   = 0.0002
         DO 2  IMM = 1,2000
          IF( IMM .EQ.  500 ) PADD = 0.002
          IF( IMM .EQ. 1000 ) PADD = 0.02
          IF( IMM .EQ. 1000 ) PADD = 0.2
          PX = PX + PADD
          IF( PX .GT. 100 .OR. PX .LT. 0.05 ) GO TO 2
          DD = DEDXTP(PX,FMASS,1.)
          PP = PX
          PP = ALOG10(PP)
          PP = FRAC1 + (1.0 + PP)*15.0
C
C                            CALCULATE BETA AND GAMMA FOR
C                            INPUT TO A. WAGNERS FUNCTION
C
C         EAW    = SQRT(PX**2 + FMASS2)
C         BETAW  = PX / EAW
C         GAMMAW = EAW / FMASS
C         DD     = THDEDX( BETAW, GAMMAW, DMINAW )
C
          IF(.NOT. LARGE .AND. (DD .GT. 25 .OR. DD .LT. 0.0 )) GO TO 2
          IF(LARGE .AND. (DD .GT. 200 .OR. DD .LT. 0.0 ))      GO TO 2
          IMMM = IMMM + 1
C
          DDSC = DD
          IF( LARGE ) DDSC = DD * 0.125
C
C                            FIRST TIME, MOVE TO STARTING POINT
C

          IF( IMMM .NE. 1) GO TO 3
            CALL MOVEA(PP,DDSC)
            XLABEL = PP
            YLABEL = DDSC
            GO TO 2
C
C                            DRAW DASHED LINE FOR ELECTRON CURVE
C                            DRAW NORMAL LINE FOR PI,K,P
C
  3       IF( IMS .EQ. 1 ) GO TO 4
            CALL DRAWA(PP,DDSC)
            GO TO 2
C
  4         CALL DASHA(PP,DDSC,14)
C
  2     CONTINUE
C
C                            DRAW LABEL FOR CURVE
C
        IF( SMALL ) GO TO 1
        XLABEL = XLABEL - 1.0
        YLABEL = YLABEL + 1.2
        IF( IMS .EQ. 1 ) YLABEL = YLABEL - 1.7
        IF( IMS .EQ. 4 .AND. LARGE ) YLABEL = YLABEL - 0.7
C
        IF( IMS .EQ. 2 ) GO TO 6
        IF( IMS .EQ. 3 ) GO TO 5
          CALL MOVEA( XLABEL, YLABEL )
          CALL EOUTST( 1, PLABEL(IMS) )
          GO TO 1
C
  5     IF( LARGE ) XLABEL = XLABEL - 0.5
        CALL DRAWPI( XLABEL , YLABEL + 0.3 )
        GO TO 1
C
  6     IF( LARGE ) XLABEL = XLABEL - 0.5
        CALL DRAWMU( XLABEL , YLABEL + 0.3 )
C
  1   CONTINUE
C
      RETURN
      END
