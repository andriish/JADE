C   01/11/84            MEMBER NAME  HITMRK   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE HITMRK( INDEX, X0, Y0, SCRUNI, IDC )
C-----------------------------------------------------------------------
C
C    AUTHOR:   J. OLSSON        ?     :  DRAW HITS
C
C  LAST MOD:   J. HAGEMANN   09/10/84 :  NEW FEATURES INCLUDED
C
C
C     THE COORDINATES OF HITS ARE DRAWN AS CROSSES.
C
C       ARGUMENTS :
C           INDEX  =  VIEW NUMBER (SEE MEMBER CAPMRK)
C           X0     =  X - COORDINATE OF HIT
C           Y0     =  Y - COORDINATE OF HIT
C           SCRUNI =  SIZE OF HIT CROSSES
C           IDC    =  IBM DASH CODE ( IF .LE. 0 HITS NOT DASHED )
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
C PMF 05/11/98     LOGICAL   DSPDTL, SSTPS, PSTPS, FREEZE, FL18, FL22, FL24
C The first four variables are already declared in macro cgraph
      LOGICAL  FL18, FL22, FL24
C
C-----------------------------------------------------------------------
C                            MACRO CGRAPH .... GRAPHICS COMMON
C-----------------------------------------------------------------------
C
      LOGICAL DSPDTL,SSTPS,PSTPS,FREEZE
C
      COMMON / CGRAPH / JUSCRN,NDDINN,NDDOUT,IDATSV(11),ICREC,MAXREC,
     +                  LSTCMD,ACMD,LASTVW,ISTANV,
     +                  SXIN,SXAX,SYIN,SYAX,XMIN,XMAX,YMIN,YMAX,
     +                  DSPDTL(30),SSTPS(10),PSTPS(10),FREEZE(30),
     +                  IREADM,LABEL,LSTPS(10),IPSVAR
C
C------- END OF MACRO CGRAPH -------------------------------------------
C
C
      COMMON / CWORK1 / R,FI,R1,FI1,X1,Y1,R2,FI2,X2,Y2,ZET,XX,YY,RAD
      COMMON / CPROJ  / XMINR,XMAXR,YMINR,YMAXR,IPRJC,FL18,FL22,FL24
C
C-----------------  C O D E  -------------------------------------------
C
      IF( IPRJC.NE.0 )  RETURN
      call setcol('HITS')       ! PMF 23/11/99: set colour
C                            ZET DEPENDENT POINT
      IF( ZET.LT.0. .AND. DSPDTL(6) ) GO TO 10
C                            PLUS Z SIGN
      IF( INDEX.GT.3 .AND. INDEX.NE.10 ) GO TO 20
         RAD = SQRT(X0**2 + Y0**2 +1.)
         RAD = SCRUNI/RAD
         X00 = RAD*X0
         Y00 = RAD*Y0
         XX  = X0 - X00
         YY  = Y0 - Y00
         CALL MOVEA(XX,YY)
C
         XX  = X0 + X00
         YY  = Y0 + Y00
         IF( IDC .LE. 0 )  CALL DRAWA(XX,YY)
         IF( IDC .GT. 0 )  CALL DASHA(XX,YY,IDC)
C
         IF( .NOT. DSPDTL(6) ) then
            call setcol(' ')
            RETURN
         endif
         XX  = X0 - Y00
         YY  = Y0 + X00
         CALL MOVEA(XX,YY)
C
         XX  = X0 + Y00
         YY  = Y0 - X00
         IF( IDC .LE. 0 )  CALL DRAWA(XX,YY)
         IF( IDC .GT. 0 )  CALL DASHA(XX,YY,IDC)
         call setcol(' ')    ! PMF 23/11/99: reset colour
         RETURN
   10 CONTINUE
C                            MINUS Z SIGN
      IF( INDEX.GT.3 .AND. INDEX.NE.10 ) GO TO 20
         RAD = SQRT(X0**2 + Y0**2 + 1.)
         RAD = 1.41421356*RAD
         RAD = SCRUNI/RAD
         X00 = RAD*(X0+Y0)
         Y00 = RAD*(X0-Y0)
         XX  = X0 - X00
         YY  = Y0 - Y00
         CALL MOVEA(XX,YY)
C
         XX  = X0 + X00
         YY  = Y0 + Y00
         IF( IDC .LE. 0 )  CALL DRAWA(XX,YY)
         IF( IDC .GT. 0 )  CALL DASHA(XX,YY,IDC)
C
         XX  = X0 - Y00
         YY  = Y0 + X00
         CALL MOVEA(XX,YY)
C
         XX  = X0 + Y00
         YY  = Y0 - X00
         IF( IDC .LE. 0 )  CALL DRAWA(XX,YY)
         IF( IDC .GT. 0 )  CALL DASHA(XX,YY,IDC)
         call setcol(' ')    ! PMF 23/11/99: reset colour
         RETURN
   20 CONTINUE
      XX  = X0 - SCRUNI
      YY  = Y0
      CALL MOVEA(XX,YY)
C
      XX  = X0 + SCRUNI
         IF( IDC .LE. 0 )  CALL DRAWA(XX,YY)
         IF( IDC .GT. 0 )  CALL DASHA(XX,YY,IDC)
C
      IF( .NOT. DSPDTL(6) ) then
         call setcol(' ')    ! PMF 23/11/99: reset colour
         RETURN
      endif
      XX  = X0
      YY  = Y0 - SCRUNI
      CALL MOVEA(XX,YY)
C
      YY  = Y0 + SCRUNI
         IF( IDC .LE. 0 )  CALL DRAWA(XX,YY)
         IF( IDC .GT. 0 )  CALL DASHA(XX,YY,IDC)
      call setcol(' ')    ! PMF 23/11/99: reset colour
      RETURN
      END