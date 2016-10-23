C   16/11/82 606091753  MEMBER NAME  JFTEST   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE JFTEST ( X, P, IRETRN, *, CA, SA )
C-----------------------------------------------------------------------
C
C   AUTHOR:   J. HAGEMANN  16/11/82 :  CKECKS FRAME CONDITION
C             R. RAMCKE
C
C        THIS SUBROUTINE CALCULATES NEXT POINT R OF
C        CHECKS FRAME CONDITIONS FOR DRIFT SPACE IN VERTEX CHAMBER.
C        IRETRN IS RETURN CODE:
C
C           IRETRN = 0 : NEXT ELEMENT, INCREASING X
C                  = 1 : NEXT ELEMENT, DECREASING X
C                  = 2 : NEXT ELEMENT, INCREASING Z
C                  = 3 : NEXT ELEMENT, DECREASING Z
C                  = 4 : PARTICLE STOPPED IN SYSTEM OR DECAY CONDITION
C                        REACHED.
C                  = 5 : NEXT ELEMENT, CHANGING PHI
C
C        IF PARTICLE IS IN FRAME RETURN ELSE RETURN 1.
C-----------------------------------------------------------------------
C
      COMMON / CGEOV  / RPIPV, DRPIPV, XRLPIV, RVXC, DRVXC, XRLVXC,
     +                  ZVXCM, DZVCM, XRZVCM, ZVXCP, DZVCP, XRZVCP,
     +                  XRVTXC
      COMMON / CJVDAT / XSLOPV, YSLOPV, XLV, XHV,
     *                  SV, SV2, TANLOR
C
      DIMENSION X(3), P(7)
C
      DATA EPSIL0, EPSIL1 / 1.E-3, 5.E-3 /
C
C------------------------  C O D E  ------------------------------------
C
      X4 = SQRT ( X(1) * X(1) + X(2) * X(2) )
C
      IF ( X4 .LE. XHV + EPSIL0 )   GOTO 20
                     IRETRN = 0
                     RETURN 1
   20 IF ( X4 .GE. XLV - EPSIL0 )   GOTO 30
                     IRETRN = 1
                     RETURN 1
   30 IF ( ABS(X(2)) .LE. YSLOPV * X(1) + EPSIL1 ) GOTO 40
                     IRETRN = 5
                     RETURN 1
   40 IF ( X(3) .LE. ZVXCP )  GOTO 50
                     IRETRN = 2
                     RETURN 1
   50 IF ( X(3) .GE. ZVXCM )  RETURN
                     IRETRN = 3
                     RETURN 1
      END
