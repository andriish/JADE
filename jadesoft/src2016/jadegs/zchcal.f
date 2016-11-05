C   27/05/85 804061503  MEMBER NAME  ZCHCAL   (JADEGS)      FORTRAN77
************************************************************************
      SUBROUTINE ZCHCAL(KRUN,ZCXCH,ZCVDR)
************************************************************************
*                                                                      *
*     FILLS IN CONSTANTS FOR THE Z-CHAMBER                             *
*                                                                      *
************************************************************************
*
******COMMONBLOCKS
      COMMON/CZCALI/IBAD(64),IZPED(2,64),IZT0(64)
******
*     ZERO BAD WIRE ARRAY
******
      CALL VZERO(IBAD,64)
******
*     IF MONTE CARLO, ZERO PEDESTALS, THEN RETURN
******
      IF(KRUN.LT.100)             THEN
         CALL VZERO(IZPED,128)
         RETURN
      ENDIF
******
*     ZCXCH = CONSTANT FOR EFFECTIVE LENGTH (CHARGE DIVISION)
******
      ZCXCH = 0.890
******
*     ZCVDR = DRIFT VELOCITY
*             0.28 1984      0.44 1985/1      0.36 1985/2
******
                                  ZCVDR = 0.36
      IF(KRUN.LT.19080)           ZCVDR = 0.44
      IF(KRUN.LT.19059)           ZCVDR = 0.28
******
*     SET BAD WIRES
******
      IBAD( 6) = 1
      IBAD(10) = 1
      IBAD(20) = 1
      IBAD(39) = 1
      IF(KRUN.LT.20232)           IBAD( 3) = 1
      IF(KRUN.GT.20316)           IBAD(26) = 1
      IF(KRUN.GT.20333)           IBAD(14) = 1
      IF(KRUN.GT.22427)           IBAD(64) = 1
      IF(KRUN.GT.22826)           IBAD(57) = 1
      IF(KRUN.GT.23676)           IBAD(37) = 1
      IF(KRUN.GT.26103)           IBAD(27) = 1
******
*     MAKE CORRECTION FOR CHANGE OF V34 CARD
******
CCCC  IF(KRUN.GE.20000)           IZPED(2,2) = 45 REPL 6.4.88  EE
      IF(KRUN.GE.20000)  THEN
        IZPED(2,2) = 45
      ELSE
        IZPED(2,2) = 35
      ENDIF
      RETURN
      END
************************************************************************
      BLOCK DATA PEDEST
      COMMON/CZCALI/IBAD(64),IZPED(2,64),IZT0(64)
      DATA IZPED/58,62, 47,35, 58,38, 47,37, 45,39, 52,54,
     *           46,54, 54,36, 37,27, 41,30, 53,41, 45,39,
     *           46,38, 43,43, 54,43, 48,46, 40,44, 43,38,
     *           41,38, 44,38, 33,36, 42,43, 44,50, 42,35,
     *           40,29, 34,33, 30,36, 41,39, 46,30, 39,42,
     *           39,36, 45,40, 44,40, 31,43, 45,45, 40,39,
     *           45,41, 45,42, 42,48, 20,28, 26,24, 22,28,
     *           27,30, 32,24, 31,40, 29,29, 34,25, 32,28,
     *           34,39, 52,45, 52,43, 50,42, 54,46, 50,48,
     *           43,49, 48,43, 41,39, 42,42, 49,50, 40,41,
     *           42,24, 51,47, 55,51, 47,41/
      DATA IZT0/ 2,2,2,2, 2,3,2,2, 2,3,2,2, 2,3,1,2,
     *           3,3,3,5, 3,5,3,3, 3,4,4,4, 3,3,3,4,
     *           2,2,1,2, 2,2,2,2, 2,2,2,2, 2,2,2,2,
     *           2,1,2,2, 2,2,2,2, 2,2,2,2, 2,2,2,2/
      END
