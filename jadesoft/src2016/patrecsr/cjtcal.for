C-------------------------------------------
C  MACRO CJTCAL .... JET CHAMBER CALIBRATION
C               ....  P. STEFFEN : 78/01/17
C-------------------------------------------
      COMMON /CJTCAL/ HJTCAL(50688)
                      INTEGER*2 HTMPED(1536), HAMCAL(8,1536,4)
                      EQUIVALENCE (HTMPED(  1),HJTCAL(   1))
     ,               ,            (HAMCAL(1,1),HJTCAL(1537))
C
C     HTMPED(IWIR)        : DRIFT TIME PEDESTAL IN UNITS OF .01
C     HAMCAL(IHIT,IWIR,J) : AMPLITUDE CALLIBRATION
C     J = 1 : AMP(-Z), IN UNITS OF 4096
C     J = 2 : PED(-Z), IN UNITS OF    1
C     J = 3 : AMP(+Z), IN UNITS OF 4096
C     J = 4 : PED(+Z), IN UNITS OF    1
C----------- END OF MACRO CJTCAL -----------
