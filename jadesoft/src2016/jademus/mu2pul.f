C   23/02/84 403091204  MEMBER NAME  MU2PUL   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      INTEGER FUNCTION  MU2PUL( IMONTH, IYEAR )
C-----------------------------------------------------------------------
C
C NEW FUNC    15.15  23/02/84   C. BOWDERY - DOUBLE PULSE CUT FOR MUCOOR
C
C     THIS FUNCTION RETURNS THE DOUBLE PULSE CUT (IN CLOCK PULSES)  THAT
C     IS NEEDED BY MUCOOR TO ELIMINATE "ECHO" PULSES CAUSED BY ELECTRON-
C     IC PROBLEMS IN THE DIGITISERS. USE TIME TO IMPLEMENT THESE BODGES,
C     RATHER THAN RUN NO. SO  THAT  MONTE-CARLO  WILL  HAVE THE CORRECT
C     VALUE TOO.
C
C     INPUT:   IMONTH   =  MONTH WHEN EVENT WAS RECORDED
C              IYEAR    =  YEAR  WHEN EVENT WAS RECORDED
C
C     OUTPUT:  MU2PUL   =  15       ( 1/1979 - 8/1983 )
C                       =  30       ( 9/1983 -   ?    )
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
C------------------  C O D E  ------------------------------------------
C
      ITIME  = IYEAR * 100  +  IMONTH
C
      MU2PUL = 15
C
      IF( ITIME .GT. 198308 ) MU2PUL = 30
C
      RETURN
      END
