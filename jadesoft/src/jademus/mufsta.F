C   23/02/84 402231603  MEMBER NAME  MUFSTA   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE  MUFSTA( IMONTH, IYEAR )
C-----------------------------------------------------------------------
C
C NEW SUBR    15.35  23/02/84   C. BOWDERY - FIX-UP CHAMBER STATUS
C
C     THIS ROUTINE ALTERS THE STATUS OF CERTAIN CHAMBERS FOR THOSE
C     PERIODS WHERE THE CALIBRATION FILES HAVE NOT BEEN UPDATED.
C     USE TIME TO IMPLEMENT THESE BODGES, RATHER RUN NO. SO THAT
C     MONTE-CARLO WILL PICK UP CORRECT CHAMBER STATUS.
C
C     INPUT:   IMONTH    =  MONTH WHEN EVENT WAS RECORDED
C              IYEAR     =  YEAR  WHEN EVENT WAS RECORDED
C
C     OUTPUT:  HMCSTA(N) =  1  (= OFF) FOR CERTAIN VALUES 'N'
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C                                           CALIBRATION COMMON
#include "cmucalib.for"
C
C------------------  C O D E  ------------------------------------------
C
      ITIME  = IYEAR * 100  +  IMONTH
C
C                            BODGE FOR 1980 SUSPECT CHAMBERS
C
C                             1/1979  -  9/1980  ---   NO CHANGES
C                            10/1980  -  5/1981  ---   15 CHAMBERS OFF
C
      IF( ITIME .GE. 198200 ) GO TO 2
      IF( ITIME .LT. 198010 ) GO TO 1
      IF( ITIME .GT. 198105 ) GO TO 1
C
          HMCSTA(6)   = 1
          HMCSTA(22)  = 1
          HMCSTA(26)  = 1
          HMCSTA(111) = 1
          HMCSTA(120) = 1
          HMCSTA(168) = 1
          HMCSTA(170) = 1
          HMCSTA(178) = 1
          HMCSTA(217) = 1
          HMCSTA(232) = 1
          HMCSTA(246) = 1
          HMCSTA(253) = 1
          HMCSTA(256) = 1
          HMCSTA(257) = 1
          HMCSTA(258) = 1
C
C                            ALL DATA UP TILL END OF 1981,
C                            SWITCH 210-213 OFF ALWAYS (DIGITISER)
C
  1       HMCSTA(210) = 1
          HMCSTA(211) = 1
          HMCSTA(212) = 1
          HMCSTA(213) = 1
C
  2   CONTINUE
      RETURN
      END
