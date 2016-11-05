C   24/06/85 506241019  MEMBER NAME  DRFTIM   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      FUNCTION DRFTIM( NCHA, IA1, IA2, IPED, NCD )
C-----------------------------------------------------------------------
C
C    AUTHOR:   J. HAGEMANN   17/10/84 :  FUNCTION TO CALCULATE DRIFT
C                                        TIME FOR VERTEX CHAMBER HITS
C                                        IN UNITS OF ( 0.1 NS )
C  LAST MOD:   J. HAGEMANN   21/06/85 :  NOW IN OWN MEMBER
C
C                       NCHA   :  CHANNEL NUMBER WHERE THE HIT BEGINS
C                       IA1    :  AMPLITUDE OF FIRST HIT CHANNEL
C                       IA2    :  AMPLITUDE OF SECOND HIT CHANNEL
C                       IPED   :  PEDASTAL
C                       NCD    :  HELP COUNTER FOR DOUBLE HITS
C
C-----------------  C O D E  -------------------------------------------
C
C
      IH1    = MIN0( IA1, 64 ) - IPED
      IH2    = MIN0( IA2, 64 ) - IPED
C
      DRFTIM = (FLOAT(NCHA+NCD) + FLOAT(IH2)/FLOAT(IH1 + IH2))*100.0
C
      RETURN
      END
