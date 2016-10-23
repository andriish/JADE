C   02/07/86 801121223  MEMBER NAME  TP       (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      PROGRAM  TP
C-----------------------------------------------------------------------
C
C     Author:  C. Bowdery       29/09/86       TP version 9
C
C
C
C     Program to reconstruct JADE events and produce summary (TP) banks.
C
C------------------  C O D E  ------------------------------------------

C                            Initialise the program

      CALL TPSTAR

C                            Process the events

      CALL TPPROC

C                            Tidy up and terminate the program

      CALL TPEND

      STOP
      END
