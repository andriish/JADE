C   25/11/83 402061230  MEMBER NAME  MCNEWS0  (S)           FORTRAN
C
C----------------------------------------------------------------------
      SUBROUTINE MCNEWS
C----------------------------------------------------------------------
C
C   AUTHOR:   E. ELSEN     05/05/81 :  PRINTS OUT THE LATEST NEWS
C
C        MOD  E. ELSEN     13/05/83 :
C        MOD  C. BOWDERY   25/05/83 :  MAJOR MC UPDATE + NEW ROUTINES
C   LAST MOD  C. BOWDERY   25/05/83 :  NEW ROUTINES FOR INPUT CHECKING
C
C----------------------------------------------------------------------
C
      WRITE(6,9101)
 9101 FORMAT(1X,70('+')//
     +  7X,'       M C J A D E - MONTE CARLO TRACKING PROGRAM      '/
     +  7X,'       ------------------------------------------      '//
     +  7X,' ------------------------------------------------------'/
     +  7X,' -  THIS VERSION HAS OPTIONAL MEIER LG TRACKING,      -'/
     +  7X,' -  IMPROVED MULTIPLE SCATTERING IN I.D. AND KZERO/L  -'/
     +  7X,' -  AND NEUTRON TRACKING. INPUT DATA ARE CHECKED AND  -'/
     +  7X,' -  BAD EVENTS REJECTED. CHECK YOUR OUTPUT !  6/02/84 -'/
     +  7X,' ------------------------------------------------------'/
     +  7X,' -  NEW PALL BANK CONTAINS ALL THE PARTICLES PRODUCED -'/
     +  7X,' -  IN THE 4-VECTOR GENERATOR (IF STORED CORRECTLY).  -'/
     +  7X,' -  SEE JADE COMPUTER NOTE 69. ALSO NEW TRACEBACK     -'/
     +  7X,' -  BANK HITL HAS BEEN ADDED. SEVERAL BUGS REMOVED.   -'/
     +  7X,' ------------------------------------------------------'/
     +  7X,' -  MUON TRACKING OMITTED IN STANDARD MC JOB.         -'/
     +  7X,' -  USE MEMBER  #PRODMU FOR MUON+STANDARD TRACKING.   -'/
     +  7X,' ------------------------------------------------------'//
     +  1X,70('+')/)
      RETURN
      END
