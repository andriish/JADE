C   20/03/85 503291845  MEMBER NAME  MUANA    (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE  MUANA( IJOIN )
C-----------------------------------------------------------------------
C
C        THIS ROUTINES COORDINATES MUON ANALYSIS IN JADE
C
C LAST CHANGE 13.30 20/03/85 CHRIS BOWDERY- CORRECT RUN YEAR IF NEEDED.
C      CHANGE 13.30 22/02/82 HUGH MCCANN - FOR IMPROVED ERROR OUTPUT.
C      CHANGE 15.30 04/02/82 HUGH MCCANN - NOW CALLS MUCUTS.
C      CHANGE 15.15 05/11/81 HUGH MCCANN - CORRECT BUG , IMP INT*2!!
C      CHANGE 17.46 19/10/81 CHRIS BOWDERY- INSERT MACRO CMUBCS
C      CHANGE 12.30 19/10/81 HUGH MCCANN  - INSERT COMMON CMUIEV.
C      CHANGE 09.12 13/05/80 JOHN ALLISON --COMMENT OUT CALL MUANAJ.
C      CHANGE 16.00 02/05/80 HUGH MCCANN ---COMMENT OUT CALL MUANAL.
C      CHANGE 10.56 18/05/79 JOHN ALLISON.
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
C------------------  C O D E  ------------------------------------------
C
C
C                            STANDARD MUON ANALYSIS PROCEDURE
C
C                            IJOIN = 0   SUPPRESSES CALLS TO ROUTINES
C                            WHICH USE BANKS OTHER THAN MUON BANKS
C
C
C                            BEGIN BY EXTRACTING DATA FROM HEAD BANK
C
      CALL MUANAB
C
C                            CONVERT MUON SIGNALS TO COORDINATES
C
      CALL MUANAC
C
C                            FOLLOW EACH INNER DETECTOR TRACK OUT
C                            (PHILOSOPHY 2).  AT THE MOMENT, MUANAF
C                            AND MUANAL ARE COMPLETELY INDEPENDENT
C
      IF( IJOIN .NE. 0 ) CALL MUANAF
      IF( IJOIN .NE. 0 ) CALL MUCUTS
C
C                            ****** FROM 18/03/80 , ONLY WANT PHIL2
C                            RUN IN STANDARD MU ANALYSIS. THIS WILL BE
C                            DONE IF 'IJOIN' IS GREATER THAN ZERO .
C
C
C                            FIND MUON LINES - PHILOSOPHY 1 MUON
C                            PATTERN RECOGNITION.
C
C     CALL MUANAL
C
C                            ATTEMPT TO JOIN MUON CLUSTERS WITH INNER
C                            DETECTOR TRACKS AND WITH LG CLUSTERS. ALSO
C                            CORRELATE RESULTS OF PHILOSPHY 1 AND 2.
C
C     IF( IJOIN .NE. 0 ) CALL MUANAJ
C
C
      RETURN
      END
