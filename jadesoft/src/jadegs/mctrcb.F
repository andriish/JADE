C   07/10/83 310261819  MEMBER NAME  MCTRCB   (JADEGS)      FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE MCTRCB( ITRACK, NPART, IPART, IVECT, FRACT )
C-----------------------------------------------------------------------
C
C   AUTHOR    C. BOWDERY   7/10/83 :  TRACEBACK FROM 'PATR' TO 'VECT'
C
C   LAST MOD  C. BOWDERY  21/10/83 :  CALL MCTR4V. CHECK -VE ITRACK.
C
C   GIVEN A 'PATR' TRACK NUMBER, DETERMINE THE 4_VECTOR PARTICLE IN
C   ONE OF THE 'VECT' BANKS THAT IS ASSOCIATED WITH IT. 'TR4V' IS USED.
C
C   INPUT:  ITRACK      =  'PATR' TRACK NUMBER
C   OUTPUT: NPART       =  NUMBER OF 4_VECTORS ASSOCIATED (USUALLY 1)
C           IPART(1..3) =  ARRAY OF ASSOCIATED 'VECT' PARTICLE NUMBERS
C           IVECT(1..3) =  ARRAY OF 'VECT' BANK NO. CORRESP. TO IPART
C           FRACT(1..3) =  ARRAY OF HIT FRACTIONS FOR EACH ASSOC. PART.
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL SELECT
C
#include "cmubcs.for"
C
      DIMENSION  IPART(3), IVECT(3), FRACT(3)
C
      DATA IEFL1, IEFL2 / 0 , 0 /
C
C-------------------  C O D E  -----------------------------------------
C
      DO  2  I = 1,3
        IPART(I) = 0
        IVECT(I) = 0
        FRACT(I) = 0.0
 2    CONTINUE
C
C                       CREATE THE 'TR4V' BANK IF IT DOES NOT EXIST
C
      CALL MCTR4V(0, IERROR)
C
C                       CHECK THAT THE 'TR4V' BANK EXISTS. ERROR CODE 1
C                                                        IF IT DOES NOT.
      IPT = IDATA( IBLN ('TR4V') )
      IF(IPT .GT. 0 ) GO TO 4
      NPART = -1
      IEFL1 = IEFL1 + 1
      IF( IEFL1 .GT. 10 ) RETURN
        WRITE(6,5)
  5     FORMAT(/' **** WARNING ****  MCTRCB FOUND NO ''TR4V'' BANK'/)
        RETURN
C
C                       CHECK THAT TRACK NUMBER DOES NOT EXCEED MAX.
C                       NUMBER OF PATR TRACKS. ERROR CODE 2 IF IT DOES.
C
  4   MAXTRK = IDATA ( IPT + 1 )
      IF( ITRACK .LE. MAXTRK .AND. ITRACK .GT. 0 ) GO TO 6
      NPART = -2
      IEFL2 = IEFL2 + 1
      IF( IEFL2 .GT. 50 ) RETURN
        WRITE(6,7) ITRACK, MAXTRK
  7     FORMAT(/' **** ERROR **** MCTRCB INPUT TRACK ',I4,' > NO. OF'
     +         ,' TRACKS ( = ',I4,') OR < 1'/)
        RETURN
C
C                       DETERMINE THE NUMBER OF ASSOCIATED 4 VECTORS
C                       AND THE TOTAL NUMBER OF HITS ALONG THE TRACK
C
  6   INDEX  = IPT + 1 + (ITRACK-1)*8
      NAPART = IDATA ( INDEX + 1 )
      NAHITS = IDATA ( INDEX + 2 )
      IF( NAPART .LE. 0 ) RETURN
C
C                       FIND THE ENCODED PARTICLE INFO AND EXTRACT
C                       THE PARTICLE AND VECT BANK NUMBER
C                       ICODE = 2 * 4_VECTOR_PARTICLE + VECT_BANK_NUMBER
C
C                       THEN CALCULATE HIT FRACTION
C
      NPART    = 1
      ICODE    = IDATA( INDEX + 3 )
      IPART(1) = ICODE / 2
      IVECT(1) = MOD( ICODE , 2 )
      FRACT(1) = FLOAT( IDATA( INDEX + 4 ) ) / FLOAT( NAHITS )
C
      IF( NAPART .EQ. 1 ) RETURN
C
C
      ICODE    = IDATA( INDEX + 5 )
      NHITS2   = IDATA( INDEX + 6 )
      FRACT2   = FLOAT( NHITS2 ) / FLOAT( NAHITS )
C
C                       DOES THE SECOND OPTION BELONG TO ANOTHER TRACK ?
C                       SCAN THE 'TR4V' BANK AND SET THE 'SELECT' FLAG.
C
      CALL SCTR4V(ICODE,NHITS2,FRACT2,IPT,SELECT)
      IF( .NOT. SELECT ) GO TO 1
      NPART    = 2
      IPART(2) = ICODE / 2
      IVECT(2) = MOD( ICODE, 2 )
      FRACT(2) = FRACT2
C
  1   IF( NAPART .EQ. 2 ) RETURN
C
      NHITS3       = IDATA( INDEX + 8 )
      FRACT3       = FLOAT( NHITS3 ) / FLOAT( NAHITS )
      ICODE        = IDATA( INDEX + 7 )
C
C                       DOES THE NEXT OPTION BELONG TO ANOTHER TRACK ?
C                       SCAN THE 'TR4V' BANK AND SET THE 'SELECT' FLAG.
C
      CALL SCTR4V(ICODE,NHITS3,FRACT3,IPT,SELECT)
      IF( .NOT. SELECT ) RETURN
C
C                       NPART COULD BE 2 OR 3 DEPENDING ON THE LAST STEP
C
      NPART        = NPART + 1
      IPART(NPART) = ICODE / 2
      IVECT(NPART) = MOD( ICODE, 2 )
      FRACT(NPART) = FRACT3
C
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE SCTR4V(ICODE,NHITS,FRACT,IPT,SELECT)
C-----------------------------------------------------------------------
C
C             THIS SUBROUTINE SCANS THE 'TR4V' BANK AND SEARCHES TO SEE
C             WHETHER THERE IS AN ENTRY IN THE BANK FOR 4_VECTOR
C             PARTICLE WITH CODE  ICODE WHICH HAS MORE HITS THAN NHITS,
C             IF THERE ARE LESS THAN 8 HITS OR WHICH HAS A GREATER HIT
C             FRACTION.
C             IF YES, SELECT IS 'FALSE', 'TRUE' OTHERWISE.
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL SELECT
C
#include "cmubcs.for"
C
C-------------------  C O D E  -----------------------------------------
C
      SELECT = .FALSE.
      NTRKS  = IDATA( IPT + 1 )
C
      DO  1  I = 1,NTRKS
        DO  2  J = 3,7,2
          JCODE = IDATA( IPT + 1 + (I-1)*8 + J )
          IF(JCODE .NE. ICODE ) GO TO 2
C                                              HITS FOR THIS PATR TRACK
            ITOT   = IDATA( IPT + 1 + (I-1)*8+ 2 )
C                                              HITS FOR THIS POSSIBILITY
            JHITS  = IDATA( IPT + 1 + (I-1)*8 + J + 1 )
            FRACTJ = FLOAT( JHITS ) / FLOAT( ITOT )
C
C                            ARE THERE 8 HITS OR MORE FOR BOTH PARTICLES
C
            IF( NHITS .GE. 8  .AND. JHITS .GE. 8 ) GO TO 3
C
C                            NO SO JUST COMPARE THE NUMBER OF HITS
C
              IF( JHITS .GT. NHITS ) RETURN
              GO TO 2
C
C                            YES SO COMPARE HIT FRACTIONS
C
   3        IF( FRACTJ .GT. FRACT ) RETURN
  2     CONTINUE
 1    CONTINUE
C
      SELECT = .TRUE.
C
      RETURN
      END
