C   03/12/81 502132004  MEMBER NAME  MUONS    (JADEMUS)     FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE MUONS( NUMBER, IARRAY, IERROR )
C-----------------------------------------------------------------------
C
C   AUTHOR:   C. BOWDERY     29/09/84 : RETURNS NUMBER OF MUCUTS MUONS
C
C LAST MOD:   C. BOWDERY     13/02/85 : .GT. 10  --> .GE. 10
C
C
C     THIS ROUTINE EXAMINES THE MUR2/1 BANK TO SEARCH FOR MUONS.
C
C     OUTPUT:   NUMBER         NO. OF MUONS IN THIS EVENT  (0,1,2,3....)
C               IARRAY(N,K)    ARRAY OF TRACK NUMBERS AND MUON TYPES
C                              N HAS RANGE 1..2
C                              K HAS RANGE 1..10   (1..NUMBER   DEFINED)
C                              N = 1   TRACK NUMBER
C                              N = 2   MUON TYPE  (1,5 OR 9)
C
C               IERROR         0 = NO ERROR
C                              1 = NO 'PATR' BANK
C                              2 = NO 'MUR2'/0 BANK = NO MU ANALYSIS
C                              3 = NO 'MUR2'/1 BANK = MAYBE NO MU HITS
C                                                     OR NO PATR TRACKS
C                              4 = ILLEGAL MUON TYPE FLAG FOUND. EVENT
C                                  PROCESSING STOPPED. (MESSAGE PRINTED
C                                  FOR THE FIRST 10 OCCURRENCES.)
C                              5 = MORE THAN 10 MUONS FOUND = POSSIBLY
C                                  ANALYSIS ERROR. (MESSAGE PRINTED FOR
C                                  THE FIRST 10 OCCURRENCES.)
C
C-----------------------------------------------------------------------
C
C      MUON TYPES
C      ----------
C
C      MUCUTS FLAGS TRACKS WITH A   0, 1, 5 OR 9. BECAUSE OF BACKGROUND,
C      SOME TRACKS WITH FLAG 1, 5 AND 9 WILL NOT BE PROMPT MUONS BUT
C      THE PROBABILITY DECREASES AS THE FLAG VALUE INCREASES. TRACKS
C      WITH FLAG 0 ARE MAINLY NON-PROMPT MUONS AND OTHER PARTICLES.
C
C      NEW SCHEME:  TYPE A MUONS INCLUDE ALL MUONS WITH FLAGS 1, 5 AND 900000527
C                   TYPE B MUONS INCLUDE ALL MUONS WITH FLAGS    5 AND 900000528
C                   TYPE C MUONS INCLUDE ALL MUONS WITH FLAG           900000529
C
C
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
C----------START OF MACRO CMUBCS----------------------------------------
      COMMON /BCS/IDATA(1)
      DIMENSION HDATA(1),ADATA(1)
      EQUIVALENCE (HDATA(1),ADATA(1),IDATA(1))
C----------END OF MACRO CMUBCS------------------------------------------
C
      DIMENSION IARRAY(2,10)
C
      DATA  ICNT1 / 0 /, ICNT2 / 0 /
C
C------------------  C O D E  ------------------------------------------
C
      NUMBER = 0
      IERROR = 0
C
C                            FIND 'PATR' BANK. IF NOT THERE, RETURN WITH
C                            ERROR CODE 1
C
      IPATR  = IDATA( IBLN('PATR') )
      IF( IPATR .GT. 0 ) GO TO 1
        IERROR = 1
        RETURN
C
C                            FIND 'MUR2'/0 BANK. IF NOT THERE, RETURN
C                            WITH ERROR CODE 2
C
   1  CALL CLOC(MUR20, 'MUR2', 0 )
      IF( MUR20 .GT. 0 ) GO TO 2
        IERROR = 2
        RETURN
C
C                            FIND 'MUR2'/1 BANK. IF NOT THERE, RETURN
C                            WITH ERROR CODE 3
C
   2  CALL CLOC(MUR21, 'MUR2', 1 )
      IF( MUR21 .GT. 0 ) GO TO 3
        IERROR = 3
        RETURN
C
C                            FIND NUMBER OF PATR TRACKS AND NO. OF WORDS
C                            PER TRACK IN 'MUR2'/1
C
   3  NTRK = IDATA( MUR20 + 1 )
      NWRD = IDATA( MUR20 + 2 )
C
C                            LOOP OVER TRACKS TO FIND THOSE WITH
C                            A NON-ZERO MUCUTS FLAG ==> MUON
C
      DO  10  ITRK = 1,NTRK
        IP = ( ITRK - 1 ) * NWRD + MUR21
        IQ = HDATA( 2 * IP + 53 )
        IF( IQ .EQ. 0 ) GO TO 10
C
C                            VALID FLAG?  1, 5 OR 9 ?
C
        IF(                    .NOT.
     +      ( IQ .EQ. 1  .OR.  IQ .EQ. 5  .OR. IQ .EQ. 9 )
     +                                                     ) GO TO 15
C
        IF( NUMBER .GE. 10 ) GO TO 20
C
        NUMBER = NUMBER + 1
        IARRAY(1,NUMBER) = ITRK
        IARRAY(2,NUMBER) = IQ
  10  CONTINUE
C
      RETURN
C
C                            PROBLEMS
C
  15  ICNT1 = ICNT1 + 1
      IF( ICNT1 .GT. 10 ) RETURN
      IERROR = 4
      WRITE(6,16) IQ
  16  FORMAT(' ****   E R R O R   ****   ILLEGAL MUON TYPE FLAG (',I5,')
     + FOUND IN MUR2/1 BANK BY S/R MUONS. EVENT DROPPED')
      RETURN
C

  20  ICNT2 = ICNT2 + 1
      IF( ICNT2 .GT. 10 ) RETURN
C
      WRITE(6,21)
  21  FORMAT(' ****   W A R N I N G   ****    MORE THAN 10 MUONS IN THIS
     + EVENT FOUND BY S/R MUONS. POSSIBLE ANALYSIS ERROR!')
      IERROR = 5
      RETURN
C
      END
