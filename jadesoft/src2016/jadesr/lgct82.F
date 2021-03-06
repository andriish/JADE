C   11/10/79 203030844  MEMBER NAME  LGCT828  (JADESR)      FORTRAN
      SUBROUTINE LGCT82(IPLG)
C
C      SUM UP LEAD GLASS ENERGIES IN 12 DIFFERENT HALF-WORLDS AND SET
C      THE NUMBER JEMPTY = NR OF EMPTY HALFWORLDS
C      THE NUMBER UNBAL = SMALLEST RATIO BETWEEN OPPOSITE HALFWORLDS
C      IPLG = ADDRESS OF LEADGLASS BANK IN /BCS/
C                     J.OLSSON 11.10.79 (AFTER AN IDEA OF W.BARTEL)
C                     LAST CHANGE:       23.02.80
C
      IMPLICIT INTEGER*2 (H)
C
#include "cdata.for"
C
#include "cheadr.for"
C
      COMMON /CWORK/ SUM12(12),SUMWRD(12)
C
      COMMON /CRESR1/ ETOT,EBAR,EC1,EC2,E7MIN,
     ,               JEMPTY,UNBAL,IACCLG
C
C
C  THE FOLLOWING ARRAY GIVES HELP INDICES FOR THE ENDCAP BLOCKS
C  A ROUGH DIVISION INTO 12 SECTORS CAUSES A ENDCAP BLOCK TO BELONG TO
C  ONE OR AT MOST TWO (NEIGHBOURING) SECTORS
C
      DIMENSION HELP(2,96)
      DATA HELP /1,0,1,2,2,3,3,0,1,0,1,2,2,0,2,3,3,0,1,0,1,0,1,2,
     1           2,3,3,0,3,0,1,0,1,0,1,2,2,0,2,0,2,0,2,3,3,0,3,0,
     2           4,0,4,5,5,6,6,0,4,0,4,5,5,0,5,6,6,0,4,0,4,0,4,5,
     3           5,6,6,0,6,0,4,0,4,0,4,5,5,0,5,0,5,0,5,6,6,0,6,0,
     4           7,0,7,8,8,9,9,0,7,0,7,8,8,0,8,9,9,0,7,0,7,0,7,8,
     5           8,9,9,0,9,0,7,0,7,0,7,8,8,0,8,0,8,0,8,9,9,0,9,0,
     6 10,0,10,11,11,12,12,0,10,0,10,11,11,0,11,12,12,0,10,0,10,0,10,11,
     7 11,12,12,0,12,0,10,0,10,0,10,11,11,0,11,0,11,0,11,12,12,0,12,0/
C
      DATA EMINK /50./
C
      DATA IPRN /0/
C
C                                       INITIALIZE RESULTS
      JEMPTY = 0
      UNBAL  = 1.E+06
      E7MIN  = 0.
      ETOT   = 0.
      EBAR   = 0.
      EC1    = 0.
      EC2    = 0.
      IACCLG = 0
      DO 1 I=1,12
      SUMWRD(I)=0.
    1 SUM12(I)=0.
C
C      GET LEAD GLASS ADRESSES
C
      NWO=IDATA(IPLG)
      IF(NWO.GT.3) GO TO 2
      PRINT 2001, HHEADR(18),HHEADR(19)
 2001 FORMAT(' NO LEAD GLASS HITS IN RUN AND EVENT ',2I10)
      GO TO 999
2     IPLG2=2*IPLG + 8
      NWO=IPLG2+2*NWO-8
C
C     CALCULATE ROW , RING , ENDCAP ENERGIES
C
      IADMX = 0
      EBLMX = 0.
      DO 20 IJK=IPLG2,NWO,2
        IADR=HDATA(IJK-1)
        EBL  = HDATA(IJK)
        ETOT = ETOT + EBL
        IF(IADR.GT.2687) GO TO 11
C
C                                       CYLINDER
          EBAR = EBAR + EBL
C                                       ROW NUMBER   0 - 83
          IROW = IADR/32
C                                 SECTOR NUMBER 1 - 12 (7 ROWS / SECTOR)
          IND12 = IROW/7 + 1
          SUM12(IND12) = SUM12(IND12) + EBL
          GO TO 20
   11   CONTINUE
C
C                                       END CAPS
          IADR = IADR - 2687
C                                       KEEP HIGHEST BLOCK IN END CAP
          IF(EBL.LE.EBLMX) GOTO 16
            EBLMX = EBL
            IADMX = IADR
   16     CONTINUE
          IF(IADR.GT.96) GOTO 18
             EC1  = EC1 + EBL
             GOTO 19
   18     CONTINUE
             IADR = IADR - 96
             IF(IADR.GT.96) GOTO 20
             EC2  = EC2 + EBL
   19     CONTINUE
          IND1 = HELP(1,IADR)
          IND2 = HELP(2,IADR)
          IF(IND1*IND2 .NE. 0) EBL = EBL * .5
          IF(IND1.NE.0) SUM12(IND1) = SUM12(IND1) + EBL
          IF(IND2.NE.0) SUM12(IND2) = SUM12(IND2) + EBL
   20 CONTINUE
C
C     CHECK IF 'SPINNING' BLOCK
      IF(EBLMX.LT.ETOT*.95) GOTO 22
C     0 FOR -Z, 1 FOR +Z
      NE = 0
      IF(IADMX.GT.96) NE = 1
C     REDUCE TO 1 - 96
      IADMX = IADMX - NE*96
C     GET QUADRANT NUMBER 0 - 3
      NQ = (IADMX - 1)/24
C     REDUCE TO 1 - 24
      IADMX = IADMX - NQ*24
      IF(IADMX.LT.5) GO TO 22
      IF(IADMX.GT.15.AND.IADMX.NE.20) GO TO 22
      IPRN = IPRN + 1
      IF(IPRN.LE.20) PRINT 2002, HHEADR(18),HHEADR(19),IADMX,EBLMX
 2002 FORMAT(' EVENT',2I6,': ADDRESS AND ENERGY OF BAD BLOCK:',I6,F10.2)
      RETURN
C
C
   22 CONTINUE
C
C     CALCULATE ENERGY ASYMMETRY, MEASURED BY NUMBER OF EMPTY H-WORLDS
C     A HALF-WORLD CONSISTS OF ANY CONSECUTIVE 6 SECTORS
C     CALCULATE ALSO RATIO BETWEEN OPPOSITE HALFWORLDS
C
      E7MIN = 1.E10
      DO 40  I6 = 1,6
        IF(I6.NE.1) GOTO 32
          E6SM1 = SUM12(1)+SUM12(2)+SUM12(3)
     ,          + SUM12(4)+SUM12(5)+SUM12(6)
          E7SM1 = E6SM1 + SUM12(12)
          E7SM2 = ETOT  - E6SM1 + SUM12( 1)
          GOTO 33
   32   CONTINUE
          E7SM1 = E6SM1 + SUM12(I6+5)
          E6SM1 = E7SM1 - SUM12(I6-1)
          E7SM2 = ETOT  - E6SM1 + SUM12(I6)
   33   CONTINUE
        E6SM2 = ETOT - E6SM1
C
        E1    = AMIN1(E6SM1,E6SM2)
        E2    = ETOT - E1
        RAT   = 0.
        IF(E1.GT.0.) RAT = E1 / E2
        IF(RAT.LT.UNBAL) UNBAL = RAT
C
        IF(E6SM1.LT.EMINK) JEMPTY = JEMPTY + 1
        IF(E6SM2.LT.EMINK) JEMPTY = JEMPTY + 1
        E7MIN = AMIN1(E7SM1,E7SM2,E7MIN)
        SUMWRD(I6  ) = E6SM1
        SUMWRD(I6+6) = E6SM2
   40 CONTINUE
C
C
C
  999 CONTINUE
      IACCLG = 1
      RETURN
      END
