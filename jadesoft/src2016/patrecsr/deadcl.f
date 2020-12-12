C   12/03/81 701121325  MEMBER NAME  DEADCL   (PATRECSR)    FORTRAN
C
      LOGICAL FUNCTION DEADCL(ICELL,NRUN)
C
C     LAST UPDATE :   26/04/80
C     LAST UPDATE :   04/11/80
C     LAST UPDATE :   01/03/81
C     LAST UPDATE :   11/03/81
C     LAST UPDATE :   09/01/87  E ELSEN
C     FUNCTION DEADCL COPES WITH FADC DATA BY CALLING DDC300
C     FUNCTION DEADCL NOW RUN# DEPENDENT
C
C     FUNCTION DEADCL ACCEPTS ONLY CELLNUMBERS BETWEEN 1 AND 96 INCL.
C     FUNCTION DEADCL RETURNS .TRUE.  IF THE CELL 'ICELL' IS DEAD
C     FUNCTION DEADCL RETURNS .FALSE. IF THE CELL 'ICELL' IS STILL ALIVE
C     FUNCTION DEADCL NOW ALSO APPLICABLE FOR MC-DATA:   MC INPUT IS
C                            READ IN FROM COMMON /CRDSTA/ AT EACH EVENT
C-----------------------------------------------------------------------
      INTEGER*2 HITD,HCELLD
      LOGICAL IC(96),LC(96),LCS(96)
      COMMON / CRDSTA / NDEAD, NCDEAD, HITD(10), HCELLD(10)
      EXTERNAL RDMTCO
      LOGICAL DDC300
C
      DATA MERR/0/
      DATA   IC /
     ,  .FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,
     ,  .FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,
     ,  .TRUE. ,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,
     ,  .FALSE.,.FALSE.,.FALSE.,.TRUE. ,.FALSE.,.FALSE.,.FALSE.,.FALSE.,
     ,  .FALSE.,.FALSE.,.FALSE.,.FALSE.,.TRUE. ,.FALSE.,.FALSE.,.FALSE.,
     ,  .FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,
     ,  .FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,
     ,  .FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,
     ,  .TRUE. ,.TRUE. ,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,
     ,  .FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,
     ,  .TRUE. ,.TRUE. ,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,
     ,  .FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE./
*** PMF 01/07/99 put some DATA statements from below here
       DATA NCALL / 0 /
       DATA NCA /0/ 
*** PMF (end)
C
C
C                                      --- CHECK CELL NUMBER ---
  100 IF (ICELL.GE.1.AND.ICELL.LE.96) GOTO 110
      DEADCL = .TRUE.
      MERR = MERR + 1
      IF (MERR.LT.100) PRINT 6000, ICELL
 6000 FORMAT(' ********** ILLEGAL CELL NUMBER',I10,' FOUND **********')
      RETURN
C
C                                      --- CHECK RUN NUMBER ---
  110 IF (NRUN.LE.100) GOTO 300
C
C                                      --- REAL DATA ---
***   DATA NCA /0/  ! PMF 01/07/99 put in decl. part
      IF (NCA.EQ.0) PRINT 6101
 6101 FORMAT('0---->  DEADCL FUNCTION  <----  LAST UPDATE 12/01/87'/' ')
      NCA = NCA + 1
C
  112 IF (NRUN.LT.24200) GOTO 113
        DEADCL = DDC300( ICELL )
        RETURN
C
  113 IF (NRUN.LT.6286) GOTO 114
      DEADCL = .FALSE.
      RETURN
C
  114 IF (NRUN.LT.5800) GOTO 115
      DEADCL = .FALSE.
      IF (ICELL.EQ.65 .OR. ICELL.EQ.66) DEADCL = .TRUE.
      RETURN
C
  115 IF (NRUN.LT.5548) GOTO 120
      DEADCL = IC(ICELL)
      RETURN
C                                      --- OLD RUNS ---
  120 CALL UCOPY(IC(1),LC(1),96)
      IF (NRUN.NE.5547) GOTO 190
      DO 180 J=25,48
  180 LC(J) = .TRUE.
      GOTO 200
  190 LC(28) = .FALSE.
      IF (NRUN.GE.3311) GOTO 200
      LC(73) = .TRUE.
      LC(74) = .TRUE.
      IF (NRUN.GE.2789) GOTO 200
      LC(81) = .FALSE.
      LC(82) = .FALSE.
      IF (NRUN.GE.2776) GOTO 200
      LC(65) = .FALSE.
      LC(66) = .FALSE.
      IF (NRUN.GE.2745) GOTO 200
      LC(73) = .FALSE.
      LC(74) = .FALSE.
      IF (NRUN.GE.2304) GOTO 200
      LC(37) = .FALSE.
  200 DEADCL = LC(ICELL)
      RETURN
C
C                                      --- MONTE CARLO DATA ---
  300 CONTINUE
***   DATA NCALL / 0 / ! PMF 01/07/99 put in decl. part
      NCALL = NCALL + 1
      DO 310 J=1,96
  310 LC(J) = .FALSE.
C                                      --- READ DEAD CELLS FROM /CRDSTA/
      MCDEAD = 0
      IF (NCDEAD.LE.0 .OR. NCDEAD.GT.10) GOTO 330
      MCDEAD = NCDEAD
      DO 320 K=1,NCDEAD
      KC = HCELLD(K)
      IF (KC.LE.0 .OR. KC.GT.96) GOTO 320
      LC(KC) = .TRUE.
  320 CONTINUE
C                                       --- AT 1. CALL:
C                                           PRINT DEAD CELL STATUS ---
  330 IF (NCALL.GT.1) GOTO 340
      PRINT 6600
      IF (MCDEAD.EQ.0) PRINT 6601
      IF (MCDEAD.NE.0) PRINT 6602, MCDEAD,(HCELLD(IPR),IPR=1,MCDEAD)
      PRINT 6603
 6600 FORMAT('0',16('DEADCELL'),'DEAD'/'       LOGICAL FUNCTION ',
     ,       '  D E A D C L   CALLED FOR MONTE CARLO DATA')
 6601 FORMAT('       NO DEAD CELLS FOUND IN COMMON/CRDSTA/')
 6602 FORMAT(' ',6X,I2,' DEAD CELLS FOUND IN COMMON/CRDSTA/ :',10I4)
 6603 FORMAT('       NO FURTHER CELLS SET TO <DEAD>'/
     ,       ' ',16('CELLDEAD'),'CELL'/' ')
      IF (NCALL.NE.1) GOTO 350
C
      DO 335 J=1,96
  335 LCS(J) = LC(J)
      GOTO 350
C                                      --- AT ALL LATER CALLS:
C                                          CHECK STATUS OF DEAD CELLS --
  340 ICHECK = 0
      DO 345 J=1,96
  345 ICHECK = ICHECK + LXOR(LC(J),LCS(J))
      IF (ICHECK.EQ.0) GOTO 350
C                                       --- PRINT NEW STATUS ---
      PRINT 6604
 6604 FORMAT('0',16('DEADCELL'),'DEAD'/'       STATUS OF DEAD CELLS IN',
     ,       ' MONTE CARLO CHANGED, NEW STATUS IS:')
      PRINT 6602, MCDEAD,(HCELLD(IPR),IPR=1,MCDEAD)
      PRINT 6603
C
      DO 348 J=1,96
  348 LCS(J) = LC(J)
C                                      --- CHECK CHANGE OF DEAD CELLS --
  350 DEADCL = LC(ICELL)
C
      RETURN
      END
      LOGICAL FUNCTION DDC300( ICELL )
C-----------------------------------------------------------
C  VERSION OF 09/01/87    LAST MOD 09/01/87    E ELSEN
C  CHECK STATUS OF CELL ICELL ( RANGE 1:96 )
C  IF NO HITS ARE FOUND IN THE JETC BANK IN THAT CELL AND
C     THE HARDWARE TRIGGER BIT HIT-WALL IS ON THEN
C        DDC300 = .TRUE.        ( CELL IS LIKELY TO BE DEAD )
C  ELSE  DDC300 = .FALSE.
C  ENDIF
C-----------------------------------------------------------
      IMPLICIT INTEGER*2 (H)
      COMMON / BCS / IW(1)
      DIMENSION HW(1)
      EQUIVALENCE (HW(1),IW(1))
      INTEGER CELL, SECTOR, TRIG2W, BIT, BITPAT
      INTEGER ONE 
      DATA ONE / 1 /
      LOGICAL FIRST 
      DATA FIRST / .TRUE. /
C
      IF( .NOT. FIRST ) GO TO 1
C       WRITE(6,9201)
C9201   FORMAT(' +++ DDC300 VERSION OF 09-01-87')
        IPJETC = IBLN('JETC')
        IPTRIG = IBLN('TRIG')
        FIRST = .FALSE.
    1 CONTINUE
C
      CELL = ICELL - 1
      DDC300 = .FALSE.
      NPJETC = IW(IPJETC)
      IF( NPJETC.LE.0 ) GO TO 8000
        IF( HW(NPJETC*2+CELL+4) -
     *      HW(NPJETC*2+CELL+3) .GT. 0 ) GO TO 8000
C                                           CELL HAS NO HITS
C                                           HAS THE T2 BIT BEEN SET?
          NPTRIG = IW(IPTRIG)
          IF( NPTRIG.LE.0 ) GO TO 8000
            IF( IW(NPTRIG-1).LE.0 .OR. IW(NPTRIG-2).EQ.2 ) GO TO 100
              NPTRIG = IW(NPTRIG-1)
  100         IF( IW(NPTRIG-2) .NE. 2 ) GO TO 8000
C                                           TRIG 2 IS THERE
                IF( CELL.LT.48 ) GO TO 1000
                  SECTOR = MOD( CELL, 48 )
                  TRIG2W = SECTOR/8*2 + 2
                  BIT = MOD( CELL, 8 ) * 2 + 16
                  GO TO 1100
 1000           CONTINUE
                  SECTOR = MOD( CELL, 24 )
                  TRIG2W = SECTOR/4*2 + 2
                  BIT = MOD( CELL, 4 ) * 2
                  IF( CELL.LT.24 ) BIT = BIT + 8
 1100           CONTINUE
                BITPAT = ISHFTL( ONE, BIT )
                DDC300 = LAND( IW(NPTRIG+TRIG2W), BITPAT ) .NE. 0
 8000 CONTINUE
      RETURN
      END
