C   20/08/86 608281851  MEMBER NAME  PRJ68K   (S)           FORTRAN77
      SUBROUTINE PRJ68K( XS, YS, DEL, SIZE, IRETUR )
C-----------------------------------------------------------
C    VERSION OF 26/08/86    LAST MOD 26/08/86   E ELSEN
C    PRINT ROUTINE FOR J68K BANK UNDER GRAPHICS
C    IRETUR IS AN I/O ARGUMENT. FOR IRETUR.GT.0 P/O WILL
C    CONTINUE AFTER LAST LINE OUTPUT.
C    IF IRETUR.EQ.0 A NEW P/O WILL BE INITIALISED.
C    IRETUR > 0 ON EXIT IF P/O IS NOT YET COMPLETE
C-----------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
      COMMON / BCS / IW(1)
      DIMENSION HW(1),RW(1)
      EQUIVALENCE (HW(1),IW(1),RW(1))
C
      COMMON / CWORK1 / HWORK(80)
      CHARACTER*160 CLINE
      EQUIVALENCE (CLINE,HWORK(1))
C
C
      INTEGER PRC2CL
      INTEGER OLDWIR
C
      DATA IPRINT / 0 /, IPRLIM / 0 /
C
      EXTERNAL CL2PRC
C                                           INIT LINE COUNTER
      LINE = 0
      IF( IRETUR .EQ. 0 ) LINE = -1
C
      NPJ68K = IW(IBLN('J68K'))
      IF( NPJ68K.GT.0 ) THEN
C                                           PRINT J68K HEAD
        WRITE(CLINE,9101)
 9101   FORMAT(' J68K HEAD  STATUS(HEX) LENGTH       FEP',
     *         '   GENERAL SCAN_DATA  RAW-DATA')
        CALL J68KLI( XS, YS, DEL, SIZE, LINE, *8000 )
        WRITE(CLINE,9102) (HW(NPJ68K*2+I),I=1,HW(NPJ68K*2+4))
 9102   FORMAT((2I5,8X,Z4,1I8,4I10))
        CALL J68KLI( XS, YS, DEL, SIZE, LINE, *8000 )
C
C                                           FEP SECTION
        NPFEP = HW(NPJ68K*2+5)
        IF( NPFEP.GT.0 ) THEN
          NPFEP = NPFEP + NPJ68K*2
          L0 = HW(NPFEP+1)
          L1 = HW(NPFEP+2)
          WRITE(CLINE,9111) L0, L1
 9111     FORMAT('  FEP-SECTION:  PROCESSOR   STATUS(HEX)',
     *                         'TOTAL LENGTH=',I6,' WORDS/PROC=',I3)
          CALL J68KLI( XS, YS, DEL, SIZE, LINE, *8000 )
          J = 2
  100     IF( J .LT. L0 ) THEN
            WRITE(CLINE,9112) (HW(NPFEP+J+I),I=1,L1)
 9112       FORMAT(20X,I4,10X,Z4)
            CALL J68KLI( XS, YS, DEL, SIZE, LINE, *8000 )
            J = J + L1
            GO TO 100
          ENDIF
        ENDIF
C
C                                           SCAN DATA SECTION
        NPSCAN = HW(NPJ68K*2+7)
        IF( NPSCAN.GT.0 ) THEN
          NPSCAN = NPSCAN + NPJ68K*2
          NCRATE = HW(NPSCAN+1)
          WRITE(CLINE,9131) NCRATE
 9131     FORMAT('  SCAN-SECTION WITH DATA FOR ',I6,' DL300 CRATES')
          CALL J68KLI( XS, YS, DEL, SIZE, LINE, *8000 )
          NPSCAN = NPSCAN + 1
C
          DO 390 ICRATE=1,NCRATE
            L2 = HW(NPSCAN+1)
            IDL300 = HW(NPSCAN+2)
            OLDWIR = -1
            WRITE(CLINE,9132) IDL300, PRC2CL(IDL300),
     *                      PRC2CL(IDL300)+2, L2
 9132       FORMAT('   LENGTH OF SCAN_DATA FOR DL300 CRATE',I3,
     *             ' (CELL ',I2,'-',I2,') IS ',I6,' HALF WORDS')
            CALL J68KLI( XS, YS, DEL, SIZE, LINE, *8000 )
            IF( L2 .GT. 0 ) THEN
              NPSCAN = NPSCAN + 2
              LPT0 = 2
  310         IF( LPT0 .GE. L2 ) GO TO 380
                L3 =  HW(NPSCAN+1)
                LPT0 = LPT0 + L3
                IF( L3.GT.5 ) THEN
                  IWIRE = HW(NPSCAN+2)/256
                  ITOFF = HW(NPSCAN+2) - IWIRE*256
                  IPEDL = HW(NPSCAN+3)
                  IPEDR = HW(NPSCAN+4)
                  IAFAC = HW(NPSCAN+5)
                  IF( IWIRE .EQ. OLDWIR ) GO TO 370
                    WRITE(CLINE,9133)
                    CALL J68KLI( XS, YS, DEL, SIZE, LINE, *8000 )
 9133               FORMAT('    WIRE->ABS   T0 PEDESTALS AFAC',
     *                     '  PULSETRAIN (  L/ R )')
  370             OLDWIR = IWIRE
                  NPSCAN = NPSCAN + 5
                  L3 = L3 - 5
*** PMF 10/09/66
***               WRITE(CLINE,9134) IWIRE, IWIRE+PRC2CL(IDL300)*16,
***  *                          ITOFF, IPEDL, IPEDR, IAFAC,
***  *                 (HW(NPSCAN+I)/256,LAND(HW(NPSCAN+I),255),
***  *                                 I=1,MIN0(18,L3))
                  WRITE(CLINE,9134) IWIRE, IWIRE+PRC2CL(IDL300)*16,
     *                          ITOFF, IPEDL, IPEDR, IAFAC,
     *                 (HW(NPSCAN+I)/256,hLAND(HW(NPSCAN+I),hint(255)),
     *                                 I=1,MIN0(18,L3))
*** PMF (end)
 9134             FORMAT(3X,6I5,18(I3,I3,'|'))
                  CALL J68KLI( XS, YS, DEL, SIZE, LINE, *8000 )
                  DO 375 K=18, L3-1, 18
*** PMF 10/06/99
***                 WRITE(CLINE,9135)
***  *                   (HW(NPSCAN+K+I)/256,LAND(HW(NPSCAN+K+I),255),
***  *                                   I=1,MIN0(L3-K,18))
                    WRITE(CLINE,9135)
     *              (HW(NPSCAN+K+I)/256,hLAND(HW(NPSCAN+K+I),hint(255)),
     *                                   I=1,MIN0(L3-K,18))
*** PMF (end)
 9135               FORMAT((33X,18(I3,I3,'|')))
                    CALL J68KLI( XS, YS, DEL, SIZE, LINE, *8000 )
  375             CONTINUE
                ENDIF
                NPSCAN = NPSCAN + L3
                GO TO 310
  380         CONTINUE
            ENDIF
  390     CONTINUE
        ENDIF
      ENDIF
C                                           NORMAL EXIT
C                                           P/O COMPLETE
      IRETUR = 0
      RETURN
C                                           SET DEFAULT IRETUR FOR
C                                           INCOMPLETE P/O
 8000 IRETUR = 27
      RETURN
      END
      SUBROUTINE J68KLI( XS, YS, DEL, SIZE, LINE, * )
      IMPLICIT INTEGER*2 (H)
      COMMON / CWORK1 / HWORK(80)
      CHARACTER*160 CLINE
      EQUIVALENCE (CLINE,HWORK(1))
      INTEGER LLINE / 0 /
C
      IF( LINE.LT.0 ) THEN
        LLINE = 0
        LINE = 0
      ENDIF
      IF( LINE .GE. LLINE ) THEN
        YS = YS - DEL
        IF( YS .LE. 50 ) RETURN 1
        L = LENGCH(CLINE)
        CALL SYSSYM( XS, YS, SIZE, HWORK, L, 0. )
        LLINE = LLINE + 1
      ENDIF
      LINE = LINE + 1
      END
      INTEGER FUNCTION LENGCH(STRING)
      CHARACTER*(*) STRING
      L = LEN(STRING)
  100 IF( L.GT.0 ) THEN
        IF( STRING(L:L) .NE. ' ' )  GO TO 200
        L = L - 1
        GO TO 100
      ENDIF
  200 LENGCH = L
      END
