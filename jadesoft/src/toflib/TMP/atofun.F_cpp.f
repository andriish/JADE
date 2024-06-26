C   15/02/82 704231156  MEMBER NAME  ATOFUN   (S)           FORTRAN
      SUBROUTINE ATOFUN(IPTOF)
C
C  ROUTINE TO UNPACK ATOF BANK
C  TOF COUNTERS WILL BE IN ARRAYS HADC,HTDC
C  FORWRD MUON COUNTERS WILL BE IN ARRAYS HADCF,HTDCF
C
      IMPLICIT INTEGER*2 (H)
C   18/02/82 202180024  MEMBER NAME  CHEAD    (S)           FORTRAN
      INTEGER *2  HEADR(108)
      COMMON /CHEADR/ IHEADR(54)
      EQUIVALENCE (IHEADR(1),HEADR(1))
***PMF include "ddatas.for"
C   12/03/79 007281422  MEMBER NAME  DDATA    (S)           FORTRAN
C
      COMMON /BCS/ IDATA(10000)
      DIMENSION DATA(10000)
      INTEGER *2 HDATA(20000)
      EQUIVALENCE (IDATA(1),DATA(1)), (HDATA(1),IDATA(1))
C
      COMMON/TFPED/ HADC(2,42),HTDC(2,42),HSTAT(42),HON(42)
      COMMON/FWPED/ HADCF(2,16),HTDCF(2,16),HFSTAT(16),HFON(16)
      COMMON/TFPED1/HTDC1(2,42),HON1(42),HTSPAR(16)
C
      DATA IBUG /10/,IERR,IERRAD/2*0/
C
      IBUG = IBUG + 1
      NRUN = HEADR(18)
      IEV  = HEADR(19)
C
      CALL SETSL(HON,0,42*2,0)
      CALL SETSL(HFON,0,16*2,0)
      CALL SETSL(HON1,0,42*2,0)
      JPTOF = IPTOF*2
      NLENG = IDATA(IPTOF)*2
      ITOFE =  JPTOF+NLENG
      HADC(1,1) = -99
C
C  BANK DESCRIPTOR
      IBDSCR = HDATA(JPTOF+1)
C
C  FOR MONTE CARLO
      IF(NRUN.EQ.0.AND.IBDSCR.EQ.0) IBDSCR = 1
      IF(IBDSCR.LT.1.OR.IBDSCR.GT.4)  GOTO  999
      GOTO  (100,200,300,400),IBDSCR
  999 IERR = IERR + 1
      IF(IERR.GT.10) RETURN
      PRINT 901,NRUN,IEV,IERR,IBDSCR,JPTOF
  901 FORMAT(' MESSAGE FROM TOF PROGRAM : BANK DESCR ERROR, PRINTING
     1 WILL STOP AFTER 20 ERRORS!',5I8)
      CALL ATOFRW(IPTOF)
      RETURN
C
  100 CONTINUE
      ITOFA =  2
      ITOFT = 93
      IMUA =  201
      IMUT =  240
      DO   1   I=1,44
      IF1 = 2*I-1
      IF2 = IF1 + 1
      JTOFT = JPTOF + ITOFT + IF1 + (IF1-1)/8
      JTOFA = JPTOF + ITOFA + IF1 + (IF1-1)/12
      IF(JTOFA+1.GT.ITOFE) GOTO  998
      IF(I.GT.16)  GOTO  2
C FORWARD MUON COUNTERS
      JMUT = JPTOF + IMUT + IF1 + (IF1-1)/8
      JMUA = JPTOF + IMUA + IF1 + (IF1-1)/12
      HADCF(1,I) = HDATA(JMUA)
      HADCF(2,I) = HDATA(JMUA+1)
      HTDCF(1,I) = HDATA(JMUT)
      HTDCF(2,I) = HDATA(JMUT+1)
      IF(HTDCF(1,I).LT.2048.OR.HTDCF(2,I).LT.2048) HFON(I) = I
    2 IF(I.GT.42)  GOTO  3
C TOFCOUNTERS
      HADC(1,I) = HDATA(JTOFA)
      HADC(2,I) = HDATA(JTOFA+1)
      HTDC(1,I) = HDATA(JTOFT)
      HTDC(2,I) = HDATA(JTOFT+1)
      IF(HTDC(1,I).LT.2048.OR.HTDC(2,I).LT.2048) HON(I) = I
      GOTO  1
    3 CONTINUE
C SPARE TDCS
      HTDC1(1,I-42) = HDATA(JTOFT)
      HTDC1(2,I-42) = HDATA(JTOFT+1)
      IF(HTDC1(1,I-42).LT.2048.OR.HTDC1(2,I-42).LT.2048)HON1(I-42)=I-42
    1 CONTINUE
      RETURN
  998 IERR = IERR + 1
      IF(IERR.GT.10) RETURN
      PRINT 903,NRUN,IEV,IERR,JPTOF,NLENG
  903 FORMAT(' MESS. FROM TOFPROG: WRONG BANKLENGTH PRINTING WILL STOP
     1 AFTER 20 !',7I8)
      CALL ATOFRW(IPTOF)
      RETURN
C
  200 CONTINUE
      CALL SETSL(HADC,0,42*4,0)
      CALL SETSL(HADCF,0,32*4,0)
      DO   35   I=1,42
      IF(I.LE.16) HTSPAR(I) = 2048
      DO   35   L=1,2
      HTDC1(L,I) = 2048
      HTDC(L,I) = 2048
      IF(I.LE.16) HTDCF(L,I) = 2048
   35 CONTINUE
      ITOFA =  JPTOF +4
      ITOFAA=  ITOFA
   30 IADD = HDATA(ITOFA)
      IAD = IADD
      IF(IAD.LE.0)  GOTO  39
      ITDC1 = HDATA(ITOFA+1)
      ITDC2 = HDATA(ITOFA+2)
      IADC1 = HDATA(ITOFA+3)
      IADC2 = HDATA(ITOFA+4)
   36 CONTINUE
      IF(IAD.GT.42)  GOTO  37
      HTDC(1,IAD)=ITDC1
      HTDC(2,IAD)=ITDC2
      HADC(1,IAD)=IADC1
      HADC(2,IAD)=IADC2
      IF(HTDC(1,IAD).LT.2048.OR.HTDC(2,IAD).LT.2048) HON(IAD) = IAD
      GOTO  31
   37 IF(IAD.GE.100) GOTO  38
      IBD = IAD-42
      IF(IBD.GT.42)  GOTO  39
      HTDC1(1,IBD) = ITDC1
      HTDC1(2,IBD) = ITDC2
      IF(HTDC1(1,IBD).LT.2048.OR.HTDC1(2,IBD).LT.2048) HON1(IBD) = IBD
      GOTO  31
   38 CONTINUE
      IAD = IAD - 99
      IF(IAD.GT.16)  GOTO  39
      HTDCF(1,IAD)=ITDC1
      HTDCF(2,IAD)=ITDC2
      IF(HTDCF(1,IAD).LT.2048.OR.HTDCF(2,IAD).LT.2048) HFON(IAD) = IAD
      HADCF(1,IAD)=IADC1
      HADCF(2,IAD)=IADC2
   31 CONTINUE
      GOTO  33
   39 IERRAD = IERRAD + 1
      IF(IERRAD.LE.10) PRINT 902,NRUN,IEV,IBUG,IERRAD,IADD,IAD,IBD
  902 FORMAT(' MESS. FROM TOF PROG : NRUN,IEV,NEV ',3I8,
     *    ' ERROR IN TOF ADDR',7I5)
      IF(IERRAD.LE.10) CALL ATOFRW(IPTOF)
C TRY TO FIND NEW ADDRESS
   34 ITOFA = ITOFA + 1
      IAD = HDATA(ITOFA)
      IF(ITOFA+4.GT.ITOFE)  RETURN
      IF(IAD.GT.0)  GOTO  34
      ITOFA = ITOFA + 1
      GOTO 32
C
   33 ITOFA = ITOFA + 6
   32 IF(ITOFA+4.LE.ITOFE)  GOTO  30
C
      RETURN
C
  300 CONTINUE
C CRATE 3
C TOFCOUNTERS  1-30, SPARES 1-8
      ITOFA =  2
      ITOFT = 76
      DO  51   I=1,30
      IF1 = 2*I-1
      JTOFA = JPTOF + ITOFA + IF1 + (IF1-1)/12
      IF4 = 4*I-3
      JTOFT = JPTOF + ITOFT + IF4 + (IF4-1)/8
      IF(JTOFT+3.GT.ITOFE) GOTO  998
      HADC(1,I) = HDATA(JTOFA)
      HADC(2,I) = HDATA(JTOFA+1)
      HTDC(1,I) = HDATA(JTOFT)
      HTDC(2,I) = HDATA(JTOFT+1)
      HTDC1(1,I) = HDATA(JTOFT+2)
      HTDC1(2,I) = HDATA(JTOFT+3)
      IF(HTDC(1,I).LT.2048.OR.HTDC(2,I).LT.2048) HON(I) = I
      IF(HTDC1(1,I).LT.2048.OR.HTDC1(2,I).LT.2048) HON1(I) = I
   51 CONTINUE
C CRATE 4
C TOFCOUNTERS  31-42,SPARES 9-16
      ITOFA =  211
      ITOFT =  237
      IA = 30
      DO  55   J=31,42
      I = J - IA
      IF1 = 2*I-1
      JTOFA = JPTOF + ITOFA + IF1 + (IF1-1)/12
      IF4 = 4*I-3
      JTOFT = JPTOF + ITOFT + IF4 + (IF4-1)/8
      IF(JTOFT+3.GT.ITOFE) GOTO  998
      HADC(1,J) = HDATA(JTOFA)
      HADC(2,J) = HDATA(JTOFA+1)
      HTDC(1,J) = HDATA(JTOFT)
      HTDC(2,J) = HDATA(JTOFT+1)
      HTDC1(1,J) = HDATA(JTOFT+2)
      HTDC1(2,J) = HDATA(JTOFT+3)
      IF(HTDC(1,J).LT.2048.OR.HTDC(2,J).LT.2048) HON(J) = J
      IF(HTDC1(1,J).LT.2048.OR.HTDC1(2,J).LT.2048) HON1(J) = J
   55 CONTINUE
C
C FORWARD MUON COUNTERS
      IMUA =  300
      IMUT =  339
      DO   61   I=1,16
      IF1 = 2*I-1
      IF2 = IF1 + 1
      JMUT = JPTOF + IMUT + IF1 + (IF1-1)/8
      JMUA = JPTOF + IMUA + IF1 + (IF1-1)/12
      HADCF(1,I) = HDATA(JMUA)
      HADCF(2,I) = HDATA(JMUA+1)
      HTDCF(1,I) = HDATA(JMUT)
      HTDCF(2,I) = HDATA(JMUT+1)
      IF(HTDCF(1,I).LT.2048.OR.HTDCF(2,I).LT.2048) HFON(I) = I
   61 CONTINUE
C
C SPARE TDCS
      ISP1 =  67
      ISP2 =  291
      DO   65   I=1,8
      JSP1 = JPTOF + ISP1 + I
      JSP2 = JPTOF + ISP2 + I
      HTSPAR(I) = HDATA(JSP1)
      HTSPAR(I+8) = HDATA(JSP2)
   65 CONTINUE
      RETURN
C
  400 CONTINUE
      CALL SETSL(HADC,0,42*4,0)
      CALL SETSL(HADCF,0,32*4,0)
      DO  405   I=1,42
      IF(I.LE.16) HTSPAR(I) = 2048
      DO  405   L=1,2
      HTDC1(L,I) = 2048
      HTDC(L,I) = 2048
      IF(I.LE.16) HTDCF(L,I) = 2048
  405 CONTINUE
      ITOFA =  JPTOF+4
      ITOFAA =  ITOFA
  410 IADD = HDATA(ITOFA)
      ITFAD = 8
      IF(IADD.GT.99)  GOTO 460
      IF(ITOFA+ITFAD-2.GT.ITOFE)  RETURN
      IF(IADD.LT.1)  GOTO 450
      IF(IADD.GT.42)  GOTO 450
      HTDC(1,IADD)= HDATA(ITOFA+1)
      HTDC(2,IADD)= HDATA(ITOFA+2)
      HTDC1(1,IADD)= HDATA(ITOFA+3)
      HTDC1(2,IADD)= HDATA(ITOFA+4)
      HADC(1,IADD)= HDATA(ITOFA+5)
      HADC(2,IADD)= HDATA(ITOFA+6)
      IF(HTDC(1,IADD).LT.2048.OR.HTDC(2,IADD).LT.2048)HON(IADD) = IADD
      IF(HTDC1(1,IADD).LT.2048.OR.HTDC1(2,IADD).LT.2048)HON1(IADD)=IADD
      GOTO 480
  450 IBD = IADD+1
      ITFAD = 8
      IF(IBD.LT.1.OR.IBD.GT.44)  GOTO 470
      IF(IBD.GE.44)  IBD = 9
      DO  453  L=1,6
      HTSPAR(IBD-1+L) = HDATA(ITOFA+L)
  453 CONTINUE
      GOTO  480
  460 CONTINUE
      ITFAD = 6
      IF(ITOFA+ITFAD-2.GT.ITOFE)  RETURN
      IAD = IADD - 99
      IF(IAD.LT.1)  GOTO  470
      IF(IAD.GT.16)  GOTO  470
      HTDCF(1,IAD)=HDATA(ITOFA+1)
      HTDCF(2,IAD)=HDATA(ITOFA+2)
      IF(HTDCF(1,IAD).LT.2048.OR.HTDCF(2,IAD).LT.2048) HFON(IAD) = IAD
      HADCF(1,IAD)=HDATA(ITOFA+3)
      HADCF(2,IAD)=HDATA(ITOFA+4)
C     PRINT 1401,(HDATA(ITOFA+L),L=1,4),HFON
      GOTO  480
  470 IERRAD = IERRAD + 1
      IF(IERRAD.LE.10) PRINT 902,NRUN,IEV,IBUG,IERRAD,IADD,IAD,IBD
      IF(IERRAD.LE.10) CALL ATOFRW(IPTOF)
C TRY TO FIND NEW ADDRESS
  475 ITOFA = ITOFA + 1
      IAD = HDATA(ITOFA)
      IF(ITOFA+ITFAD.GT.ITOFE)  RETURN
      IF(IAD.GT.0)  GOTO  475
      ITOFA = ITOFA + 1
      GOTO 410
C
  480 ITOFA = ITOFA + ITFAD
      GOTO  410
C
  500 RETURN
      END
