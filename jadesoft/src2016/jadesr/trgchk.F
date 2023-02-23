      SUBROUTINE TRGCHK(LBTRCK,LBTRBT)
      IMPLICIT INTEGER*2 (H)
      LOGICAL TBIT
#include "cdata.for"
#include "cheadr.for"
      COMMON /CWORK/ ILGE(84),HCLWR(96)
      DATA MKTREV /Z'673'/,MKT2AC/Z'73'/,MKEHIG/Z'600'/,MKUNTR/Z'F88C'/
      INTEGER MKTRBT( 7) /1,2,4,8,16,32,64/
      INTEGER NCTOF ( 7) /2,3,3,3,3,3,3/
      INTEGER ICTOF ( 7) /0,1,2,3,4,5,6/
      LBTRCK = 0
      IF(
     - LAND(LBTRBT,MKUNTR).NE.0
     -)THEN
        LBTRCK = 32
        RETURN
      ENDIF
      IF(LAND(LBTRBT,MKTREV).EQ.0) RETURN
      IF(LAND(LBTRBT,MKEHIG).NE.0) LBTRCK = 16
      IF(LAND(LBTRBT,MKT2AC).EQ.0) RETURN
      CALL SETSL(ILGE(1),0,528,0)
16000 CONTINUE
        IPALGN = IDATA(IBLN('ALGN'))
      IF(
     - IPALGN.LE.0
     -)THEN
      GOTO 16001
      ENDIF
      IF(
     - IDATA(IPALGN).LE. 3
     -)THEN
      GOTO 16001
      ENDIF
        IPLG2 = IPALGN*2
        NBARR = HDATA(IPLG2+ 4) - HDATA(IPLG2+ 3)
      IF(
     - NBARR.LE.0
     -)THEN
      GOTO 16001
      ENDIF
        IP0 = IPLG2 + 7
        IP9 = IP0 + NBARR - 1
      DO 13000 IP=IP0,IP9,2
          NBL  = HDATA(IP)
          IROW = ISHFTR(NBL,5)
          ILGE(IROW+1) = ILGE(IROW+1) + HDATA(IP+1)
13000 CONTINUE
13001 CONTINUE
      IF(.NOT.(
     - .TRUE.
     -))GOTO 16000
16001 CONTINUE
      IPJETC = IDATA(IBLN('JETC'))
      IF(IPJETC.LE.0) RETURN
      IF(IDATA(IPJETC).LE.50) RETURN
      IPJC2 = IPJETC*2
      NHTR3 = HDATA(IPJC2+99) - HDATA(IPJC2+51)
      IF(NHTR3.LE.0) RETURN
      IP0 = IPJC2 + 100 + HDATA(IPJC2+51)
      IP9 = IP0 + NHTR3 - 1
      IWIR0 =-1
      DO 13002 IP=IP0,IP9,4
        IWIR = HDATA(IP)
        IWIR = ISHFTR(IWIR,3)
        HCLL = ISHFTR(IWIR,3) - 95
        IF(IWIR.NE.IWIR0) HCLWR(HCLL) = HCLWR(HCLL) + 1
        IWIR0 = IWIR
13002 CONTINUE
13003 CONTINUE
      IPLATC = IDATA(IBLN('LATC'))
      IF(IPLATC.LE.0) RETURN
      IPLT2 = IPLATC*2
      IP0 = IPLT2 + 6
      IP9 = IP0 + 5
      ITLST =-100
      ITOF0 = 0
      ICLL0 = 0
      NTRCK = 0
      NTREL = 0
16002 CONTINUE
        ITRBT = HDATA(IP0)
      DO 13004 JTOF=1,7
      IF(
     - LAND(MKTRBT(JTOF),ITRBT) .NE. 0
     -)THEN
            ITOF = ITOF0 + JTOF
            JCTOF = (ICTOF(JTOF) + ICLL0)*2 - 1
            LBTRK = 0
      IF(
     - ITOF-ITLST.LE.2
     -)THEN
              HCLL9 = NCTOF(JTOF)*2 + JCTOF - 1
              HCLL1 =-99
              MTRK = 0
      DO 13006 ICLL=HCLL0,HCLL9
      IF(
     - HCLWR(ICLL).GE.6
     -)THEN
                  HDCLL = ICLL - HCLL1
      IF(
     - HDCLL.EQ.1
     -)THEN
                    HCLL1 = -99
      ELSE
      IF(
     - HDCLL.NE.3 .OR. TBIT(ICLL,31)
     -)THEN
                      MTRK = MTRK + 1
                      HCLL1 = ICLL
      ENDIF
      ENDIF
      ENDIF
13006 CONTINUE
13007 CONTINUE
              IF(MTRK.GE.2) LBTRK = 1
      ELSE
              NHIT5 = 0
              NHIT6 = 0
      IF(
     - JCTOF.LT.0
     -)THEN
                NHIT1 = HCLWR(95)
                NHIT2 = HCLWR(96)
                NHIT3 = HCLWR( 1)
                NHIT4 = HCLWR( 2)
                IF(NHIT2.GE.3 .AND. HCLWR(93).GE.6) NHIT2 = 8
                IF(NHIT4.GE.3 .AND. HCLWR( 3).GE.6) NHIT4 = 8
      ELSE
                NHIT1 = HCLWR(JCTOF  )
                NHIT2 = HCLWR(JCTOF+1)
                NHIT3 = HCLWR(JCTOF+2)
                NHIT4 = HCLWR(JCTOF+3)
      IF(
     - NCTOF(JTOF).EQ.3
     -)THEN
                  NHIT5 = HCLWR(JCTOF+4)
                  NHIT6 = HCLWR(JCTOF+5)
      ELSE
                  IF(NHIT2.GE.3 .AND. HCLWR(JCTOF-2).GE.6) NHIT2 = 8
                  IF(NHIT4.GE.3 .AND. HCLWR(JCTOF+4).GE.6) NHIT4 = 8
      ENDIF
      ENDIF
              IF(MAX0(NHIT1,NHIT2,NHIT3,NHIT4,NHIT5,NHIT6).GE.6) LBTRK=1
CAV              00000185
      ENDIF
      IF(
     - LBTRK.NE.0
     -)THEN
              NTRCK = NTRCK + 1
              ITLST = ITOF
              HCLL0 = JCTOF
              IR = ITOF*2 - 3
      IF(
     - ITOF.EQ.1
     -)THEN
                ILGENG = ILGE(83)+ILGE(  84)+ILGE(   1)+ILGE(   2)
      ELSE
      IF(
     - ITOF.EQ.42
     -)THEN
                  ILGENG = ILGE(82)+ILGE(  83)+ILGE(  84)+ILGE(   1)
      ELSE
                  ILGENG = ILGE(IR)+ILGE(IR+1)+ILGE(IR+2)+ILGE(IR+3)
      ENDIF
                IF(ILGENG.GT.1000) NTREL = NTREL + 1
      ENDIF
      ENDIF
      ENDIF
13004 CONTINUE
13005 CONTINUE
      ICLL0 = ICLL0 + 8
      ITOF0 = ITOF0 + 7
      IP0   = IP0   + 1
      IF(.NOT.(
     - IP0.GT.IP9
     -))GOTO 16002
16003 CONTINUE
      LB1TCL = 0
      IF(
     - NTRCK.EQ.1 .AND. NTREL.EQ.0
     -)THEN
        IPJC2 = IPJETC*2
        NHTR1 = HDATA(IPJC2+27) - HDATA(IPJC2+ 3)
      IF(
     - NHTR1.GT.96 .AND. NHTR1.LT.196
     -)THEN
          CALL SETS(HCLWR(1),0,96,0)
          IP0 = IPJC2 + 100 + HDATA(IPJC2+ 3)
          IP9 = IP0 + NHTR1 - 1
          IWIR0 =-1
      DO 13008 IP=IP0,IP9,4
            IWIR = HDATA(IP)
            IWIR = ISHFTR(IWIR,3)
            HCLL = ISHFTR(IWIR,3) + 1
            IF(IWIR.NE.IWIR0) HCLWR(HCLL) = HCLWR(HCLL) + 1
            IWIR0 = IWIR
13008 CONTINUE
13009 CONTINUE
          MTRK = 0
          HCLL1 =-99
      DO 13010 ICLL=1,48
      IF(
     - HCLWR(ICLL).GE.6
     -)THEN
              HDCLL = ICLL - HCLL1
      IF(
     - HDCLL.EQ.1
     -)THEN
                HCLL1 = -99
      ELSE
      IF(
     - HDCLL.NE.3 .OR. TBIT(ICLL,31)
     -)THEN
                  MTRK = MTRK + 1
                  HCLL1 = ICLL
      ENDIF
      ENDIF
      ENDIF
13010 CONTINUE
13011 CONTINUE
          IF(MTRK.EQ.2 .OR. MTRK.EQ.3) LB1TCL = 1
      ENDIF
      ENDIF
      IF(NTRCK .EQ.2 .AND. LAND(LBTRBT,64).NE.0) NTRCK = NTRCK - 1
      IF(NTRCK .GE.2) LBTRCK = LBTRCK + 2
      IF(NTRCK .EQ.1 .AND. LAND(LBTRBT,2).NE.0) LBTRCK = LBTRCK + 1
      IF(NTREL .EQ.1) LBTRCK = LBTRCK + 4
      IF(LB1TCL.EQ.1) LBTRCK = LBTRCK + 8
      RETURN
      END
