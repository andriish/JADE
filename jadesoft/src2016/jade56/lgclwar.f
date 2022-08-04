C   27/04/81 705171421  MEMBER NAME  LGCLWAR  (S)           FORTRAN
      SUBROUTINE USER(INDEX)
C---
C---     USER ROUTINE FOR CUTS AND INTERACTIVE DECISION MAKING.
C--       SPECIAL VERSION FOR READING EVENTS WITH RESO BANK
C--       DELETE LGCL BANK AND SCALE ALGN WITH LEAKAGE CORRECTION
C--       RECREATE LGCL, UPDATE RESO
C                                                 LAST CHANGE 15.05.87
      IMPLICIT INTEGER*2 (H)
C
      COMMON // BLCOMM(4000)
#include "cgraph.for"
#include "cgeo1.for"
#include "cdata.for"
#include "creso.for"
#include "ctrgmn.for"
#include "cintrg.for"
#include "cextra.for"
#include "cpoint.for"
      COMMON /CHEADR/ HEAD(108)
      COMMON /CTLIM/ ISECLF
      COMMON /CJTRIG/ PI,TWOPI
      DIMENSION IGG(10),JIGG(10)
      COMMON /CWORK1/ IER,NTRR,TRES(10,60)
      DIMENSION ITRES(10,60),PARX(4,4),P41(4),P42(4)
      EQUIVALENCE (TRES(1,1),ITRES(1,1))
      LOGICAL*1 TEXT
      DIMENSION TEXT(72)
      DATA ICNT4 /0/
C--
C---
C        INDEX=0   INITIAL CALL, BEFORE FIRST EVENT READ.
C              1   CALLED AT THE BEGINNING OF EACH NEW RUN.
C              2   CALLED IMMEDIATELY AFTER EVENT IS READ INTO CDATA.
C              3   LEAD GLASS ENERGIES HAVE BEEN COMPUTED.
C              4   FAST Z VERTEX RECONSTRUCTION HAS BEEN DONE.
C              5   INNER DETECTOR PATTERN RECOGNITION HAS BEEN RUN.
C              6   ENERGIES CLUSTERS IN THE LEAD GLASS HAVE BEEN FOUND.
C              7   TRACKS AND CLUSTERS HAVE BEEN ASSOCIATED.
C              8   MUON CHAMBER TRACKING HAS BEEN DONE.
C              9   MUON AND INNER DETECTOR TRACKS HAVE BEEN ASSOCIATED.
C             10   UNUSED
C---
C---     CHECK WHETHER CALL AT END OF JOB
      IF(INDEX.EQ.100) GOTO 9900
C
      GO TO (100,200,300,400,500,600,700,800,900,1000),INDEX
C---
C---     INDEX=0 INITIALIZATION.
C---
C
      DO 99 I = 1,10
      JIGG(I) = 0
99    IGG(I) = 0
      ISECLF = 2
      IRESO = 0
      IVXCOR = 6
      WRITE (6,6666)
6666  FORMAT(' LGCLVAR  VERSION FROM    2.05.87')
C
      READ 7608, TEXT
      WRITE(6,7508) TEXT
7608  FORMAT(72A1)
7508  FORMAT(' ',72A1)
C
      READ 7610, MAXEV1,MAXEV2,HRUNMI,HRUNMX,IHIST,IPRO,IBANK
      WRITE(6,7510) MAXEV1,MAXEV2,HRUNMI,HRUNMX,IHIST,IPRO,IBANK
7610  FORMAT(7I10)
7510  FORMAT(' ',7I10)
      READ 7608, TEXT
      WRITE(6,7508) TEXT
      READ 7611, SCLGC5,SCLGC6
      WRITE(6,7511) SCLGC5,SCLGC6
7611  FORMAT(7F10.3)
7511  FORMAT(' ',7F10.3)
C
C JBWTOR TO ENABLE OPERATOR TO CANCEL IN ORDERED FASHION...
C
      CALL JBWTOR(&2727)
      GO TO 2728
2727  WRITE(6,2729)
2729  FORMAT(' $$$$$$$$   JBWTOR INIT ERROR   $$$$$$$   ')
C     STOP
2728  CONTINUE
      RETURN
C -------------------------------------------------------------------
100   CONTINUE
      JIGG(1) = JIGG(1) + 1
      HRUN = HEAD(18)
      EBM = EBEAM(HRUN)*.001
      EBM3 = EBM/3.
      NRUN = HRUN
      ELCUTT = EBM*.25
      INDEX=INDEX+1
      RETURN
C -------------------------------------------------------------------
200   CONTINUE
C                                    SELECT EVENT
C
C CHECK IF OPERATOR ISSUED STOP OF JOB
C
      CALL JBWTOR(&3454)
C
      JIGG(2) = JIGG(2) + 1
      HEVENT = HEAD(19)
C
      IF((JIGG(2)/500)*500.EQ.JIGG(2))
     $ WRITE(6,2702) JIGG(2),HEAD(18),HEAD(19)
2702  FORMAT(' -*-*-*-*-*-*-*-*-*-*- RECORD RUN EVENT ',3I8)
C
      IF(HRUN.LT.HRUNMI) GO TO 7171
      IF(JIGG(2).LT.MAXEV1) GO TO 7171
      IF(HRUN.GT.HRUNMX) GO TO 7171
      IF(JIGG(2).GT.MAXEV2) GO TO 7171
C
C GET BANK RESO, UPDATE COMMON /CRESO/
C
      IPRESO = IDATA(IBLN('RESO'))
      IF(IPRESO.GT.0) GO TO 2007
      ICNT4 = ICNT4 + 1
      IF(ICNT4.LT.10) WRITE(6,2008) HRUN,HEVENT
2008  FORMAT(' WARNING ***** NO RESO BANK, EVENT NOT WRITTEN',2I8)
      GO TO 1
C FILL COMMON /CRESO/
2007  NWRES = IDATA(IPRESO)
      NWRES4 = 4*IDATA(IPRESO)
      IPRES1 = 4*IPRESO
      CALL MVCL(ARRESO,0,IDATA,IPRES1,NWRES4)
C               TO        FROM      BYTES
C
C
C  IF NO UNCONVERTED PHOTONS, WRITE EVENT OUT AGAIN
C
      IF(NMOMGM.NE.0) GO TO 967
      WRITE(6,968) HRUN,HEVENT
968   FORMAT(' EVENT WITH NMOMGM ZERO ',2I8)
      GO TO 1
967   ICNV = 0
C
      DO 964  IGM = 1,NMOMGM
      IF(HGMTYP(IGM).EQ.-2) ICNV = ICNV + 1
964   CONTINUE
      IF(NMOMGM.EQ.ICNV) GO TO 11
C
      CALL BDLS('LGCL',1)
C
C  SCALE ALL ENERGIES IN ALGN WITH FACTOR SCLGCL
C
      IALGN=IBLN('ALGN')
      IPJ=IDATA(IALGN)
      IF(IPJ.LE.0) GO TO 1
C
      NWO=IDATA(IPJ)
      IF(NWO.LE.3) GO TO 1
      IPJ=2*IPJ + 8
      NWO=IPJ+2*NWO-8
C
      DO 5004 IJK=IPJ,NWO,2
      IAD=HDATA(IJK-1)
      IEBLO=HDATA(IJK)
      EBLO = FLOAT(IEBLO)
      ISF6 = 0
      IF(IAD.GT.2687) GO TO 5009
      NFI = IAD/32
      NZ = IAD - NFI*32
      IF(NZ.GE.13.AND.NZ.LE.18) ISF6 = 1
      CALL BBLEAK(NZ,RLEAK)
      EBLO = EBLO/(1.-RLEAK)
C
5009  CONTINUE
      SCALE = SCLGC5
      IF(ISF6.EQ.1) SCALE = SCLGC6
C
      EBLO = SCALE * EBLO
C
      HDATA(IJK) = IFIX(EBLO)
C
5004  CONTINUE
C
      INDEX=INDEX+1
      RETURN
C -------------------------------------------------------------------
300   CONTINUE
C                                    LEAD GLASS CALIBRATION DONE
      JIGG(3) = JIGG(3) + 1
C                                                    ******************
C                                                    *  LRJCT = 11-20 *
C                                                    ******************
      INDEX=INDEX+1
      RETURN
C -------------------------------------------------------------------
400   CONTINUE
C                                    ZVERTEX CALCULATED
      JIGG(4) = JIGG(4) + 1
C                                                    ******************
C                                                    *  LRJCT = 21-30 *
C                                                    ******************
      INDEX=INDEX+1
      RETURN
C -------------------------------------------------------------------
500   CONTINUE
C                                    PATREC PERFORMED
      JIGG(5) = JIGG(5) + 1
      INDEX=INDEX+1
      RETURN
C -------------------------------------------------------------------
600   CONTINUE
C                                    CLUSTER ANALYSIS PERFORMED
      JIGG(6) = JIGG(6) + 1
      INDEX=INDEX+1
      RETURN
C -------------------------------------------------------------------
700   CONTINUE
C                                LGCDIR HAS BEEN CALLED
      JIGG(7) = JIGG(7) + 1
C
C LOOP OVER PHOTONS
C
      IPLGCL=IDATA(IBLN('LGCL'))
      IF(IPLGCL.GT.0) GO TO 1233
      WRITE(6,1234) HRUN,HEVENT
1234  FORMAT(' NO LGCL BANK, AFTER DELETE AND NEW ANALYSIS..',2I8)
      GO TO 11
C
1233  CONTINUE
C
      IF(IPRO.GT.0) CALL RESOWR
C
      NCLST = IDATA(IPLGCL+7)
      NWPCL = IDATA(IPLGCL+25)
      JUMPCL = IDATA(IPLGCL+2)
      IP = IDATA(IPLGCL+3) + IPLGCL - 1 - NWPCL
      ITP = 0
C
      DO 522  IGM = 1,NCLST
      IP = IP + NWPCL
C
C IS THIS A USED CLUSTER
C
      KMOMGM = 0
C
      DO 1006  IK = 1,NMOMGM
      IF(KGGTR(IK).EQ.IGM) KMOMGM = IK
      IF(KGGTR(IK).EQ.IGM) GO TO 1010
1006  CONTINUE
      GO TO 522
C
1010  CONTINUE
C
      CALL TRACK(IP,ITP,RMIN,NHT,FI,THE,PTOT,PT,PL,PX,PY,PZ)
C
      FITRA(NMOMTR+KMOMGM) = FI
      TETRA(NMOMTR+KMOMGM) = THE
      PETRA(NMOMTR+KMOMGM) = PT
      PETOT(NMOMTR+KMOMGM) = PTOT
      DPETOT(KMOMGM) = ADATA(IP+3)
522   CONTINUE
C
      IF(NMOMGM.GT.0) CALL UVXCR1
C
      CALL BDLS('RESO',0)
      CALL RESBNK
C
      IF(IPRO.GT.0) CALL RESOWR
C
      GO TO 11
C -------------------------------------------------------------------
800   CONTINUE
      JIGG(8) = JIGG(8) + 1
      INDEX=INDEX+1
      RETURN
C -------------------------------------------------------------------
900   CONTINUE
      JIGG(9) = JIGG(9) + 1
      INDEX=INDEX+1
      RETURN
C -------------------------------------------------------------------
1000  CONTINUE
      JIGG(10) = JIGG(10) + 1
      INDEX=INDEX+1
      RETURN
C -------------------------------------------------------------------
C---     END OF JOB: FINAL CALCULATIONS + PRINTOUT
9900  CONTINUE
      WRITE(JUSCRN,9901) JIGG
      WRITE(JUSCRN,9902) IGG
9901  FORMAT(' JIGG ',12I10)
9902  FORMAT(' IGG ',10I10)
      CALL BSTA
      IF(IHIST.NE.0) CALL HISTDO
      RETURN
C---
C---     RETURNS FOR STEERING ANALYSIS TO DESIRED NEXT STEP.
C---     'GO TO 1' MEANS REJECT EVENT AND GO TO NEXT EVENT.
C---     'GO TO 11' MEANS ACCEPT EVENT, WRITE IT AND GO TO NEXT EVENT
C---     'GO TO 12' MEANS END THE JOB, WRITE FINAL RESULTS AND PLOTS
C---
1     CONTINUE
7171  INDEX = 1
      RETURN
2     INDEX = 2
      RETURN
3     INDEX = 3
      RETURN
4     INDEX = 4
      RETURN
5     INDEX = 5
      RETURN
6     INDEX = 6
      RETURN
7     INDEX = 7
      RETURN
8     INDEX = 8
      RETURN
9     INDEX = 9
      RETURN
10    INDEX = 10
      RETURN
11    INDEX = 11
      RETURN
3454  CONTINUE
C
C  OPERATOR STOP
C
      WRITE(6,1470)  NNREC
1470  FORMAT(' $ $ $ $ $ $  OPERATOR STOP  $ $ $ $ $  EVENT ',I6)
12    INDEX = 12
      RETURN
      END
