      SUBROUTINE CNEWID(INDEX)
      IMPLICIT INTEGER*2 (H)
#include "cdata.for"
#include "calibr.for"
      DATA ISTEAR/-2/,IRLAST/-99999/,IPLAST/-99999/,IPOINT/0/,IRPLST/0/
      DATA ICLV /0/
      DATA NCAL1/0/,IPLHDR/0/,IPRLIM/100/,IPRLIN/0/
      COMMON/JSCALD/ JESCAL,JESKEY,JESDRW
      INTEGER NVERTS/0/,IVERTS/0/,NVRUN(300,2)/600*0/
      REAL XVRUN(300)/300*0./,YVRUN(300)/300*0./
      INTEGER IUPDJS(6,40),NUPDJS/0/
      REAL RUPDJS(6,40)
      EQUIVALENCE (IUPDJS(1,1),RUPDJS(1,1))
      IF(
     - INDEX.LT.0
     -)THEN
         ISTEAR=INDEX
      IF(
     - ISTEAR.LE.-2
     -)THEN
            PRINT 110,IRLAST
      IF(
     - ISTEAR.EQ.-2
     -)THEN
               PRINT 111
      ELSE
               PRINT 112
      ENDIF
      ELSE
            PRINT 120,IRLAST
      ENDIF
110   FORMAT(//,' **** USE NEW CALIBRATION WHEN AVAILABLE AFTER RUN',I7)
111   FORMAT(6X,'WITH NEW RUN VERTEX',//)
112   FORMAT(6X,'WITHOUT SETTING NEW RUN VERTEX',//)
120   FORMAT(//,' **** USE OF OLD CALIBRATION FORCED AFTER RUN',I7,//)
         RETURN
      ENDIF
      IF(
     - NCAL1.EQ.0
     -)THEN
         NCAL1=1
         IPLHDR=IBLN('HEAD')
         JESKEY=54321
         IPOINT=ICALIB(13)
      ENDIF
      IPHDR2=IDATA(IPLHDR)*2
      IRUN=HDATA(IPHDR2+10)
      IF(
     - ISTEAR.GT.-2 .OR. IRUN.LE.100
     -)THEN
         JESCAL=0
         RETURN
      ENDIF
      ICDNUM=ICALIB(IPOINT+1)
      IF(
     - ICDNUM.LT.654320.OR.ICDNUM.GT.654322
     -)THEN
      IF(
     - IPRLIN.LT.IPRLIM
     -)THEN
            PRINT 9821,IRUN
9821        FORMAT(' *** NO VALID RECORD FOR NEW CALIBRATION',
     +      ' PRESENT AT RUN',I8)
            IPRLIN=IPRLIN+1
      ENDIF
         JESCAL=0
         RETURN
      ENDIF
      IF(
     - IRUN.NE.IRLAST
     -)THEN
         IRLAST=IRUN
      IF(
     - ICALIB(IPOINT+2).NE.IPLAST
     -)THEN
            IPLAST=ICALIB(IPOINT+2)
            IRPLST=ICALIB(IPOINT+3)
      IF(
     - ICDNUM.EQ.654320
     -)THEN
               JESCAL=0
               PRINT 721, IPLAST,IRPLST
721            FORMAT(/' **** OLD ID CALIBRATION WILL BE USED FOR',
     +         ' RUNS',I6,' TO',I6)
      ELSE
               JESCAL=IPLAST
               NVERTS=ICALIB(IPOINT+4)
      IF(
     - NVERTS.GT.0
     -)THEN
      DO 13000 J=1,NVERTS
                     NVRUN(J,1)=ICALIB(3*J+IPOINT+10275)
                     XVRUN(J)=  ACALIB(3*J+IPOINT+10276)
                     YVRUN(J)=  ACALIB(3*J+IPOINT+10277)
13000 CONTINUE
13001 CONTINUE
                  NVRUN(1,1)=IPLAST
                  I=1
15000 CONTINUE
      IF(
     - I.LT.NVERTS
     -)THEN
                     NVRUN(I,2)=NVRUN(I+1,1)-1
                     I=I+1
      GOTO 15000
      ENDIF
15001 CONTINUE
                  NVRUN(NVERTS,2)=IRPLST
                  IVERTS=1
15002 CONTINUE
      IF(
     - IRUN.GT.NVRUN(IVERTS,2)
     -)THEN
                     IVERTS=IVERTS+1
      GOTO 15002
      ENDIF
15003 CONTINUE
      ENDIF
               NUPDJS=ICALIB(IPOINT+5)
      IF(
     - NUPDJS.GT.0
     -)THEN
      DO 13002 J2=1,NUPDJS
      DO 13004 J1=1,6
                        IUPDJS(J1,J2)=ICALIB(6*J2+J1+IPOINT+11171)
13004 CONTINUE
13005 CONTINUE
13002 CONTINUE
13003 CONTINUE
      ENDIF
               JESDRW=ICDNUM-654321
               PRINT 720, JESCAL,IRPLST
720            FORMAT(/' **** NEW ID CALIBRATION WILL BE USED FOR',
     +         ' RUNS',I6,' TO',I6)
      ENDIF
      ENDIF
      IF(
     - IRUN.LT.IPLAST.OR.IRUN.GT.IRPLST
     -)THEN
      IF(
     - IPRLIN.LT.IPRLIM
     -)THEN
               PRINT 9823,IRUN,IPLAST,IRPLST
9823           FORMAT(' *** RUN',I7,' OUT OF RANGE OF CURRENT',
     +         ' CALIBRATION PERIOD:',I7,' TO',I7)
               IF(JESCAL.GT.0) PRINT 9824
9824           FORMAT(' FIRST RUN IN THIS PERIOD ASSUMED FOR',
     +         ' VERTEX AND UPDATES')
               IPRLIN=IPRLIN+1
      ENDIF
            IRUN=IPLAST
      ENDIF
      IF(
     - JESCAL.GT.0 .AND. NVERTS.GT.0
     -)THEN
      IF(
     - IRUN.GT.NVRUN(IVERTS,2)
     -)THEN
16000 CONTINUE
                  IVERTS=IVERTS+1
      IF(.NOT.(
     - IRUN.LE.NVRUN(IVERTS,2)
     -))GOTO 16000
16001 CONTINUE
      ELSE
      IF(
     - IRUN.LT.NVRUN(IVERTS,1)
     -)THEN
16002 CONTINUE
                     IVERTS=IVERTS-1
      IF(.NOT.(
     - IRUN.GE.NVRUN(IVERTS,1)
     -))GOTO 16002
16003 CONTINUE
      ENDIF
      ENDIF
      ENDIF
      IF(
     - JESCAL.GT.0 .AND. NUPDJS.GT.0
     -)THEN
      DO 13006 J=1,NUPDJS
               IND=IUPDJS(4,J)*96+IUPDJS(3,J)-91+IPOINT
      IF(
     - IRUN.GE.IUPDJS(1,J).AND.IRUN.LE.IUPDJS(2,J)
     -)THEN
                  ACALIB(IND)=RUPDJS(5,J)
      ELSE
                  ACALIB(IND)=RUPDJS(6,J)
      ENDIF
13006 CONTINUE
13007 CONTINUE
      ENDIF
      ENDIF
      IF(
     - JESCAL.GT.0 .AND. NVERTS.GT.0 .AND. ISTEAR.EQ.-2
     -)THEN
      ICLV = ICLV + 1
      IF(ICLV.EQ.1) WRITE(6,864)
864   FORMAT('  CNEWID ***:  RUN VERTICES CORRESPONDING TO NEW ID CALIBR
     $ATION SET IN /CALIBR/')
         IPV  = ICALIB(10)
         ACALIB(IPV+ 1) = XVRUN(IVERTS)
         ACALIB(IPV+ 3) = YVRUN(IVERTS)
      ENDIF
      RETURN
      END
