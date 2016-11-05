      SUBROUTINE TRCDCK
      IMPLICIT INTEGER*2 (H)
      LOGICAL TBIT
#include "cpatlm.for"
#include "cworkpr.for"
      COMMON /CWORK/ DWORK(1000)
#include "cworkeq.for"
      EQUIVALENCE
     ,           (ICELL ,IDWRK(1)),(NHIT  ,IDWRK(2)),(IRING ,IDWRK(3))
     ,         , (IERRCD,IDWRK(4)),(NTRKEL,IDWRK(5))
      DIMENSION NTRC( 3), LHIT(3),MHIT(3)
      INTEGER*2 HZW(2)
      EQUIVALENCE (HZW(1),IZW)
      COMMON /CPTSLM/ BKTRLM(20),HXREF(200,3)
                      INTEGER LMBKTR(20)
                      EQUIVALENCE (BKTRLM(1),LMBKTR(1))
 2003 FORMAT(1H0,A4,I6,2(/,1X,24I4),/,1X,50I2)
 2004 FORMAT(1H0,'BACKTR:',5X,20I6)
 2005 FORMAT(1H ,12X,20I6)
 2006 FORMAT(1X,4I6,I3,2F10.3,I3,2F10.3,1X,Z5,2I3,2X,3I3)
      IF(HNTR.LE.0) RETURN
      ASSIGN 17001 TO IZZZ01
      GOTO 17000
17001 CONTINUE
      MINHIT = 1
      ASSIGN 17003 TO IZZZ02
      GOTO 17002
17003 CONTINUE
      ASSIGN 17004 TO IZZZ01
      GOTO 17000
17004 CONTINUE
      MINHIT = 5
      ASSIGN 17005 TO IZZZ02
      GOTO 17002
17005 CONTINUE
      ASSIGN 17006 TO IZZZ01
      GOTO 17000
17006 CONTINUE
      RETURN
17002 CONTINUE
      DO 13000 ITRL=1,HNTR
      IF(
     - HXREF(ITRL,2).NE.0
     -)THEN
          NTRC(1) = HXREF(ITRL,1)
          NTRC(2) = HXREF(ITRL,2)
          NTRC(3) = HXREF(ITRL,3)
          LHIT(1) = 0
          LHIT(2) = 0
          LHIT(3) = 0
          MHIT(1) = 0
          MHIT(2) = 0
          MHIT(3) = 0
          ITRC = 1
16000 CONTINUE
            JTRC = NTRC(ITRC)
            NTRL = HNREL(JTRC)
            LHIT0 = 0
            MHIT0 = 0
      DO 13002 JTRL=1,NTRL
              KTRL = HISTR(JTRL,JTRC)
              KTRL = IABS(KTRL)
      IF(
     - HXREF(KTRL,2).EQ.0
     -)THEN
                MHIT0 = NRHT(KTRL) + MHIT0
      ELSE
                LHIT0 = NRHT(KTRL) + LHIT0
      ENDIF
13002 CONTINUE
13003 CONTINUE
            IF(MHIT0.LT.MINHIT) NTRC(ITRC) =-NTRC(ITRC)
            MHIT(ITRC) = MHIT0
            LHIT(ITRC) = LHIT0
          ITRC = ITRC + 1
      IF(.NOT.(
     - ITRC.GT.3 .OR. NTRC(ITRC).EQ.0
     -))GOTO 16000
16001 CONTINUE
      IF(
     - NTRC(1).LE.0 .AND. NTRC(2).LE.0 .AND. NTRC(3).LE.0
     -)THEN
            LHIT(1) = MHIT(1) + LHIT(1)
            LHIT(2) = MHIT(2) + LHIT(2)
            LHIT(3) = MHIT(3) + LHIT(3)
            IF(MAX0(LHIT(1),LHIT(2)).LT.LHIT(3)) NTRC(3) =-NTRC(3)
            IF(MAX0(LHIT(1),LHIT(3)).LT.LHIT(2)) NTRC(2) =-NTRC(2)
            IF(MAX0(LHIT(2),LHIT(3)).LT.LHIT(1)) NTRC(1) =-NTRC(1)
      IF(
     - NTRC(1).LE.0 .AND. NTRC(2).LE.0 .AND. NTRC(3).LE.0
     -)THEN
      IF(
     - LHIT(1).GE.LHIT(2)
     -)THEN
                NTRC(1) =-NTRC(1)
      ELSE
                NTRC(2) =-NTRC(2)
      ENDIF
      ENDIF
      ENDIF
          ITRC = 1
16002 CONTINUE
            JTRC = NTRC(ITRC)
      IF(
     - JTRC.LT.0
     -)THEN
              JTRC = IABS(JTRC)
              NTRL = HNREL(JTRC)
      DO 13004 JTRL=1,NTRL
                KTRL = HISTR(JTRL,JTRC)
                KTRL = IABS(KTRL)
      IF(
     - HXREF(KTRL,1).EQ.JTRC
     -)THEN
                  HXREF(KTRL,1) = HXREF(KTRL,2)
                  HXREF(KTRL,2) = HXREF(KTRL,3)
                  HXREF(KTRL,3) = 0
      ELSE
      IF(
     - HXREF(KTRL,2).EQ.JTRC
     -)THEN
                    HXREF(KTRL,2) = HXREF(KTRL,3)
                    HXREF(KTRL,3) = 0
      ELSE
                    IF(HXREF(KTRL,3).EQ.JTRC) HXREF(KTRL,3)=0
      ENDIF
      ENDIF
13004 CONTINUE
13005 CONTINUE
              NBYTE = (NTR-JTRC)*2
              IF(NBYTE.GT.0)
     ?        CALL MVC2(HNREL(JTRC),0,HNREL(JTRC+1),0,NBYTE) !PMF 28/06/99 MVC -> MVC2
              NBYTE = NBYTE*9
              IF(NBYTE.GT.0)
     ?        CALL MVCL2(HISTR(1,JTRC),0,HISTR(1,JTRC+1),0,NBYTE) !PMF 28/06/99 MVCL -> MVCL2
              HNREL(NTR) = 0
              CALL SETS(HISTR(1,NTR),0,18,0)
              NTR = NTR - 1
      DO 13006 I=1,HNTR
                IF(HXREF(I,1).GT.JTRC) HXREF(I,1) = HXREF(I,1) - 1
                IF(HXREF(I,2).GT.JTRC) HXREF(I,2) = HXREF(I,2) - 1
                IF(HXREF(I,3).GT.JTRC) HXREF(I,3) = HXREF(I,3) - 1
13006 CONTINUE
13007 CONTINUE
      ENDIF
          ITRC = ITRC + 1
      IF(.NOT.(
     - ITRC.GT.3 .OR. NTRC(ITRC).EQ.0
     -))GOTO 16002
16003 CONTINUE
      ENDIF
13000 CONTINUE
13001 CONTINUE
      GOTO IZZZ02
17000 CONTINUE
        PRINT 2003, HPRO,HNTR,HNTCEL
        IP0 = HPTE0
      DO 13008 I1=1,HNTR
        PRINT 2006, IP0,I1,(TRKAR(I1,I2),I2=1,11),
     ,              HXREF(I1,1),HXREF(I1,2),HXREF(I1,3)
           IP0 = IP0 + HLDTE
13008 CONTINUE
13009 CONTINUE
        PRINT 2004,NTR, (HNREL(I1),I1=1,NTR)
      DO 13010 ITR=1,NTR
          NELM = HNREL(ITR)
          PRINT 2005, (HISTR(I1,ITR),I1=1,NELM)
13010 CONTINUE
13011 CONTINUE
      GOTO IZZZ01
      END
