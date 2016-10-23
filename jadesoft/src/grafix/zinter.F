C   15/02/80 002271401  MEMBER NAME  ZINTER   (JADESR)      FORTRAN
C   13/03/79         4  MEMBER NAME  ORZINTER (JADESR)      FORTRAN
C   13/03/79 C9031301   MEMBER NAME  ZINTER8  (JADESR)      FORTRAN
      SUBROUTINE ZINTER
      IMPLICIT INTEGER*2 (H)
C
#include "cgraph.for"
#include "clbpgm.for"
#include "cdata.for"
C
#include "czvpar.for"
                      DIMENSION AZVPAR(12),IZVPAR(12)
                      EQUIVALENCE(AZVPAR(1),IZVPAR(1),ZLOW)
C
#include "cworkzv.for"
C     COMMON /CZINTR/ ZVTX,DZV1,DZV2,PEAK,BACK,IFLAG,
C    ,                HUFLO,HOFLO,MAXZ ,HIST(100)
C                     DIMENSION ZVRSLT(58)
C                     EQUIVALENCE (ZVRSLT(1),ZVTX)
C
      DATA INSTRC/1/
C
 2002 FORMAT(8X,F7.0,2F6.0,2F6.1,4I6)
 2003 FORMAT(8X,F7.0,2F6.0,2F6.1,I6,' : FROM /CDATA/   ')
 2004 FORMAT(' ',3F7.0,2F7.1,I7)
 2005 FORMAT(' ',F6.0,F6.1,6I6,F6.1,F6.3)
 2006 FORMAT(' ',F7.0,2F7.1,2F7.0,I7)
C
C
      ITHIS=0
      IF(INSTRC.EQ.1)  CALL TRMOUT(80,
     1   'WELCOME TO ZINTER (MENU OF INST. KEYS: TYPE 1 !)^')
C
1     CALL TRMOUT(80,'ZV..^')
      INSTR=TERNUM(DUMMY)
      IF(INSTR.LT.1 .OR. INSTR.GT.13) RETURN
      GOTO (100,200,300,400,500,600,700,800,900,1000,
     ,     1100,1200,1300), INSTR
C
C
C                                     **********************************
C                                     *       MENU OF INSTR. KEYS      *
C                                     **********************************
 100  CONTINUE
      CALL TRMOUT(80,' 0: EXIT                  1: INSTRUCTION LIST
     1 2: HARDCOPY            ^')
      CALL TRMOUT(80,' 3: JOYSTICK              4: RESET SCALE
     1 5: COMMENT OPTION      ^')
      CALL TRMOUT(80,' 6: SHOW EVENT            7: CALL ZVERTF
     1 8: CHANGE PARAMETERS   ^')
      CALL TRMOUT(80,' 9: RESET PARAMETERS     10: CHANGE RESULTS
     111: PRINT RESULTS       ^')
      CALL TRMOUT(80,'12: SHOW HISTOGRAM       13: RESULTS TO /CDATA/
     114: EXIT                ^')
      GOTO 1
C
C
C                                     **********************************
C                                     *      HARDCOPY                 *
C                                     **********************************
200   CALL HDCOPY
      GO TO 1
C
C
C                                     **********************************
C                                     *      JOYSTICK                  *
C                                     **********************************
300   CALL JOYS(0,0.) ! PMF 02/12/99 : second argument added
      GO TO 1
C
C
C                                     **********************************
C                                     *      RESET SCALE               *
C                                     **********************************
400   CALL SETSCL(ISTANV)
      GO TO 1
C
C
C                                     **********************************
C                                     *      COMMENT                   *
C                                     **********************************
500   CALL COMENT
      GO TO 1
C
C
C                                     **********************************
C                                     *   DISPLAY EVENT +  RESULTS     *
C                                     **********************************
  600 CONTINUE
      CALL ERASE
      CALL JADISP(4)
      CALL EVDISP(4)
C                                        DRAW VERTEX HISTOGRAM
      GOTO 1200
C
C
C                                     **********************************
C                                     *      CALL ZVERTF               *
C                                     **********************************
  700 CONTINUE
      CALL ZVERTF
      GOTO 1
C
C
C                                     **********************************
C                                     *   CHANGE PARAMETERS + LIMITS   *
C                                     **********************************
  800 CONTINUE
      CONTINUE !CALL ERASE PMF 02/12/99
      CALL TRMOUT(80,'ZVERT PARAMETERS:^')
      CALL TRMOUT(80,
     ,'  Z0:1 BIN:2 NBN:3 NR1:4 NC0:5 NC1:6 NC2:7 NPK:8 S/B:9 DFI:10^')
      WRITE(6,2005) (AZVPAR(I1),I1=1,7),NPKMIN,SBRAT,DFIMAX
 801  CALL TRMOUT(80,
     ,'CHANGE PAR. BY ENTERING INDEX AND NEW VALUE^')
      DO 810 I1=1,20
        CALL TONUM(IND,PAR)
        IF(IND.LE.0 .OR. IND.GT.10) GOTO 820
        IF(IND.GT.7) IND = IND + 2
        IF(IND.LE.2 .OR. IND.GT.10) GOTO 802
        IZVPAR(IND) = PAR+.5
        GOTO 810
  802   AZVPAR(IND) = PAR
  810   CONTINUE
  820 CONTINUE
      CALL TRMOUT(80,'NEW ZVERT PARAMETERS:^')
      CALL TRMOUT(80,
     ,'  Z0:1 BIN:2 NBN:3 NR1:4 NC0:5 NC1:6 NC2:7 NPK:8 S/B:9 DFI:10^')
      WRITE(6,2005) (AZVPAR(I1),I1=1,7),NPKMIN,SBRAT,DFIMAX
      CALL TRMOUT(80,'OK, YES OR NO?^')
      CALL DECIDE(IANSW)
      IF(IANSW.EQ.2) GOTO 801
      GOTO 1
C
C
C                                     **********************************
C                                     *  RESET PARAMETERS TO DEFAULT   *
C                                     **********************************
  900 CONTINUE
      CALL TRMOUT(80,'DEFAULT ZVERT PARAMETERS RESTORED:^')
      CALL INITZV
      CALL TRMOUT(80,
     ,'  Z0:1 BIN:2 NBN:3 NR1:4 NC0:5 NC1:6 NC2:7 NPK:8 S/B:9 DFI:10^')
      WRITE(6,2005) (AZVPAR(I1),I1=1,7),NPKMIN,SBRAT,DFIMAX
      GOTO 1
C
C
C                                     **********************************
C                                     *     CHANGE RESULTS             *
C                                     **********************************
 1000 CONTINUE
      CONTINUE !CALL ERASE PMF 02/12/99
      CALL TRMOUT(80,'CHANGE RESULTS OF ZVERTF^')
      CALL TRMOUT(80,' ZVTX:1 DZV1:2 DZV2:3 PEAK:4 BACK:5 FLAG:6^')
      WRITE(6,2004) ZVTX,DZV1,DZV2,PEAK,BACK,IFLAG
1001  CALL TRMOUT(80,'CHANGE PAR. BY ENTERING INDEX AND NEW VALUE^')
      DO 1010 I1=1,20
        CALL TONUM(IND,PAR)
        IF(IND.LE.0 .OR. IND.GT. 6) GOTO 1020
        IF(IND.LT.6) GOTO 1002
        IFLAG = PAR+.5
        GOTO 1010
 1002   FZRSLT(IND) = PAR
 1010   CONTINUE
 1020 CONTINUE
      CALL TRMOUT(80,'NEW ZVERT RESULTS:^')
      CALL TRMOUT(80,' ZVTX:1 DZV1:2 DZV2:3 PEAK:4 BACK:5 FLAG:6^')
      WRITE(6,2006) ZVTX,DZV1,DZV2,PEAK,BACK,IFLAG
      CALL TRMOUT(80,'OK, YES OR NO?^')
      CALL DECIDE(IANSW)
      IF(IANSW.EQ.2) GOTO 1001
      GOTO 1
C
C
C                                     **********************************
C                                     *         PRINT RESULTS          *
C                                     **********************************
 1100 CONTINUE
      CALL TRMOUT(63,
     ,'           ZVTX  DZV1  DZV2  PEAK  BACK  FLAG  UFLO  OFLO   MAX')
      I0 = IDATA(IBLN('ZVTX')) + 1
      IF(I0.LE.1) GOTO 1102
      I9 = I0 + 5
      WRITE(6,2003) (IDATA(I1),I1=I0,I9)
 1102 WRITE(6,2002) ZVTX,DZV1,DZV2,PEAK,BACK,IFLAG,
     ,                   HUFLO,HOFLO,MAXZ
      GOTO 1
C
C
C                                     **********************************
C                                     *    SHOW HISTOGRAM + RESULT     *
C                                     **********************************
 1200 X=ZLOW
      Y=0.
      CALL MOVEA(X,Y)
      IF(NBINZ.LT.1) GOTO 1
      DO 1210 I=1,NBINZ
      Y=100.*HIST(I)/(MAXZ+0.01)
      CALL DRAWA(X,Y)
      X=ZLOW+BINZ*I
      CALL DRAWA(X,Y)
 1210 CONTINUE
      Y=0.
      CALL DRAWA(X,Y)
C                                       DRAW ZVERTEX WITH ERRORS
      X1  = ZVTX - DZV1
      X11 = ZVTX - DZV2
      X2  = ZVTX + DZV1
      X22 = ZVTX + DZV2
      Y0  = - 70.
      Y1  = - 90.
      Y2  = - 50.
      CALL MOVEA(X1  , Y1)
      CALL DRAWA(X1  , Y2)
      CALL MOVEA(X1  , Y0)
      CALL DRAWA(X2  , Y0)
      CALL MOVEA(X2  , Y1)
      CALL DRAWA(X2  , Y2)
      CALL MOVEA(X22 , Y2)
      CALL DRAWA(X22 , Y1)
      CALL MOVEA(ZVTX, Y1)
      CALL DRAWA(ZVTX, Y2)
      CALL MOVEA(X11 , Y2)
      CALL DRAWA(X11 , Y1)
      CALL MOVEA(X1  , Y1)
      GOTO 1
C
C
C                                     **********************************
C                                     *    COPY RESULTS TO /CDATA/     *
C                                     **********************************
 1300 CONTINUE
      CALL TRMOUT(80,'NEW RESULTS TO /CDATA/: YES OR NO?^')
      CALL DECIDE(IANSW)
      IF(IANSW.EQ.2) GOTO 1
      IPZV = IDATA(IBLN('ZVTX'))
      IF(IPZV.GT.0) GOTO 1302
         NWRES = 6
         NBNK = 99
         CALL CCRE(IPZV,'DEDX',NBNK,NWRES,IERR)
         IF(IERR.NE.0) GO TO 1307
         CALL BSAW(1,'ZVTX')
         GOTO 1304
 1302 CONTINUE
         IDATA(IPZV-2) = 99
 1304 CONTINUE
         ADATA(IPZV+1) = FZRSLT(1)
         ADATA(IPZV+2) = FZRSLT(2)
         ADATA(IPZV+3) = FZRSLT(3)
         ADATA(IPZV+4) = FZRSLT(4)
         ADATA(IPZV+5) = FZRSLT(5)
         ADATA(IPZV+6) = FZRSLT(6)
      GOTO 1
1307  CALL TRMOUT(80,' ERROR IN CREATING BANK ZVTX ^')
      GO TO 1
C
C
      END
