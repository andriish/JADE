C   24/11/78 206041944  MEMBER NAME  USUNIGAM (JADEMC2)     FORTRAN
      SUBROUTINE USER(INDEX)
C---
C---     USER ROUTINE FOR CUTS AND INTERACTIVE DECISION MAKING.
C---
      IMPLICIT INTEGER*2 (H)
C
      COMMON // BLCOMM(15000)
#include "cgraph.for"
C
#include "cdata.for"
      COMMON /CHEADR/ HEAD(108)
      COMMON /CADMIN/ IEVTP,NRREAD,NRWRIT,NRERR
      DIMENSION IGG(30),JIGG(10)
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
99    JIGG(I) = 0
      DO 98 I = 1,30
98    IGG(I) = 0
      IPHMC = 25
      CALL HBOOK1( 1,' TOTAL ENERGY ALGN INT 1$',100,0.,2000.)
      CALL HBOOK1( 2,' TOTAL ENERGY ALGN INT 1$',100,0.,600.)
      CALL HBOOK1( 3,' TOTAL ENERGY ALGN INT 1$',100,0.,200.)
      CALL HBOOK1(11,' TOTAL ENERGY ALGN INT 2$',100,0.,2000.)
      CALL HBOOK1(12,' TOTAL ENERGY ALGN INT 2$',100,0.,600.)
      CALL HBOOK1(13,' TOTAL ENERGY ALGN INT 2$',100,0.,200.)
      CALL HBOOK1(21,' TOTAL ENERGY ALGN INT 3$',100,0.,2000.)
      CALL HBOOK1(22,' TOTAL ENERGY ALGN INT 3$',100,0.,600.)
      CALL HBOOK1(23,' TOTAL ENERGY ALGN INT 3$',100,0.,200.)
      CALL HBOOK1(31,' TOTAL ENERGY ALGN INT 4$',100,0.,2000.)
      CALL HBOOK1(32,' TOTAL ENERGY ALGN INT 4$',100,0.,600.)
      CALL HBOOK1(33,' TOTAL ENERGY ALGN INT 4$',100,0.,200.)
      CALL HBOOK1(41,' TOTAL ENERGY ALGN INT 5$',100,0.,2000.)
      CALL HBOOK1(42,' TOTAL ENERGY ALGN INT 5$',100,0.,600.)
      CALL HBOOK1(43,' TOTAL ENERGY ALGN INT 5$',100,0.,200.)
      CALL HBOOK1(51,' TOTAL ENERGY ALGN INT 6$',100,0.,2000.)
      CALL HBOOK1(52,' TOTAL ENERGY ALGN INT 6$',100,0.,600.)
      CALL HBOOK1(53,' TOTAL ENERGY ALGN INT 6$',100,0.,200.)
      CALL HBOOK1(61,' TOTAL ENERGY ALGN INT 7$',100,0.,2000.)
      CALL HBOOK1(62,' TOTAL ENERGY ALGN INT 7$',100,0.,600.)
      CALL HBOOK1(63,' TOTAL ENERGY ALGN INT 7$',100,0.,200.)
      CALL HBOOK1(71,' TOTAL ENERGY ALGN INT 8$',100,0.,2000.)
      CALL HBOOK1(72,' TOTAL ENERGY ALGN INT 8$',100,0.,600.)
      CALL HBOOK1(73,' TOTAL ENERGY ALGN INT 8$',100,0.,200.)
      CALL HBOOK1(81,' TOTAL ENERGY ALGN INT 9$',100,0.,2000.)
      CALL HBOOK1(82,' TOTAL ENERGY ALGN INT 9$',100,0.,600.)
      CALL HBOOK1(83,' TOTAL ENERGY ALGN INT 9$',100,0.,200.)
      CALL HBOOK1(91,' TOTAL ENERGY ALGN INT 0$',100,0.,2000.)
      CALL HBOOK1(92,' TOTAL ENERGY ALGN INT 0$',100,0.,600.)
      CALL HBOOK1(93,' TOTAL ENERGY ALGN INT 0$',100,0.,200.)
      CALL HBOOK1(101,' TOTAL ENERGY ALGN INT 10$',100,0.,2000.)
      CALL HBOOK1(102,' TOTAL ENERGY ALGN INT 10$',100,0.,600.)
      CALL HBOOK1(103,' TOTAL ENERGY ALGN INT 10$',100,0.,200.)
      CALL HBOOK1(111,' TOTAL ENERGY ALGN INT 11$',100,0.,2000.)
      CALL HBOOK1(112,' TOTAL ENERGY ALGN INT 11$',100,0.,600.)
      CALL HBOOK1(113,' TOTAL ENERGY ALGN INT 11$',100,0.,200.)
      CALL HBOOK1(121,' TOTAL ENERGY ALGN INT 12$',100,0.,2000.)
      CALL HBOOK1(122,' TOTAL ENERGY ALGN INT 12$',100,0.,600.)
      CALL HBOOK1(123,' TOTAL ENERGY ALGN INT 12$',100,0.,200.)
      KIBO = -1
      INDEX=INDEX+1
      RETURN
C -------------------------------------------------------------------
100   CONTINUE
      JIGG(1) = JIGG(1) + 1
      INDEX=INDEX+1
      RETURN
C -------------------------------------------------------------------
200   CONTINUE
C                                    SELECT EVENT
      JIGG(2) = JIGG(2) + 1
C     IBO = (JIGG(2)-1)/1000
      IF((JIGG(2)/500)*500.EQ.JIGG(2)) WRITE(6,117) JIGG(2),HEAD(19)
117   FORMAT(' RECORD AND EVENT NR ',2I10)
      IBO = (HEAD(19) - 1)/1000
      IBO = 10*IBO
      IF(IBO.EQ.KIBO) GO TO 222
      KIBO = IBO
      LIGO = 0
      WRITE(6,911) IBO,JIGG(2),HEAD(19)
911   FORMAT(' IBO JIGG2 HEAD19 ',3I10)
222   LIGO = LIGO + 1
      IF(LIGO.GT.10) GO TO 333
      CALL ERGTOT(ECYL,ECAMI,ECAPL)
      ETOT1 = ECYL + ECAMI + ECAPL
333   CALL RMALGN(IPHMC)
      CALL ERGTOT(ECYL,ECAMI,ECAPL)
      ETOT = ECYL + ECAMI + ECAPL
      IF(LIGO.GT.10) GO TO 444
      WRITE(6,445) ETOT1,ETOT
445   FORMAT(' ETOT BEFORE AND AFTER RMALGN ',2E12.4)
444   CALL HFILL(IBO + 1,ETOT,0,1.)
      CALL HFILL(IBO + 2,ETOT,0,1.)
      CALL HFILL(IBO + 3,ETOT,0,1.)
      GO TO 11
C -------------------------------------------------------------------
300   CONTINUE
C                                    LEAD GLASS CALIBRATION DONE
      JIGG(3) = JIGG(3) + 1
      INDEX=INDEX+1
      RETURN
C -------------------------------------------------------------------
400   CONTINUE
C                                    ZVERTEX CALCULATED
      JIGG(4) = JIGG(4) + 1
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
      JIGG(7) = JIGG(7) + 1
      INDEX=INDEX+1
      RETURN
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
      CALL HISTDO
      RETURN
C---
C---     RETURNS FOR STEERING ANALYSIS TO DESIRED NEXT STEP.
C---     'GO TO 1' MEANS REJECT EVENT AND GO TO NEXT EVENT.
C---     'GO TO 11' MEANS ACCEPT EVENT, WRITE IT AND GO TO NEXT EVENT
C---     'GO TO 12' MEANS END THE JOB, WRITE FINAL RESULTS AND PLOTS
C---
1     INDEX = 1
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
      IGG(11) = IGG(11) + 1
      RETURN
12    INDEX = 12
      RETURN
      END
