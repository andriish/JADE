C   08/11/78 102191158  MEMBER NAME  JDMAIN   (PATRECSR)    FORTRAN
C
C     MAIN PROGRAM FOR TESTS: P.STEFFEN(78/11/15)
C
      IMPLICIT INTEGER*2 (H)
C
      COMMON /CDATA/ NWORD,IDATA(4000)
                     DIMENSION HJCA(10)
                     EQUIVALENCE (HJCA(1),IDATA(1))
                     EQUIVALENCE (IPJCA,IDATA(61))
C
      COMMON /CBIN/ TIME(6),ZOF,ZRS,ZL,ZSC,EPSI(3),DOUB(3),IPN(3)
C
      COMMON /CCYCP/ HPTSEC(98)
C
      COMMON /CZVPAR/ LBZVDF,ZLOW,  BINZ,  NBINZ,
     ,                NWRDR1,LWRDC0,LWRDC1,LWRDC2,
     ,                IDZ1LM,IDZ2LM,NPKMIN,SBRAT,DFIMAX,
     ,                DZVPAR(5),
     ,                ZVTX,DZV1,DZV2,PEAK,BACK,IFLAG,
     ,                HUFLO(2),HOFLO(2),MAXZ ,HIST(100)
C
      COMMON /CWORK/ WORK(5000)
C
      COMMON /CLBPGM/ LBPGM(30)
C
      DATA NUNIT /1/
      DATA NREC /0/
C
 2001 FORMAT('0EOF REACHED ... STOP,',I6,' RECORDS READ')
 2002 FORMAT('0TIME LIMIT  ... STOP,',I6,' RECORDS READ')
C
C
 100   CONTINUE
C                                       CHECK TIME
        IF(IUHR(2).NE.1) GOTO 905
C                                                 READ MC-DATA
        NREC = NREC + 1
        CALL READMC(NUNIT,'DE',*900)
C                                         INITIALIZATION AFTER 1. RECORD
        IF(NREC.EQ.1) CALL INIT0
C                                                 FILL JCA-ARRAY
        NWORD2 = NWORD + NWORD
C                                                 INITIALIZE PGM-LABEL
        DO 120 I1=1,30
           LBPGM(I1) = 0
120     CONTINUE
C                                                 CHECK IF HITS IN JET C
      IF(IPJCA.LE.0) GOTO 100
C                                       PREPARE CYCLIC POINTER ARRAY
        CALL PRCYCP
C                                                 CALC. Z(VERTEX)
        CALL ZVERTF
C
        PRINT 2901, NREC, NWORD2
2901    FORMAT(1H1,'EVENT =',2I6,/,(1X,20I6))
C                                                 CHECK IF GOOD VERTEX
      IF(IFLAG.LT.0 .OR. ABS(ZVTX).GT.200.)  GOTO 100


C                                                 PATREC
      CALL PATREC
C                                                 CHECK IF END OF LOOP
      GOTO 100
C
 905  WRITE(6,2002) NREC
      GOTO 910
C                                                  EOF ... FINAL EVAL.
 900  CONTINUE
        WRITE(6,2001) NREC
 910    CONTINUE
        STOP
C
       END
C
      SUBROUTINE INIT0
C
C     CALL OF DIFFERENT INITIALIZATION ROUTINES
C
      CALL INITZV
      CALL INPATR
      CALL INPATC
      RETURN
      END
