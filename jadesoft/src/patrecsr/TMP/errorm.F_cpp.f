C   06/12/79 102191154  MEMBER NAME  ERRORM   (PATRECSR)    FORTRAN
      SUBROUTINE ERRORM(SUBR,NERROR,NT)
      IMPLICIT INTEGER*2(H)
      REAL*8 SUBR
      COMMON/CADMIN/IEVTP,NREC,NRWRIT,NRERR
      COMMON/CHEADR/HEAD(17),HRUN,HEV
C----------------------------------------------
C  MACRO CPATLM .... PATTERN RECOGNITION LIMITS
C----------------------------------------------
      COMMON /CPATLM/ PATRLM(5),FLINLM(10),TRELLM(20),ZFITLM(10),BKK(20)
     *               ,XYF(20),IGFP(20),XBKK(40),IADMIN(5),YBKK(20)
      INTEGER IXYF(20),LMPATR(5),LMFLIN(10)
      INTEGER LMTREL(20),LMZFIT(10),IBKK(20)
      DIMENSION GFP(20),IXBKK(40),IYBKK(20)
      EQUIVALENCE (PATRLM(1),LMPATR(1)),(IXBKK(1),XBKK(1)),(IYBKK(1),
     *YBKK(1))   ,(FLINLM(1),LMFLIN(1)),(TRELLM(1),LMTREL(1))
     *           ,(ZFITLM(1),LMZFIT(1)),(BKK(1),IBKK(1))
     *           ,(XYF(1),IXYF(1)),(GFP(1),IGFP(1)),(IADMIN(1),IMCERT)
     *           ,(IYBKK(20),IPPASS),(IADMIN(2),IPFAST)
C----------- END OF MACRO CPATLM --------------
C
C
C-----------------------------------------------------------------------
C
C         -------------- SUBROUTINE ERRORM  ------------------
C         --- G.F.PEARCE .. LAST UPDATE : 1200 ON  6/12/80 ---
C
C     THIS SUBROUTINE PRINTS OUT A SINGLE LINE ERROR MESSAGE
C     REQUIRED :
C     SUBR   = ALPHANUMERIC SUBROUTINE NAME (8 BYTES) IN WHICH ERROR
C              OCCURRED (E.G. 'XYFIT   ')
C     NERROR = ERROR IDENTIFICATION NUMBER
C     NT     = TRACK NUMBER ON WHICH ERROR OCCURED
C
C-----------------------------------------------------------------------
C
C
      IXYF(11) = IXYF(11) - 1
      IF(IXYF(11).LT.0)RETURN
      PRINT1,SUBR,NERROR,NREC,HRUN,HEV,NT
 1    FORMAT(1X,20('#'),A8,' ERROR MESSAGE ',I3
     +  ,'  RECORD/RUN/EVENT/TRACK =',4I6,1X,20('#'))
      IF(IXYF(11).EQ.0)PRINT2
 2    FORMAT(1X,20('$'),' ERRORM ERROR MESSAGES NOW SUSPENDED')
      RETURN
      END
