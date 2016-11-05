      SUBROUTINE VTXERR(IERR) 
C*860529*KLEINWORT***************************************************
C*  COPIED FROM F22KLE.VERTEX.S(VTXERR)   18.9.86
C*                                                                  *
C*   PRINT ERROR-TEXT FOR   V E R T E X -FIT                        *
C*                                                                  *
C********************************************************************
      IMPLICIT INTEGER*2 (H)
C PMF 03.11.98 
      LOGICAL TBIT
C%MACRO MVERTEX2
C     MACRO FOR VERTEX-FIT ROUTINES ( AXIS AND STATISTICS )
      COMMON /CVTX2/ MODE,TAXIS(12),SVR,HVTXST(120)
C
      DIMENSION IVTXST(1)
C
C
      REAL*8 DTEXT(27)
      DIMENSION NSTART(10)
C
      DATA NSTART / 1, 4, 5, 6, 8, 14, 19, 24, 25, 28 /
      DATA DTEXT /
     *'NV < 1 O','R NV > 2','0       ',
     *'NT  <= 0',
     *'NTR <= 0',
     *'DET = 0.','0       ',
     *'NTR < 2 ','AFTER RE','JECTING ','TRACK WI','TH DS/R ','> 90 DEG',
     *'ERROR OF',' VERTEXC','OORDINAT','E(S) = 0','.0      ',
     *'NTR < 2 ','AFTER RE','JECTING ','BAD TRAC','K       ',
     *'NTR = 1 ',
     *'COLLINEA','R 2-PRON','G       ' /
C
      IF ((IERR.LT.1).OR.(IERR.GT.9)) GOTO 100
C        IF (HVTXST(4+IERR).GT.0)
      N1 = NSTART(IERR)
      N2 = NSTART(IERR+1)-1
      WRITE(6,9000) IERR,(DTEXT(K),K=N1,N2)
 9000 FORMAT(' ERROR ',I2,' : ',10A8)
      GOTO 200
C
  100 CONTINUE
C
      WRITE(6,9010) IERR
 9010 FORMAT(' ERROR ',I6,' IS UNKNOWN ')
C
  200 CONTINUE
C
      RETURN
      END
