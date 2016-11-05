C   26/06/87 712141919  MEMBER NAME  ADPATR   (S)           FORTRAN77
      SUBROUTINE ADPATR( NRUN )
C-----------------------------------------------------------
C   VERSION OF 26/06/87     LAST MOD 08/12/87   E ELSEN
C   ADJUST RUN DEPENDENT PATREC LIMITS
C-----------------------------------------------------------
      IMPLICIT INTEGER*2 (H)
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
      COMMON /BCS/ HW(1)
      INTEGER IW(1)
      EQUIVALENCE (IW(1),HW(1))
      INTEGER IPHEAD / 0 /
C
      IF( IPHEAD .LE. 0 ) IPHEAD = IBLN('HEAD')
      NPHEAD = IW(IPHEAD)
C
      IF( NRUN .GE. 24200 .OR.
     *  ( NRUN .LT. 100 .AND. HW(NPHEAD*2+8) .GT. 1985 ) ) THEN
        FLINLM(2) = 1.0
        GFP(3) = 2.0
        ZFITLM(1) = 70.
        ZFITLM(2) = 40.
      ELSE
        FLINLM(2) = 3.0
        GFP(3) = 4.5
        ZFITLM(1) = 50.
        ZFITLM(2) = 20.
      ENDIF
      END
