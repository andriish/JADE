CDECK  ID>, QQCONE.
      SUBROUTINE QQCONE( IDIM, NTRAK, PTRAK, MAXJET, NBC, XCR, XCE, ECMS
     &                  ,NJCE, NJCR )
      IMPLICIT NONE
C  Routine computes epsilon- and R-cone jets from array PTRAK
C  Input:   IDIM    1st dimension of array PTRAK
C           NTRAK   Number of entries in PTRAK
C           PTRAK() array of 5-momenta
C           NBC     Number of bins for cone jets
C           XCR     Radian values for cone jets at fixed energy
C           XCE     Energy values for cone jets at fixed cone size
C           ECMS    Centre of mass energy for cone jets
C  Output:  NJCE  Number of jets found with epsilon cone jet finder
C           NJCR  Number of jets found with R cone jet finder
C  Author: Stefan Kluth
C  Date: 08.10.02
C  Modifications:
      INTEGER IDIM, NTRAK, MAXJET, NBC, NJCE(NBC), NJCR(NBC)
      REAL PTRAK(IDIM,*), XCR(NBC), XCE(NBC), ECMS
      LOGICAL LPRINT
      PARAMETER( LPRINT=.FALSE. )
      INTEGER IERR, I, NYCLMX
      PARAMETER( NYCLMX=500 )
      INTEGER IPASS(NYCLMX), IJMUL(MAXJET)
      REAL EVIS, EPSLON, PJET(5,MAXJET)
      IF( NTRAK .GE. 2 ) THEN
        IF( NTRAK.GT.NYCLMX ) THEN
          PRINT *, 'QQCONE: Local arrays too short, no jet finding:'
     &         ,NYCLMX, NTRAK
          RETURN
        ENDIF
C       Cone jets, first vary epsilon, then the angle R:
        EVIS= 0.0
        DO I= 1, NTRAK
          EVIS= EVIS+PTRAK(4,I)
        ENDDO
        DO I= 1, NBC
          EPSLON= XCE(I)*EVIS/ECMS
          CALL PXCONE( NTRAK, IDIM, PTRAK, 0.7, EPSLON, MAXJET
     &                ,NJCE(I), PJET, IPASS, IJMUL, IERR )
          IF( IERR.NE.0 ) THEN
            PRINT *, 'QQCONE: Cone jet E error', IERR
            NJCE(I)= -1
          ENDIF
        ENDDO
        EPSLON= 7.0*EVIS/ECMS
        DO I= 1, NBC
          CALL PXCONE( NTRAK, IDIM, PTRAK, XCR(I), EPSLON, MAXJET
     &                ,NJCR(I), PJET, IPASS, IJMUL, IERR)
          IF( IERR.NE.0 ) THEN
            PRINT *, 'QQCONE: Cone jet R error', IERR
            NJCR(I)= -1
          ENDIF
        ENDDO
      ELSE
        DO I= 1, NBC
          NJCE(I)= -1
          NJCR(I)= -1
        ENDDO
      ENDIF
C
C  The End:
      RETURN
      END
