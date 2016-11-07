CDECK  ID>, QQJETS.
      SUBROUTINE QQJETS( IDIM, NTRAK, PTRAK, IJTYP, MAXJET, NXJ, YFLIP )
      IMPLICIT NONE
C  Routine computes recombination jet algorithms
C  from array PTRAK
C  Input:   IDIM    1st dimension of array PTRAK
C           NTRAK   Number of entries in PTRAK
C           PTRAK() array of 5-momenta
C           IJTYP   Jet algorithm number in YKERN, 1=JADE E0, 5=Durham, 
C                   7=Cambridge
C           MAXJET  Max. number of jets
C  Output:  NXJ:  Number of y_flip values > 1.0E-4
C           YFLIP: y-flip values
C  Author: Stefan Kluth
C  Date: 08.10.02
C  Modifications:
      INTEGER IDIM, NTRAK, IJTYP, MAXJET, NXJ
      REAL PTRAK(IDIM,*), YFLIP(MAXJET)
      LOGICAL LPRINT
      PARAMETER( LPRINT=.FALSE. )
      INTEGER NYCLMX, IERR, I
      PARAMETER( NYCLMX=500 )
      REAL PYF(10,NYCLMX), YL, YH
      IF( NTRAK .GE. 2 ) THEN
        IF( NTRAK.GT.NYCLMX ) THEN
          PRINT *, 'QQJETS: Local arrays too short, no jet finding:'
     &         ,NYCLMX, NTRAK
          RETURN
        ENDIF
        IF( IJTYP .NE. 7 ) THEN
C         YKERN jets:
          CALL YKERN( IJTYP, NTRAK, IDIM, PTRAK, IERR )
          IF( IERR.NE.0 ) THEN
            PRINT *, 'QQJETS: YKERN error', IERR
            NXJ= 0
          ELSE
            CALL YTREE( LPRINT, PYF, IERR )
            IF( IERR.NE.0 ) THEN
              PRINT *, 'QQJETS: YTREE error', IERR
              NXJ= 0
            ELSE
              DO I= 1, MIN(NTRAK,MAXJET)
                IF( PYF(7,I).GT.1.0E-6 ) THEN
                  NXJ= I
                  YFLIP(I)= PYF(7,I)
                ELSE
                  GOTO 10
                ENDIF
              ENDDO
 10           CONTINUE
            ENDIF
          ENDIF
        ELSE
C         Cambridge jets:
          CALL CKERN( IDIM, NTRAK, PTRAK, MIN(20,NTRAK), IERR )
          IF( IERR.NE.0 ) THEN
            PRINT *, 'QQJETS: Cambrige CKERN error', IERR
            NXJ= 0
          ELSE
            DO I= 2, MIN(NTRAK,MAXJET,20)
              CALL CYJET( I, YL, YH, IERR )
              IF( IERR.EQ.0 .AND. YH.GT.1.0E-6 ) THEN
                NXJ= I-1
                YFLIP(I-1)= YH
              ELSE
                GOTO 20
              ENDIF
            ENDDO
 20         CONTINUE
          ENDIF          
        ENDIF
      ELSE
        NXJ= 0
      ENDIF
C     Always return 5 jets and fill return array with 
C     sane values in case of few jets:
      IF( NXJ.LT.5 ) THEN
        DO I= NXJ+1, 5
          YFLIP(I)= -1.0
        ENDDO
        NXJ= 5
      ENDIF
C
C  The End:
      RETURN
      END
