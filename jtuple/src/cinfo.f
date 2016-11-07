
CDECK  ID>, CINFO.
      SUBROUTINE CINFO
*.----------------------------------------------------------------------
*.
*.    CINFO: Print all found yflip values to standard output
*.
*.  CREATED :  11-12-1997, STAN BENTVELSEN
*.  LAST MOD:
*.-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NMXY , NMXP
      PARAMETER (NMXY = 300)
      PARAMETER (NMXP = 30)
      DOUBLE PRECISION YTRANS(NMXY)
      DOUBLE PRECISION PCMJ(NMXP,4,NMXP)
      INTEGER          NTRANS(NMXY), NJITER, NJMAX, NTRACK
      INTEGER          ICMJ(NMXP,NMXY)
      COMMON / CKCOM / YTRANS, PCMJ, NTRANS, ICMJ, NJITER, NJMAX, NTRACK
      INTEGER I, IDUB,NJDUB(20),NJOC(20),INON,NJNON(20)

      WRITE(*,'(/A)'    ) '*******************************************'
      WRITE(*,'( A)'    ) '* CINFO: Information Cambridge clustering *'
      WRITE(*,'(A,I3,A)') '*   Clustering resolved to ',NJMAX
     +     ,' Jets       *'
      WRITE(*,'(A,2(I3,A))') '* Iterations: ',NJITER
     +                    ,',  Number of tracks: ',NTRACK,' *'
      WRITE(*,'( A)'    ) '*******************************************'
      WRITE(*,'( A)'    ) '* Iteration   Ycut              Njet      *'
      DO I=1,NJITER
         WRITE(*,'(A,I4,G20.8,I8,A)') '* ',I,YTRANS(I),NTRANS(I)
     +        ,'        *'
      ENDDO
      WRITE(*,'( A)'    ) '*******************************************'
      CALL CDBLE(IDUB,NJDUB,NJOC)
      WRITE(*,'(A,I2,A)') '* Number of double regions for NJET: '
     +      ,IDUB,'   *'
      DO I=1,IDUB
         WRITE(*,'(A,3I9,A)') '* ',I,NJDUB(I),NJOC(I),'             *'
      ENDDO
      WRITE(*,'( A)'    ) '*******************************************'
      CALL CNONE(INON,NJNON)
      WRITE(*,'(A,I2,A)') '* Number of impossible NJET states : '
     +      ,INON,'   *'
      DO I=1,INON
         WRITE(*,'(A,2I9,A)') '* ',I,NJNON(I),'                      *'
      ENDDO
      WRITE(*,'(A/)'    ) '*******************************************'
      

      RETURN
      END
