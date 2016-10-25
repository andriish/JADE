      SUBROUTINE JFOPEN(FILEN,U,L)
      IMPLICIT NONE
      character*80 FILEN
      INTEGER U, I, L, IERR
      OPEN (U, FILE=FILEN, STATUS='NEW')
 
      RETURN
      END

      SUBROUTINE JFCLOSE(U)
      IMPLICIT NONE
      INTEGER U
      CLOSE(U)
      RETURN
      END

      SUBROUTINE JFWRITE(U, M)
      IMPLICIT NONE      
      integer U,M, IERR
      
      if ( M. EQ. 0 ) then 
      CALL WRCPRD(U,IERR)
      end if 
      if ( M. EQ. 1 ) then 
      CALL PRCPRD(U,IERR)
      end if 
      if (IERR .NE. 0) then 
      WRITE(*,*)"ERROR IN WRITE ",IERR
      end if
      RETURN
      END


      SUBROUTINE WRCPRD(LUN,IERR)
      IMPLICIT NONE
*********************************************************************
*.
*.  Write out 4-vector information in a binary format
*.  (JADE 'CPROD' format, see JADE Computer Note 69)
*.
*********************************************************************
C  4-vector format for the JADE simulation program MCJADE
C  (see JADE Computer Note 69)
      REAL PP,PF,XM,XMF,BEAM,PT,THETA,PHI,PSTRT
      INTEGER NEV,NP,NC,NN,JCH,JTP,JP,NF,NCF,NNF,ICF,ITF,IFLAVR
      CHARACTER*16 CP,CF
      COMMON/CPROD/ NEV,BEAM,PT,PHI,THETA,IFLAVR,
     *        NP,NC,NN,PP(4,500),XM(500),JCH(500),JTP(500),JP(500,2),
     *        NF,NCF,NNF,PF(4,300),XMF(300),ICF(300),ITF(300),
     *        PSTRT(3,300)
      COMMON/CHCPRD/ CP(500),CF(300)
      INTEGER I1,I2,I3,I4,I5,N,N2
      INTEGER LUN,IERR
Code:
      WRITE(UNIT=LUN,ERR=10,IOSTAT=IERR)
     *     NEV, BEAM, PT, PHI, THETA, IFLAVR,
     *     NP, NC, NN, ( (PP(I4,N), I4=1,4), XM(N), JCH(N), JTP(N),
     *     (JP(N,I2), I2=1,2), N=1,NP),
     *     NF, NCF, NNF, ( (PF(I4,N2),I4=1,4), XMF(N2), ICF(N2),ITF(N2),
     *     (PSTRT(I3,N2),I3=1,3),N2=1,NF)
      RETURN
 10   WRITE(LUN,'(A,2I7)') ' WRCPRD: WRITE ERROR, NEV, IOSTAT=',NEV,IERR
      RETURN
      END
C
CDECK  ID>, PRCPRD. 
      SUBROUTINE PRCPRD(LUN,IERR)
      IMPLICIT NONE
*********************************************************************
*.
*.  Print out 4-vector information in common /CPROD/
*.  in a readable format.
*.
*********************************************************************
C  4-vector format for the JADE simulation program MCJADE
C  (see JADE Computer Note 69)
      REAL PP,PF,XM,XMF,BEAM,PT,THETA,PHI,PSTRT
      INTEGER NEV,NP,NC,NN,JCH,JTP,JP,NF,NCF,NNF,ICF,ITF,IFLAVR
      CHARACTER*16 CP,CF
      COMMON/CPROD/ NEV,BEAM,PT,PHI,THETA,IFLAVR,
     *        NP,NC,NN,PP(4,500),XM(500),JCH(500),JTP(500),JP(500,2),
     *        NF,NCF,NNF,PF(4,300),XMF(300),ICF(300),ITF(300),
     *        PSTRT(3,300)
      COMMON/CHCPRD/ CP(500),CF(300)
      INTEGER I1,I2,I3,I4,I5,N,N2
      INTEGER LUN,IERR
Code:
      WRITE(LUN,'(/A,I8)') ' PRCPRD: Content of common /CPROD/ in event'
     *     ,NEV
      WRITE(LUN,'(/A6,I7,A6,F6.2,A8,I2)')
     *     ' NEV=',NEV,' BEAM=',BEAM,' IFLAVR=',IFLAVR
      WRITE(LUN,'(A14,3F7.4)') ' PT/PHI/THETA=',PT, PHI, THETA
      WRITE(LUN,'(/A15,A10,3I4)') ' All particles:'
     *     ,' NP/NC/NN=',NP, NC, NN
      WRITE(LUN,'(T21,5A9,A5,A8,A6)')
     *     'PP(1,I)','PP(2,I)','PP(3,I)','PP(4,I)','PP(5,I)'
     *     ,'JCH','JTP','JP'
      WRITE(LUN,'(I3,A17,5F9.3,I5,I8,2I4)')
     *     ( N,CP(N),(PP(I4,N), I4=1,4), XM(N), JCH(N), JTP(N),
     *     (JP(N,I2), I2=1,2), N=1,NP)
      WRITE(LUN,'(/A23,A13,3I4)') ' Final state particles:'
     *     ,' NF/NCF/NNF=',NF, NCF, NNF
      WRITE(LUN,'(T21,5A9,2A5,A21)')
     *     'PF(1,I)','PF(2,I)','PF(3,I)','PF(4,I)','PF(5,I)'
     *     ,'ICF','ITF','PSTRT-X -Y -Z'
      WRITE(LUN,'(I3,A17,5F9.3,2I5,3F9.3)')
     *     (N2,CF(N2),(PF(I4,N2),I4=1,4), XMF(N2), ICF(N2),ITF(N2),
     *     (PSTRT(I3,N2),I3=1,3),N2=1,NF)
C
      RETURN
      END
