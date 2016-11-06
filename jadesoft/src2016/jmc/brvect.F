C   03/12/83 312030144  MEMBER NAME  BRVECT   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE BRVECT( IUNIT, *, * )
C-----------------------------------------------------------------------
C
C   AUTHOR    E. ELSEN             :  READ MC EVENT AND FILL VECT/0
C
C        MOD  C. BOWDERY  29/06/83 :  ALSO CREATE PALL/0 BANK
C        MOD  C. BOWDERY  20/10/83 :  FOR CPROD  MACRO
C        MOD  C. BOWDERY  17/11/83 :  CALL TO MCVALI TO CHECK INPUT
C        MOD  C. BOWDERY  22/11/83 :  IMPROVED ERROR MESSAGE CONTROL
C   LAST MOD  C. BOWDERY   2/12/83 :  CHANGED MESSAG (13/14) TO (18/19)
C   LAST MOD
C    P. Movilla Fernandez 21/12/00 :  Use subroutine CPREAD to read
C                                     CPROD file with different endian format
C
C
C  READ EVENT AND FILL 4 VECTOR BANK VECT/0 AND PALL/0. THE LATTER HAS
C  INFORMATION ABOUT ALL THE PARTICLES THE GENERATOR MADE.
C
C  RETURN 1    FOR READ ERROR, NOT ENOUGH SPACE IN BCS OR BAD INPUT DATA
C  RETURN 2    FOR EOF
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL VALID
C
      COMMON / BCS /  IW(1)
C
      COMMON / CVERR / MESSAG(20)
C
#include "cprod.for"
C
C PMF, 21/12/2000:
C MAXCP is the size of the CPROD common, see 'cprod.for'
      INTEGER MAXCP
      PARAMETER( MAXCP=7512 )
      INTEGER NBUFF(MAXCP)
      REAL RBUFF(MAXCP)
      EQUIVALENCE( NBUFF(1), RBUFF(1) )
C PMF, 21/12/00: IFORM tells us the endian format of the CPROD file
      INTEGER IFORM
      COMMON /CPFORM/ IFORM
C
C-----------------------  C O D E  -------------------------------------
C
C                                           READ EVENTS
C
      IF( IFORM.GE.0 ) THEN
      READ(IUNIT,ERR=8000,END=8100)  NEV, BEAM, PT, PHI, THETA, IFLAVR,
     *     NP, NC, NN, ( (PP(I4,N), I4=1,4), XM(N), JCH(N), JTP(N),
     *     (JP(N,I2), I2=1,2), N=1,NP),
     *     NF, NCF, NNF, ( (PF(I4,N2),I4=1,4), XMF(N2), ICF(N2),ITF(N2),
     *     (PSTRT(I3,N2),I3=1,3),N2=1,NF)
      ELSE
C PMF, 21/12/00: Read binary with different endian format using CFGET
         CALL CPREAD(NBUFF,'I',*8000,*8100)
         NEV   =NBUFF(1)
         BEAM  =RBUFF(2)
         PT    =RBUFF(3)
         PHI   =RBUFF(4)
         THETA =RBUFF(5)
         IFLAVR=NBUFF(6)
         NP    =NBUFF(7)
         NC    =NBUFF(8)
         NN    =NBUFF(9)
         DO N=1,NP
            DO I4=1,4
               PP(I4,N) = RBUFF( 9 + 9*(N-1) + I4  )
            ENDDO
             XM(N) = RBUFF( 9 + 9*(N-1) + 5 )
             JCH(N)= NBUFF( 9 + 9*(N-1) + 6 )
             JTP(N)= NBUFF( 9 + 9*(N-1) + 7 )
             DO I2=1,2
                JP(N,I2) = NBUFF( 9 +  9*(N-1) + 7 + I2 )
             ENDDO
          ENDDO
          NF    =NBUFF( 9 + 9*NP + 1 )
          NCF   =NBUFF( 9 + 9*NP + 2 )
          NNF   =NBUFF( 9 + 9*NP + 3 )
          DO N2=1,NF
             DO I4=1,4
                PF(I4,N2) = RBUFF( 9 + 9*NP + 3 + 10*(N2-1) + I4  )
             ENDDO
             XMF(N2) =RBUFF( 9 + 9*NP + 3 + 10*(N2-1) + 5 )
             ICF(N2) =NBUFF( 9 + 9*NP + 3 + 10*(N2-1) + 6 )
             ITF(N2) =NBUFF( 9 + 9*NP + 3 + 10*(N2-1) + 7 ) 
             DO I3=1,3
                PSTRT(I3,N2) =RBUFF( 9 + 9*NP + 3 + 10*(N2-1) + 7 + I3 )
             ENDDO
          ENDDO
       ENDIF
C
C                                           CHECK INPUT
C
      CALL MCVALI( VALID )
      IF( .NOT. VALID ) RETURN 1
C
C                                           CREATE VECT BANK
C
      CALL LHVECT( LENGTH )
      CALL BCRE( NPVECT, 'VECT', 0 , LENGTH, *8200, IER )
      CALL STVECT( LENGTH, IW(NPVECT+1) )
      CALL BSAW( 1, 'VECT' )
C
C                                           CREATE PALL BANK
C
      CALL LHPALL( LENGTH)
      CALL BCRE( NPALL, 'PALL', 0, LENGTH, *8300, IER )
      CALL STPALL( LENGTH, IW(NPALL + 1) )
      CALL BSAW( 1, 'PALL' )
      RETURN
C                                           READ ERROR
 8000 MESSAG(18) = MESSAG(18) + 1
      IF( MESSAG(18) .GT. 20 ) RETURN 1
C
      WRITE(6,8005) IUNIT
 8005 FORMAT(/' *** READ ERROR ***  INPUT UNIT NUMBER = ',I4,'    ***'/)
      RETURN 1
C                                           BOS ERROR (VECT)
 8200 MESSAG(19) = MESSAG(19) + 1
      IF( MESSAG(19) .GT. 20 ) RETURN 1
C
      WRITE(6,8205) IER
 8205 FORMAT(/' *** BOS ERROR ***  BCRE RETURNED ERROR CODE ',I2,
     +        '   WHILE CREATING VECT/0 IN BRVECT    ***'/)
      RETURN 1
C                                           BOS ERROR (PALL)
 8300 MESSAG(19) = MESSAG(19) + 1
      IF( MESSAG(19) .GT. 20 ) RETURN 1
C
      WRITE(6,8305) IER
 8305 FORMAT(/' *** BOS ERROR ***  BCRE RETURNED ERROR CODE ',I2,
     +        '   WHILE CREATING PALL/0 IN BRVECT    ***'/)
      RETURN 1
C                                           EOF DETECTED
 8100 RETURN 2
      END
C
C
C
      SUBROUTINE CPREAD( NBUFF, COPT, *, *)
      IMPLICIT NONE
C-----------------------------------------------------------------------
C
C     21/12/2000      Pedro Movilla Fernandez
C
C     Reads a CPROD record using the CERNLIB package CFIO [Z310].
C     This routine should be used if the binary CPROD data has an endian
C     format different from the one of the current platform.
C
C-----------------------------------------------------------------------
      INTEGER NBUFF(*)
      CHARACTER*1 COPT
C Get file pointer (firstly set by CFOPEN in MCMAIN)
      INTEGER NREAD,IOS,NWORDS,IFORM,IPTR,NDUM
      COMMON /MYIO/ IPTR
C Get record length
      NREAD=1
      CALL CFGET(IPTR,0,1,NREAD,NWORDS,IOS)
      IF( IOS.EQ.-1 ) THEN
         RETURN 2
      ELSEIF( IOS.NE.0 ) THEN
         RETURN 1
      ENDIF
      CALL VXINVB(NWORDS,1)
      NWORDS=NWORDS/4
C Read record
      CALL CFGET( IPTR, 0, NWORDS, NREAD, NBUFF,IOS )
      IF( IOS.NE.0 .OR. NREAD.NE.NWORDS ) RETURN 1
C Read last word of fortran record (=record length)
      NREAD=1
      CALL CFGET( IPTR, 0, 1, NREAD, NDUM, IOS)
      IF( IOS.NE.0 ) RETURN 1
C Perform byte-by-byte inversion of buffer
      IF( COPT.EQ.'I' ) CALL VXINVB(NBUFF,NWORDS)
C
      RETURN
      END
