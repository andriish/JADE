* This is a private version of CERNLIB routine RANMAR.
*   13/07/2000 Pedro Movilla Fernandez
*
* Modifications:
*     - The counters NTOT, NTOT2 are not reset to zero at the end
*       of RMARIN, i.e.: These counters now refer to the very
*       beginning of the random numbers sequence and not only to
*       the number of random numbers generated since the first call
*       of RMARIN. Thus, a correct handling of NTOT2 is ensured.
*
*     - An initialization of the random generator is now also possible
*       externally by setting common /RANMA1/. This kind of initialization
*       is much faster than using RMARIN and must be indicated by the
*       user by the variable `RANINI' which is here appended at the end of
*       common /RANMA1/.
*
*       RANINI = .TRUE.: Initialization was done externally or by calling RMARIN.
*                .FALSE.: Perform default initialization by using RMARIN,
*                         if FIRST.EQ..TRUE.
*
*     - An additional flag `RANPRIV' was introduced and appended to
*       common  /RANMA1/ in order to let the user know that this version of
*       RANMAR is used. Use this information in order to handle the counters
*       NTOT and NTOT2 correctly!
*     
*
* $Id: ranmar.F,v 1.1.1.1 1996/02/15 17:49:53 mclareni Exp $
*
* $Log: ranmar.F,v $
* Revision 1.1.1.1  1996/02/15 17:49:53  mclareni
* Modified          2000/07/13           Pedro Movilla Fernandez
* Kernlib
*
*
      SUBROUTINE RANMAR(RVEC,LENV)
C
C CERN PROGLIB# V113    RANMAR          .VERSION KERNFOR  4.21  890323
C ORIG. 01/03/89 FCA + FJ
C
      DIMENSION RVEC(*)
C 
C PMF: - Introduce `RANPRIV' as privat-version-flag of RANMAR
C      - Append `RANINI' to common /RANMA1/ to inform RANMAR if
C        initialization has already been done externally via common /RANMA1/.
      LOGICAL RANPRIV,RANINI
C
      COMMON/RANMA1/IJKL,NTOT,NTOT2,I97,J97,C,U(97),RANINI,RANPRIV
      LOGICAL FIRST
      PARAMETER (TWOM24=2.**(-24),TWOM48=2.**(-48))
      PARAMETER (CD=7654321.*TWOM24,CM=16777213.*TWOM24)
      PARAMETER (CINT=362436.*TWOM24,MODCNS=1000000000)
      SAVE /RANMA1/, FIRST
      DATA FIRST/.TRUE./
      EXTERNAL RANDAT
C
C PMF: Report that this version of RANMAR is called
      IF( FIRST ) THEN
        WRITE(*,*) 
     >  'RANMAR: Private version of RANMAR (CERN PROGLIB# V113) called.'
        RANPRIV=.TRUE.
      ENDIF
C PMF: Check if initialization has already been done externally
      IF( RANINI.AND.FIRST ) THEN 
         WRITE(*,*)
     >  'RANMAR: Initialization done externally via /RANMA1/.' 
        FIRST=.FALSE.
      ELSEIF( FIRST ) THEN
         WRITE(*,*)
     >  'RANMAR: Will perform default initialization using RMARIN.' 
      ENDIF
C
      IF(FIRST) THEN
        IJKL = 54217137
        NTOT = 0
        NTOT2 = 0
        GO TO 70
      ENDIF
C
   80 CONTINUE
      DO 100 IVEC= 1, LENV
        UNI = U(I97)-U(J97)
        IF (UNI .LT. 0.) UNI=UNI+1.
        U(I97) = UNI
        I97 = I97-1
        IF (I97 .EQ. 0)  I97=97
        J97 = J97-1
        IF (J97 .EQ. 0)  J97=97
        C = C - CD
        IF (C .LT. 0.)   C=C+CM
        UNI = UNI-C
        IF (UNI .LT. 0.) UNI=UNI+1.
C
C   Replace exact zeroes by uniform distr. *2**-24
C
        IF (UNI .EQ. 0.)  THEN
          UNI = TWOM24*U(2)
C
C   An exact zero here is very unlikely, but let's be safe.
C
          IF (UNI .EQ. 0.) UNI= TWOM48
        ENDIF
        RVEC(IVEC) = UNI
  100 CONTINUE
C
      NTOT = NTOT + LENV
      IF (NTOT .GE. MODCNS)  THEN
        NTOT2 = NTOT2 + 1
        NTOT  = NTOT - MODCNS
      ENDIF
      RETURN
      ENTRY RMARIN(IJKLIN,NTOTIN,NTO2IN)
C
      IF( FIRST )
     >  WRITE(*,*) 
     >  'RANMAR: Private version of RANMAR (CERN PROGLIB# V113) called.'
C
      FIRST = .FALSE.
      IJKL  = IJKLIN
C PMF: Initialize with default seed if IJKLN.EQ.0
      IF( IJKL .EQ. 0 ) IJKL = 54217137
      NTOT  = NTOTIN
      NTOT2 = NTO2IN
C PMF: Prevent from initialisation by RMARIN if RANINI.EQ..TRUE.
      IF( RANINI ) THEN
         WRITE(*,*) 
     >   'RMARIN: Initialization is performed externally, so RMARIN'
     >  //' will let common /RANMA1/ unchanged.'
         RANPRIV=.TRUE.
         RETURN
      ELSE
         WRITE(*,*) 
     >   'RMARIN called for initialization.'
      ENDIF
C
   70 CONTINUE
      IJ = IJKL/30082
      KL = IJKL - 30082*IJ
      I = MOD(IJ/177, 177) + 2
      J = MOD(IJ, 177)     + 2
      K = MOD(KL/169, 178) + 1
      L = MOD(KL, 169)
      DO 30 II= 1, 97
        S = 0.
        T = .5
        DO 20 JJ= 1, 24
          M = MOD(MOD(I*J,179)*K, 179)
          I = J
          J = K
          K = M
          L = MOD(53*L+1, 169)
          IF (MOD(L*M,64) .GE. 32)  S = S+T
          T = 0.5*T
  20    CONTINUE
        U(II) = S
  30  CONTINUE
      C   = CINT
      I97 = 97
      J97 = 33
C       Complete initialization by skipping
C            (NTOT2*MODCNS + NTOT) random numbers
      NITER = MODCNS
      DO 50 LOOP2= 1, NTOT2+1
        IF(LOOP2.GT.NTOT2) NITER=NTOT
        DO 40 IDUM = 1, NITER
          UNI = U(I97)-U(J97)
          IF (UNI .LT. 0.) UNI=UNI+1.
          U(I97) = UNI
          I97 = I97-1
          IF (I97 .EQ. 0)  I97=97
          J97 = J97-1
          IF (J97 .EQ. 0)  J97=97
          C = C - CD
          IF (C .LT. 0.)   C=C+CM
   40   CONTINUE
   50 CONTINUE
C OB/PMF: The following two lines are wrong:
C     NTOT  = 0
C     NTOT2 = 0
      IF(FIRST) THEN
        FIRST = .FALSE.
        GO TO 80
      ENDIF
      RETURN
      ENTRY RMARUT(IJKLUT,NTOTUT,NTO2UT)
C
      NTOTUT = NTOT
      NTO2UT = NTOT2
      IJKLUT = IJKL
C
      END
C     Initialize RANINI and RANPRIV
      BLOCK DATA RANDAT
      LOGICAL RANPRIV,RANINI
      COMMON/RANMA1/IJKL,NTOT,NTOT2,I97,J97,C,U(97),RANINI,RANPRIV
      DATA RANPRIV/.FALSE./,RANINI/.FALSE./
      END
