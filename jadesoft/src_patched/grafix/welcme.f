C   08/06/86 810171936  MEMBER NAME  WELCME   (S)        M  FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE WELCME
C-----------------------------------------------------------------------
C
C    AUTHOR:   C. BOWDERY   8/06/86 :  BEGINS GRAPHICS PROGRAM DIALOGUE
C
C
C
C     HANDLES THE OPENING PLEASANTRIES OF THE GRAPHICS PROGRAM.
C     BASED ON CODE FROM GPHMAIN BY J. OLSSON AND L. O'NEILL.
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      REAL*8    DATE,TIME
      INTEGER*4 BINTIM
C
      character*2 hblank, HNAM, HWORK, HTM2, HTM1
      COMMON / CGAMOD / MODEGA, GAHEX
      COMMON / CWORK  / DATE, TIME, DUMMY, ITIME, HWORK(40), HNAM(8)
      
C
      DIMENSION  HTM1(4), HTM2(4)
      DIMENSION  HNAME(4)
C
      EQUIVALENCE  ( DATE, HTM1(1) )
      EQUIVALENCE  ( TIME, HTM2(1) )
C
      EQUIVALENCE  ( HNAM(1), HNAME(1) )
C 
      
      DATA  HBLANK /'  '/
C
C------------------  C O D E  -----------------------------------------
C
*** PMF 20/06/00 
C     CALL TRMOUT(80,'                J A D E  Interactive Graphics Prog
C    +ram  Vs. 17. October   1988^')
      CALL TRMOUT(80,' *************************************************
     >******^')
      CALL TRMOUT(80,' *       J A D E  Interactive Graphics Program    
     >     *^')
      CALL TRMOUT(80,' *                                                
     >     *^')
      CALL TRMOUT(80,' *  Previous Version:               17. October  1
     >988  *^')
      CALL TRMOUT(80,' *  Reanimated and Adapted Version: 18. November 1
     >999  *^')
      CALL TRMOUT(80,' *                           (Pedro Movilla Fernan
     >dez) *^')
      CALL TRMOUT(80,' *************************************************
     >******^')
C - Welcome message on HIGZ window
      CALL IGWLCM 
*** PMF (end)
      CALL TRMOUT(80,' ^')
C
C                            CLEAR NAME STRING
C
      DO  10  I = 1,8
        HNAM(I) = HBLANK
  10  CONTINUE
C
C                            FIND USERID
C
      CALL IDFUN(HNAME,NBOK)
C
C                            FIND TIME OF DAY
C
      ITIME = BINTIM(DUMMY)/360000
C
C                           GREET THE USER ACCORDING TO TIME OF DAY
C
      IF( ITIME .LT. 12 ) WRITE(6,20) (HNAM(IS),IS=1,5)
      IF( ITIME .GE. 12  .AND. ITIME .LT. 18 )
     +                    WRITE(6,30) (HNAM(I),I=1,5)
      IF( ITIME .GE. 18 ) WRITE(6,40) (HNAM(IS),IS=1,5)
C
  20  FORMAT('   Good Morning, ',4A2,'  Great to see you again '
     +,A2/)
  30  FORMAT('   Good Afternoon, ',4A2,'  Nice to see you again ',
     +A2/)
  40  FORMAT('   Good Evening, ',4A2,'   Working late again ? ',
     +A2/)
C
C                            WRITE OUT THE LATEST NEWS
C
      CALL NOTICE
C
C                            FIND DATE AND TIME STRINGS
C                            EQUIVALENCED TO HTM1 AND HTM2
C
      CALL DAY(DATE,TIME)
C
      DO  50  I = 1,4
        HWORK(I) = HTM1(I)
  50  CONTINUE
C
      HWORK(5) = HBLANK
      DO  60  I = 1,4
        HWORK(I+5) = HTM2(I)
  60  CONTINUE
C
      WRITE(6,70) (HWORK(I),I=1,9)
  70  FORMAT(' Scan Session Beginning at DATE and TIME: ',9A2)
C
      RETURN
      END
