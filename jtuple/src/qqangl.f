CDECK  ID>, QQANGL.
      SUBROUTINE QQANGL( BZA, KSWA, MNRA, C34A )
      IMPLICIT NONE
C  Routine computes 4-jet angular variables from information 
C  in the YKERN common block
C  you're on your own when you run this without running 
C  a YKERN jet finder first
C  Input: none
C  Output: BZA    Bengston-Zerwas Angle 
C          KSWA   Koerner-Schierholtz Willrodt Angle 
C          MNRA   modified Nachtmann-Reiter Angle 
C          C34A   angle between two least energetic jets
C  Author: Jochen Schieck, Stefan Kluth
C  Date: 08.10.02
C  Modifications:
      REAL BZA, KSWA, MNRA, C34A
      INTEGER NYCLMX, IERR, I
      PARAMETER( NYCLMX=500 )
      INTEGER IJET(NYCLMX)
      REAL THET1, THET2, PNJ(10,10)
      REAL VEC1(3), VEC2(3), VEC3(3), VEC4(3)
      REAL VEC5(3), VEC6(3), VEC7(3), VEC8(3)
C     Default values:
      BZA= -10.0
      KSWA= -10.0
      MNRA= -10.0
      C34A= -10.0  
C     Try to get the jet axis of four jets:
      CALL YASSO( 4, PNJ, IJET, IERR )
      IF( IERR.NE.0 ) THEN
        PRINT *, 'QQANGL: YASSO error', IERR
      ELSE            
C       Calculate the angular observables:
        DO I= 1, 3
          VEC1(1)= PNJ(1,1)
          VEC1(2)= PNJ(2,1)
          VEC1(3)= PNJ(3,1)
          VEC2(1)= PNJ(1,2)
          VEC2(2)= PNJ(2,2)
          VEC2(3)= PNJ(3,2)
          VEC3(1)= PNJ(1,3)
          VEC3(2)= PNJ(2,3)
          VEC3(3)= PNJ(3,3)
          VEC4(1)= PNJ(1,4)
          VEC4(2)= PNJ(2,4)
          VEC4(3)= PNJ(3,4)
        ENDDO              
C       Bengston Zerwas angle:
        CALL PXCRO3( VEC1, VEC2, VEC5 )            
        CALL PXCRO3( VEC3, VEC4, VEC6 )            
        CALL PXANG3( VEC5, VEC6, BZA, THET1, IERR )              
C       Koerner-Schierholtz-Willrodt angle:
        CALL PXCRO3(VEC1,VEC4,VEC5)            
        CALL PXCRO3(VEC2,VEC3,VEC6) 
        CALL PXANG3(VEC5,VEC6,KSWA,THET1,IERR)
        CALL PXCRO3(VEC1,VEC3,VEC7)            
        CALL PXCRO3(VEC2,VEC4,VEC8)           
        CALL PXANG3(VEC7,VEC8,KSWA,THET2,IERR)
        KSWA=COS(0.5*(THET1+THET2))
C       modified Nachtmann-Reiter angle:
        DO I= 1, 3
          VEC5(I)= VEC1(I)-VEC2(I)
          VEC6(I)= VEC3(I)-VEC4(I)
        ENDDO            
        CALL PXANG3( VEC5, VEC6, MNRA, THET1, IERR )
C       cos(alpha_34):
        CALL PXANG3( VEC3, VEC4, C34A, THET1, IERR )
      ENDIF
C     
C     The End:
      RETURN
      END
