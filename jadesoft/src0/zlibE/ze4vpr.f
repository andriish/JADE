C   23/04/87 704231839  MEMBER NAME  ZE4VPR   (S)           FORTRAN77
      SUBROUTINE ZE4VPR
C-----------------------------------------------------------
C  DUMP ZE4V-BANK IN READABLE FORMAT
C-----------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      COMMON / BCS / IW(1)
      DIMENSION RW(1), HW(1)
      EQUIVALENCE (HW(1),RW(1),IW(1))
C-----------------------------------------------------------
C
      NPZE4V = IW(IBLN('ZE4V'))
      IF( NPZE4V.LE.0 ) GO TO 6000
C
      NZ2 = NPZE4V*2
      LLH = HW( NZ2 + 1 ) - HW( NZ2 + 3 )*HW( NZ2 + 2 )
     &                       - HW( NZ2 + 4)
      WRITE(6,9103) IW(NPZE4V-2), IW(NPZE4V),(HW(NZ2+I),I=1,14),
     *              RW(NPZE4V+8),(RW( NPZE4V+ LLH + I ),I=1,3),
     *              IW(NPZE4V+31)
 9103 FORMAT(//' DUMP OF ZE4V  ',I4,' LENGTH=',I5,5X,/,
     *1X,' LHD  LVX  NVX  LMC   LT   NP  LCH  NCH  LNE  NNE  LRE  NRE',
     *'   # RUN   # EVT   E-BEAM   X-VX1   Y-VX1   Z-VX1  MCREDU',/,
     * 1X,12(I4,1X),1X,I6,2X,I6,2X,F7.3,3(1X,F7.2),6X,I2/)
      J = NPZE4V+9
      K = NPZE4V+28
      WRITE(6,9104) (RW(I),I=J,K)
 9104 FORMAT( 5X,3F10.3,2X,F10.3,'            '/
     *        5X,3F10.3,2X,F10.3,'  SPHERICITY'/
     *        5X,3F10.3,2X,F10.3,'           '/
     *        5X,3F10.3,2X,F10.3,'  THRUST'/
     *        5X,3F10.3,2X,F10.3,'  AKOPLANARITY')
C
      N = HW( NZ2 + 6 )
      LTR = HW( NZ2 + 5 )
      LTRCH = HW( NZ2 + 7 )
      LTRNE = HW( NZ2 + 9 )
      LTRRE = HW( NZ2 + 11)
      J = 0
      NP = NPZE4V + HW(NZ2+1)
      IF( N.LE.0 ) GO TO 5100
      WRITE(6,9105)
 9105 FORMAT(1X,'NR    EX    EY    EZ   PTOT  Q',
     *       ' PH TY VO VS PL OR PT',
     *       '   ECL S_ECL M_ECL  DET CL12 PA RZRF',
     *       ' X-TRK Y-TRK Z-TRK  DEDX SDEDX  R-MIN')
 5100 IF( J.GE.N ) GO TO 6000
      J = J + 1
      NP2 = NP*2
         IF( HW(NP*2 +18 ) .EQ. 1 )  THEN
      WRITE(6,9108)J,RW(NP+1),RW(NP+2),RW(NP+3),RW(NP+6),INT(RW(NP+7)),
     *                    HW(NP2+7),HW(NP2+8),HW(NP2+9),HW(NP2+10),
     *                    HW(NP2+15),HW(NP2+16),HW(NP2+18),
     &                    (RW(NP+LTR+I),I=1,3),(HW(NP2+LTR*2+I),I=7,10),
     &                    (RW(NP+LTR+I),I=6,11)
 9108       FORMAT(1X,I2,1X,3(F5.2,1X),F6.3,1X,I2,1X,6(I2,1X),I2,
     &            3(1X,F5.2),2(1X,I4),1X,I2,1X,I4,3(1X,F5.1),
     &            2(1X,F5.2),1X,F6.2)
            NP = NP + LTR + LTRCH
C
          ELSEIF ( HW(NP*2 + 18) .EQ. 0 ) THEN
C
      WRITE(6,9109)J,RW(NP+1),RW(NP+2),RW(NP+3),RW(NP+6),INT(RW(NP+7)),
     *                    HW(NP2+7),HW(NP2+8),HW(NP2+9),HW(NP2+10),
     *                    HW(NP2+15),HW(NP2+16),HW(NP2+18),
     &                    (RW(NP+LTR+I),I=1,3),(HW(NP2+LTR*2+I),I=7,8)
 9109       FORMAT(1X,I2,1X,3(F5.2,1X),F6.3,1X,I2,1X,6(I2,1X),I2,
     &                 3(1X,F5.2),2(1X,I4))
            NP = NP + LTR + LTRNE
C
          ELSEIF( HW(NP*2 + 18) .EQ. 2 )  THEN
C
      WRITE(6,9110)J,RW(NP+1),RW(NP+2),RW(NP+3),RW(NP+6),INT(RW(NP+7)),
     *                    HW(NP2+7),HW(NP2+8),HW(NP2+9),HW(NP2+10),
     *                    HW(NP2+15),HW(NP2+16),HW(NP2+18),
     &                    (RW(NP+LTR+I),I=1,3),(HW(NP2+LTR*2+I),I=7,8)
 9110       FORMAT(1X,I2,1X,3(F5.2,1X),F6.3,1X,I2,1X,6(I2,1X),I2,
     &                 3(1X,F5.2),2(1X,I4))
            NP = NP + LTR + LTRRE
C
          ELSEIF( HW(NP*2 + 18) .EQ.-1 )  THEN
C
            NP = NP + LTR
C
         ENDIF
C
         GO TO 5100
 6000 CONTINUE
C
 8000 CONTINUE
      RETURN
      END
