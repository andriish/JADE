C   15/02/88 805261721  MEMBER NAME  PRSINB   (S)           F77
      SUBROUTINE PRSINB( NAME, NR )
C-----------------------------------------------------------
C  Version of 15/02/88
C  Last Mod 27/04/88 E Elsen
C  Print BOS bank of name NAME and number NR.
C-----------------------------------------------------------
      IMPLICIT INTEGER*2 (H)
      COMMON/BCS/IW(1)
      DIMENSION RW(1), HW(1)
      EQUIVALENCE(HW(1),RW(1),IW(1))
C
      PARAMETER (NNAMES=4)
      CHARACTER*4 NAME, LISTNM(NNAMES)
     *        / 'VECT', 'PALL', 'ZE4V', 'DEDX'/
C
      CHARACTER*1 MUQUA(0:3) / ' ','A','B','C'/
C
      CALL CLOC( NPNAME, NAME, NR )
      IF( NPNAME.GT.0 ) THEN
C                                           NAME IN LIST?
        INDEX = 0
        DO 10 J=1,NNAMES
          IF( LISTNM(J).EQ.NAME ) INDEX=J
   10   CONTINUE
C                                           HEAD LINE IF KNOWN
        IF( INDEX.GT.0 ) THEN
          WRITE(6,8901) (IW(NPNAME+I),I=-3,0), NPNAME
 8901     FORMAT(/' Bank    Name    Nr     Ptr   Words   Index'/
     *            '         ',A4,I6,3I8)
        ENDIF
C
        GO TO ( 1001, 1002, 1003, 1004 ), INDEX
C
C                                           NO FORMAT AVAILABLE
        CALL BPRS( NAME, NR )
        GO TO 8000
C                                           VECT
 1001   CONTINUE
        IF( IW(NPNAME-2) .EQ.0 ) THEN
          L0 = IW(NPNAME+1)
          WRITE(6,9011) (IW(NPNAME+I),I=1,L0)
 9011     FORMAT('   l0  l1  event   n  nc  nn     phi cs(the)',
     *           ' flavor     Jet Pointers      Beam E'/
     *         1X,2I4,I7,3I4,2F8.3,5I7)
          L1 = IW(NPNAME+2)
          NP = NPNAME + L0
          NP9 = NP + (IW(NPNAME+4)-1)*L1
          IF( IW(NPNAME+4).GT.0 ) THEN
            WRITE(6,9012) ((J-NP)/L1+1,
     *                     (RW(J+I),I=1,5),IW(J+6),IW(J+7),
     *                     (RW(J+I),I=8,10),J=NP,NP9,L1)
 9012     FORMAT('     #       px       py       pz        E        m',
     *           '   q  type        x        y        z'/
     *          (1X,I5,5F9.3,I4,I6,3F9.3))
          ENDIF
        ELSEIF( IW(NPNAME-2) .EQ.1 ) THEN
          L0 = IW(NPNAME+1)
          WRITE(6,9013) (IW(NPNAME+I),I=1,L0)
 9013     FORMAT('   l0  l1  event   n'/
     *         1X,2I4,I7,I4)
          L1 = IW(NPNAME+2)
          NP = NPNAME + L0
          NP9 = NP + (IW(NPNAME+4)-1)*L1
          IF( IW(NPNAME+4).GT.0 ) THEN
          WRITE(6,9014) ((J-NP)/L1+1,
     *                     (RW(J+I),I=1,5),IW(J+6),IW(J+7),
     *                     (RW(J+I),I=8,10),(HW(J*2+I),I=21,22),
     *                      RW(J+12),J=NP,NP9,L1)
 9014     FORMAT('     #       px       py       pz        E        m',
     *           '   q  type        x        y        z',
     *           ' vect part s(parnt)'/
     *          (1X,I5,5F9.3,I4,I6,3F9.3,2I5,F9.3))
          ENDIF
        ELSE
          CALL BPRS( NAME, NR )
        ENDIF
        GO TO 8000
C                                           PALL
 1002   CONTINUE
        L0 = IW(NPNAME+1)
        WRITE(6,9021) (IW(NPNAME+I),I=1,L0)
 9021   FORMAT('   l0  l1  event   n  nc  nn     phi cs(the)  flavor'/
     *         1X,2I4,I7,3I4,2F8.3,I8)
        L1 = IW(NPNAME+2)
        NP = NPNAME + L0
        NP9 = NP + (IW(NPNAME+4)-1)*L1
        IF( IW(NPNAME+4).GT.0 ) THEN
          WRITE(6,9022) ((J-NP)/L1+1,
     *                     (RW(J+I),I=1,5),(IW(J+I),I=6,L1),
     *                      J=NP,NP9,L1)
 9022     FORMAT('     #       px       py       pz        E        m',
     *           '   q  type  pointer   parton'/
     *          (1X,I5,5F9.3,I4,I6,I9,I9))
        ENDIF
        GO TO 8000
C                                           ZE4V
 1003   CONTINUE
C
        NZ2 = NPNAME*2
        LLH = HW( NZ2 + 1 ) - HW( NZ2 + 3 )*HW( NZ2 + 2 )
     &                         - HW( NZ2 + 4)
C
*** PMF 09/12/99        WRITE(6,9131)                          (HW(NZ2+I),I=1,14),
*     *                 RW(NPNAME+8),(RW( NPNAME+ LLH + I ),I=1,3),
*     *                 IW(NPNAME+31),
*     *                 HW(NZ2+57)/100,MOD(HW(NZ2+57),100),HW(NZ2+58)
* Shuffle first MOD argument into an integer*4 variable
        ihelp=HW(NZ2+57)
        WRITE(6,9131)                          (HW(NZ2+I),I=1,14),
     *                 RW(NPNAME+8),(RW( NPNAME+ LLH + I ),I=1,3),
     *                 IW(NPNAME+31),
     *                 HW(NZ2+57)/100,MOD(ihelp,100),HW(NZ2+58)
*** PMF(end)
 9131  FORMAT(
     *1X,' LHD  LVX  NVX  LMC   LT   NP  LCH  NCH  LNE  NNE  LRE  NRE',
     *'   # RUN   # EVT   E-BEAM   X-VX1   Y-VX1   Z-VX1  MCREDU',
     * 7X,'DATE',/,
     * 1X,12(I4,1X),1X,I6,2X,I6,2X,F7.3,3(1X,F7.2),6X,I2,
     * 1X,I2,'/',I2,'/',I4/)
C
        J = NPNAME+9
        K = NPNAME+28
        WRITE(6,9032) (RW(I),I=J,K)
 9032   FORMAT( 5X,3F10.3,2X,F10.3,'            '/
     *          5X,3F10.3,2X,F10.3,'  SPHERICITY'/
     *          5X,3F10.3,2X,F10.3,'           '/
     *          5X,3F10.3,2X,F10.3,'  THRUST'/
     *          5X,3F10.3,2X,F10.3,'  AKOPLANARITY')
C
        N = HW( NZ2 + 6 )
        LTR = HW( NZ2 + 5 )
        LTRCH = HW( NZ2 + 7 )
        LTRNE = HW( NZ2 + 9 )
        LTRRE = HW( NZ2 + 11)
        NP = NPNAME + HW(NZ2+1)
        IF( N.GT.0 ) THEN
C
      IF ( HW( NZ2 + 7 ) .EQ. 12 ) THEN
C
C
        WRITE(6,9033)
 9033   FORMAT(1X,'NR    EX    EY    EZ   PTOT  Q',
     *         ' PH TYPE VO VS PL OR PT',
     *         '   ECL SECL M_ECL  DET CL PA RZRF',
     *         ' X-TRK Y-TRK Z-TRK  DEDX SDEDX  R-MIN M')
C
      ELSEIF ( HW( NZ2 + 7 ) .EQ. 13 ) THEN
C
        WRITE(6,9115)
 9115   FORMAT(1X,'NR    EX    EY    EZ   PTOT  Q',
     *         ' PH TYPE VO VS PL OR PT',
     *         '   ECL SECL M_ECL  DET CL PA RZRF',
     *         ' X-TRK Y-TRK Z-TRK  DEDX   SIG NH  R-MIN M VC')
C
      ELSEIF ( HW( NZ2 + 7 ) .EQ. 14 ) THEN
C
        WRITE(6,9116)
 9116   FORMAT(1X,'NR    EX    EY    EZ   PTOT  Q',
     *         ' PH TYPE VX12 PL OR',
     *         '   ECL SECL M_ECL  DET CL PA RZRF',
     *         ' X-TRK Y-TRK Z-TRK  DEDX  SIG NH  R-MIN M VXC BETA')
C
      ENDIF
C
        DO 2031 J=1,N
          NP2 = NP*2
          IF( HW(NP*2 +18 ) .EQ. 1 )  THEN
C
         IF ( HW( NZ2 + 7 ) .EQ. 12 ) THEN
C
*** PMF 09/12/99      WRITE(6,9034)J,RW(NP+1),RW(NP+2),RW(NP+3),RW(NP+6),INT(RW(NP+7)),
*     *                    HW(NP2+7),HW(NP2+8)/100,MOD(HW(NP2+8),100),
*     *                    HW(NP2+9),HW(NP2+10),
*     *                    HW(NP2+15),HW(NP2+16),HW(NP2+18),
*     &                    (RW(NP+LTR+I),I=1,3),(HW(NP2+LTR*2+I),I=7,10),
*     &                    (RW(NP+LTR+I),I=6,11),
*     &                    MUQUA( (IW(NP+LTR+12) + 3)/4 )
* Shuffle first MOD argument into an integer*4 variable
      ihelp=HW(NP2+8)
      WRITE(6,9034)J,RW(NP+1),RW(NP+2),RW(NP+3),RW(NP+6),INT(RW(NP+7)),
     *                    HW(NP2+7),HW(NP2+8)/100,MOD(ihelp,100),
     *                    HW(NP2+9),HW(NP2+10),
     *                    HW(NP2+15),HW(NP2+16),HW(NP2+18),
     &                    (RW(NP+LTR+I),I=1,3),(HW(NP2+LTR*2+I),I=7,10),
     &                    (RW(NP+LTR+I),I=6,11),
     &                    MUQUA( (IW(NP+LTR+12) + 3)/4 )
*** PMF (end)
 9034     FORMAT(1X,I2,1X,3(F5.2,1X),F6.3,1X,I2,1X,I2,1X,2I2,1X,
     &            2(I2,1X),I2,I3,1X,I2,
     &            F6.2,F5.2,F6.2,I5,I3,1X,I2,1X,I4,3(1X,F5.1),
     &            2(1X,F5.2),1X,F6.2,1X,A1)
C
      ELSEIF ( HW( NZ2 + 7 ) .EQ. 13 ) THEN
C
*** PMF 09/12/99       WRITE(6,9118)J,RW(NP+1),RW(NP+2),RW(NP+3),RW(NP+6),INT(RW(NP+7)),
*     *                     HW(NP2+7),HW(NP2+8)/100,MOD(HW(NP2+8),100),
*     *                     HW(NP2+9),HW(NP2+10),
*     *                     HW(NP2+15),HW(NP2+16),HW(NP2+18),
*     &                    (RW(NP+LTR+I),I=1,3),(HW(NP2+LTR*2+I),I=7,10),
*     &                    (RW(NP+LTR+I),I=6,10),
*     &                    HW(NP2+2*LTR+25),RW(NP+LTR+11),
*     &                    MUQUA( (IW(NP+LTR+12) + 3)/4 ),
*     &                    HW(NP2+2*LTR+26)
* Shuffle first MOD argument into an integer*4 variable
       ihelp=HW(NP2+8)
       WRITE(6,9118)J,RW(NP+1),RW(NP+2),RW(NP+3),RW(NP+6),INT(RW(NP+7)),
     *                     HW(NP2+7),HW(NP2+8)/100,MOD(ihelp,100),
     *                     HW(NP2+9),HW(NP2+10),
     *                     HW(NP2+15),HW(NP2+16),HW(NP2+18),
     &                    (RW(NP+LTR+I),I=1,3),(HW(NP2+LTR*2+I),I=7,10),
     &                    (RW(NP+LTR+I),I=6,10),
     &                    HW(NP2+2*LTR+25),RW(NP+LTR+11),
     &                    MUQUA( (IW(NP+LTR+12) + 3)/4 ),
     &                    HW(NP2+2*LTR+26)
*** PMF(end)
 9118     FORMAT(1X,I2,1X,3(F5.2,1X),F6.3,1X,I2,1X,I2,1X,2I2,1X,
     &            2(I2,1X),I2,I3,1X,I2,
     &            F6.2,F5.2,F6.2,I5,I3,1X,I2,1X,I4,3(1X,F5.1),
     &            2(1X,F5.2),1X,I2,1X,F6.2,1X,A1,I3)
C
      ELSEIF ( HW( NZ2 + 7 ) .EQ. 14 ) THEN
C
***       WRITE(6,9119)J,RW(NP+1),RW(NP+2),RW(NP+3),RW(NP+6),INT(RW(NP+7)),
*     *                     HW(NP2+7),HW(NP2+8)/100,MOD(HW(NP2+8),100),
*     *                     HW(NP2+9),HW(NP2+10),
*     *                     HW(NP2+15),HW(NP2+16),
*     &                    (RW(NP+LTR+I),I=1,3),(HW(NP2+LTR*2+I),I=7,10),
*     &                    (RW(NP+LTR+I),I=6,10),
*     &                    HW(NP2+2*LTR+25),RW(NP+LTR+11),
*     &                    MUQUA( (IW(NP+LTR+12) + 3)/4 ),
*     &                    (HW(NP2+2*LTR+I),I=26,27),
*     &                    FLOAT(HW(NP2+2*LTR+28))/1000.
* Shuffle first MOD argument into an integer*4 variable
       ihelp=HW(NP2+8)
       WRITE(6,9119)J,RW(NP+1),RW(NP+2),RW(NP+3),RW(NP+6),INT(RW(NP+7)),
     *                     HW(NP2+7),HW(NP2+8)/100,MOD(ihelp,100),
     *                     HW(NP2+9),HW(NP2+10),
     *                     HW(NP2+15),HW(NP2+16),
     &                    (RW(NP+LTR+I),I=1,3),(HW(NP2+LTR*2+I),I=7,10),
     &                    (RW(NP+LTR+I),I=6,10),
     &                    HW(NP2+2*LTR+25),RW(NP+LTR+11),
     &                    MUQUA( (IW(NP+LTR+12) + 3)/4 ),
     &                    (HW(NP2+2*LTR+I),I=26,27),
     &                    FLOAT(HW(NP2+2*LTR+28))/1000.
 9119     FORMAT(1X,I2,1X,3(F5.2,1X),F6.3,1X,I2,1X,I2,1X,2I2,2X,
     &            2(I1,1X),I2,I3,
     &            F6.2,F5.2,F6.2,I5,I3,1X,I2,1X,I4,3(1X,F5.1),
     &            1X,2F5.2,1X,I2,1X,F6.2,1X,A1,I2,I2,F5.2)
C
      ENDIF
C
            NP = NP + LTR + LTRCH
C
          ELSEIF ( HW(NP*2 + 18) .EQ. 0 ) THEN
            IF ( HW( NZ2 + 7 ) .EQ. 12 .OR. HW(NZ2 + 7) .EQ. 13 ) THEN
C
      WRITE(6,9035)J,RW(NP+1),RW(NP+2),RW(NP+3),RW(NP+6),INT(RW(NP+7)),
     *                    HW(NP2+7),HW(NP2+8),
     *                    HW(NP2+9),HW(NP2+10),
     *                    HW(NP2+15),HW(NP2+16),HW(NP2+18),
     &                    (RW(NP+LTR+I),I=1,3),(HW(NP2+LTR*2+I),I=7,8)
 9035       FORMAT(1X,I2,1X,3(F5.2,1X),F6.3,1X,I2,1X,I2,1X,I4,1X,
     &                 2(I2,1X),I2,2I3,
     &                 F6.2,F5.2,F6.2,I5,I3)
C
        ELSEIF ( HW( NZ2 + 7 ) .EQ. 14 ) THEN
C
      WRITE(6,9039)J,RW(NP+1),RW(NP+2),RW(NP+3),RW(NP+6),INT(RW(NP+7)),
     *                    HW(NP2+7),HW(NP2+8),
     *                    HW(NP2+9),HW(NP2+10),
     *                    HW(NP2+15),HW(NP2+16),
     &                    (RW(NP+LTR+I),I=1,3),(HW(NP2+LTR*2+I),I=7,8)
 9039       FORMAT(1X,I2,1X,3(F5.2,1X),F6.3,1X,I2,1X,I2,1X,I4,1X,
     &                 2I2,2I3,
     &                 F6.2,F5.2,F6.2,I5,I3)
        ENDIF
            NP = NP + LTR + LTRNE
C
          ELSEIF( HW(NP*2 + 18) .EQ. 2 )  THEN
C
      WRITE(6,9036)J,RW(NP+1),RW(NP+2),RW(NP+3),RW(NP+6),INT(RW(NP+7)),
     *                    HW(NP2+7),HW(NP2+8),
     *                    HW(NP2+9),HW(NP2+10),
     *                    HW(NP2+15),HW(NP2+16),HW(NP2+18),
     &                    (RW(NP+LTR+I),I=1,3),(HW(NP2+LTR*2+I),I=7,8)
 9036       FORMAT(1X,I2,1X,3(F5.2,1X),F6.3,1X,I2,1X,I2,1X,I4,1X,
     &                 4(I2,1X),I2,
     &                 F6.2,F5.2,F6.2,I5,I3)
            NP = NP + LTR + LTRRE
C
          ELSEIF( HW(NP*2 + 18) .EQ.-1 )  THEN
C
            NP = NP + LTR
C
         ENDIF
C
 2031    CONTINUE
        ENDIF
        GO TO 8000
C                                           DEDX
 1004   CONTINUE
C
        WRITE(6,9041) IW(NPNAME+1), IW(NPNAME+2)
 9041   FORMAT(' Error Code ',I6,'  Tracks ',I4)
        IF( IW(NPNAME+2).GT.0 ) THEN
          WRITE(6,9042)
 9042     FORMAT(' Track Hits   dEdx  sdEdx   chi e    chi pi',
     *     '   chi K    chi p  min        p       dp')
          NP = NPNAME + 2
          DO 2041 J=1,IW(NPNAME+2)
            WRITE(6,9043) J, IW(NP+1),(RW(NP+I),I=2,7), IW(NP+8),
     *                    RW(NP+9), RW(NP+10)
 9043       FORMAT(1X,2I5,2F7.2,4F9.3,I4,2F9.3)
            NP = NP + 10
 2041     CONTINUE
        ENDIF
C
        GO TO 8000
      ENDIF
C
C
 8000 CONTINUE
      END
