      SUBROUTINE PRITPEV(IPTPEV)
*
*   Print the content of the TPEV bank, with pointer IPTPEV
*   IPTPEV points to the 4th word of the BOS header, i.e. to LENGTH
*   (this is BOS convention)   IPTPEV is a pointer in array IEVENT
*
*   J. Olsson  11.06.2008
*   Last Update  
*
C********** MACRO CEVENT **************
C   Event Buffer
C   The size corresponds to 2 MByte event size, maximum
C
      COMMON /CEVENT/IEVENT(525000)
      DIMENSION REVENT(525000)
      EQUIVALENCE (IEVENT(1),REVENT(1))
*=====================================
*
      CHARACTER CHTPEV*4,CHASCI*4,CHEBCD*4
      INTEGER IHALF(200)
*
*   J.Olsson   14.12.2005
* ----------------  CODE  ----------------------
*
      CHTPEV = CHASCI(IEVENT(IPTPEV-3))
      II = IPTPEV
*
      WRITE(6,1001) CHTPEV,IEVENT(II-2),IEVENT(II-1),IEVENT(II)
 1001 FORMAT(' ',A4,' ',2I6,'    Length ',I5)
      WRITE(6,1002)
 1002 FORMAT(' =================================')
*
      NRTPEV = IEVENT(II-2)
      LETPEV = IEVENT(II)
*
 
      WRITE(6,1003) (IEVENT(II+J),J=1,2)
 1003 FORMAT('    TP Program version ',I2,'  Production date ',I12)
*                  Prod.date has the form  YYMMDDHH
*
*   Convert the 16 bit word info in TPEV, to local array IHALF
*
      DO 100 J = 1,15
       CALL TWOIN1(IEVENT(II+2+J),IHALF(J*2-1),IHALF(J*2))
 100  CONTINUE
*
      IK = -9
 200  CONTINUE
      IF(JK.GE.30) GO TO 201
      IK = IK+ 10
      JK = IK + 9
      WRITE(6,1110) IK,JK,(IHALF(IK+LL),LL=0,9)
1110  FORMAT(' counters ',I4,'-',I4,2X,10(1X,I7))
      GO TO 200
*
 201  CONTINUE
*
*  conversion to IEEE FP
*
      DO 2001 J = 18,27
       CALL CNVIBM3E(IEVENT(II+J))
 2001 CONTINUE
*
      WRITE(6,1081) (REVENT(II+J),J=18,21)
 1081 FORMAT(' Energies ',4(F12.6))
      WRITE(6,1082) (REVENT(II+J),J=22,27)
 1082 FORMAT(' Miss.mom.xyz +- dxyz ',3(F12.6),' +- ',3F12.6)
*
      CALL TWOIN1(IEVENT(II+28),IHALF(1),IHALF(2))
      WRITE(6,1083) IHALF(1),IHALF(2)
 1083 FORMAT(' Sphericity flag and #tracks ',2I8)
*
*  conversion to IEEE FP
*
      DO 2002 J = 29,40
       CALL CNVIBM3E(IEVENT(II+J))
 2002 CONTINUE
*
      WRITE(6,1084) (REVENT(II+J),J=29,31)
 1084 FORMAT(' Tensor ellips Eigenvalues ',3(F12.6))
      WRITE(6,1085) (REVENT(II+J),J=32,34)
 1085 FORMAT(' Cosdir Q3 dir ',3(F12.6))
      WRITE(6,1086) (REVENT(II+J),J=35,37)
 1086 FORMAT(' Cosdir Q2 dir ',3(F12.6))
      WRITE(6,1087) (REVENT(II+J),J=38,40)
 1087 FORMAT(' Cosdir Q1 dir ',3(F12.6))
*
*
      CALL TWOIN1(IEVENT(II+41),IHALF(1),IHALF(2))
      WRITE(6,1088) IHALF(1),IHALF(2)
 1088 FORMAT(' Thrust: #tracks, max.tracks  ',2I8)
*
*  conversion to IEEE FP 
*
      DO 2003 J = 42,55
       CALL CNVIBM3E(IEVENT(II+J))
 2003 CONTINUE
*
      WRITE(6,1089) (REVENT(II+J),J=42,45)
 1089 FORMAT(' Thrust, cosdir ',4(F12.6))
*
      WRITE(6,1090) (REVENT(II+J),J=46,50)
 1090 FORMAT(' not used  ',5(F12.6))
*
      WRITE(6,1091) (REVENT(II+J),J=51,55)
 1091 FORMAT(' TOF and 2-prong info ',5(F12.6))
*
      DO 300 J = 1,5
       CALL TWOIN1(IEVENT(II+55+J),IHALF(J*2-1),IHALF(J*2))
 300  CONTINUE
*
      WRITE(6,1092) (IHALF(LL),LL=1,10)
 1092 FORMAT(' error flags ',10(1X,I7))
*
*-----------------------
 9999 CONTINUE
      RETURN
      END
