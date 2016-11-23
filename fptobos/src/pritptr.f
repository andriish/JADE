      SUBROUTINE PRITPTR(IPTPTR)
*
*   Print the content of the TPTR bank, with pointer IPTPTR
*   IPTPTR points to the 4th word of the BOS header, i.e. to LENGTH
*   (this is BOS convention)   IPTPTR is a pointer in array IEVENT
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
      CHARACTER CHTPTR*4,CHASCI*4,CHEBCD*4
      INTEGER IHALF(200)
*
*   J.Olsson   14.12.2005
* ----------------  CODE  ----------------------
*
      CHTPTR = CHASCI(IEVENT(IPTPTR-3))
      II = IPTPTR
*
      WRITE(6,1001) CHTPTR,IEVENT(II-2),IEVENT(II-1),IEVENT(II)
 1001 FORMAT(' ',A4,' ',2I6,'    Length ',I5)
      WRITE(6,1002)
 1002 FORMAT(' =================================')
*
      NRTPTR = IEVENT(II-2)
      LETPTR = IEVENT(II)
*  Note that TPTR length differs for particle types:
*             Charged tracks  80     Photons   50   Others  40 words
*
*   Convert the 16 bit word info in TPTR, to local array IHALF
*
      DO 100 J = 1,9
       CALL TWOIN1(IEVENT(II+J),IHALF(J*2-1),IHALF(J*2))
 100  CONTINUE
*
       WRITE(6,1110) (IHALF(LL),LL=1,10)
1110  FORMAT(' Indices, flags ',10(1X,I7))
       WRITE(6,1110) (IHALF(LL),LL=11,18)
*
*  conversion to IEEE FP
*
      DO 2001 J = 10,19
       CALL CNVIBM3E(IEVENT(II+J))
 2001 CONTINUE
       CALL CNVIBM3E(IEVENT(II+21))
*
      WRITE(6,1081) (REVENT(II+J),J=10,15)
 1081 FORMAT(' Orig.xyz +- dxyz ',3F12.6,' +- ',3F12.6)
      WRITE(6,1082) (REVENT(II+J),J=16,19)
 1082 FORMAT(' DCA,errors,chi^2',4F12.6)
*
      WRITE(6,1083) IEVENT(II+20),REVENT(II+21),IEVENT(II+22)
 1083 FORMAT(' NDF of rphi-fit',I4,' Chi^2 rz-fit ',F12.6,
     $       ' NDF of rphi-fit',I4)
*
*  conversion to IEEE FP
*
      DO 2002 J = 23,25
       CALL CNVIBM3E(IEVENT(II+J))
 2002 CONTINUE
*
      WRITE(6,1084) (REVENT(II+J),J=23,25),IEVENT(II+26)
 1084 FORMAT(' El.charge, Momentum, error, dir-type ',3(F12.6),I5)
*
      DO 2003 J = 27,33
       CALL CNVIBM3E(IEVENT(II+J))
 2003 CONTINUE
*
      WRITE(6,1085) (REVENT(II+J),J=27,29)
 1085 FORMAT(' Cosdir  ',3(F12.6))
      WRITE(6,1086) (REVENT(II+J),J=30,33)
 1086 FORMAT(' Cosdir errors, dummy ',4(F12.6))
      CALL TWOIN1(IEVENT(II+34),IHALF(1),IHALF(2))
      WRITE(6,1088) IHALF(1),IHALF(2)
 1088 FORMAT(' MC input mass type, most likely part.type  ',2I8)
*
*  conversion to IEEE FP 
*
      DO 2004 J = 35,38
       CALL CNVIBM3E(IEVENT(II+J))
 2004 CONTINUE
*
      WRITE(6,1087) (REVENT(II+J),J=35,38)
 1087 FORMAT(' Amass,ETOT,ESH,dESH ',4(F12.6))
*
      CALL TWOIN1(IEVENT(II+39),IHALF(1),IHALF(2))
      WRITE(6,10088) IHALF(1),IHALF(2)
10088 FORMAT(' ESH quality, uniqueness  ',2I8)
*      
      IF(LETPTR.LE.40) GO TO 9999
*
*  conversion to IEEE FP 
*
      DO 2005 J = 40,42
       CALL CNVIBM3E(IEVENT(II+J))
 2005 CONTINUE
*
      WRITE(6,1089) (REVENT(II+J),J=40,42)
 1089 FORMAT(' Chi^2 for LG shower,dummy ',3(F12.6))
*
      DO 300 J = 1,6
       CALL TWOIN1(IEVENT(II+42+J),IHALF(J*2-1),IHALF(J*2))
 300  CONTINUE
*
      WRITE(6,1092) (IHALF(LL),LL=1,12)
 1092 FORMAT(' Muon info flags ',12(1X,I6))
*
      IF(LETPTR.LE.50) GO TO 9999
*
*  conversion to IEEE FP 
*
      DO 2006 J = 49,57
       CALL CNVIBM3E(IEVENT(II+J))
 2006 CONTINUE
*
      WRITE(6,1090) (REVENT(II+J),J=49,52)
 1090 FORMAT(' dEdx info  ',4(F12.6))
*
      WRITE(6,1091) (REVENT(II+J),J=53,57)
 1091 FORMAT(' dEdx info  ',5(F12.6))
*
*
*  conversion to IEEE FP 
*
      DO 2007 J = 59,71
       CALL CNVIBM3E(IEVENT(II+J))
 2007 CONTINUE
*
      WRITE(6,1093) IEVENT(II+58),(REVENT(II+J),J=59,61)
 1093 FORMAT(' TOF info  ',I8,3(F12.6))
*
      WRITE(6,1094) (REVENT(II+J),J=62,64)
 1094 FORMAT(' TOF info  ',3(F12.6))
*
      WRITE(6,1095) (REVENT(II+J),J=65,68)
 1095 FORMAT(' chi^2 for mass hyps  ',4(F12.6))
*
      WRITE(6,1096) (REVENT(II+J),J=69,71)
 1096 FORMAT(' TOF dEdx, dummies  ',3(F12.6))
*
*
*  conversion to IEEE FP 
*
      DO 2008 J = 73,718
       CALL CNVIBM3E(IEVENT(II+J))
 2008 CONTINUE
*
      WRITE(6,1097) IEVENT(II+72),(REVENT(II+J),J=73,74)
 1097 FORMAT(' dEdx info  ',I8,2(F12.6))
*
      WRITE(6,1098) (REVENT(II+J),J=75,78)
 1098 FORMAT(' chi^2 for mass hyps  ',4(F12.6))
*
      WRITE(6,1099) (IEVENT(II+J),J=79,80),(IEVENT(II+J),J=79,80)
 1099 FORMAT(' Part.type w.lowest chi^2, dummy ',2I12,2(1X,Z8))
*
*-----------------------
 9999 CONTINUE
      RETURN
      END
