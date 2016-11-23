      SUBROUTINE PRITPVX(IPTPVX)
*
*   Print the content of the TPVX bank, with pointer IPTPVX
*   IPTPVX points to the 4th word of the BOS header, i.e. to LENGTH
*   (this is BOS convention)   IPTPVX is a pointer in array IEVENT
*
*   J. Olsson  11.06.2008
*   Last Update 12.06.2008   J.O.  
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
      CHARACTER CHTPVX*4,CHASCI*4,CHEBCD*4
      INTEGER IHALF(200)
*
*   J.Olsson   14.12.2005
* ----------------  CODE  ----------------------
*
      CHTPVX = CHASCI(IEVENT(IPTPVX-3))
      II = IPTPVX
*
      WRITE(6,1001) CHTPVX,IEVENT(II-2),IEVENT(II-1),IEVENT(II)
 1001 FORMAT(' ',A4,' ',2I6,'    Length ',I5)
      WRITE(6,1002)
 1002 FORMAT(' =================================')
*
      NRTPVX = IEVENT(II-2)
      LETPVX = IEVENT(II)
*
*  Note that TPVX length differs from bank to bank
*
*   Convert the 16 bit word info in TPVX, to local array IHALF
*
       CALL TWOIN1(IEVENT(II+1),IHALF(1),IHALF(2))
*
       WRITE(6,1110) IHALF(1),IHALF(2)
1110  FORMAT(' TPTR nr, Fitting mode ',2(1X,I7))
*
      MODE = IHALF(2) - 10*(IHALF(2)/10)
*
*  conversion to IEEE FP
*
      DO 2001 J = 2,8
       CALL CNVIBM3E(IEVENT(II+J))
 2001 CONTINUE
*
      WRITE(6,1081) (REVENT(II+J),J=2,4)
 1081 FORMAT(' Vertex xyz ',3F12.6)
      WRITE(6,1082) (REVENT(II+J),J=5,8)
 1082 FORMAT(' xyz errors, chi^2_fit',4F12.6)
*
      IF(MODE.EQ.2.OR.MODE.EQ.3) THEN
        CALL CNVIBM3E(IEVENT(II+9))
        WRITE(6,1083) REVENT(II+9)
 1083   FORMAT(' Chi^2-variable of fit, flag2/3 ',F12.6)
      ELSE
        WRITE(6,1084) IEVENT(II+9)
 1084   FORMAT(' NDF of fit ',I12)
      ENDIF
*
*  conversion to IEEE FP 
*
       CALL CNVIBM3E(IEVENT(II+10))
*
      WRITE(6,1087) REVENT(II+10)
 1087 FORMAT(' Average cos of track_pair_angles ',F12.6)
*
      LEREST = LETPVX-10
*
      DO 300 J = 1,LEREST
       CALL TWOIN1(IEVENT(II+10+J),IHALF(J*2-1),IHALF(J*2))
 300  CONTINUE
*
      WRITE(6,1092) (IHALF(LL),LL=1,10)
 1092 FORMAT(' Multiplicity info ',10(1X,I6))
*
      LEREST = LEREST - 5
      LEHALF = LEREST*2
*
      KK = LEHALF/10
      KKREST = LEHALF - KK*10
*
      JJ = 0
      IF(KK.GT.0) THEN
        DO 400 J = 1,KK
         JJ = JJ + 10
         WRITE(6,1093) (IHALF(JJ+LL),LL=1,10)
 1093    FORMAT(' Sec.track bank nrs ',10(1X,I6))
 400    CONTINUE
      ENDIF
*
      IF(KKREST.GT.0) THEN
        WRITE(6,1093) (IHALF(JJ+10+LL),LL=1,KKREST)
      ENDIF
*
*-----------------------
 9999 CONTINUE
      RETURN
      END
