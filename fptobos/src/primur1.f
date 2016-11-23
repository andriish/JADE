      SUBROUTINE PRIMUR1(IPMUR1)
*
*   Print the content of the MUR1 bank, with pointer IPMUR1
*   IPHEAD points to the 4th word of the BOS header, i.e. to LENGTH
*   (this is BOS convention)   IPMUR1 is a pointer in array IEVENT
*
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
      CHARACTER CHMUR1*4,CHASCI*4
      INTEGER IHALF(10800)
*
*   J.Olsson   11.06.2008
*
*  The MUR1 bank (JCN22) comes in 7 versions, 
*         with different bank number
*         and different formats                       
*  Current version of CODE covers only the bank numbers 0,1,2
* ----------------  CODE  ----------------------
*
      CHMUR1 = CHASCI(IEVENT(IPMUR1-3))
      II = IPMUR1
*
      WRITE(6,1001) CHMUR1,IEVENT(II-2),IEVENT(II-1),IEVENT(II)
 1001 FORMAT(' ',A4,' ',2I6,'    Length ',I5)
      WRITE(6,1002)
 1002 FORMAT(' =================================')
*
*
      NRMUR1 = IEVENT(IPMUR1-2)
      LEMUR1 = IEVENT(IPMUR1)
*
      IF(NRMUR1.EQ.0) THEN
*                           MUR1:0 is an I*4 bank
*
        WRITE(6,2001) (IEVENT(IPMUR1+J),J=1,4)
 2001   FORMAT('  Nr hits ',I6,'  Nr clusters(tracks) ',I6,
     $         '  Nr I*2 words /hit ',I3,'  NWPCL ',I3)
*
*
        WRITE(6,2002) (IEVENT(IPMUR1+J),J=5,9)
 2002   FORMAT('  Flag_MULINE ',I2,'  Flag_Cl_join ',I2,
     $         '  Flag_Cl_PBG_join ',I2,'  Version_date ',I10,
     $         '  Cal.data_identifier ',Z8)
        IF(IEVENT(II).NE.9) THEN
          WRITE(6,2003) IEVENT(II)
 2003     FORMAT('  Warning:  Bank Length is not 9, but ',I4)
        ENDIF 
      ELSEIF(NRMUR1.EQ.1) THEN
*
*   Convert the 16 bit word info in HEAD, to local array IHALF
*   MUR1:1 contains only 16-bit words, 9 for each muon hit
*
        MULEN1 = IEVENT(II)
        MULEN12 = 2*MULEN1
        NMUHIT = MULEN12/9
*
        IF(NMUHIT.GT.1200) THEN
          WRITE(6,2009) NMUHIT
 2009 FORMAT(' PRIMUR1 Warning, NMUHIT ',I5,'>1200, no storage!')
          GO TO 9999
        ENDIF
*
        DO 100 J = 1,MULEN1
         CALL TWOIN1(IEVENT(II+J),IHALF(J*2-1),IHALF(J*2))
 100    CONTINUE
* 
*  There are 9 16-bit words for each muon hit
*
        JJ = -9
        DO 300 IMU = 1,NMUHIT 
         JJ =  JJ + 9           
         WRITE(6,2010) (IHALF(JJ+J),J=1,9)
 2010    FORMAT(' ',I6,1X,I6,3X,3I6,3X,3I6,3X,I6)
 300    CONTINUE
      ELSEIF(NRMUR1.EQ.2) THEN
        NMUHIT = 2*LEMUR1 
*
        IF(NMUHIT.GT.10800) THEN
          WRITE(6,2029) NMUHIT
 2029 FORMAT(' PRIMUR1 Warning, NMUHIT ',I5,'>10800, no storage!')
          GO TO 9999
        ENDIF
*
        DO 400 J = 1,NMUHIT
         CALL TWOIN1(IEVENT(II+J),IHALF(J*2-1),IHALF(J*2))
 400    CONTINUE
* 
*  There are 1 16-bit words for each muon hit
*
        DO 500 IMU = 1,NMUHIT 
         WRITE(6,2020) IMU,IHALF(IMU)
 2020    FORMAT('   hit ',I4,'  bit-code ',Z8)
 500    CONTINUE
*
      ENDIF
*
 9999 CONTINUE
      RETURN
      END
