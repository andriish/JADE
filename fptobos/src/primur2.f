      SUBROUTINE PRIMUR2(IPMUR2)
*
*   Print the content of the MUR2 bank, with pointer IPMUR2
*   IPPATR points to the 4th word of the BOS header, i.e. to LENGTH
*   (this is BOS convention)   IPMUR2 is a pointer in array IEVENT
*
*   11.06.2008   J.O.
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
      CHARACTER CHMUR2*4,CHASCI*4,CHEBCD*4
      INTEGER IHALF(4800)

*
*   J.Olsson   14.12.2005
* ----------------  CODE  ----------------------
*
      CHMUR2 = CHASCI(IEVENT(IPMUR2-3))
      II = IPMUR2
*
      WRITE(6,1001) CHMUR2,IEVENT(II-2),IEVENT(II-1),IEVENT(II)
 1001 FORMAT(' ',A4,' ',2I6,'    Length ',I5)
      WRITE(6,1002)
 1002 FORMAT(' =================================')
*
      NRMUR2 = IEVENT(II-2)
      LEMUR2 = IEVENT(II)
*
*   MUR2 comes with 7 different Bank numbers (0 - 6), all different formats
*   All numbers are considered in this version
*
      IF(NRMUR2.EQ.0) THEN
*                          MUR2:0  has only I*4 words
*
        WRITE(6,2001) (IEVENT(II+J),J=1,4)
 2001   FORMAT('  NTR_patr ',I4,'  NWPTR ',I4,'  NTPH ',I4,
     $         '  NPXPLTR ',I4)
*
        IF(LEMUR2.NE.4) THEN
          WRITE(6,2002) LEMUR2
 2002     FORMAT(' PRIMUR2:0 Warning, length ',I4,' ne.4 !')
          GO TO 9999
        ENDIF
      ELSEIF(NRMUR2.EQ.1) THEN
        NTR = LEMUR2/38
        IF(NTR*38.NE.LEMUR2) THEN
          WRITE(6,2009) NTR,LEMUR2
 2009     FORMAT(' PRIMUR2 Warning, NTR,LEMUR2 incompatible ',2I8)
          GO TO 9999
        ENDIF
*
        II = II - 38
        DO 200  ITR = 1,NTR
         II = II + 38
*         CALL CNVIBM3E(IEVENT(II+2))
         WRITE(6,2010) IEVENT(II+1),CHEBCD(IEVENT(II+2)),
     $                 (IEVENT(II+J),J=3,6)
 2010    FORMAT(' #IDTR ',I3,' ProgID ',A4,' Vers.date ',I8,
     $          ' hitinfo ',I8,' Acc.,Q-flags ',I3,1X,I6)
*
*                               Words 7-10 are packed I*2
         DO 100 J = 1,4
          CALL TWOIN1(IEVENT(II+6+J),IHALF(J*2-1),IHALF(J*2))
 100     CONTINUE
*
         WRITE(6,2011) (IHALF(J),J=1,8)
 2011    FORMAT(' shar.tr.info ',8I8)
*      
         DO 2020 J = 11,26
          CALL CNVIBM3E(IEVENT(II+J))
 2020    CONTINUE
*
         WRITE(6,2021) (REVENT(II+J),J=11,15)
 2021    FORMAT(' CHI**2, dEdx info ',5F12.6)
         WRITE(6,2022) (REVENT(II+J),J=16,20)
 2022    FORMAT(' Energy, dEdx info ',5F12.6)
         WRITE(6,2023) (REVENT(II+J),J=21,26)
 2023    FORMAT(' Probabilites ',6F12.6)
*
         WRITE(6,2024) (IEVENT(II+J),J=27,29)
 2024    FORMAT(' Cluster info ',3I10)
*
         DO 2030 J = 30,38
          CALL CNVIBM3E(IEVENT(II+J))
 2030    CONTINUE
*
         WRITE(6,2025) (REVENT(II+J),J=30,33)
 2025    FORMAT(' Part.Probabilites ',4F12.6)
*
         WRITE(6,2026) (REVENT(II+J),J=34,38)
 2026    FORMAT(' Part.Probabilites ',4F12.6,'  Momentum ',F12.6)
*
 200    CONTINUE
      ELSEIF(NRMUR2.EQ.2) THEN
*
*  There are 4 16-bit words for each muon hit
*
        NMUHIT = LEMUR2/2
        IF(NMUHIT.GT.1200) THEN
          WRITE(6,2031) NMUHIT
 2031     FORMAT(' PRIMUR2 Warning, NMUHIT ',I5,'>1200, no storage!')
          GO TO 9999
        ENDIF
*
        DO 250  J = 1,LEMUR2
         CALL TWOIN1(IEVENT(II+J),IHALF(J*2-1),IHALF(J*2))
 250    CONTINUE
*
        JJ = -4
        DO 300 IMU = 1,NMUHIT
         JJ =  JJ + 4
         WRITE(6,2080) (IHALF(JJ+J),J=1,4)
 2080    FORMAT(' ID-track info ',4I8)
 300    CONTINUE
*
      ELSEIF(NRMUR2.EQ.3) THEN
*
*  There are 4 16-bit words for each muon hit
*
        NMUHIT = LEMUR2/2
        IF(NMUHIT.GT.1200) THEN
          WRITE(6,2032) NMUHIT
 2032     FORMAT(' PRIMUR2 Warning, NMUHIT ',I5,'>1200, no storage!')
          GO TO 9999
        ENDIF
*
        DO 260  J = 1,LEMUR2
         CALL TWOIN1(IEVENT(II+J),IHALF(J*2-1),IHALF(J*2))
 260    CONTINUE
*
        JJ = -4
        DO 400 IMU = 1,NMUHIT
         JJ =  JJ + 4
         WRITE(6,2081) (IHALF(JJ+J),J=1,4)
 2081    FORMAT(' Hit ambiguity info ',4I8)
 400    CONTINUE
*
      ELSEIF(NRMUR2.EQ.4) THEN
*
        NTRIDMU = LEMUR2/15
        IF(NTRIDMU*15.NE.LEMUR2) THEN
          WRITE(6,2034) NTRIDMU,LEMUR2
 2034     FORMAT(' PRIMUR2 Warning, incompatible NTRIDMU,LEMUR2 ',2I8)
          GO TO 9999
        ENDIF
*
        DO 2033 J = 1,LEMUR2
         CALL CNVIBM3E(IEVENT(II+J))
 2033   CONTINUE
*
        JJ = II - 15
        DO 2040  ITR = 1,NTRIDMU
         JJ = JJ + 15
         WRITE(6,2041) (REVENT(JJ+J),J=1,6)
 2041    FORMAT(' Tr.points ',3F12.5,3X,3F12.5)
         WRITE(6,2041) (REVENT(JJ+J),J=7,12)
         WRITE(6,2041) (REVENT(JJ+J),J=13,15)
 2040   CONTINUE
*
      ELSEIF(NRMUR2.EQ.5) THEN
*                 Ellipse info, each ellipse has 7 words
*
        NELLIPS = LEMUR2/7
        IF(NELLIPS*7.NE.LEMUR2) THEN
          WRITE(6,2051) NELLIPS,LEMUR2
 2051     FORMAT(' PRIMUR2 Warning, incompatible NELLIPS,LEMUR2 ',2I8)
          GO TO 9999
        ENDIF
*
        DO 2053 J = 1,LEMUR2
         CALL CNVIBM3E(IEVENT(II+J))
 2053   CONTINUE
*
        JJ = II - 7
        DO 2054  ITR = 1,NELLIPS
         JJ = JJ + 7
*
         DO 2056 J = 1,3
          CALL CNVIBM3E(IEVENT(JJ+J))
 2056    CONTINUE
*
         WRITE(6,2061) (REVENT(JJ+J),J=1,3)
 2061    FORMAT(' Ellips cent.coord ',3E14.6)
*
         CALL TWOIN1(IEVENT(JJ+4),IHALF(1),IHALF(2))
         WRITE(6,2058) (ihalf(j),j=1,2)
 2058    FORMAT('  IDtrack#, chamb.orientation ',2I8)
*
         DO 2055 J = 5,7
          CALL CNVIBM3E(IEVENT(JJ+J))
 2055    CONTINUE
*
         WRITE(6,2062) (REVENT(JJ+J),J=5,7)
 2062    FORMAT(' Ellips variances ',3E14.6)
*
 2054   CONTINUE
*
      ELSEIF(NRMUR2.EQ.6) THEN
*                 List of bad chambers, all I*2 words
*
        NBADCH = LEMUR2*2
*
        DO 600 J = 1,LEMUR2
         CALL TWOIN1(IEVENT(II+J),IHALF(J*2-1),IHALF(J*2))
 600    CONTINUE
*
        NBAD1 = NBADCH/10
        NBDREST = NBADCH - NBAD1*10
        JJ =  -10
*
        IF(NBAD1.GT.0) THEN
          DO 700 IB = 1,NBAD1
           JJ =  JJ + 10
           WRITE(6,2070) (IHALF(JJ+J),J=1,10)
 2070      FORMAT(' Bad chamber list ',10I6)
 700      CONTINUE
        ENDIF
* 
        IF(NBDREST.GT.0) THEN
          JJ =  JJ + 10
          WRITE(6,2070) (IHALF(JJ+J),J=1,NBDREST)
        ENDIF
*
      ENDIF
*
*-----------------------
 9999 CONTINUE
      RETURN
      END
