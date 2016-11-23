      SUBROUTINE PRILGCL(IPLGCL)
*
*   Print the content of the LGCL bank, with pointer IPLGCL
*   IPLGCL points to the 4th word of the BOS header, i.e. to LENGTH
*   (this is BOS convention)   IPLGCL is a pointer in array IEVENT
*
*   J. Olsson  10.06.2008
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
      CHARACTER CHLGCL*4,CHASCI*4
*
*   J.Olsson   14.12.2005
* ----------------  CODE  ----------------------
*
      CHLGCL = CHASCI(IEVENT(IPLGCL-3))
      II = IPLGCL
*
      WRITE(6,1001) CHLGCL,IEVENT(II-2),IEVENT(II-1),IEVENT(II)
 1001 FORMAT(' ',A4,' ',2I6,'    Length ',I5)
      WRITE(6,1002)
 1002 FORMAT(' =================================')
*
      NRLGCL = IEVENT(II-2)
*      WRITE(6,4320) II,NRLGCL,II,NRLGCL
* 4320 FORMAT('  debug  2x(II NRLGCL) ',2I12,4X,Z8,2X,Z8)
*
      WRITE(6,1003) (IEVENT(II+J),J=1,4)
 1003 FORMAT(' Pointers ',4I8)
      II = II + 4
*
      WRITE(6,1004)
 1004 FORMAT(' General: Version, date_time NCLST NCLSB/ZM/ZP ')
      WRITE(6,1005) (IEVENT(II+J),J=1,6)
 1005 FORMAT('         ',I5,2X,5I8)
       DO 2005  K = 1,4
         CALL CNVIBM3E(IEVENT(II+6+K))
         CALL CNVIBM3E(IEVENT(II+11+K))
 2005  CONTINUE
      WRITE(6,1006)
 1006 FORMAT(' General: ETOT ETOTB/M/P  ')
      WRITE(6,1007) (REVENT(II+6+J),J=1,4)
 1007 FORMAT('         ',4F12.6)
      WRITE(6,1016)
 1016 FORMAT(' General: NNEUT  EGTOT EGTOTB/M/P ')
      WRITE(6,1017) IEVENT(II+11),(REVENT(II+11+J),J=1,4)
 1017 FORMAT('         ',I5,2X,4F12.6)
      WRITE(6,1008)
 1008 FORMAT(' General: IER  ISTEP  ICORR ITRCON IFLG_TP  NWPCL ')
      WRITE(6,1009) (IEVENT(II+15+J),J=1,6)
 1009 FORMAT('         ',6I8)
*--
      NCLST = IEVENT(II+3)
      NWPCL = IEVENT(II+21)
      II = II + 21
*      WRITE(6,4321) II,NCLST,NWPCL,II,NCLST,NWPCL
* 4321 FORMAT('  debug  2x(II NCLST NWPCL) ',3I12,4X,3(Z8,2X))
*                  
      IF(NCLST.LE.0) THEN
        WRITE(6,4545)
 4545   FORMAT('  No LG Clusters! ')
        GO TO 9999
      ENDIF
*                     Cluster Map info
      NCLST1 = NCLST/8
      NCLREST = NCLST - NCLST1*8
      KK = II
*
      IF(NCLST1.GT.0) THEN
        KK = KK - 8
        DO 2001  ICLST1 = 1,NCLST1
         KK = KK + 8
         WRITE(6,2011) (IEVENT(KK+J),J=1,8)
 2011    FORMAT('  NCLST map ',8(Z8,2X))
 2001   CONTINUE
      ENDIF
      IF(NCLREST.GT.0) THEN
         WRITE(6,2011) (IEVENT(KK+J),J=1,NCLREST+1)
      ENDIF
*                     Cluster Map info, using TWOIN1
      NCLST1 = NCLST/8
      NCLREST = NCLST - NCLST1*8
      KK = II
*
      IF(NCLST1.GT.0) THEN
        KK = KK - 8
        DO 2041  ICLST1 = 1,NCLST1
         KK = KK + 8
         DO 2041  J=1,8
         CALL TWOIN1(IEVENT(KK+J),IA1,IA2)
         WRITE(6,2051) (ICLST-1)*8+J,IA1,IA2 
 2051    FORMAT('  NCLST map, cluster ',I3,'  pointers ',2I5)
 2041   CONTINUE
      ENDIF
      IF(NCLREST.GT.0) THEN
        DO 2061 J=1,NCLREST
         CALL TWOIN1(IEVENT(KK+J),IA1,IA2)
         WRITE(6,2051) J,IA1,IA2
 2061   CONTINUE
      ENDIF
*
*                      Cluster Info
      II = II + NCLST + 1
*      WRITE(6,4322) II,NCLST1,II,NCLST1
* 4322 FORMAT('  debug  2x(II NCLST1) ',2I12,4X,Z8,2X,Z8)
*
      II = II - NWPCL
      DO 2002  ICLST = 1,NCLST
        II = II + NWPCL
        JPART  = IEVENT(II + 1)
*      WRITE(6,4323) II,JPART,II,JPART
* 4323 FORMAT('  debug  2x(II JPART) ',2I12,4X,Z8,2X,Z8)
        DO 2003  K = 1,6
         CALL CNVIBM3E(IEVENT(II+1+K))
 2003   CONTINUE
        ENERGY = REVENT(II+2)
        SIGENR = REVENT(II+3)
        PHICLU = REVENT(II+4)
        ZCLU   = REVENT(II+5)
        SIGPHI = REVENT(II+6)
        SIGZCL = REVENT(II+7)         
*
        DO 2004  K = 1,8
         CALL CNVIBM3E(IEVENT(II+8+K))
 2004   CONTINUE
        NCHTR   = IEVENT(II+8)
        DIRCOS1 = REVENT(II+9)
        DIRCOS2 = REVENT(II+10)
        DIRCOS3 = REVENT(II+11)
        EIGVAL1 = REVENT(II+12)
        EIGVAL2 = REVENT(II+13)
        EIGVAL3 = REVENT(II+14)
        FREDGE  = REVENT(II+15)
        EUNCOR  = REVENT(II+16)
        WRITE(6,2012)  ICLST
 2012   FORMAT('      ===> Cluster ',I3,' <=== ')
        WRITE(6,2013) JPART,ENERGY,SIGENR 
 2013   FORMAT('  JPART:',I4,'   ENERGY +- SIGENR ',F12.6,' +-',F12.6)
        WRITE(6,2014) PHICLU,SIGPHI,ZCLU,SIGZCL 
 2014   FORMAT('  PHI/Z (X/Y) +- sigma ',2(F12.6,' +-',F12.6,3X))
        WRITE(6,2015) NCHTR,DIRCOS1,DIRCOS2,DIRCOS3 
 2015   FORMAT('  NCHTR:',I6,'   Dircos1-3:',3F12.6)
        WRITE(6,2016) EIGVAL1,EIGVAL2,EIGVAL3,FREDGE,EUNCOR
 2016   FORMAT('  Eigval1-3: ',3F12.6,'  Edgefrac EUNCOR: ',2F12.6)
*
 2002 CONTINUE
*
*-----------------------
 9999 CONTINUE
      RETURN
      END
