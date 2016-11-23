      SUBROUTINE PRIGVTX(IPGVTX)
*
*   Print the content of the GVTX bank, with pointer IPGVTX
*   IPGVTX points to the 4th word of the BOS header, i.e. to LENGTH
*   (this is BOS convention)   IPGVTX is a pointer in array IEVENT
*   The format of GVTX is given in JCN 32 (P. Dittmann)
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
      CHARACTER CHGVTX*4,CHASCI*4
*
*   J.Olsson   14.12.2005
* ----------------  CODE  ----------------------
*
      CHGVTX = CHASCI(IEVENT(IPGVTX-3))
      II = IPGVTX
*
      WRITE(6,1001) CHGVTX,IEVENT(II-2),IEVENT(II-1),IEVENT(II)
 1001 FORMAT(' ',A4,' ',2I6,'    Length ',I5)
      WRITE(6,1002)
 1002 FORMAT(' =================================')
*
      NRGVTX = IEVENT(II-2)
*--
      NVX = IEVENT(II+1)
      NTVX = IEVENT(II+NVX*10+2)
*
      WRITE(6,1031) NVX,NTVX
 1031 FORMAT(' Nr. of Vertices, nr. of Tracks ',2I10)
*
      IF(NVX.LE.0.OR.NTVX.LE.0) THEN
        WRITE(6,4545) NVX,NTVX
 4545   FORMAT('  No vertices, or no tracks! ',2I10)
        GO TO 9999
      ENDIF
*
      LENVX = 10
      LENTRVX = 15
      KTR = 0
      II = II + 1 
*---- 
 100  CONTINUE
      KTR = KTR + 1
      IF(KTR.GT.NVX) GO TO 200
*
      WRITE(6,1005) KTR
 1005 FORMAT('            Vertex nr. ',I3)
      WRITE(6,1006)
 1006 FORMAT('            **************')
*
      DO 2001 J = 2,7
       CALL CNVIBM3E(IEVENT(II+J))
 2001 CONTINUE
      CALL CNVIBM3E(IEVENT(II+9))
*
      WRITE(6,1007) IEVENT(II+1),IEVENT(II+8),IEVENT(II+10)
 1007 FORMAT(' Vtx-Flag: ',I3,'  #Trcks used in fit:',I3,
     $       '  #Trcks belonging to vtx:',I3) 
      WRITE(6,1008)
 1008 FORMAT('  Vertex coord. XYZ  +- sigma ')
      WRITE(6,1009) (REVENT(II+1+K),REVENT(II+4+K),K=1,3)
 1009 FORMAT('   ',3(F12.6,' +- ',F12.6,4X))
      WRITE(6,1003) REVENT(II+9)
 1003 FORMAT('   Chi**2 of fit (NDF=2*NTVX-3):  ',F12.6)
*
      II = II + LENVX      
*
      GO TO 100
*
*        Track part of GVTX bank
*
 200  CONTINUE
      II = II + 1
      KTR = 0      
 210  CONTINUE
      KTR = KTR + 1
      IF(KTR.GT.NTVX) GO TO 9999
      WRITE(6,1035) KTR
 1035 FORMAT('            Vtx-track nr. ',I3)
      WRITE(6,1036)
 1036 FORMAT('            ******************')
*
      WRITE(6,1023) IEVENT(II+1),IEVENT(II+13),IEVENT(II+14)
 1023 FORMAT(' Track-Flag: ',I3,'  #Hits on trck:',I3,
     $       '  Vtx-number:',I3)
*
      DO 2002 J = 2,12
       CALL CNVIBM3E(IEVENT(II+J))
 2002 CONTINUE
      CALL CNVIBM3E(IEVENT(II+15))
*
      WRITE(6,1018)
 1018 FORMAT('  Radius,  phi/theta +- sigma ')
      WRITE(6,1019) REVENT(II+2),
     $              (REVENT(II+2+K),REVENT(II+7+K),K=1,2)
 1019 FORMAT('   ',F12.6,6X,2(F12.6,' +- ',F12.6,4X))  
      WRITE(6,1020)
 1020 FORMAT('  Tr.point of closest appr.  XYZ  +- sigma ')
      WRITE(6,1021) (REVENT(II+4+K),REVENT(II+9+K),K=1,3)
 1021 FORMAT('   ',3(F12.6,' +- ',F12.6,4X))
      WRITE(6,1022) REVENT(II+15)
 1022 FORMAT('  Extrapol. Arc Length ',F12.6)
*
      II =  II + LENTRVX
      GO  TO 210
*-----------------------
 9999 CONTINUE
      RETURN
      END
