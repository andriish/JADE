      SUBROUTINE PRITAGG(IPTAGG)
*
*   Print the content of the TAGG bank, with pointer IPTAGG
*   IPPATR points to the 4th word of the BOS header, i.e. to LENGTH
*   (this is BOS convention)   IPTAGG is a pointer in array IEVENT
*
*   J. Olsson  12.06.2008   J.O.
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
      CHARACTER CHTAGG*4,CHASCI*4,CHEBCD*4
      INTEGER IHALF(200)

*
*   J.Olsson   14.12.2005
* ----------------  CODE  ----------------------
*
      CHTAGG = CHASCI(IEVENT(IPTAGG-3))
      II = IPTAGG
*
      WRITE(6,1001) CHTAGG,IEVENT(II-2),IEVENT(II-1),IEVENT(II)
 1001 FORMAT(' ',A4,' ',2I6,'    Length ',I5)
      WRITE(6,1002)
 1002 FORMAT(' =================================')
*
      NRTAGG = IEVENT(II-2)
      LETAGG = IEVENT(II)
*
*     Format is different for different NRTAGG,  bank numbers
*
      IF(NRTAGG.EQ.0) THEN
*
        WRITE(6,1003) IEVENT(II+1)
 1003   FORMAT(' Program ID, date DDMMY  ',I8)
*
        DO 100 J = 1,26
         CALL TWOIN1(IEVENT(II+1+J),IHALF(J*2-1),IHALF(J*2))
 100    CONTINUE
*
        WRITE(6,1005) (IHALF(J),J=1,6)
 1005   FORMAT(' ntr/m/p, clusters/m/p, NNEUT,ICOL ',8I8)
*
        WRITE(6,1006) (IHALF(J),J=9,14)
 1006   FORMAT(' ITYPE IER ICORR IPBLAT 2xdumm  ',6I8)
*
        WRITE(6,1007) (IHALF(J),J=15,24)
        WRITE(6,1007) (IHALF(J),J=25,34)
        WRITE(6,1007) (IHALF(J),J=35,44) 
        WRITE(6,1007) (IHALF(J),J=45,50)
 1007   FORMAT(' Scalers  ',10I6)
*
        WRITE(6,1008) (IHALF(J),J=51,52)
 1008   FORMAT(' NWPCL NWPTR  ',10I6)
*
*
*  conversion to IEEE FP
*
        DO 2003 J = 28,34
         CALL CNVIBM3E(IEVENT(II+J))
 2003   CONTINUE
*
        WRITE(6,1089) (REVENT(II+J),J=28,31)
 1089   FORMAT(' ACOLAN ETOT/ZM/ZP  ',4(F12.6))
*
        WRITE(6,1090) (REVENT(II+J),J=32,34)
 1090   FORMAT(' ENTOT/M/P  ',4(F12.6))
*
      ELSEIF(NRTAGG.EQ.1) THEN
*
        DO 200 J = 1,LETAGG
         CALL TWOIN1(IEVENT(II+J),IHALF(J*2-1),IHALF(J*2))
 200    CONTINUE
*
        L2TAGG = 2*LETAGG
*
        LL = L2TAGG/10
        LLREST = L2TAGG - LL*10        
*
        KK = -10
        IF(LL.GT.0) THEN
          DO 300 JJ = 1,LL 
           KK = KK + 10 
           WRITE(6,3535) (IHALF(KK+J),J=1,10)
 3535      FORMAT(' cluster ind. ',10I6)
 300      CONTINUE
        ENDIF
*
        IF(LLREST.GT.0) THEN
          KK = KK + 10
          WRITE(6,3535) (IHALF(KK+J),J=1,LLREST)
        ENDIF
*
      ELSEIF(NRTAGG.EQ.2) THEN
*
        DO 400 J = 1,4
         CALL TWOIN1(IEVENT(II+J),IHALF(J*2-1),IHALF(J*2))
 400    CONTINUE
*  
        WRITE(6,1011) (IHALF(J),J=1,8)
 1011   FORMAT(' ICLUS JPART IPASS ITRA ICOLIN 3xdumm  ',8I6)
*       
*
*  conversion to IEEE FP
*
        DO 2005 J = 5,13
         CALL CNVIBM3E(IEVENT(II+J))
 2005 CONTINUE
*
        WRITE(6,1079) (REVENT(II+J),J=5,6)
 1079   FORMAT(' Cluster_Energy, Error  ',2(F12.6))
*
        WRITE(6,1078) (REVENT(II+J),J=7,10)
 1078   FORMAT(' Cluster XY, errors  ',4F12.6)
*
        WRITE(6,1077) (REVENT(II+J),J=11,13)
 1077   FORMAT(' Cluster dir.cos  ',3F12.6)
*
      ELSEIF(NRTAGG.GT.2) THEN
*
        WRITE(6,3355) NRTAGG
 3355   FORMAT(' PRITAGG WARNING!!!  bank nr. ',I4,'  NOT FORESEEN!')
*
      ENDIF
*
*-----------------------
 9999 CONTINUE
      RETURN
      END
