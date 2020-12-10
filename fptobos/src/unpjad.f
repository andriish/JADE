C   26/06/92 512091935  MEMBER NAME  CHCKEV7  (ONLINE.S) M  FVS
      SUBROUTINE UNPJAD(IUNPFL,IBYTSW,MAXEVN,MAXEVP,
     $                  IPRFLG,MAXREC)
*
*     Unpacking of JADE Exabyte files
*     Subroutine version of the main program  UNPJAD1
*
*     Return flag  IUNPFL     0   New event in /CEVENT/
*                             1   End of input reached
*                                 (either by EoF, or by MAXEVN
*
*     IBYTSW:  Byte swapping is performed, if non-zero
*     MAXEVN:  Nr. of events to read
*     MAXEVP:  Nr. of events to print
*     IPRFLG:  bit steering of print    1: hexdump  
*                                       2: banklist
*                                       4: detailed print of banks
*=================================================================
*
*   VERSION with DIRECT ACCESS    13.12.2005   J.O.
*   Last Update   12.06.2008   J.O.
*
      CHARACTER*(*) DATE*8, TIME*8,CHARST*8
*
*#include "cevent.for"
C********** MACRO CEVENT **************
C   Event Buffer
C   The size corresponds to 2 MByte event size, maximum
C
       COMMON /CEVENT/IEVENT(525000)
*
*#include "cbank.for"
C********** MACRO CBANK    Modified for JADE data  ***********
C  Bank handling variables
C
C    N2:       Nr of Bank Names  (Max 500)
C    KB:       Bank pointers
C              Pointers for the position of Banks in the array IEVENT
C              Pointer refers to last word before start of bankheader
C    KNOWNB:   Nr of known banks in array NAMEBA
C    NAMEBA:   Holds the bank names in ASCII character representation
C                integers, e.g. hex.48454144  is HEAD
C    NBFREQ:   Frequency of the bank names in the events
C    KFORM:    FORMAT words of the bank
C    NFORM:    Nr. of FORMAT words for the bank
C    NBLMIN:   Minimum Length of the banks in the events
C    NBLMAX:   Maximum Length of the banks in the events
C    NBLAVE:   Average Length of the banks in the events
C    NELMIN:   Minimum Length of the events
C    NELMAX:   Maximum Length of the events
C    NELAVE:   Average Length of the events
C
       COMMON /CBANK/N2,KB(12000),KNOWNB,NAMEBA(500),
     $               NBFREQ(500),KFORM(8,500),NFORM(500),
     $               NBLMIN(500),NBLMAX(500),NBLAVE(500),
     $               NELMIN,NELMAX,NELAVE
C**************************************
      COMMON/CUNPAF/ KEYWD1,KEYWD2,KRUNNR,KRUNEV
      COMMON/CBUGGY/ IPZ
*      DIMENSION IBUF(6930)
      DIMENSION IBUF(7000)
      DIMENSION NRUNSD(8)
      CHARACTER*(*) DSNAME*52,CBNAME*4,CHASCI*4
**************************
      LOGICAL ISTART 
      DATA ISTART /.TRUE./
      SAVE
*
* ---------------  CODE  -----------------------------
*
*
*  Init section
*
      IF(ISTART) THEN
        WRITE(6,1000)
1000  FORMAT(' ******************************************************')
      WRITE(6,1001)
1001  FORMAT(' *              UNPJAD   Version 16 from 12.06.08     *')
        WRITE(6,1002)
 1002 FORMAT(' *                    -- DIRECT  ACCESS --            *')
        WRITE(6,1000)
*
        IEV = 0
        INEWRC = 0
        INEWEV = 0
        L2OLD = 0
        IRECB = 0
        ISTART = .FALSE.
      ENDIF
*                  Input data set
*                  If new record not needed, go to next event
      IF(INEWRC.NE.0) GO TO 15
*
10    CONTINUE
      LLLUN = 1
*
      IRECB = IRECB + 1
      IF(IPZ.GT.0) WRITE(6,1061) IRECB
 1061 FORMAT(' UNPJAD   reading record ',I6)
*
      READ  (UNIT=1,
     $       ERR=888,
     $       REC = IRECB,  
     $       IOSTAT=IOS) (IBUF(I),I=1,6930)
      LENBUF = 6930
*
*--   Perform byte swapping
*
      IF(IBYTSW.EQ.1) THEN
        DO 150 I = 1,LENBUF
         CALL BYTSWP(IBUF(I),1)
 150    CONTINUE
      ENDIF
*
      IF(IAND(IPRFLG,1).NE.0) THEN
        WRITE(6,2269) IRECB,IBUF(1),IBUF(1)
 2269   FORMAT(' UNPJAD:  Rec. nr. ',I6,' with length ',I6,' ',Z8)
      ENDIF
*
      IF(IAND(IPRFLG,1).NE.0.AND.IRECB.LE.MAXREC) THEN
        WRITE(6,2271) IRECB,IBUF(1),IBUF(1),NBYTE
 2271 FORMAT(' Hexadecimal dump of record nr ',I6,
     $       ' with length ',I10,' (hex) ',Z8,' NBYTE ',I10)
*                         dump the data in hex format
*                         (code from  sel96.s(readfp.e)
        LL = 0
        NBYTE = 4 * LENBUF
        LKL = NBYTE/4 
        IK = -9
 200    CONTINUE
        IK = IK+ 10
        IF(IK+9.GT.LKL) GO TO 210
        JK = IK + 9
        WRITE(6,1110) IK,JK,(IBUF(IK+LL),LL=0,9)
*1110    FORMAT(' ',I4,'-',I4,2X,10(1X,I10))
1110    FORMAT(' ',I4,'-',I4,10(1X,Z8))
        GO TO 200
*
210     CONTINUE
      ENDIF
*
*  unpack the events
*
*       INEWRC:  Flag for New Physical Record   0:new    1:old
*       INEWEV:  Flag for New Logical Record (Event) 0:new  1:old
*
*JO121205  i.e.:  INEWEV = 0  means that the event is complete and
*                             can be analysed
*                 INEWEV = 1  means that the event is incomplete and
*                             needs more data, from new record
*                 INEWRC = 0  means that the record has been used up
*                             and new data should be read in
*                 INEWRC = 1  means that the present record still has
*                             data to be used, in a new event
*                 The subr. returns only if
*                    the event is ready:   INEWEV = 0, INEWRC = 0 or 1
*                    the event is not ready:  INEWEV = 1. In this case
*                                             INEWRC = 0 must be!
*                    Thus, INEWEV=INEWR=1 is an impossible combination
* 
 15   CONTINUE
*
      IF(IPZ.GT.0) WRITE(6,4515)
 4515 FORMAT(' UNPJAD   calling UNPAFF ...')
*===========
      CALL UNPAFF(IBUF,KB,N2,L2OLD,INEWRC,INEWEV,III,IPBANK)
*===========
      IF(IPZ.GT.0) WRITE(6,4516) INEWRC,INEWEV,III,N2,L2OLD,IPBANK
 4516 FORMAT(' UNPJAD:  Return from UNPAFF ',/,
     $ '   INEWRC/EV III N2 L2OLD IPBANK ',6I8)
*
      IF(INEWEV.NE.0.AND.INEWRC.EQ.0) GO TO 10
*                              Ready event ?
      IF(INEWEV.EQ.0) THEN
        IEV = IEV + 1
*
        IF(IPZ.GT.0) WRITE(6,4521) IEV
 4521   FORMAT(' UNPJAD 4521   IEV ',I7)
*
        IF(IEV.GT.MAXEVN) THEN
          IUNPFL = 1
          GO TO 9999
        ENDIF
*           
*--------------------------  Debug print section
*
*
        IF(IAND(IPRFLG,2).NE.0.AND.IEV.LT.MAXEVP) THEN
*   Print a listing of banks in the event
*
          WRITE(6,4021) IEV,KRUNNR,KRUNEV
 4021     FORMAT(' ',/,' UNPJAD Event ',I7,
     $           '    JADE Run and event: ',2I6)
          WRITE(6,4022)
 4022     FORMAT(' =====================================',
     $           '===================')
*
          LB = 0
 2020     LB = LB + 1
          IF(LB.GE.N2) GO TO 2030
          IPOS = KB(LB)
          CBNAME = CHASCI(IEVENT(IPOS+1))
          WRITE(6,2221) IPOS,CBNAME,
     $                  (IEVENT(IPOS+J),J=2,10)
          WRITE(6,2222) (IEVENT(IPOS+J),J=5,10)
2221  FORMAT(' ',I6,' BOS:',A8,1X,2I2,I6,' DTA ',5(Z8,1X),Z8)
2222  FORMAT(' ',30X,' DTA ',5(I6,1X),I6)
*
          GO TO 2020
2030      CONTINUE
        ENDIF
*
*   Conversion of Floating point, bank after bank...
*   Print in addition, with corresponding formats, if printflg set
*
        LB = 0
        KPR = 0
        IF(IAND(IPRFLG,4).NE.0) KPR = 1
*
 2040   LB = LB + 1
        IF(LB.GE.N2) GO TO 2050
        IPOS = KB(LB)
*
             CBNAME = CHASCI(IEVENT(IPOS+1))
             IF(CBNAME.EQ.'HEAD') THEN
                CALL PRIHEAD(IPOS+4)
             ENDIF
             IF(CBNAME.EQ.'ZVTX') THEN
                CALL PRIZVTX(IPOS+4)
             ENDIF
             IF(CBNAME.EQ.'PATR') THEN
                CALL PRIPATR(IPOS+4)
             ENDIF
             IF(CBNAME.EQ.'ZEHD') THEN
                CALL PRIZEHD(IPOS+4)
             ENDIF
             IF(CBNAME.EQ.'ZE4V') THEN
                CALL PRIZE4V(IPOS+4)
             ENDIF
             IF(CBNAME.EQ.'LGCL') THEN
                CALL PRILGCL(IPOS+4)
             ENDIF
             IF(CBNAME.EQ.'GVTX') THEN
                CALL PRIGVTX(IPOS+4)
             ENDIF
             IF(CBNAME.EQ.'MUR1') THEN
                CALL PRIMUR1(IPOS+4)
             ENDIF
             IF(CBNAME.EQ.'MUR2') THEN
                CALL PRIMUR2(IPOS+4)
             ENDIF
             IF(CBNAME.EQ.'TPEV') THEN
                CALL PRITPEV(IPOS+4)
             ENDIF
             IF(CBNAME.EQ.'TPTR') THEN
                CALL PRITPTR(IPOS+4)
             ENDIF
             IF(CBNAME.EQ.'TPVX') THEN
                CALL PRITPVX(IPOS+4)
             ENDIF
             IF(CBNAME.EQ.'TAGG') THEN
                CALL PRITAGG(IPOS+4)
             ENDIF
*
*   ................  other conversion/print routines
*
        GO TO 2040
*
 2050   CONTINUE
        IUNPFL = 0
        GO TO 9999
      ENDIF
*                    Not needed, never come here in subr. version
      IF(IPZ.GT.0) WRITE(6,4518) INEWRC
 4518 FORMAT(' UNPJAD 4518     INEWRC ',I8)
* 
      IF(INEWRC.EQ.0) GO TO 10
      GO TO 15
*
********************
888   CONTINUE
      WRITE(6,891) LLLUN,IOS
 891  FORMAT(' UNPJAD: READ ERROR FOR UNIT=',I2,'  IOSTAT=',I10)
      IUNPFL = -1
      GO TO 9999
*=======
999   CONTINUE
      WRITE(6,991) LLLUN,IRECB,IEV
 991  FORMAT(' UNPJAD: EOF on UNIT=',I2,'  Nr of records, evs ',2I6)
*
      IUNPFL = 1
      IF(INEWEV.NE.0) THEN
        WRITE(6,996)
 996    FORMAT(' UNPJAD: Last Event not complete !!')
      ELSE
        WRITE(6,997)
 997    FORMAT(' UNPJAD: Last Event complete and in order ')
      ENDIF
*
      GO TO 9999
*************
9999  CONTINUE
      RETURN
      END
