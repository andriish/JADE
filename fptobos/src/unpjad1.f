C   26/06/92 512091935  MEMBER NAME  CHCKEV7  (ONLINE.S) M  FVS
       PROGRAM MAIN
C      PROGRAM UNPJAD
*
*JO121205    Modifed version for unpacking of JADE Exabyte files
*=================================================================
*
* Read events without standard FPack, Write them out, without St. Fpack
*   VERSION with DIRECT ACCESS    13.12.2005   J.O.
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
*
       WRITE(6,1000)
1000   FORMAT(' ******************************************************')
       WRITE(6,1001)
1001   FORMAT(' *              UNPJAD   Version 07 from 16.12.05     *')
       WRITE(6,1002)
 1002  FORMAT(' *                    -- DIRECT  ACCESS --            *')
       WRITE(6,1003)
 1003  FORMAT(' *                   -- WITHOUT SWAPPING --           *')
       WRITE(6,1000)
*
*      CALL DAY(DATE,TIME)
*      WRITE(6,70) DATE,TIME
  70  FORMAT(' UNPJAD Job starting at DATE and TIME: ',A8,2x,A8)
*
*  Read a steering file, to be adapted later
*  Steering should  also include a write out flag, and in this case an
*     OPEN section for the output file.
*
      READ(5,3030) ILENG,MAXEVN,MAXEVP,IPRFLG,MAXREC,IPZ
3030  FORMAT(6I10)
      WRITE(6,3031) ILENG
3031  FORMAT(' UNPJAD Steer.flg ILENG ',
     $       '  Readflg, only IBU1 (0), all (1), 6930 (2): ',I2)
      WRITE(6,3032) MAXEVN
 3032 FORMAT(' UNPJAD Steer.flg MAXEVN ',
     $       '  Maximum number of events to read:  ',I8)
      WRITE(6,3035) MAXREC
 3035 FORMAT(' UNPJAD Steer.flg MAXREC ',
     $       '  Maximum number of records to print:',I8)
      WRITE(6,3033) MAXEVP
 3033 FORMAT(' UNPJAD Steer.flg MAXEVP ',
     $       '  Maximum number of events to print: ',I8)
      WRITE(6,3034) IPRFLG
 3034 FORMAT(' UNPJAD Steer.flg IPRFLG ',
     $       ' (0) no action (1) Hexdump, (2) banklist: ',I3)
      WRITE(6,3036) IPZ
 3036 FORMAT(' UNPJAD Steer.flg IPZ ',
     $       ' (0) no action (1) UNPAFF debug print: ',I3)
****************
      READ(5,3040) DSNAME
3040  FORMAT(A52) 
      WRITE(6,3041) DSNAME
 3041 FORMAT(' DSN: ',A52)
*
*  Open on LUNIT = 1
*
      LLLUN = 1
      OPEN(UNIT=LLLUN,
     $     ERR = 777,
     $     FILE=DSNAME,
     $     STATUS='OLD',
     $     ACCESS='DIRECT',
     $     RECL=27720,
     $     FORM='UNFORMATTED',
     $     IOSTAT = IOS)
*
*  Init section
*
      IKPEEV = 0
      IEV = 0
      IEVOU = 0
      IEVD = 0
      INEWRC = 0
      INEWEV = 0
      L2OLD = 0
      IRECB = 0
      NRECSP = 0
      KEYFPK = 0
      NRUNFS = -1
*                  Input data set
*                  Identification of the input file
*  OPEN it, enter the name in the steering instead
*      CALL DSNFDD(FTNAME,DSNAME,IERDSN)  outdated IBM mainframe routine
*      WRITE(6,66) DSNAME
66    FORMAT(' INPUT: ',A52)
      WRITE(6,67)
67    FORMAT('        ',52('='))
*
*   Not needed,  initialization of data for all known banks (in H1)
*     CALL INITBN
*   
*   Use here a flag to steer the reading of only first word (length)
*   useful for the very first study of files, and for debug of problem rec.s
*
10    CONTINUE
      LLLUN = 1
      NBYTE = 0
*
      IRECB = IRECB + 1
      IF(IPZ.GT.0) WRITE(6,1061) IRECB
 1061 FORMAT(' UNPJAD   reading record ',I6)
*
      IF(IAND(ILENG,1).NE.0) THEN
        READ  (UNIT=1,
     $         ERR=888,
*     $         END=999,
     $         REC=IRECB,
*    $         NUM=NBYTE,
     $         IOSTAT=IOS) IBUF(1),(IBUF(I),I=2,IBUF(1))
        LENBUF = IBUF(1)
      ELSEIF(IAND(ILENG,2).NE.0) THEN
        READ  (UNIT=1,
     $         ERR=888,
*     $         END=999,
     $         REC = IRECB,  
*     $         NUM=NBYTE,
     $         IOSTAT=IOS) (IBUF(I),I=1,6930)
*     $         IOSTAT=IOS) (IBUF(I),I=1,7000)
*        LENBUF = 7000 
        LENBUF = 6930
      ELSEIF(ILENG.EQ.0) THEN
        READ  (UNIT=1,
     $         ERR=888,
*     $         END=999,
     $         REC=IRECB,
*     $         NUM=NBYTE,
     $         IOSTAT=IOS) IBUF(1)
        LENBUF = 1
      ENDIF
*
*      IRECB = IRECB + 1
*  check if option  NUM=NBYTE is actually working?  No, not at MPI!
*
*--   Perform byte swapping
*
*      DO 150 I = 1,LENBUF
*       CALL BYTSWP(IBUF(I),1)
* 150  CONTINUE
*
      IF(ILENG.EQ.0.AND.IAND(IPRFLG,1).NE.0) THEN
        WRITE(6,2269) IRECB,IBUF(1),IBUF(1),NBYTE
 2269   FORMAT(' UNPJAD:  Rec. nr. ',I6,' with length ',I6,' ',Z8,
     $         ' NBYTE ',I10)
      ENDIF
*
      IF(IPRFLG.NE.0.AND.ILENG.NE.0) THEN
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
 200      CONTINUE
          IK = IK+ 10
          IF(IK+9.GT.LKL) GO TO 210
          JK = IK + 9
          WRITE(6,1110) IK,JK,(IBUF(IK+LL),LL=0,9)
*1110      FORMAT(' ',I4,'-',I4,2X,10(1X,I10))
1110      FORMAT(' ',I4,'-',I4,10(1X,Z8))
          GO TO 200
*
210       CONTINUE
        ENDIF
      ENDIF     
      IF(ILENG.EQ.0) GO TO 10
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
      IF(ILENG.LT.2) GO TO 10
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
        IF(IEV.GT.MAXEVN) GO TO 9999
*--------------------------  Debug print section
*                            only active if read whole buffer
        IF(IPRFLG.NE.0.AND.ILENG.NE.0.AND.IEV.LT.MAXEVP) THEN
*
          IF(IAND(IPRFLG,2).NE.0) THEN
*   Print a listing of banks in the event
*
            WRITE(6,4021) IEV,KRUNNR,KRUNEV
 4021       FORMAT(' ',/,' UNPJAD Event ',I7,
     $             '    JADE Run and event: ',2I6)
            WRITE(6,4022)
 4022       FORMAT(' =====================================',
     $             '===================')
*
            LB = 0
 2020       LB = LB + 1
            IF(LB.GE.N2) GO TO 2030
            IPOS = KB(LB)
            CBNAME = CHASCI(IEVENT(IPOS+1))
            WRITE(6,2221) IPOS,CBNAME,
     $                    (IEVENT(IPOS+J),J=2,10)
            WRITE(6,2222) (IEVENT(IPOS+J),J=5,10)
2221  FORMAT(' ',I6,' BOS:',A8,1X,2I2,I6,' DTA ',5(Z8,1X),Z8)
2222  FORMAT(' ',30X,' DTA ',5(I6,1X),I6)
*
            GO TO 2020
2030        CONTINUE
          ENDIF
*   Print in addition single banks,  with corresponding formats
          IF(IAND(IPRFLG,4).NE.0) THEN
            LB = 0
 2040       LB = LB + 1
            IF(LB.GE.N2) GO TO 2050
            IPOS = KB(LB)
*                                     HEAD  bank
*              The HEAD bank is 100 full words, but I*2 packed
*            IF(IEVENT(IPOS+1).EQ.IHEAD) THEN
*              WRITE(6,2241) (IEVENT(IPOS+J),J=1,4)
* 2241         FORMAT(' ',Z8,1X,2I2,I6)
*            ENDIF
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
             IF(CBNAME.EQ.'ZE4V') THEN
                CALL PRIZE4V(IPOS+4)
             ENDIF
*
*
*
             GO TO 2040
 2050        CONTINUE
           ENDIF
         ENDIF
*
*  New run?  This has to be modified for JADE HEAD bank format  
230      CONTINUE
*        IF(IEVENT(KB(1)+6).NE.NRUNFS) THEN
*          NRUNFS = IEVENT(KB(1)+6)
*          WRITE(6,2345) NRUNFS,IEV
2345      FORMAT(' New Run ',I6,'  current event ',I7)
*        ENDIF
*                  Event output
*   This has to be modified for JADE HEAD bank format
*        NRUNOL = IEVENT(KB(1)+6)
*        NEVE = IEVENT(KB(1)+7)
*        CALL PAKFF2(0,N2,KB)
*        IEVOU = IEVOU + 1
      ENDIF
*
      IF(IPZ.GT.0) WRITE(6,4518) INEWRC
 4518 FORMAT(' UNPJAD 4518     INEWRC ',I8)
*
      IF(INEWRC.EQ.0) GO TO 10
      GO TO 15
*
********************
 777  CONTINUE
      WRITE(6,791) LLLUN,IOS
 791  FORMAT(' UNPJAD: OPEN ERROR FOR UNIT=',I2,'  IOSTAT=',I10)
      GO TO 9999
*======== 
888   CONTINUE
      WRITE(6,891) LLLUN,IOS
 891  FORMAT(' UNPJAD: READ ERROR FOR UNIT=',I2,'  IOSTAT=',I10)
      GO TO 9999
*=======
999   CONTINUE
      WRITE(6,991) LLLUN,IRECB,IEV
 991  FORMAT(' UNPJAD: EOF on UNIT=',I2,'  Nr of records, evs ',2I6)
*
      IF(INEWEV.NE.0) THEN
        WRITE(6,996)
 996    FORMAT(' UNPJAD: Last Event not complete !!')
      ELSE
        WRITE(6,997)
 997    FORMAT(' UNPJAD: Last Event complete and in order ')
      ENDIF
*
*
*     CALL PAKFF2(1,N2,KB)
      GO TO 9999
*************
9999  CONTINUE
*      CALL DAY(DATE,TIME)
*      WRITE(6,170) DATE,TIME
 170  FORMAT(' UNPJAD Job ending at DATE and TIME: ',A8,2x,A8)
      STOP
      END
