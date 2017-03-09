      PROGRAM FPTOBOS
*
*    Read an FPACK file, unpack the BOS banks, feed them to /BCS/
*    Write the BOS-events out to a BOS file
*
*    The event is obtained in /CEVENT/   after a call to UNPJAD
*    
*   J.Olsson   16.12.2005
*
*       taken from (PCL246.mppmu.mpg.de)   ~skluth/jade/jtuple/bos.inc
*=========================== BOS.INC =======================
C -*- mode: fortran -*-
C BOS memory declarations:
      INTEGER MBOS
      PARAMETER( MBOS=99000 )
      INTEGER IW(MBOS)
      INTEGER*2 HW(MBOS*2)
      REAL RW(MBOS)
      COMMON /BCS/ IW
      EQUIVALENCE ( HW(1), IW(1), RW(1) )
C BOS functions:
      integer ibln
*===============================
C********** MACRO CEVENT *******
C   Event Buffer
C   The size corresponds to 2 MByte event size, maximum
C
      COMMON /CEVENT/IEVENT(525000)
*==================================
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
      CHARACTER*(*) DSNAME1*52,DSNAME2*52
      CHARACTER*(*) CBNAME*4,CHASCI*4,CHEBCD*4,CHAINT*4
      INTEGER NBLIST(100)
*
      DATA MAXRL /1558/,  MODE /2/, JUNIT /2/ 
*
* ----------  CODE  ----------------
*
       WRITE(6,1000)
1000   FORMAT(' ******************************************************')
       WRITE(6,1001)
1001   FORMAT(' *             FPTOBOS   Version 02 from 16.12.05     *')
       WRITE(6,1002)
 1002  FORMAT(' *                    AIX version, EBCDIC!            *')
       WRITE(6,1000)
*
*  Read steering textfile from unit 5
*
*
      READ(5,3030) IBYTSW,MAXEVN,MAXEVP,IPRFLG,MAXREC,IPZ,IWRITE
3030  FORMAT(7I10)
      WRITE(6,3031) IBYTSW
3031  FORMAT(' FPTOBOS Byte swapping.flg IBYTSW ',
     $       ' No swapping (0), swapping (1): ',I2)
      WRITE(6,3032) MAXEVN
 3032 FORMAT(' FPTOBOS Steer.flg MAXEVN ',
     $       '  Maximum number of events to read:  ',I8)
      WRITE(6,3035) MAXREC
 3035 FORMAT(' FPTOBOS Steer.flg MAXREC ',
     $       '  Maximum number of records to print:',I8)
      WRITE(6,3033) MAXEVP
 3033 FORMAT(' FPTOBOS Steer.flg MAXEVP ',
     $       '  Maximum number of events to print: ',I8)
      WRITE(6,3034) IPRFLG
 3034 FORMAT(' FPTOBOS Steer.flg IPRFLG ',
     $       ' (0) no action (1) Hexdump, (2) banklist: ',I3)
      WRITE(6,3036) IPZ
 3036 FORMAT(' FPTOBOS Steer.flg IPZ ',
     $       ' (0) no action (1) UNPAFF debug print: ',I3)
      WRITE(6,3037) IWRITE
 3037 FORMAT(' FPTOBOS Steer.flg IWRITE ',
     $       ' (0) no action (1) WRITE BOS outputfile: ',I3)
****************
      READ(5,3040) DSNAME1
3040  FORMAT(A52) 
      WRITE(6,3041) DSNAME1
 3041 FORMAT(' DSN: ',A52)
*
      READ(5,3040) DSNAME2
      WRITE(6,3041) DSNAME2
*
*  Open input file on LUNIT = 1
*
      LLLUN = 1
      OPEN(UNIT=LLLUN,
     $     ERR = 777,
     $     FILE=DSNAME1,
     $     STATUS='OLD',
     $     ACCESS='DIRECT',
     $     RECL=27720,
     $     FORM='UNFORMATTED',
     $     IOSTAT = IOS)
      IF(IWRITE.NE.0) THEN
        LLLUN = 2
        OPEN(UNIT=LLLUN,
     $       ERR = 777,
     $       FILE=DSNAME2,
     $       STATUS='NEW',
*     $     ACCESS='DIRECT',
*    $     RECL=1558,
     $     FORM='UNFORMATTED',
     $       IOSTAT = IOS)
      ENDIF 
*---------------------------
*  Init BOS
       CALL BINT(MBOS,MBOS,500,8000)
*
*  The parameters MAXRL  and MODE  are taken from JADEBD
*  They are defining the output mode of writing, and BOS rec.length
*  
       CALL BWRO(JUNIT,MAXRL,MODE)
*
       NEVIN = 0
       NEVOUT = 0
*------------------   Event loop
*
 3000  CONTINUE
       CALL UNPJAD(IUNPFL,IBYTSW,MAXEVN,MAXEVP,IPRFLG,MAXREC)
*  Last event reached ?
       IF(IUNPFL.EQ.1) GO TO 8000
       NEVIN = NEVIN + 1
*
*   Event lies now in /CEVENT/
*   IEVENT(1) contains very first word, namely 'HEAD'
*         i.e.  KB(1) = 0
*       
         LB = 0
 2020    LB = LB + 1
         IF(LB.GE.N2) GO TO 2030
         IPOS = KB(LB)
*
*  Conversion to EBCDIC code
*
*         WRITE(6,6791)
* 6791    FORMAT(' NO CNV AtoI  store as ASCII ')
         ICOPNM = IEVENT(IPOS+1)
         CBNAME=CHASCI(ICOPNM) 
         write(*,*)ICOPNM,'->',CBNAME
*         CALL CNVATOI(ICOPNM)
*         CBNAME = CHEBCD(ICOPNM)
*
         WRITE(6,2221) IPOS,CBNAME,
     $                 (IEVENT(IPOS+J),J=2,10)
         WRITE(6,2222) (IEVENT(IPOS+J),J=5,10)
2221  FORMAT(' ',I6,' BOS:',A8,1X,2I2,I6,' DTA ',5(Z8,1X),Z8)
2222  FORMAT(' ',30X,' DTA ',5(I6,1X),I6)
*
*   Feed the bank into /BCS/
*
         II = IPOS+4
         CALL CCRE(INDB,CBNAME,IEVENT(II-2),
     $             IEVENT(II),IRETB)  
*          CALL CCRE(INDB,IEVENT(II-3),IEVENT(II-2),
*     $             IEVENT(II),IRETB) 
*
*         WRITE(6,3671) CBNAME,INDB
* 3671    FORMAT(' FPTOBOS:  create bank ',A4,' to BCS, pointer ',I7)
*
         IF(IRETB.NE.0) THEN
           WRITE(6,2251) IRETB,CBNAME
 2251      FORMAT('  CCRE Error ',I4,' for name ',A4)
         ENDIF
         IF(INDB.LE.0) THEN
           WRITE(6,2252) CBNAME
 2252      FORMAT('  CCRE Error for name ',A4,'  no bank created ')
           GO TO 2020
         ENDIF           
         CALL BSTR(INDB,IEVENT(II+1),IEVENT(II))
         CALL BSAW(1,CBNAME)
*         CALL BSAW(1,IEVENT(II-3))
*
         GO TO 2020
 2030    CONTINUE
*
*    All banks have been stored, write event to BOS output
*
         IF(NEVIN.LE.MAXEVP) THEN
*   Print the special list for checks
*          
           WRITE(6,5781) 
 5781      FORMAT(' ============>  BOS  PRINT the Current List ')
           CALL BSLT
*=============================================================
*                             BPRL, BPRM  prints nonsense !!
*           CALL BPRL(10)
*           CALL BPRM
*==============================================================
           CALL BMRT(NNBK,NBLIST(1),100)
           WRITE(6,5782) NNBK,(NBLIST(JJ),JJ=1,10)
 5782      FORMAT(' BMRT returns NNBK ',I10,/,
     $            ' NBLIST: ',10(1X,Z8))
           WRITE(6,5783) (CHASCI(NBLIST(JJ)),JJ=1,10)
 5783      FORMAT(' NBLIST: ',10(1X,A4))
*
           INDJHTL = IW(IBLN('JHTL'))
           WRITE(6,4792) INDJHTL
 4792      FORMAT('  IBLN of JHTL returns pointer ',I7)
           INDHEAD = IW(IBLN('HEAD'))
           WRITE(6,4793) INDHEAD
 4793      FORMAT('  IBLN of HEAD returns pointer ',I7)
           LLRUN = HW(2*INDHEAD+10)
           LLEVE = HW(2*INDHEAD+11)
           WRITE(6,4794) LLRUN,LLEVE
 4794      FORMAT(' LLRUN LLEVE ',2I10)
         ENDIF
*
         IF(IWRITE.NE.0) THEN
           CALL BSLW
           CALL BWRITE(JUNIT)
           NEVOUT = NEVOUT + 1
         ENDIF
*
         CALL BSLT
         CALL BDLG
*
         GO TO 3000
*
*=============================
*
 777  CONTINUE
      WRITE(6,791) LLLUN,IOS
 791  FORMAT(' FPTOBOS: OPEN ERROR, UNIT=',I2,'  IOSTAT=',I10)
      GO TO 9999
*--------
 8000 CONTINUE
*  End of Input file, no more events,  perform last action
       IF(IWRITE.EQ.0) THEN
         CALL BMLT(0,NDUM)
         CALL BWRITE(JUNIT)
       ENDIF        
*
       WRITE(6,4051) NEVIN,NEVOUT
 4051  FORMAT(' END OF Input, Nr. of events in/out ',2I7)
       CALL BSTA
*
 9999  CONTINUE
       STOP
       END










