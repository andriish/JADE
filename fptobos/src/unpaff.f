C   30/03/90 502241117  MEMBER NAME  UNPAFF   (ONLINE2.) M  FVS
       SUBROUTINE UNPAFF(IBUF,KB,N2,L2OLD,
     $             INEWRC,INEWEV,III,IPBANK)
*
*  LAST CHANGE  19.10.94     J. Olsson   (Formats from Input)
*  (From UNPAFF9 on ONLINE.S, but without COMLGB macro)
*  Note: in this version KEYFPK is not transferred (COMLGB,CBANK)
*    Commented version  12.12.05  J.O.
*    Adjusted version, to be used in unpacking JADxxx Exabyte files
*
*
*JO121205  The only difference between this and the online4.s versions
*          is a further comment on the absence of KEYFPK and macro COMLGB
*          KEYFPK is a variable in /COMINT/, is not used in UNPAFF, but
*          is uncoded from the data word 9 in IBUF (FPACK classification) 
*
*    The F-formatted block is kept in array IBUF
*    The unpacked event is returned in array IEVENT
*    The BOS pointers are returned in array KB. There are N2 -1 banks
*    On return, the variables INEWRC and INEWEV reveal if event is
*       complete or if a new input record is needed
*
*   For every bank, the Subroutine  FORMST is called, to store the for-
*   mat in COMMON /CBANK/
*
*     Note!! No conversions are made!!!
*
*    The F-formatted Block has the following content:
*
*  Word  1:   Length of physical record, i.e. Block length 5850 (6930)
*  Word  2:   Length of used words, i.e. from 2 to 5850 (6930)
*
*        Logical Record: Segment header
*  Word  3:  Byte swapping pattern  1+256*(2+256*(3+256*4))=67305985
*  Word  4:  Format/Origin code  16*(   1,  2,  3,,) +   0,  1,   2,  3
*                                   (IEEE,IBM,VAX,,)  UNKN BOS LOOK B-L
*  Word  5:  Record Number 100*(Phys.Rec. Nr.(1-...))+int.position(0-99)
*  Word  6:  Name 1    'RUNE' or 'RUNS' (Run Event, Run End, Run Start)
*  Word  7:  Name 2    'VENT', 'TART' or 'ND  '
*  Word  8:  Number 1      Normally NRUN  (from HEAD bank)
*  Word  9:  Number 2      Normally NEVE  (from HEAD bank)
*  Word 10:  Cycle nr. Nr of Phys.rec. for this logical rec., start at 0
*  Word 11:  Data word for classification  (Integer) (KEYFPK)
*  Word 12:  Segment code:  0,1,2,3  (Complete, first, middle, last)
*  Word 13:  Nr of words following for this segment in this phys.record
*
*        Logical Record: Data header 1
*  Word 14:  Nr. of words, including Format words and including itself
*  Word 15:  Name 1   Normally the BOS bank name
*  Word 16:  Name 2
*  Word 17:  Number of bank
*  Word 18:  NCOL    Here always set to 1
*  Word 19:  NROW    Here always set to nr of data words in bank
*  Word 20:  Segment code  0,1,2,3  (Complete, first, middle, last)
*  Word 21:  Nr of data words in previous subsegments
*  Word 22:  Nr of data words in this subsegment
*  Word 23:  Format code   Word(14) - 9
*  .....
*                        Data
*  Word mm:  Data Word 1
*  .....
*  Word nn:  Data Word nn-mm + 1
*
*        Logical Record: Data header 2
*  Word nn+1:  Nr. of words, including Format words and including itself
*
*#include "cevent.for"
C********** MACRO CEVENT **************
C   Event Buffer
C   Changed 31.10.91 to 2 MByte (524290) with some margin.
C
       COMMON /CEVENT/IEVENT(525000)
*----------------------------- 
*
      COMMON/CUNPAF/ KEYWD1,KEYWD2,KRUNNR,KRUNEV,IORIGF
       DIMENSION KB(2000)
       DIMENSION IBUF(8200)
      COMMON/CBUGGY/ IPZ
       CHARACTER*(*) DATE*8, TIME*8
       DATA ICALLL/0/
       SAVE
*
*   Local Variables and their meaning:
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
*  ---------------------  CODE  --------------------
*
*                                          *****************************
*                                          *  Start of PHYS. RECORD    *
*                                          *****************************
      IF(IPZ.GT.0) WRITE(6,69) INEWRC,INEWEV,IBUF(2),III,N2
69    FORMAT(' UF 69: INEWRC INEWEV IBUF2 III N2 ',5I8)
*
      IF(INEWRC.EQ.0) THEN
        INEWRC = 1
        III = 2
      ENDIF
*                                          *****************************
*                                          *  Check that pieces fit    *
*                                          *****************************
      IF(INEWEV.NE.0) THEN
        IEUNPF = 0
        IF(IBUF(III+6).NE.KRUNNR.OR.IBUF(III+7).NE.KRUNEV) THEN
          WRITE(6,82) KRUNNR,KRUNEV,IBUF(III+6),IBUF(III+7)
82        FORMAT(' UNPAFF: Expect Run&Ev ',2I6,'  encountered ',2I6)
          IEUNPF = 1
        ENDIF
        IF(L2OLD.NE.0.AND.NAMEOL.NE.IBUF(III+13)) THEN
          WRITE(6,83) KRUNNR,KRUNEV,NAMEOL
          WRITE(6,84) IBUF(III+6),IBUF(III+7),IBUF(III+13)
83        FORMAT(' UNPAFF:  Run&Ev ',2I6,'  expect cont.bank ',Z8)
84        FORMAT(' UNPAFF:  Run&Ev ',2I6,'  encountered, with bank ',Z8)
          IEUNPF = 2
        ENDIF
      ENDIF
*                                          *****************************
*                                          *  LOG.RECORD  Segment Head *
*                                          *****************************
20    CONTINUE
      IF(INEWEV.EQ.0) THEN
        INEWEV = 1
        IORIGF = IBUF(III+2)
        KEYWD1 = IBUF(III+4)
        KEYWD2 = IBUF(III+5)
        KRUNNR = IBUF(III+6)
        KRUNEV = IBUF(III+7)
        KEYFPK = IBUF(III+9)
        N2 = 1
        KB(1) = 1
        IPBANK = 1
        IPHEAR = 0
        IPZRCT = 0
        IPTLV1 = 0
        IEVENT(1) = 0
      ENDIF
*
      ISEGME = IBUF(III+10)
*
      INRWD = IBUF(III+11)
      KNRWD = 0
*
      III = III + 11
*                                          *****************************
*                                          * LOG.RECORD Data bank Head *
*                                          *****************************
30    CONTINUE
      IF(IPZ.GT.0) WRITE(6,70) INEWRC,INEWEV,N2,L2OLD,III,KRUNEV
70    FORMAT(' UF 70: INEWRC INEWEV N2 L2OLD III KEV ',5I6,I10)
      IF(IPZ.GT.0) WRITE(6,71) INRWD,KNRWD,ISEGME,ISEGMB,NAMEOL
71    FORMAT(' UF 71: INRWD KNRWD ISEGME ISEGMB',4I6,
     $       ' NAMEOL ',Z8)
*                               Next bank
*
      IF(III.GE.IBUF(2)) THEN
        INEWRC = 0
        IF(ISEGMB.EQ.1.OR.ISEGMB.EQ.2) THEN
*             Record complete, Bank not complete, Event not complete
          L2OLD = N2
        ELSE
          IF(ISEGME.EQ.0.OR.ISEGME.EQ.3) THEN
*             Record complete, Bank complete, Event  complete
            INEWEV = 0
          ENDIF
*             Record complete, Bank complete, Event not complete
          L2OLD = 0
        ENDIF
        IF(IPZ.GT.0) THEN
          WRITE(6,3451) III,L2OLD,N2,INEWEV,ISEGME,NAMEOL
 3451     FORMAT(' UF 3451 III L2OLD N2 INEWEV ISEGME ',5I8,
     $           ' NAMEOL ',Z8)
        ENDIF
        GO TO 9999
      ENDIF
*
        IF(IPZ.GT.0) THEN
          WRITE(6,3452) KNRWD,INRWD,NAMEOL
 3452     FORMAT(' UF 3452  KNRWD INRWD ',2I8,'  NAMEOL ',Z8)
        ENDIF
      IF(KNRWD.EQ.INRWD) THEN
*                         Bank is complete, Event is complete
        INEWEV = 0
        GO TO 9999
      ENDIF
*
      IPBANK = KB(N2)
      IF(IPZ.GT.0) WRITE(6,72) IPBANK,L2OLD,N2
72    FORMAT(' UF 72: IPBANK L2OLD N2   ',5I6)
      IF(L2OLD.EQ.0) THEN
        N2 = N2 + 1
        IEVENT(IPBANK+1) = IBUF(III+2)
        NAMEOL = IBUF(III+2)
        IF(IPZ.GT.0) WRITE(6,7221) N2,NAMEOL
 7221   FORMAT(' UF  7221   N2 NAMEOL ',I4,2X,Z8)
        IEVENT(IPBANK+2) = IBUF(III+4)
        IEVENT(IPBANK+3) = 0
        IEVENT(IPBANK+4) = MAX(IBUF(III+5),IBUF(III+6))
        IPBANK = IPBANK + 4
*       NAMENN = NAMEOL
*JO131205    H1 special
*        IF(IORIGF.LT.32) CALL TRA0E0(NAMENN,4)
        IEVENT(1) = IEVENT(1) + 4
*        CALL FORMST(IBUF(III+1),IERFRM)
      ENDIF
      L2OLD = 0
      ISEGMB = IBUF(III+7)
*
      IPHLEN = III + 1
      IPBLEN = III + 9
*
      III = III + IBUF(IPHLEN)
      KNRWD = KNRWD + IBUF(IPHLEN)
*                                          *****************************
*                                          *   Data words of this bank *
*                                          *****************************
      LBADAT = IBUF(IPBLEN)
      IEVENT(1) = IEVENT(1) + LBADAT
      KNRWD = KNRWD + LBADAT
      IF(IPZ.GT.0) WRITE(6,73) LBADAT,KNRWD,ISEGMB,III,N2,NAMEOL
73    FORMAT(' UF 73: LBDT KNRWD ISGMB III N2 ',5I8,' NAMEOL ',Z8)
****
      III4 = III*4
      ISTQT4 = IPBANK*4
      LBDAT4 = LBADAT*4
*
      CALL MVCL(IEVENT,ISTQT4,IBUF,III4,LBDAT4)
*                   TO        FROM      BYTES
*
      III = III + LBADAT 
      IPBANK = IPBANK + LBADAT
      KB(N2) = IPBANK
      IF(IPZ.GT.0) WRITE(6,74) IPBANK,III,N2,NAMEOL
74    FORMAT(' UF 74: IPBANK III N2 ',3I6,'  NAMEOL ',Z8)
      GO TO 30
*
***********************
9999  CONTINUE
      IF(IPZ.GT.0) WRITE(6,75) IPBANK,III,IEVENT(1),N2,INEWRC,INEWEV
75    FORMAT(' UF 75: IPBANK III EV1 N2 INRC INEV ',6I6)
      IF(IPZ.GT.0) WRITE(6,7521) NAMEOL
 7521 FORMAT(' UF 75: NAMEOL ',Z8)
      RETURN
      END
