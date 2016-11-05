C   23/11/84 707281246  MEMBER NAME  ZCDATA0  (ZS)          FORTRAN77
************************************************************************
      SUBROUTINE ZCDATA(NTPAT,NZHIT,NASS1,NASS2,IRET)
************************************************************************
*                                                                      *
*     ROUTINE TO DECODE Z-CHAMBER DATA                                 *
*                                                                      *
*     INPUT ARGUMENTS:           NONE                                  *
*     OUTPUT ARGUMENTS:          NTPAT = NUMBER OF PATR TRACKS         *
*                                NZHIT = NUMBER OF Z-CH HITS           *
*                                NASS1 = NO OF TRACKS WITH .GE.1       *
*                                           ASSOCIATED Z-CHAMBER HIT   *
*                                NASS2 = NO OF TRACKS WITH 2           *
*                                           ASSOCIATED Z-CHAMBER HITS  *
*                                IRET  = 0    EVERYTHING OK            *
*                                        1    NO PATR BANK             *
*                                        2    NO ZETC BANK             *
*                                        3    NO TRACKS                *
*                                        4    .GT.100 TRACKS           *
*                                        5    NO Z-CH HITS             *
*     COMMONBLOCKS FILLED:      /CZDATA/ Z-CHAMBER COORDINATES         *
*                               /CZTRK / TRACK ASSOCIATION DETAILS     *
*                                                                      *
*     S. CARTWRIGHT              23/11/84                              *
*                                                                      *
************************************************************************
*
******COMMONBLOCKS
      COMMON/CZDATA/JZDATA(64),IZDATA(3,8,64),RZDATA(3,8,64)
******              JZDATA(NW)        NUMBER OF HITS ON WIRE NW
*                   IZDATA(*,         ADC1,ADC2,TDC
*                            NH,      HIT NUMBER (1--8)
*                               NW)   WIRE NUMBER (1--64)
*                   RZDATA(*,         Z-DZ,Z+DZ,PHI
******                       NH,NW)   HIT NO, WIRE NO.
      COMMON/CZTRK /RZTRK(5,100),IZTRK(3,2,100)
******              RZTRK(*,NTR)      ZI1,PHI1,ZI2,PHI2,TANTH
*                                         FOR TRACK NUMBER NTR
*                   IZTRK(*,          IM(=1,-DZ,=2,+DZ),NH,NW
*                           IL,       LAYER(=1,INNER,=2,OUTER)
******                         NTR)   TRACKNO
      COMMON/CZASS /IZASS(8,64)
******
*                   IZASS(I,NW)       NTR ASS. WITH I'TH HIT ON WIRE NW
******
*%MACRO CBCS
*%MACRO CZGEO
*%MACRO CZMKON
C ----------------------------------------------- BOS COMMON
      COMMON/BCS/IW(1)
      REAL RW(1)
      INTEGER*2 HW(1)
      EQUIVALENCE(IW(1),RW(1),HW(1))
C ----------------------------------------------- END BOS DECLARATIONS
C ----------------------------------------------- Z-CHAMBER CONSTANTS
C ----------------------------------------------- Z-CHAMBER CONSTANTS 3
      COMMON /CZMKON/ ZCVDR, ZCXCH, ZCTZER, ZCAPED, ZXLI, ZXLO
       COMMON /CZGEO/ RZCHI,RZCHA,NZRPHI,NZZ,Z1ZCH,Z2ZCH,
     +              ZCHA, ZCHB, ZCHSS, ZCHDL, ZCHDLL,
     +              DLZZ, DLZPMI, DLZW1, DLZW2
******END COMMONBLOCKS
******ARRAYS AND DECLARATIONS
      REAL WIRLEN(2)
      INTEGER NCALL/0/,KRUN0/-1/
      PARAMETER(PI=3.1415927,TWOPI=6.2831853,PHICUT=0.349,DWIR=10.)
******END ARRAYS AND DECLARATIONS
******INITIALISATION
      IF(NCALL.EQ.0)                   THEN
         KZETC     = IBLN('ZETC')
         KPATR     = IBLN('PATR')
         KHEAD     = IBLN('HEAD')
         WIRLEN(1) = NZRPHI/2 * DLZW1 - 200.
         WIRLEN(2) = NZRPHI/2 * DLZW2 - 200.
      ENDIF
*
      CALL VZERO(JZDATA,  64)
      CALL VZERO(IZDATA,1536)
      CALL VZERO(RZDATA,1536)
      CALL VZERO(RZTRK , 400)
      CALL VZERO(IZTRK , 600)
      CALL VZERO(IZASS , 512)
*
      IHEAD = IW(KHEAD)
      KRUN  = HW(2*IHEAD+10)
      IF(KRUN.NE.KRUN0)                THEN
         CALL ZCHCAL(KRUN,ZCXCH,ZCVDR)
         KRUN0 = KRUN
      ENDIF
*
      IRET  = 0
      NASS1 = 0
      NASS2 = 0
      IPATR = IW(KPATR)
      IZETC = IW(KZETC)
      IF(IZETC.LE.0)                   IRET = 2
      IF(IRET .NE.0)                   RETURN
*
      JZETC = 2*IZETC
      J     = JZETC+2
      NZHIT = IW(IZETC)/2
      IF(NZHIT.LE.0)                   IRET = 5
      IF(IRET .NE.0)                   RETURN
******
*     LOOP OVER Z-CHAMBER HITS.
*     UNPACK DATA AND CONVERT TO COORDINATES.
*     FILL JZDATA,IZDATA,RZDATA
******
      CALL ZCOOR(JZETC,NZHIT,WIRLEN,ZCXCH,ZCVDR)
******
*     LOOP OVER PATR TRACKS
*     THIS IS THE ASSOCIATION SECTION OF THE ROUTINE
******
      IF(IPATR.LE.0)                   THEN
         IRET = 1
         RETURN
      ENDIF
*
      IPATS = IW(IPATR+1)
      NTPAT = IW(IPATR+2)
      NWPAT = IW(IPATR+3)
      IPATT = IPATR + IPATS
      IF(NTPAT.LE.  0)                 IRET = 3
      IF(NTPAT.GT.100)                 IRET = 4
      IF(IRET .NE.0)                   RETURN
*
      CALL ZCASS(IPATT,NTPAT,NWPAT,NASS1,NASS2)
*
******END OF SUBROUTINE*************************************************
      END
