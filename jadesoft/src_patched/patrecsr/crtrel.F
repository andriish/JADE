      SUBROUTINE CRTREL(IPHT,IERRFL)
      IMPLICIT INTEGER*2 (H)
      LOGICAL TBIT
#include "cdata.for"
#include "ccycp.for"
#include "cworkpr.for"
#include "cworkeq.for"
      EQUIVALENCE
     ,           (ICELL ,IDWRK(1)),(NHIT  ,IDWRK(2)),(IRING ,IDWRK(3))
     ,         , (IERRCD,IDWRK(4)),(NTRKEL,IDWRK(5))
     ,         , (ITR   ,IDWRK(7)),(ITRNG ,IDWRK(8))
      DATA MKATR / Z'FF01'/
      INTEGER  MKLRHT(3) / Z'800', Z'0', Z'900'/
      IF(
     - HNTR.GE.200
     -)THEN
        IERRFL = -1
        RETURN
      ENDIF
      LBLR = 1
      IF(IPHT.LT.0) LBLR =-1
      IPHT = IABS(IPHT)
      IWIR = HDATA(IPHT)
      IWIR = ISHFTR(IWIR,3)
      ICLL = ISHFTR(IWIR,4)
      ILAY = LAND(IWIR,15)
      IPTR = HNTCEL(ICLL+2)
      IRNG = ICLL / 24 + 1
      IF(IRNG.GT.3) IRNG = 3
      IC1 =  1
      IC9 = 24
      IF(ICLL.GE.24) IC1 = 25
      IF(ICLL.GE.24) IC9 = 48
      IF(ICLL.GE.48) IC1 = 49
      IF(ICLL.GE.48) IC9 = 96
      IP1  = HNTCEL(IC1  )
      IP9  = HNTCEL(IC9+1) - 1
      JTRELM = 1
      IF(IP1.LE.IP9) JTRELM = NTREL(IP9) + 1
      IF(
     - JTRELM.GE.128
     -)THEN
        IERRFL = -1
        RETURN
      ENDIF
      NTRL = 1
      IF(
     - IPTR.GT.1
     -)THEN
        NTRL = NTREL(IPTR-1) + 1
        IRNG1 = (IPCL(IPTR-1)-1)/24 + 1
        IF(IRNG1.GT.3) IRNG1 = 3
        IF(IRNG1.NE.IRNG) NTRL = 1
      ENDIF
      IF(
     - IPTR.LE.IP9
     -)THEN
      DO 13000 IP=IPTR,IP9
          NTREL(IP) = NTREL(IP) + 1
13000 CONTINUE
13001 CONTINUE
        IPHT0 = (HPTSEC(ICLL+1)-HPTSEC(1))/2 + HPHL0
        IPHT9 = (HPTSEC(IC9 +1)-HPTSEC(1))/2 + HPHL0 - 1
      DO 13002 IP=IPHT0,IPHT9
          LBHIT = HWRK(IP  )
          NTR1 = ISHFTR(LBHIT,1)
          NTR1 = LAND(NTR1,127)
          IF(NTR1.GE.NTRL) NTR1 = NTR1 + 1
          LBHIT = LAND(MKATR,LBHIT)
          LBHIT = LOR(LBHIT,ISHFTL(NTR1,1))
          HWRK(IP  ) = LBHIT
13002 CONTINUE
13003 CONTINUE
      ENDIF
      IC0 = ICLL + 1
      DO 13004 IC=IC0,96
        HNTCEL(IC+1) = HNTCEL(IC+1) + 1
13004 CONTINUE
13005 CONTINUE
      HNTR = HNTR + 1
      IF(
     - IPTR.NE.HNTR
     -)THEN
        IP = HNTR - 1
16000 CONTINUE
          TRKAR(IP+1, 1) = TRKAR(IP, 1)
          TRKAR(IP+1, 2) = TRKAR(IP, 2)
          TRKAR(IP+1, 3) = TRKAR(IP, 3)
          TRKAR(IP+1, 4) = TRKAR(IP, 4)
          TRKAR(IP+1, 5) = TRKAR(IP, 5)
          TRKAR(IP+1, 6) = TRKAR(IP, 6)
          TRKAR(IP+1, 7) = TRKAR(IP, 7)
          TRKAR(IP+1, 8) = TRKAR(IP, 8)
          TRKAR(IP+1, 9) = TRKAR(IP, 9)
          TRKAR(IP+1,10) = TRKAR(IP,10)
          TRKAR(IP+1,11) = TRKAR(IP,11)
          HRES(IP+1) = HRES(IP)
          IP = IP - 1
      IF(.NOT.(
     - IP.LT.IPTR
     -))GOTO 16000
16001 CONTINUE
      IF(
     - NTR.GT.0
     -)THEN
      DO 13006 ITR=1,NTR
            NELM = HNREL(ITR)
      DO 13008 I=1,NELM
              IELM = HISTR(I,ITR)
              INCR = ISIGN(1,IELM)
              IF(IABS(IELM).GE.IPTR) HISTR(I,ITR) = IELM + INCR
13008 CONTINUE
13009 CONTINUE
13006 CONTINUE
13007 CONTINUE
      ENDIF
      ENDIF
      ITRKAR(IPTR, 1) = IC0
      ITRKAR(IPTR, 2) = 1
      ITRKAR(IPTR, 3) = ILAY
      ITRKAR(IPTR, 4) = 0
      ITRKAR(IPTR, 5) = 0
      ITRKAR(IPTR, 6) = ILAY
      ITRKAR(IPTR, 7) = 0
      ITRKAR(IPTR, 8) = 0
      ITRKAR(IPTR, 9) = 0
      ITRKAR(IPTR,10) = NTRL
      ITRKAR(IPTR,11) = 0
      HRES(IPTR) = 0
      IPLBHT = (IPHT - HPTSEC(1))/2 + HPHL0
      LBHIT = NTRL*2
      LBHIT = LOR(LBHIT,MKLRHT(LBLR+2))
      HWRK(IPLBHT+1) = HWRK(IPLBHT)
      HWRK(IPLBHT  ) = LBHIT
      IERRFL = 0
      IPHT = IPTR
      RETURN
      END
