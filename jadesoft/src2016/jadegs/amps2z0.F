      SUBROUTINE AMPS2Z( IP, NPJETC, Z, W, IFLAG )
      IMPLICIT INTEGER*2 (H)
      COMMON /CZSCAL/ IPVERS, ZALPDI, RESFAC, SECH(5)
#include "czsprm.for"
#include "cjdrch.for"
      COMMON / BCS / HW(1)
      REAL EXTRMZ / 1250. /, ZALDEF / 1400. /
      INTEGER IDTMAX / 600 /
      Z = 0.
      W = 1.
      IFLAG = 16
      AL = HW(IP+2)/8.
      AR = HW(IP+1)/8.
      IF(
     -  AL.GT.0. .AND. AR.GT.0.
     -)THEN
      IF(
     -  ZALPDI .EQ. ZALDEF
     -)THEN
          Z = ZALPDI*(AL-AR)/(AL+AR)
      IF(
     - NZSPRD .LE. 2
     -)THEN
            W = ( AZSSAV(NZSPRD) /
     +              ( AZSRS0(NZSPRD)+
     +                AZSRSA(NZSPRD)*SQRT(AL**2+AR**2)/(AL+AR)**2
     +              )
     +          )**2
      ELSE
            W = (AZSSAV(NZSPRD)/
     +             (AZSRS0(NZSPRD)+AZSRSA(NZSPRD)/(AL+AR)))**2
      ENDIF
      ELSE
          Z = .5*ZAL*(AL-AR)/(AL+AR)
      ENDIF
      IF(
     -  ABS( Z ) .LT. EXTRMZ
     -)THEN
      IF(
     - NZSPRD.GT.2
     -)THEN
            ISEC = 0
            NP = IP - 4
            IDT = HW(NP+3)-HW(IP+3)
            IF( NP.GT.NPJETC*2+100 .AND. HW(NP)/8 .EQ. HW(IP)/8.AND.
     +          IABS(IDT).LT.IDTMAX ) ISEC = ISEC + 1
            NP = IP + 4
            IDT = HW(NP+3)-HW(IP+3)
            IF( NP.LT.NPJETC*2+100+HW(NPJETC*2+99) .AND.
     +          HW(NP)/8 .EQ. HW(IP)/8.AND.
     +          IABS(IDT).LT.IDTMAX ) ISEC = ISEC + 1
            IF( ISEC .EQ. 0 ) IFLAG = 0
      ELSE
             IFLAG = 0
      ENDIF
      ENDIF
      ENDIF
      RETURN
      END
