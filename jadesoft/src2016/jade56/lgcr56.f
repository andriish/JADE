C   26/10/81 708291843  MEMBER NAME  LGCR56   (S)           FORTRAN
      SUBROUTINE LGCR56(T,DEPTH,ALPHA,BETA2,XU,JBC)
C
C     S.YAMADA     07-03-78  10:35
C     COPIED FROM (LGCRGN) IN LGSOURCE FOR JADE STANDARD M.C.
C     LAST MODIFICATION   26-10-81  22:00
C-----THIS ADDS SHOWER CONTRIBUTION INTO LG-DATA COMMON CLGHIT/.
C     NEW CALIBRATION CONS IS SET IN LGMCIN. NOW DATA IS IN MEV.
C
C   MODIFIED FROM LGCRGN TO ACCOUNT FOR DIFFERENCES BETWEEN SF5/SF6
C       23.1.1986   J.OLSSON
C
C    SF5 OR SF6 IS GIVEN BY VALUE OF IMATTR   (2 OR 3)
C
C---- INPUTS
C     T = TRACK SEGMENT LENGTH IN RAD.LENGTH
C     ALPHA = PROJ.COSINE TO THE COUNTER SURFACE NORMAL.
C     DEPTH = THE AVERAGE DEPTH OF THE SEGMENT
C     BETA = VELOCITY OF THE TRACK
C     XU(1-3) = THE POSITION (AND THE DIRECTION) OF THE SEGMENT
C        THE UNIT OF THE POSITION IS RAD.LENGTH
C     JBC=+-1 FOR END CAP,JBC=0 FOR BARREL PART.
C
      IMPLICIT INTEGER *2 (H)
C
      DIMENSION XU(3)
      DIMENSION RVN2(2),AN2(2),AN21R(2)
      COMMON /CMATTR/ IMATTR,IMTLIS(10),XCM(10)
      COMMON /CLGHIT/ RDARRY(2880)
      COMMON /CLGG56/ CONS(2),ECUTCR,ARED(2),IADCTH
C
C  SHOWER MC COMMON
C
      COMMON /CLGD56/ RADIUS(6),ZEND(2),ZWID,ZGAP,PHWID,
     2                ECXLST(24), ECYLST(24),ZECAP(4),ZPV(4),TPV,
     3                TH(4),THECP
C
      COMMON / COLSON/ IPRO
C
C     CONSTANTS FOR N=1.6725,    SF5 REFRACTIVE INDEX
C     CONSTANTS FOR N=1.805      SF6 REFRACTIVE INDEX
C
C RVN2 = 1/N**2
C AN2  = N**2
C AN21R IS NOT KNOWN, HOWEVER, IT MULTIPLIES (N**2 - 1/BETA**2)
C        THEREFORE IT SHOULD CONTAIN 1/N**2 AND THE VALUE USED HERE
C        IS BASED ON THIS ASSUMPTION        22.4.87, J.O.
C
      DATA RVN2/0.35738,0.30693/, AN2/2.7973,3.2580/
      DATA AN21R/0.5564,0.4777/
C                                 PHIDEV IS DELTAPHI FOR ONE BLOCK
      DATA PHIDEV/7.47998E-2/
      DATA ICAL56/0/
C
      ICAL56 = ICAL56 + 1
      IF(ICAL56.EQ.1.OR.IPRO.GT.1) WRITE(6,4220) IMATTR
4220  FORMAT('   LGCR56 CALLED, IMATTR = ',I3)
C
      IF(IMATTR.NE.2.AND.IMATTR.NE.3) GO TO 3333
C
      XJJ = XCM(IMATTR)*10.
      IF(ICAL56.EQ.1.OR.IPRO.GT.1) WRITE(6,4221) XJJ
4221  FORMAT('   LGCR56:    XJJ = ',E12.4)
C
C---- CHECK VELOCITY
      IF(IPRO.GT.1) WRITE(6,931) BETA2,RVN2(IMATTR-1)
931   FORMAT(' LGCR56: BETA2 RVN2 ',2E12.4)
      IF(BETA2.LE.RVN2(IMATTR-1)) RETURN
C
      RAT = AN21R(IMATTR-1)*(AN2(IMATTR-1)-1./BETA2)
      IF(IPRO.GT.1) WRITE(6,932) IMATTR,RAT,JBC
932   FORMAT(' LGCR56: IMATTR,RAT ',I4,E12.4,'  JBC ',I3)
      IF(RAT.LE.0.) RETURN
C
C---- FIND WHICH ADC TO INCREASE
C
      IF(JBC) 2000,1000,2000
C
C###############
C          BARREL
C###############
C
 1000 PHI = ATAN2(XU(2),XU(1))
      IF(PHI.LT.0.) PHI = PHI+6.283184
      IPHI = PHI/PHIDEV
      IF(IPHI.GE.84) IPHI = IPHI-84
      IF(IPRO.GT.1) WRITE(6,4223) (XU(I),I=1,3),ZEND(1),ZWID,ZGAP
4223  FORMAT('   LGCR56:    XU = ',3E12.4,' ZEND1 ZWID,GAP ',3E12.4)
      IZ =(XU(3)*XJJ-ZEND(1))/(ZWID+ZGAP)+1.0
C********************************************************
C     WRITE(6,6001) IPHI,IZ
C6001 FORMAT(' LGCR56: IPHI,IZ=',2I5)
C********************************************************
      IF(IPRO.GT.1) WRITE(6,933) IZ,PHI,IPHI
933   FORMAT(' LGCR56 IZ ',I10,'  PHI IPHI ',E12.4,I4)
      IF(IZ.LT.1.OR.IZ.GT.32) RETURN
      ICNT = IZ+32*IPHI
      JBC12 = 1
      GO TO 3000
C
C###############
C          ENDCAPS
C###############
C               INPUT TO LGNMEC IS RAD.LENGTH
C
 2000 CALL LGNMEC(XU(1),XU(2),ICNT)
C********************************************************
C     WRITE(6,6000) XU(1),XU(2),ICNT
C6000 FORMAT(' ICNT FOR (',F8.2,1H,,F8.2,')=',I4)
C********************************************************
      IF(IPRO.GT.1) WRITE(6,934) XU(1),XU(2),ICNT
934   FORMAT(' LGCR56 ENDCAP XU1-2 ICNT ',2E13.5,I5)
      IF(ICNT.LE.0) RETURN
      IF(JBC.GT.0) ICNT = ICNT+96
      ICNT = ICNT+2688
      IF(IPRO.GT.1) WRITE(6,935) ICNT
935   FORMAT(' LGCR56 ENDCAP SECOND ICNT ',I10)
      IF(ICNT.GT.2880) RETURN
      JBC12 = 2
C
C---- NO.OF PHOTO-ELECTRONS / RADIATION LENGTH
C
 3000 CONTINUE
      IF(IPRO.GT.1) WRITE(6,4224) DEPTH,ALPHA
4224  FORMAT('   LGCR56:    DEPTH ALPHA ',2E12.4)
      IF(IMATTR.EQ.2) CALL NPECER( JBC12, DEPTH, ALPHA, PE)
      IF(IMATTR.EQ.3) CALL NPECR6( DEPTH, ALPHA, PE)
      IM = IMATTR-1
C
C PE*T IS NR OF PHOTOELECTRONS, CONS IS ADJUSTABLE (MEV RESPONSE)
C ARED (VALUE 0.25) IS OF UNKNOWN ORIGIN
C ECUTCR IS A CORRECTION FACTOR FOR ECUT
C
      RDARRY(ICNT) = RDARRY(ICNT)+PE*RAT*CONS(IM)*ECUTCR*ARED(IM)*T
C
      IF(IPRO.GT.1.OR.IPRO.LT.0) WRITE(6,936) ICNT,IM,
     $ RDARRY(ICNT),PE,T,RAT,CONS(IM),ECUTCR,ARED(IM)
936   FORMAT(' LGCR56 ICNT IM RDARRY ETC ',2I4,7E12.4)
C
      RETURN
C
3333  WRITE(6,8610) IMATTR
8610  FORMAT(' ############     WRONG MATTER INDEX IN LGCR56:  ',I10)
      RETURN
      END
C
      BLOCK DATA
C    S.YAMADA  01-09-78  12:15
C---- LAST MODIFICATION  06-11-81  18:20    INCLUDE P.V.END CAPS.
C---- LAST MODIFICATION  23.1.86  J.OLSSON  VERSION NUMBER FOR SF5-6 MC
C
C---- BLOCK DATA FOR THE LG-COUNTER GEOMETRICAL CONSTANTS
C
C
C  SHOWER MC COMMON
C
      COMMON /CLGD56/ RADIUS(6),ZEND(2),ZWID,ZGAP,PHWID,
     2                ECXLST(24), ECYLST(24),ZECAP(4),ZPV(4),TPV,
     3                TH(4),THECP
C
      COMMON/ CLGVRN/ NVRSN(20)
      DATA NVRSN/4*0, 178090112, 15*0/
C
C---- VERSION NUMBER FOR THE SF5-6 SHOWER MC
C----   THIS NUMBER IS STORED IN WORD 16 OF THE HEAD BANK
C
      COMMON /CVER56/ IVER56
C IVER56=2:  E- AND E+ SEPARATELY TREATED IN IONIZATION ENERGY LOSS
      DATA IVER56 /2/
C
C---- RADIUS IN MM
C     RADIUS(1,2) = DISTANCE TO THE COIL SURFACES(INNER AND OUTER)
C     RADIUS(3,4) = DISTANCE TO THE LEAD GLASS SURFACES(TYPE A)
C     RADIUS(5,6) = DISTANCE TO THE LEAD GLASS SURFACES(TYPE A-PR)
C
      DATA RADIUS/ 900.0, 986.4, 1100.0, 1400.0, 1150.0, 1400.0/
C
C---- THICKNESSES OF COIL, |COIL-INNER LG|, LG, LG
      DATA TH/ 86.4, 115.6, 300.0, 250.0/
C
C---- THE BARREL ENDS
      DATA ZEND/ -1705.45, 1705.45/
C
C---- COUNTER SIZE IN MM (BARREL)
      DATA ZWID/ 103.2/,  PHWID/80.8/
C
C---- COUNTER GAP
      DATA ZGAP/3.5/
C
C---- END CAP WALL POSITIONS
      DATA ZECAP/ -1510.0, -1736.0, 1510.0, 1736.0/
      DATA THECP/ 226.0/
C
C---- ECXLST,ECYLST   X AND Y POSITIONS OF THE ENDCAP COUNTERS
      DATA ECXLST/400., 350., 210., 70., 540., 490., 350., 210.,
     1          70., 680., 630., 490., 350., 210., 70., 820.,
     2          770., 2*630., 2*490., 350., 210., 70./
      DATA ECYLST/70., 210., 350., 400., 70., 210., 350., 490.,
     1          540., 70., 210., 350., 490., 630., 680., 70.,
     2          210., 350., 2*490., 2*630., 770., 820./
C
C
C----- PRESSURE VESSELS
      DATA ZPV/ -1380.0,-1490.90, 1380.0, 1490.90/
      DATA TPV/ 26.15/
      END