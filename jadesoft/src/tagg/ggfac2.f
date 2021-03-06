C   13/09/79 508301754  MEMBER NAME  GGFAC2   (S)           FORTRAN
      SUBROUTINE GGFAC2
C---  COPY CALIBRATION CONSTANTS OF LEAD-GLASS INTO COMMON /CALICO/
C---  RUNS 1057 - 1397
C
C     H.WRIEDT    13.09.79     19:00
C     LAST MODIFICATION    13.09.79   19:00
C
      COMMON /CFACT2/ FACT2(191)
      COMMON /CALICO/ FAKTOR(191)
C
        DO 1 I = 1,191
    1   FAKTOR(I) = FACT2(I)
      WRITE(6,600)
  600 FORMAT(/' *** SECOND SET OF LEAD-GLASS CALIBRATION CONSTANTS',
     &        ' INCLUDED ***')
      RETURN
      END
      BLOCK DATA
C
      COMMON /CFACT2/ FACT2
C     THIS DATA APPLICABLE FROM RUN 1057 TO RUN 1397
C     THE GAIN ASSUMES ENERGY(MEV)=CHANNEL*5.0
      REAL*4 FACT2(191)/191*1.1/
C BLOCK DATA GGBLCK ADDED BY A.J.FINCH   30/8/85
C
C---- BLOCK DATA FOR THE FORWARD SPECTROMETER GEOMETRICAL CONSTANTS
C
C***  ONLY FIRST GUESS, HAS TO BE DETERMINED MORE ACCURATELY!
C
      COMMON /CGGDMS/ X0,ZECAP(4),ZECAPX(4),THECPX(2),
     1                ECXLST(46), ECYLST(46),
     2                XANL(8),XANU(8),YANL(8),YANU(8),
     3                XBNL(8),XBNU(8),YBNL(8),YBNU(8)
C
      COMMON/ CGGVRN/ NVRSN(20)
      DATA NVRSN/4*0, 178120111, 15*0/
C
C---- THE UNIT IS EITHER MM OR X0.
C
      DATA X0/22.39/
C
C---  PB-GLASS DETECTORS
C---- END CAP WALL POSITIONS
      DATA ZECAP/ -4850.0, -5250.0, 4850.0, 5250.0/
      DATA ZECAPX/ -216.615, -234.480, 216.615, 234.480/
      DATA THECPX/ 400.0, 17.87/
C
C---- ECXLST,ECYLST   X AND Y POSITIONS OF THE ENDCAP COUNTERS
C----                 COORDINATES OF THE CENTRES OF THE BLOCKS
      DATA ECXLST/3*432.,2*371.,3*351.,2*371.,3*290.,3*270.,3*290.,
     1          4*209.,3*189.,4*209.,8*126.,8*45./
      DATA ECYLST/-81.,0.,81.,-243.,-162.,-81.,0.,81.,162.,243.,
     1          -324.,-243.,-162.,-81.,0.,81.,162.,243.,324.,
     2          -405.,-324.,-243.,-162.,-81.,0.,81.,162.,243.,324.,405.,
     3          -405.,-324.,-243.,-162.,162.,243.,324.,405.,
     4          -434.,-353.,-271.,-190.,190.,271.,353.,434./
C
C---  LUMONITOR COUNTERS
C---  FRAME B (-Z-DIRECTION): INDEX 1 TO 4;
C---  FRAME A (+Z-DIRECTION): INDEX 5 TO 8
C     XANL: LOWER X-BOUNDARY OF A-COUNTERS
C     XANU: UPPER X-BOUNDARY OF A-COUNTERS
C     YANL: LOWER Y-BOUNDARY OF A-COUNTERS
C     YANU: UPPER Y-BOUNDARY OF A-COUNTERS
C---  ALL THE VALUES IN MM, IN A COORDINATE-SYSTEM ROTATED BY 45 DEGREES
      DATA XANL/-75.,192.58,-75.,-262.58,-75.,-262.68,-75.,192.68/
      DATA XANU/75.,262.58,75.,-192.58,75.,-192.68,75.,262.68/
      DATA YANL/-262.64,-75.,192.64,-75.,192.75,-75.,-262.75,-75./
      DATA YANU/-192.64,75.,262.64,75.,262.75,75.,-192.75,75./
C---  SAME FOR B-COUNTERS
      DATA XBNL/-115.,152.58,-115.,-302.58,-115.,-302.68,-115.,152.68/
      DATA XBNU/115.,302.58,115.,-152.58,115.,-152.68,115.,302.68/
      DATA YBNL/-302.64,-115.,152.64,-115.,152.75,-115.,-302.75,-115./
      DATA YBNU/-152.64,115.,302.64,115.,302.75,115.,-152.75,115./
C
C---- GG ANALYSIS PARAMETERS
      COMMON /CGGPRM/ ITHADC,MAXCLS,CLBCNS
C
      DATA ITHADC/2/, MAXCLS/51/
      DATA CLBCNS/0.005/
C
C---  CALIBRATION FACTORS FOR PB-GLASS BLOCKS (PRELIMINARY)
C
      END
