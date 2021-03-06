C---------  MACRO CGEO2COM       GEOMETRY OF FORWARD DETECTOR   -------
C
C---  LEAD GLASS BLOCKS
C     FENDC:  WIDTH (AND HEIGHT) OF BLOCKS
C     XYHOL1: DISTANCE FROM BEAM CENTRE TO EDGE OF FIRST
C             HORIZONTAL BLOCK
C     XYHOL2: DISTANCE FROM BEAM CENTRE TO EDGE OF FIRST
C             VERTICAL BLOCK
C     BLDPFW: DEPTH OF BLOCKS
C     ZMINBL: DISTANCE FROM INTERACTION POINT TO FRONT SURFACE
C             OF LEAD GLASS BLOCKS (-Z-DIRECTION, PLUTO/CELLO)
C     ZPLUBL: DISTANCE FROM INTERACTION POINT TO FRONT SURFACE
C             OF LEAD GLASS BLOCKS (+Z-DIRECTION, MARK J)
C---  LUMONITORS
C     XSC:    LENGTH OF LONG EDGE (1: A-COUNTER, 2: B-COUNTER)
C     YSC:    LENGTH OF SHORT EDGE
C     RSC:    DISTANCE FROM BEAM CENTRE TO CENTRE OF INNERMOST LONG
C             EDGE ON A-COUNTER
C     ZMISC:  DISTANCE FROM INTERACTION POINT TO FRONT SURFACE OF
C             LUMONITOR (-Z-DIRECTION, TOWARDS PLUTO/CELLO)
C     ZPLSC:  DISTANCE FROM INTERACTION POINT TO FRONT SURFACE OF
C             LUMONITOR (+Z-DIRECTION, TOWARDS MARK J)
C     DZSC:   THICKNESS OF LUMONITORS
C
C---  DRIFT CHAMBERS
C
C  CHX(I,J): XPOSITION OF WIRE 0 IN CHAMBER I (1-3) IN PLANE J (1-4)
C  CHY AND CHZ ANALOGOUS.     WLEN IS SENSITIVE LENGTH OF WIRES
C  PITCH IS WIRE DISTANCE IN XY-PLANE, WZDIS DISTANCE IN Z BETWEEN
C  THE ODD AND EVEN WIRE PLANES
C  WIRE 0 IS CLOSEST TO THE BEAM LINE
C  PRESENT DATA SETTING OF WIRES BASED ON "EDUCATED GUESSES"
C
C     ALL VALUES IN MM      BLOCK DATA SETTING IN SUBR. JADISP
C
C     DATA FENDC/81./,XYHOL1,XYHOL2/141.,151.5/,BLDPFW/400./,
C    1 ZMINBL,ZPLUBL /-5250.,5250./,
C    2 XSC /230.,150./, YSC /150.,70./, RSC /152.48,192.48/,
C    3 ZMISC/-4235.,-4135./, ZPLSC/4235.,4135./, DZSC/6./,
C    4 CHX/140.,-400.,-140.,140.,-400.,-140.,
C    5 140.,-400.,-140.,140.,-400.,-140./,
C    6 CHY /-400.,140.,-400.,-400.,140.,-400.,
C    7 -400.,140.,-400.,-400.,140.,-400./,
C    8 CHZ /-4770.,-4720.,-4770.,-4020.,-3970.,-4020.,
C    9 4020.,3970.,4020.,4770.,4720.,4770./,
C    A WLEN/800./, PITCH /25./, WZDIS/10./
C
C------ END MACRO CGEO2COM
