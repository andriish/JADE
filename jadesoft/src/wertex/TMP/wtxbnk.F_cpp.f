      SUBROUTINE VTXBNK(IPPATR) 
C*800205*OLSSON*****************************************************
C*                                                                 *
C* C R E A T E  B A N K  GVTX  F R O M  V E R T E X  R E S U L T S *
C*                                                                 *
C*******************************************************************
C             IPPATR IS POINTER TO 'PATR' ;  SAME BOSBANK NR IS USED
      IMPLICIT INTEGER*2 (H)
C PMF 03.11.98 
      LOGICAL TBIT
C----------------------------------------------------------------------
C             MACRO CDATA .... BOS COMMON.
C
C             THIS MACRO ONLY DEFINES THE IDATA/HDATA/ADATA NAMES.
C             THE ACTUAL SIZE OF /BCS/ IS FIXED ON MACRO CBCSMX
C             OR BY OTHER MEANS. A DEFAULT SIZE OF 40000 IS GIVEN HERE.
C
C----------------------------------------------------------------------
C
      COMMON /BCS/ IDATA(40000)
      DIMENSION HDATA(80000),ADATA(40000),IPNT(50)
      EQUIVALENCE (HDATA(1),IDATA(1),ADATA(1)),(IPNT(1),IDATA(55))
      EQUIVALENCE (NWORD,IPNT(50))
C
C------------------------ END OF MACRO CDATA --------------------------
C%MACRO MVERTEX1
C     MACRO FOR VERTEX-FIT ROUTINES
      COMMON /CWORK1/ NT,T(2000),NV,V(200),A(300),B(24),NTIND(20),S(20),
     *                CHITR(20),
     *                JTGOD(50),JTBAD(50),VSAVE(10),V2(20,20)
C
      DIMENSION IT(2),IV(2)
      EQUIVALENCE (T(1),IT(1)),(V(1),IV(1))
C
      IF(NV.EQ.0) GO TO 100
C****
C
      NBNK = IDATA(IPPATR-2)
      NWRES = 2 + NV*10 + NT*15
      CALL CCRE(IPHT,'GVTX',NBNK,NWRES,IERR)
      IF(IERR.NE.0) GO TO 100
      CALL BSAW(1,'GVTX')
      IDATA(IPHT+1) = NV
      DO 1  INV = 1,NV
      INVX = IPHT+1+(INV-1)*10
      ISS = (INV-1)*10
      DO 1  I = 1,10
1     IDATA(INVX+I) = IV(ISS+I)
      IDATA(IPHT+2+NV*10) = NT
C
      DO 2  INT = 1,NT
      INTX = IPHT+2+NV*10 + (INT-1)*15
      ISS = (INT-1)*40
      DO 2  I = 1,15
2     IDATA(INTX+I) = IT(ISS+I)
  100 RETURN
      END
