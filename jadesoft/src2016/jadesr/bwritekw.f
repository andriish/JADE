C   14/08/79 003171304  MEMBER NAME  BWRITEKW (JADESR)      FORTRAN
C
        SUBROUTINE BWRITE(IUN,LENG,ID)
C---
C---     COPIED OVER FROM 'F22KAW.MTLINKS(STWAD1)' 17.20 16.08.79
C---
C
C             CODED BY S.KAWABATA AT 06.08.79
C             LAST MODIFIED       AT 14.08.79
C
        COMMON/  CPK/ NTOT,NTOLD,NBLK,NEV,NS
        COMMON/CPACK/ IX(1557)
        DIMENSION JD(52),JD2(52)
        DIMENSION ID(1)
        DATA IHD,JD2/4HHIDD,52*0/
        DATA N1538,N1548,N1557/ 1538,1548,1557/
C
      IF(NTOT.GE.0) GO TO 999
C---
C---     OUTPUT FILE IS S FORMAT.
C---
      CALL SFORWT(IUN,LENG,ID)
      RETURN
  999 CONTINUE
        IF(NEV.NE.0) GO TO 10
C
C=======================> INITIALIZATION <============================
C
                         NBLK = 0
                         NTOT = 0
                         NS   = 0
   10   NEV  = NEV + 1
        NAMEB = 0
        IX(NTOT+1) = IHD
        IX(NTOT+2) = NEV
        IX(NTOT+3) = 0
        NTOLD= NTOT
        NTOT = NTOT+LENG+5
C                        A THIS 5 IS FOR EVENT HEADER
        IF(NTOT.GT.N1557) GO TO 100
C
C=======================> COMPLETE EVENT <==============================
C
        IX(NTOLD+4) = LENG+1
C                          A THIS 1 IS FOR IC-FLAG
        IX(NTOLD+5) = 0
        CALL UCOPY2(ID(1),IX(NTOLD+6),LENG)
        IF(NTOT.GE.N1538) GO TO 400
        RETURN
C
C======================> IMCOMPLETE EVENT <=============================
C
C=============> CHECK FORMAT AND SAVE ADDRESS OF LAST WORD OF BANKS
C
  100   IB = 1
        DO 110 I=2,51
        IF(IB.GT.LENG) GO TO 115
        IBJ = IB
        IB  = IB+ID(IB+3)+4
        IB1 = IB-1
        JD2(I) = IB1
  110   CONTINUE
C
C=============> INVERSE THE ORDER
C
  115   MBANK = I-1
        CALL UCOPIV(JD2,JD,MBANK)
        NW = 0
        ICON = 0
C
C---- NW  : NO. OF WORDS OF THE EVENT ALREADY WRITTENIN IN THE RECORD.
C---- ICON: WORD-COUNT FOR BANCK HEADER
C          ICON = 0, IF NEXT RECORD HAS NON DUPLICATE BANK HEADR.(IFL=1)
C          ICON = 4, IF NEXT RECORD HAS DUPLICATE BANK HEADR.(IFL=2)
C
  120    CONTINUE
C/////////
C         WRITE(6,9120) MBANK,(JD2(KK),KK=1,MBANK)
C9120     FORMAT(20I6)
C        CALL KDEBUG(120,3,NW,ICON,NS)
C
        NTOLD4 = NTOLD+4
        NTOLD5 = NTOLD+5
C
C=============> CHECK LAST PART OF RECORD
C
        DO 130 I=1,MBANK
        NWBNK = JD(I)
        LW     = NTOLD5+ICON+NWBNK-NW
        IF(LW.GE.N1548 .AND. LW.LE.N1557) GO TO 150
        IF(LW.LT.N1548 .AND. NTOT.GT.N1557) GO TO 170
  130   CONTINUE
        WRITE(6,9130)
 9130   FORMAT(1X,'ERROR : TYPE 130')
        RETURN
C
C==============> SET FLAG
C
C-------- NON DUPLICATE BANK HEADR
  150   JJ = NWBNK-NW
        IFL= 1
        GO TO 200
C-------- DUPLICATE BANK HEADR
  170   JJ = N1557-NTOLD5-ICON
        IFL= 2
  200   IX(NTOLD4) = JJ+ICON+1
        ICF= 1
        IF(NS.EQ.1) ICF= 2
        IX(NTOLD5)= ICF
        CALL UCOPY2(ID(NW+1),IX(NTOLD5+ICON+1),JJ)
C
        IF(IFL.EQ.2) GO TO 300
C
C==============> NON DUPLICATE BANK HEADR
C
        NTOT1 = NTOLD5+ICON+JJ
        WRITE(IUN) NTOT1,IX
        NBLK= NBLK+1
        CALL UZERO(IX,N1538,N1557)
        NS= 1
        NTOT1 = NTOT-NTOT1
        NTOT= NTOT1+5
        IX(1)= IHD
        IX(2)= NEV
        IX(3)= 0
        IA1= NWBNK+1
        IA2= 6
        IF(NTOT.LE.N1557) GO TO 350
        ICON = 0
        GO TO 460
C
C==============> DUPLICATE BANK HEADER
C
  300   CONTINUE
        NAMEA= ID(NWBNK+1)
        NUMBA= ID(NWBNK+2)
        IF(NAMEA.NE.NAMEB) GO TO 310
        IF(NUMBA.EQ.NUMBB) GO TO 320
  310   IBSTOL= 0
        NAMEB= NAMEA
        NUMBB= NUMBA
  320   LNB= LW
        IF(LW.LT.6) LNB= 5
        IBSTOR= N1557-LNB-4
        IBSTOL= IBSTOL+IBSTOR
        IX(LNB+4)= IBSTOR
        WRITE(IUN) N1557,IX
        NBLK= NBLK+1
        CALL UZERO(IX,N1538,N1557)
        NS= 1
        NTOT1= NTOT-N1557
        NTOT= NTOT1+9
        IX(1)= IHD
        IX(2)= NEV
        IX(3)= 0
        IX(6)= NAMEA
        IX(7)= NUMBA
        IX(8)= ID(NWBNK+3)
        IX(9)= ID(NWBNK+4)-IBSTOL
        IA1= NW+JJ+1
        IA2= 10
        IF(NTOT.GT.N1557) GO TO 450
  350   CALL UCOPY2(ID(IA1),IX(IA2),NTOT1)
        IX(4)= NTOT-4
        IX(5)= 3
        IBSTOL = 0
        NS= 0
        IF(NTOT.LT.N1538) RETURN
  400   WRITE(IUN) NTOT,IX
        NBLK= NBLK+1
        NTOT= 0
        CALL UZERO(IX,N1538,N1557)
        RETURN
  450   ICON= 4
  460   NW= NW+JJ
        MBANK= I
        NS= 1
        NTOLD= 0
        GO TO 120
C
C=============================> BWTEND <================================
C
        ENTRY BWTEND(IUN)
C
        IF(NTOT.LT.0) RETURN
        IF(NTOT.EQ.0) GO TO 1300
        NT1= NTOT+1
        IF(NT1.GT.N1557) GO TO 1100
        CALL UZERO(IX,NT1,N1557)
 1100   WRITE(IUN) NTOT,IX
         NBLK= NBLK+1
         NTOT= 0
 1300    NS= 0
         RETURN
         END
C
      SUBROUTINE SFORWT(IUN,LENG,ID)
      IMPLICIT INTEGER*2 (H)
      DIMENSION ID(LENG)
      WRITE(IUN) LENG,ID
      RETURN
      END
C
C>>>>>>>>>>>>>>> BLOCK DATA <<<<<<<<<<<<<<<<<<<<<
C
       BLOCK DATA
C
        COMMON/  CPK/ NTOT,NTOLD,NBLK,NEV,NS
        DATA NTOT/ 0/
        DATA NEV/ 0/
        END
