C   14/08/79 105121535  MEMBER NAME  BREADKW  (JADESR)      FORTRAN
C
        SUBROUTINE BREAD(IUN,LENG,ID,*,*)
C---
C---     COPIED OVER FROM 'F22KAW.MTLINKS(STWAD1)' 17.20 16.08.79
C---
C
C             CODED BY S.KAWABATA AT 30.07.79
C             LAST MODIFIED       AT 14.08.79
C
        IMPLICIT INTEGER*2 (H)
        DIMENSION ID(LENG)
        COMMON/CUNPK/ND,NW1,NEV,M5001,INIT,NDR,N
        COMMON/CUPK/NTOT,IX(1557)
        DIMENSION HX(3114)
        EQUIVALENCE (IX(1),HX(1))
        DATA IHD/ 'HIDD'/
        DATA N5001,N1538,N1548,N1558/ 20001, 1557, 1548, 1558/
C
C
        NAME =   0
        NR   = 100
        IF( INIT .NE. 20) GO TO 1
        IF( NLOC .EQ.  1) GO TO 10
        GO TO 20
    1   IF( INIT .EQ. 10) GO TO 3
C
C========================> INITIALIZE <===========================
C
        N    =  0
        NRNS =  0
        NEVT =  0
        NEV  =  0
        IC   =  3
        NWS  =  0
        INIT = 10
        NLOC =  1
C
C=======================> FIRST READ OR S-FORMAT READ <=================
C
    3   READ(IUN,END=1000,ERR=2000) LENG,ID
        N   =  N + 1
C
        IF(  N .EQ.  1) GO TO 7
    4   NEV= NEV+1
        RETURN
    7   IF(ID(1) .NE. IHD) GO TO 4
        INIT  =  20
        NTOT  =  LENG
        CALL UCOPY2(ID(1),IX(1),NTOT)
        GO TO 11
C
C========================> M-FORMAT <==================================
C
C--------->     READ ONE RECORD
   10   READ(IUN,END=1000,ERR=2000) NTOT,IX
        N   =  N + 1
   11   CONTINUE
C
C=======================> CHECK HIDD
C
        IF( IX(1) .EQ. IHD) GO TO 40
                    CALL EWRIT(1,N,13,NRUN,NEVNT,6,'NO BLK HIDD')
        GO TO 500
C
C##20###IF( NLOC .GT. N1538) GO TO 500
   20   IF( NLOC .GT. NTOT) GO TO 500
   30   IF( IX(NLOC) .EQ. IHD) GO TO 40
C-----------> LOOK FOR HIDD
        NLOC =  NLOC + 1
C#######IF( NLOC .GT. NTOT .OR. NLOC .GT. N1538) GO TO 500
        IF( NLOC .GT. NTOT) GO TO 500
        GO TO 30
C
C=======================>
C
   40   NEVNT = IX(NLOC+1)
        NW    = IX(NLOC+3)
        ICF   = IX(NLOC+4)
C
C=======================> CHECK NO. OF WORDS
C
        IF( NW .GT.  1) GO TO 45
                    CALL EWRIT(2,N,40,NRUN,NEVNT,9,'ILLEGAL # OF WORDS')
        GO TO 30
   45   NWX  =  NLOC + NW + 3
C###### IF( NWX .LT. N1558) GO TO 50
        IF( NWX .LE. NTOT) GO TO 50
                          CALL EWRIT(1,N,45,NRUN,NEVNT,6,'TOO LARGE NW')
        GO TO 500
C
C=======================> CHECK FLAG ICF
C
   50   ICFX= ICF + 1
        IF( ICFX .GT. 0 .AND. ICFX .LT. 5) GO TO 55
                           CALL EWRIT(2,N,50,NRUN,NEVNT,5,'ILLEGAL IC')
        GO TO 400
   55   GO TO ( 60, 60, 100, 100),ICFX
C
C=======================> ICF = 0 OR 1 <================================
C
   60   IF( IC .EQ. 3) GO TO 65
                       CALL EWRIT(2,N,60,NRUN,NEVT,8,'INCOMPLETE EVENT')
   65   NLOC2 = 2*NLOC + 26
        NRUN  = HX(NLOC2)
        IF( NRUN .EQ. NRNS) GO TO 75
C###### IF( NRUN .GT. NRNS) GO TO 70
C######                CALL EWRIT(3,N,65,NRUN,NEVNT,6,'ILLEGAL RUN#')
C###### IF( NLOC .EQ. 1) GO TO 70
C#######IF( NRUN .EQ. 2 .OR. NRUN .EQ. 3) GO TO 70
C###################   CALL EWRIT(1,N,65,NRUN,NEVNT,6,'ILLEGAL RUN#')
C###### GO TO 500
C
   70   NRNS = NRUN
        NEVT = NEVNT
C        WRITE(6,9000) NRNS,NEVT,N
C9000    FORMAT(1X,'>>>>>>>>>>> NEW RUN#(',I10,') SEV#(',I10,') IN BLK',
C    -   I6,' <<<<<<<<<<<<<<<<<<<')
        GO TO 81
   75   IF( NEVNT .GT. NEVT) GO TO 80
C#######IF( NEVNT .EQ.    1) GO TO 70
C#######              CALL EWRIT(1,N,75,NRUN,NEVNT,7,'ILLEGAL EVENT#')
C#######GO TO 500
   80   NEVT = NEVNT
   81   NWS  = NW - 1
        CALL UCOPY2(IX(NLOC+5),ID(1),NWS)
        IF( ICF .EQ. 1) GO TO 130
        IC  = 3
        LENG= NWS
        NLOC= NLOC + NW + 4
C#######IF( NLOC .GT. N1538) NLOC = 1
        IF( NLOC .GT. NTOT) NLOC = 1
        NEV= NEV+1
        RETURN
C
C======================> ICF = 2 OR 3  <================================
C
  100   IF( IC .EQ. 2) GO TO 105
       CALL EWRIT(2,N,100,NRUN,NEVNT,15,'MIDDLE PART OF EVENT APPEARED')
        GO TO 400
  105   IF( NEVNT.EQ. NEVT) GO TO 110
C#######         CALL EWRIT(2,N,105,NRUN,NEVNT,10,'EVENT IS INCOMPLETE')
C#######GO TO 400
  110   NW1 = NW + NWS + 4
        IF( NW1 .LT. N5001) GO TO 115
                  CALL EWRIT(2,N,110,NRUN,NEVNT,7,'TOO LARGE EVENT')
        GO TO 400
  115   NST = 4
        NSU = 1
        IF( IX(NLOC+5) .NE. NAME) GO TO 120
        IF( IX(NLOC+6) .NE.   NR) GO TO 120
        NST = 8
        NSU = 5
        ID(IAN) = ID(IAN) + IX(NLOC+8)
  120   IS= NLOC + NST
        NW1 = NW - NSU
        CALL UCOPY2(IX(IS+1),ID(NWS+1),NW1)
        NWS = NWS + NW1
        IF( ICF .EQ. 2) GO TO 130
            LENG= NWS
            IC= 3
            NLOC= NLOC+NW+4
C###########IF(NLOC.GT.N1538) NLOC= 1
            IF(NLOC.GT.NTOT) NLOC= 1
            NWS= 0
        NEV= NEV+1
            RETURN
C
C=================> SET ADDRESS FOR NEXT RECORD <=====================
C
  130  IJ= NLOC+5
       IJ1= IJ
            DO 134 I=1,50
            IJ1= IX(IJ1+3)+IJ1+4
            IF(IJ1.LE.IJ) GO TO 135
            IF(IJ1.GT.NTOT) GO TO 136
            IJ= IJ1
  134       CONTINUE
            GO TO 500
  135       CONTINUE
                 CALL EWRIT(2,N,135,NRUN,NEVNT, 7,'# OF WS ERROR ')
            GO TO 500
  136   IC = 2
        NLOC = 1
        IF(IJ.NE.6) GO TO 137
        IF(IX(IJ).NE.NAME) GO TO 137
        IF(IX(IJ+1).EQ.NR) GO TO 10
137     NAME= IX(IJ)
        NR  = IX(IJ+1)
        IAN = NWS+IJ+3-NTOT
       GO TO 10
C
C====================> REJECT CURRENT EVENT<===========================
C
  400  NLOC= NLOC+NW+4
C########## IF(NLOC.GT.N1538) GO TO 500
            IF(NLOC.GT.NTOT) GO TO 500
            IC= 3
             NWS= 0
       GO TO 20
  500       IC= 3
            NWS= 0
            NLOC= 1
       GO TO 10
C
C=======================> REACH EOF <===================================
C
 1000  RETURN1
C
C======================> READ ERROR <==================================
C
 2000  RETURN2
      END
C
C>>>>>>>>>>>>>>>>>  ERROR MESAGE ROUTINE  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C
       SUBROUTINE EWRIT(IFG,NB,IFL,NRUN,NEVNT,N,ICH)
       INTEGER *2 ICH(20)
       IF(IFG.NE.1) GO TO 100
       WRITE(6,9000) NB,NRUN,NEVNT,IFL,(ICH(J),J=1,N)
 9000  FORMAT(1X,'*** SKIP TO THE NEXT BLOCK BY ERROR IN BLK',I6,
     - ' RUN(',I6,') EVENT(',I7,') FLAG =',I4,' : ',20A2)
       RETURN
  100  IF(IFG.NE.2) GO TO 200
       WRITE(6,9100) NB,NRUN,NEVNT,IFL,(ICH(J),J=1,N)
 9100  FORMAT(1X,'*** SKIP TO THE NEXT HIDD  BY ERROR IN BLK',I6,
     - ' RUN(',I6,') EVENT(',I7,') FLAG =',I4,' : ',20A2)
       RETURN
  200  WRITE(6,9200) NB,NRUN,NEVNT,IFL,(ICH(J),J=1,N)
 9200  FORMAT(1X,'*** WARNING ; EVENT STRUCTURE ERROR IN BLK',I6,
     - ' RUN(',I6,') EVENT(',I7,') FLAG =',I4,' : ',20A2)
       RETURN
       END
