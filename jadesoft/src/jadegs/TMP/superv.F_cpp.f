C   01/04/85 806291322  MEMBER NAME  SUPERV   (JADEGS)      FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE SUPERV
C-----------------------------------------------------------------------
C
C
C        JADE SUPERVISOR ROUTINE FOR GENERAL PURPOSE ANALYSIS.
C        -----------------------------------------------------
C
C   AUTHOR:   L. O'NEILL PREHISTORY:  THE JADE SUPERVISOR
C
C        MOD  J. OLSSON   31/08/83 :  MOD. TO GRAPHICS PART OF EV. WRITE
C        MOD  J. OLSSON    9/11/83 :  CALL LGCDIR ADDED AT LEVEL 7
C        MOD  C. BOWDERY  13/12/83 :  MUON ANALYSIS ONLY IF NO MUON BNKS
C                                  :  GENERAL TIDY-UP PERFORMED.
C        MOD  C. BOWDERY  20/12/83 :  REMOVE BLOCKDATA. ADD EXTERNAL
C                                  :  JADEBD TO LOAD BLOCKDATA.
C                                  :  IUNIT = 2 REMOVED AS SET IN JADEBD
C        MOD  C. BOWDERY  16/01/84 :  PRINT BANNER HEADLINE. IDATA(40K)
C        MOD  C. BOWDERY  19/01/84 :  USE NEW CBCSMX MACRO
C        MOD  C. BOWDERY   8/02/84 :  MACRO CADMIN NOW ON PATRECSR
C        MOD  C. BOWDERY  14/02/84 :  CODE TO CHECK IF PATR/JHTL DELETED
C                                  :  NTIME CALLED AFTER CALL TO USER
C        MOD  C. BOWDERY  17/02/84 :  SERIOUS BUG CORRECTED IN LEVEL 5
C        MOD  C. BOWDERY   8/07/84 :  NOTICE IF NDDINN HAS CHANGED
C        MOD  C. BOWDERY   1/04/85 :  CALL KALIBR FOR EVERY EVENT NOW
C        MOD  C. BOWDERY  17/07/85 :  SMALL CHANGE FOR LEVEL 11
C   LAST MOD  C. BOWDERY  28/03/88 :  LOW PRIORITY BOS SPACE INCREASED
C
C   LAST MOD
C   P. Movilla Fernandez   Oct./98 :  Adapted for use on new platforms
C                                     Get rid of old IBM stuff
C                                     Reorganize I/O handling
C                    PMF  31/05/00 :  Introduce INDEX=13 on return from
C                                     USER subroutine
C
C------------------  M O D E   O F   U S E  ----------------------------
C
C
C   THE SUPERVISOR COORDINATES THE ANALYSIS OF JADE EVENTS. AT VARIOUS
C   POINTS IN THE PROGRAM, CONTROL IS PASSED TO S/R "USER" WHICH CAN
C   MAKE DECISIONS ABOUT WHAT TO DO NEXT. AT EACH CALL, AN "INDEX" VALUE
C   IS PASSED TO "USER" TO INDICATE AT WHICH POINT OR "LEVEL" THE
C   SUPERVISOR HAS JUST FINISHED. IT IS A SUPERVISOR CONVENTION THAT IF
C   THE ANALYSIS BANKS FOR AN ANALYSIS PACKAGE ALREADY EXIST, THEN A NEW
C   ANALYSIS IS -NOT- PERFORMED. THIS SHOULD BE REMEMBERED WHEN READING
C   THE COMMENTS BELOW. TO GET A RE-ANALYSIS, SIMPLY DELETE THE EXISTING
C   RESULTS BANKS IN THE "USER" ROUTINE BEFORE THE RELEVANT LEVEL.
C
C      INDEX                        COMMENT
C      -----                        -------
C
C        0             INITIAL CALL, BEFORE FIRST EVENT READ.
C        1             CALLED AT THE BEGINNING OF EACH NEW RUN.
C        2             CALLED IMMEDIATELY AFTER EVENT IS READ INTO /BCS/
C        3             LEAD GLASS ENERGIES HAVE BEEN COMPUTED.
C        4             FAST Z VERTEX HAS BEEN FOUND AND JETCAL CALLED
C        5             INNER DETECTOR PATTERN RECOGNITION HAS BEEN RUN.
C        6             ENERGY CLUSTERS IN THE LEAD GLASS HAVE BEEN FOUND
C        7             I.D. TRACKS AND CLUSTERS HAVE BEEN ASSOCIATED.
C        8             UNUSED
C        9             MUON ANALYSIS HAS BEEN DONE
C       10             UNUSED
C      100             JUST BEFORE END OF JOB
C
C   ON RETURN, IF INDEX =  1 : THE SUPERVISOR WILL DROP CURRENT EVENT
C                              AND READ THE NEXT ONE.
C                         11 : EVENT WILL BE WRITTEN OUT AND NEW EVENT
C                              WILL BE READ
C                         12 : JOB WILL BE TERMINATED NORMALLY
C
C       PMF: NEW INDEX!   13 : Current event will be written out
C                              and the job will be terminated normally.
C
C                              This additional index makes it easier for MC data
C                              to ensure a correct bookkeeping of the random number
C                              seed corresponding to the LAST EVENT WRITTEN OUT.
C                              This is in particular important if the job
C                              is terminated BEFORE the end of the MC input file.
C                              
C
C   OTHERWISE THE SUPERVISOR WILL GO TO THE LEVEL GIVEN BY "INDEX" ON
C   RETURN FROM "USER". THUS, TO CONTINUE WITH THE ANALYSIS, THE "USER"
C   ROUTINE SHOULD INCREMENT "INDEX" BEFORE PASSING CONTROL BACK HERE.
C
C----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL SAFEOF,FLGWRT
C
C                            FORCE INCLUSION OF ERRPRT * JADE BLOCKDATA
C
      EXTERNAL ERRPRT , JADEBD
C
C-----------------------------------------------------------------------
C                            MACRO CGRAPH .... GRAPHICS COMMON
C-----------------------------------------------------------------------
C
      LOGICAL DSPDTL,SSTPS,PSTPS,FREEZE
C
      COMMON / CGRAPH / JUSCRN,NDDINN,NDDOUT,IDATSV(11),ICREC,MAXREC,
     +                  LSTCMD,ACMD,LASTVW,ISTANV,
     +                  SXIN,SXAX,SYIN,SYAX,XMIN,XMAX,YMIN,YMAX,
     +                  DSPDTL(30),SSTPS(10),PSTPS(10),FREEZE(30),
     +                  IREADM,LABEL,LSTPS(10),IPSVAR
C
C------- END OF MACRO CGRAPH -------------------------------------------
C
C==MACRO CIOUNI=========================================
      COMMON/CIOUNI/IUNIT,JUNIT,NCALI,KUNITA(10),LUNITA(10)
C==ENDMACRO CIOUNI========================================
C
C                        --- MACRO CADMIN ---
C
      LOGICAL*1 LBREAD
C
      COMMON / CADMIN / IEVTP,NRREAD,NRWRIT,NRERR,LBREAD(4),IFLG,IERCAL,
     +                  ISMEAR,IJETCI,NFLAGS(10)
C
C                                 NFLAGS IS AN ARRAY OF GENERAL FLAGS
C                                   (1) : USED BY RDDATE
C                                   (2) : USED BY RDTRIG
C                                   (3) : USED BY RDTRIG
C                                   (4) : USED BY RDTRIG / PRSTAT
C                                   (5) : USED BY EVREAD -COUNTS RECORDS
C                                   (6) : USED BY SUPERV -COUNTS ERRORS
C                                   (7) : USED BY EVWRIT -'HEAD'LESS EVS
C                                   (8) : USED BY EVREAD/RDMTCO (EVWRIT)
C                                   (9) : USED BY RDMTCO/EVWRIT
C                                  (10) : FREE
C
C                                  BLOCK DATA SET IN MEMBER JADEBD
C
C
C
C                            MACRO CBCSMX.... BOS COMMON + SIZE DEFINED
C
C-----------------------------------------------------------------------
C             MACRO CBCSMX.... BOS COMMON + SIZE DEFINITION
C
C             THIS MACRO DEFINES THE IDATA/HDATA/ADATA NAMES AND
C             FIXES THE ACTUAL SIZE OF /BCS/. IT IS THUS IDEAL FOR
C             MAIN PROGRAMS OR THE SUPERVISOR. LDATA CAN BE USED BY BINT
C
C-----------------------------------------------------------------------
C
      COMMON /BCS/ IDATA(60000)
      DIMENSION HDATA(120000),ADATA(60000)
      EQUIVALENCE (HDATA(1),IDATA(1),ADATA(1))
      DATA  LDATA / 60000 /
C
C------------------------ END OF MACRO CBCSMX --------------------------
C-----------------------------------------------------------------------
C                            MACRO CJDRCH .... JET CHAMBER CONSTANTS.
C-----------------------------------------------------------------------
C
      COMMON / CJDRCH / RDEC(4),PSIIN(3),RINCR(3),FIRSTW(3),FSENSW(3),
     +                  RDEPTH,SWDEPL,YSUSPN,TIMDEL(2,3),ZMAX,ZOFFS,
     +                  ZRESOL,ZNORM,ZAL,ZSCAL,DRIDEV,DRICOS,DRISIN,
     +                  PEDES,TZERO(3),DRIROT(96,2),SINDRI(96,2),
     +                  COSDRI(96,2),DRIVEL(96,2),T0FIX(3),
     +                  ABERR(8), DUMJDC(20)
C
C      BLOCK DATA SET TO MC VALUES, KALIBR WILL SET REAL DATA VALUES
C--->  A CHANGE OF THIS COMMON MUST BE DONE SIMULTANEOUSLY WITH  <----
C--->  A CHANGE OF THE BLOCK DATA                                <----
C
C--------------------------- END OF MACRO CJDRCH -----------------------
C
C
C------------------------------------------
C  MACRO CLBPGM ....
C------------------------------------------
      COMMON /CLBPGM/ LBPGM(30)
C--------- END OF MACRO CLBPGM ------------
C----------------------------------------------
C  MACRO CPATLM .... PATTERN RECOGNITION LIMITS
C----------------------------------------------
      COMMON /CPATLM/ PATRLM(5),FLINLM(10),TRELLM(20),ZFITLM(10),BKK(20)
     *               ,XYF(20),IGFP(20),XBKK(40),IADMIN(5),YBKK(20)
      INTEGER IXYF(20),LMPATR(5),LMFLIN(10)
      INTEGER LMTREL(20),LMZFIT(10),IBKK(20)
      DIMENSION GFP(20),IXBKK(40),IYBKK(20)
      EQUIVALENCE (PATRLM(1),LMPATR(1)),(IXBKK(1),XBKK(1)),(IYBKK(1),
     *YBKK(1))   ,(FLINLM(1),LMFLIN(1)),(TRELLM(1),LMTREL(1))
     *           ,(ZFITLM(1),LMZFIT(1)),(BKK(1),IBKK(1))
     *           ,(XYF(1),IXYF(1)),(GFP(1),IGFP(1)),(IADMIN(1),IMCERT)
     *           ,(IYBKK(20),IPPASS),(IADMIN(2),IPFAST)
C----------- END OF MACRO CPATLM --------------
C
      COMMON / CTLIM / ISECLF
C
      COMMON / CVX   / NNPATR,ICRSTR
C
C                            ICRSTR STEERS RECORD FINDING  IN  COMMANDS
C                            WRIT N, WHERE SEVERAL EVENTS MAY  BE  READ
C                            BY SUPERV, BEFORE THE WANTED ONE  APPEARS.
C                            SPECIAL  CARE  BY   REWINDING   UPON   EOF
C                            ENCOUNTER.
C
      COMMON / CBSTR  / MODE,MAXRL,SAFEOF
      COMMON / CGCALB / NBADCR,NAVALL
C
      COMMON / CHEADR / IHEADR(54)
      COMMON / CALIBR / JPOINT(16)
C
      DIMENSION HCALIB(2) , HEAD(108) , IUSERC(10)
C
      EQUIVALENCE (HCALIB(1),JPOINT(1)) , (HEAD(1),IHEADR(1))
C
      DATA IUSERC / 10*0 /
      DATA IGG/ 0 / , NRUNW/ 0 / , NEVW/ 0 /
C
C------------------  C O D E  ------------------------------------------
C
C                            PRINT SUPERVISOR BANNER HEADLINE
C                            EXCEPT IN GRAPHICS PROGRAM
C
      IF( NDDINN .EQ. 0 ) WRITE(JUSCRN,600)
  600 FORMAT( /1X,33('****')/
     +       ' *',32('    '),'  *'/
     +       ' *',32('    '),'  *'/
     +       ' *',3(8X,'J A D E   S U P E R V I S O R',5X),4X,'*'/
     +       ' *',32('    '),'  *'/
     +       ' *',3(8X,'  V E R S I O N    28/03/88  ',5X),4X,'*'/
     +       ' *',3(8X,'          M O D    31/05/00  ',5X),4X,'*'/
     +       ' *',32('    '),'  *'/
     +       ' *',32('    '),'  *'/
     +         1X,33('****')  //       )
C
C                            CALL ERRSET TO TRANSFER CONTROL  TO  PST'S
C                            ROUTINE "ERRPRT" IN CASE OF IBCOM  ERRORS.
C
***PMF      CALL ERRSET(206,0,0,0,ERRPRT,214)
***PMF      CALL ERRSET(241,0,0,0,ERRPRT,301)
C
C
C                            INITIALISE  BOS.  (IN  GRAPHICS  BINT  HAS
C                            ALREADY BEEN CALLED.)
C
      IF( NDDINN .NE. 0 ) GO TO 80
C       CALL BINT(LDATA,10000,500,4000)
        CALL BINT(LDATA,LDATA,500,4000)
***PMF        CALL DUMP0C
        CALL BWRO(JUNIT,MAXRL,MODE)
C
C                            GET THE BOS POINTERS FOR SOME DATA BANKS
C
  80  IHEAD = IBLN('HEAD')
      IALGL = IBLN('ALGL')
      IALGN = IBLN('ALGN')
      IJETC = IBLN('JETC')
      IZVTX = IBLN('ZVTX')
      IPATR = IBLN('PATR')
      IJHTL = IBLN('JHTL')
      ILGCL = IBLN('LGCL')
C
C                            INITIALISE  PARAMETERS  FOR  LEAD   GLASS,
C                            Z-VERTEX AND PATREC.
C
      CALL LGINIT
      CALL INITZV
      CALL MUINI
      CALL VTXINI
C
      NBADCR = 500
      NAVALL = 20
      FLGWRT = .FALSE.
      ICREC  = 0
      IPFAST = 2
      INDUSR = 0
      CALL USER(INDUSR)
      LASTRN = -1
C
C                ---------------------------------------
C                ------->    MAIN EVENT LOOP    <-------
C                ---------------------------------------
C
C
C                            CHECK CLOCK IF THIS IS A BATCH JOB
C
    1 ITLIM  = 1
      IF( NDDINN .NE. 0 ) GO TO 12
      IF( IUHR(ISECLF) .EQ. 1 ) GO TO 12
      LBIUHR = IUHR(ISECLF)
      IF( LBIUHR .EQ. 1 ) GO TO 12
C
      WRITE(JUSCRN,7701) (ISECLF, ITMESS = 1,10)
7701  FORMAT(10(/'  NUMBER OF SECONDS LEFT AT TIME LIMIT = ',I10) )
      GO TO 100
   12 CONTINUE
      ITLIM = 0
C
C                            IUNIT IS BLOCKDATA SET TO 2. IF GRAPHICS,
C                            USE RUN TIME LUN STORED IN /CADMIN/.
C                            THIS VALUE MAY BE CHANGED DURING SESSION.
C
      IF( NDDINN .NE. 0 ) IUNIT = NDDINN
C
C                            READ  NEXT  EVENT,  HANDLING  MONTE  CARLO
C                            CALIBRATION IF RELEVANT. IRET = 2 ON EOF.
C                            READ ERRORS ARE NOT RETURNED.
C
      CALL EVREAD(IUNIT,IRET)
C
      IF(IRET.EQ.2) GO TO 100
      ICREC = ICREC + 1
C
      CALL EXJHTL(IERJHT)
      IF(IERJHT.NE.0) WRITE(6,7985)
7985  FORMAT(' ERROR IN SUBROUTINE EXJHTL, NO ROOM FOR NEW FORMAT JHTL')
C
      IPHEAD=IDATA(IHEAD)
      IF(IPHEAD.LT.1) GO TO 1002
      CALL MVCL(IHEADR(1),0,IDATA(IPHEAD-3),0,216)
      IPH2=IPHEAD*2
      NRUN=HDATA(IPH2+10)
      IACT=HDATA(IPH2+27)
      IF(NDDINN.NE.0) IACT=0
C
C                            ARE THERE PATR AND JHTL BANKS?
C
      NPBANK = -1
      IPPATR = IDATA(IPATR)
      IF( IPPATR .LE. 0 ) GO TO 102
        NPBANK = IDATA(IPPATR-2)
C
  102 NJBANK = -1
      IPJHTL = IDATA(IJHTL)
      IF( IPJHTL .LE. 0 ) GO TO 103
        NJBANK = IDATA(IPJHTL-2)
C
C                            RECORD IF THERE IS A MONTE CARLO PATR BANK
C
  103 CALL CLOC(MCPATR,'PATR',12)
      INDUSR = 2
C
C                            KALIBR AND INPATC CALLED FOR EVERY EVENT
C
      CALL KALIBR
      CALL INPATC
C
      IF( NRUN .EQ. LASTRN ) GO TO 2
C
C                            NEW OR FIRST RUN.
C
      IF( LASTRN .NE. -1 ) GO TO 110
C
C                            VERY  FIRST   EVENT.   INITIALISE   PATREC
C                            CONSTANTS BY CALL TO INPATR.
C
      CALL INPATR
C
  110 LASTRN = NRUN
      INDUSR = 1
      GO TO 200
C
C                - 2 -       EVENT HAS BEEN READ IN
C
   2  DO  301  KLR = 1,30
        LBPGM(KLR) = 0
  301 CONTINUE
      GO TO 200
C
C                - 3 -       CALL LEADGLASS ENERGY DECODING.
C
    3 IF(IDATA(IALGL) .LE. 0) GO TO 200
      IF(IDATA(IALGN) .EQ. 0) CALL LGCALB(*1225)
      GO TO 200
C
1225  IGG = IGG + 1
      IF(IGG.LT.20) WRITE(6,1222) HDATA(IPH2+10),HDATA(IPH2+11)
1222  FORMAT(' RUN AND EVENT ',2I6,' HAS RETURN1 IN LGCALB')
      GO TO 200
    4 CONTINUE
C
C                - 4 -       JET CHAMBER CALIBRATION AND FAST Z VERTEX
C
C                            CALL I.D. CALIBRATION ROUTINE ONLY IF:
C                            A)    THIS IS NOT MONTE CARLO DATA.
C                            B)    THERE IS NO SECOND "JETC" BANK, WHICH
C                                  WOULD MEAN THAT THE FIRST "JETC" BANK
C                                  IS ALREADY CALIBRATED  ON  THE  INPUT
C                                  DATA SET.
C
      IF( NRUN .LT. 100 ) GO TO 401
      IPNTJT = IDATA(IJETC)
      IF( IPNTJT .LT. 1 ) GO TO 401
      IPNTNX = IDATA(IPNTJT-1)
      IF( IPNTNX .NE. 0 ) GO TO 401
      CALL JETCAL
  401 CONTINUE
C
C                            CALL FAST Z VERTEX FINDING ROUTINE
C
      IF( IDATA(IZVTX) .EQ. 0 ) CALL ZVERTF
      GO TO 200
C
C                - 5 -       DO INNER DETECTOR PATTERN RECOGNITION.
C
    5 IPPATR = IDATA(IPATR)
      IPJHTL = IDATA(IJHTL)
      NRPATR = -1
      IF( IPPATR .EQ. 0 ) GO TO 52
      NRPATR = IDATA(IPPATR-2)
C
   52 NRJHTL = -2
      IF( IPJHTL .EQ. 0 ) GO TO 51
      NRJHTL = IDATA(IPJHTL-2)
C
C                            IF THE LATEST PATR AND JHTL HAVE BEEN
C                            DELETED THEN RE-DO PATTERN RECOGNITION.
C                            IGNORE TEST IF 'PATR' NEVER EXISTED BUT
C                            CONTINUE IF 'PATR' DID BUT 'JHTL' DIDN'T.
C
C                            NPBANK = 'PATR' BANK NO. ON INPUT
C                            NJBANK = 'JTHL' BANK NO. ON INPUT
C                                   = -1 IF NONE EXISTED THEN
C
   51 IF( NPBANK .EQ. -1 ) GO TO 55
      IF( NRPATR .NE. NPBANK  .AND. NRJHTL .NE. NJBANK ) GO TO 53
C
C                            IF THE EVENT  HAS  NOT  BEEN  PROCESSED  BY
C                            REDUC1 AND NO PATR BANK EXISTS (OTHER  THAN
C                            MC) THEN DO PATTERN RECOGNITION
C
   55 IF( NRPATR .LE. 10 .AND. NRPATR .GE. 0 ) GO TO 54
      IF(   IACT .NE.  0 ) GO TO 54
C
   53 CALL BGAR(IGARBA)
      IND = 0
      CALL PATREC(IND)
C
C                            THIRD STAGE IN MC TRACEBACK, SEE JCN 69
C
   54 IF(NRUN.LT.100) CALL MCTR4V(0,IERRMC)
      GO TO 200
C
C                - 6 -       CALL LEAD-GLASS DETECTOR CLUSTER ANALYSIS
C
    6 IF(IDATA(ILGCL).EQ.0) CALL LGANAL
      GO TO 200
C
C                - 7 -       ASSOCIATE INNER DETECTOR TRACKS WITH
C                            CLUSTERS.
C
    7 IPPATR = IDATA(IPATR)
      IF(IPPATR.EQ.0) GO TO 200
      IPALGN = IDATA(IALGN)
      IF(IPALGN.EQ.0) GO TO 200
C
      IPLGCL = IDATA(ILGCL)
      IF(IPLGCL.EQ.0) GO TO 200
      CALL LGCDIR(IPPATR,IPALGN,IPLGCL)
      GO TO 200
C
C                - 8 -       UNUSED
C
    8 CONTINUE
      GO TO 200
C
C                - 9 -       MUON DETECTOR ANALYSIS
C
    9 CALL CLOC(MUR1,'MUR1',0)
      CALL CLOC(MUR2,'MUR2',0)
C
C                            IF 'MUR1'/0 OR 'MUR2'/0 ARE MISSING
C                            DELETE ALL 'MUR1' * 'MUR2' BANKS.
C                            OTHERWISE SKIP THE ANALYSIS.
C
      IF( MUR1 .GT. 0  .AND. MUR2 .GT. 0 ) GO TO 200
      CALL BMLT(2,'MUR1MUR2')
      CALL BDLM
C
C                            CALL FOR MUON ANALYSIS. IF NO 'PATR' BANK,
C                            SET FLAG TO PREVENT PHILOSOPHY 2 ANALYSIS.
C
      IMUARG = 1
      IF(IDATA(IPATR).LT.1) IMUARG = 0
      CALL MUANA(IMUARG)
      GO TO 200
C
C               - 10 -       UNUSED
C
   10 CONTINUE
      GO TO 200
C
C               - 11 -       EVENT HAS BEEN ACCEPTED. WRITE IT OUT.
C
   11 CONTINUE
C
C                            SKIP THIS LEVEL IN GRAPHICS UNLESS WRIT
C                            COMMAND HAS ALREADY BEEN GIVEN
C
      IF(   NDDINN .NE. 0  .AND.  NDDOUT .EQ. 0 ) GO TO 1
2172  IF( SAFEOF  .AND.  NRWRIT .GT. 0 ) CALL OPUNIT(JUNIT)
      NRUNW = HEAD(18)
      NEVW  = HEAD(19)
C
C                            PRINT A WARNING IF THE MC PATR BANK THAT
C                            WAS READ IN IS NO LONGER PRESENT
C
      CALL CLOC(MCPAT2,'PATR',12)
      IF( MCPATR .EQ. 0 .OR. MCPAT2 .NE. 0 ) GO TO 111
        NFLAGS(6) = NFLAGS(6) + 1
        IF( NFLAGS(6) .LE. 10 ) WRITE(JUSCRN,112) ICREC
  112   FORMAT(/' ****  WARNING  ****  IN SUPERV FOR EVENT ',I6,
     +          '.   THE M.C. ''PATR''/12 BANK HAS',
     +          ' BEEN DELETED OR RENAMED OR RENUMBERED. IRREGULAR!'/)
C
C
C                            EVWRIT HANDLES MC CALIBRATION IF PRESENT
C                            AS WELL AS EVENTS.
C
  111 CALL EVWRIT(JUNIT)
C
      FLGWRT=.TRUE.
      IF( SAFEOF ) END FILE JUNIT
      IF( SAFEOF ) WRITE(JUSCRN,190) JUNIT
  190 FORMAT(' SUPERV HAS CALLED EVWRIT OF',I4,' AND EOFED THE FILE.')
C
      IF(SAFEOF) CALL SAFCHK(JUNIT)
      IF( INDUSR.EQ.13 ) GO TO 201 ! PMF 31/05/00: Introduce this branching according to INDUSR=13
      IF(NDDOUT.EQ.0) GO TO 1
      IF(ICREC.LT.ICRSTR) GO TO 1
C
      WRITE(6,2471)
2471  FORMAT(' NOW REWINDING INPUT FILE...')
      REWIND NDDINN
      ICREC = 0
      CALL BDLS('+BUF',NDDINN)
      IF( INDUSR.EQ.13 ) GO TO 201 ! PMF 31/05/00: Introduce this branching according to INDUSR=13
      GO TO 1
C
C                            PASS CONTROL TO THE USER-SUPPLIED  ROUTINE
C                            "USER" WITH THE CURRENT INDEX.
C
  200 IF((INDUSR.GE.1).AND.(INDUSR.LE.10))
     +                      IUSERC(INDUSR)=IUSERC(INDUSR)+1
C
      CALL USER(INDUSR)
C
C                            LOOPING OR RUNNING OUT OF TIME?
C
      IF( NDDINN .NE.  0 ) GO TO 203
      NTIMLF = NTIME(DUMMY)
      IF( NTIMLF .GT. 20 ) GO TO 203
        WRITE(JUSCRN,202) INDUSR,ICREC
  202   FORMAT(/' ****  INTERRUPT  ****  LESS THAN 0.2 SECONDS LEFT',
     +          ' IN SUPERV AT LEVEL ',I3,' FOR EVENT ',I6//
     +          ' MAYBE THE ''USER'' PROGRAM IS IN A LOOP ?  ',
     +          ' RECOVERY ACTION: SUPERV WILL JUMP TO LEVEL 100'//)
        GO TO 100
C
  203 GO TO (1,2,3,4,5,6,7,8,9,10,11,201,11),INDUSR !PMF 31/05/00: Introduce INDUSR=13
      CALL MESSAG(3)
      GO TO 201
C
C                            NO 'HEAD' BANK IN INPUT EVENT.
C
 1002 CALL MESSAG(4)
      GO TO 1
C
  100 CONTINUE
      IF(NDDINN.EQ.0) GO TO 201
      WRITE(6,2471)
      REWIND NDDINN
      ICREC = 0
      ICRSTR = 1
      CALL BDLS('+BUF',NDDINN)
      GO TO 1
  201 CONTINUE
      IF(.NOT.FLGWRT) GO TO 199
      IF(MODE.EQ.0) GO TO 199
C
C                            WRITE OUT LAST BLOCK(S) ONTO  OUTPUT  FILE
C                            IN BOSM CASE.
C
      CALL BMLT(0,DUMMY)
      CALL BWRITE(JUNIT)
C
  199 IF( ITLIM .EQ. 0 ) GO TO 294
        WRITE(JUSCRN,196)
  196   FORMAT(2(/'  ***** WARNING! TIME LIMIT REACHED. ***** ')//)
C
        WRITE(JUSCRN,197) HDATA(IPH2+10),HDATA(IPH2+11)
  197   FORMAT(' JOB STOPPED BY TIME LIMIT AFTER RUN',I8,' EVENT',I10)
C
        WRITE(JUSCRN,209) NRREAD,NRWRIT,NRERR
 209    FORMAT(//' TIMEOUT !!!',
     $   '   NR OF EVENTS READ: ',I6,'   NR OF EVENTS WRITTEN: ',I6,
     $   '   NR OF READ ERRORS ',I3,//)
        GO TO 295
C
 294    WRITE(JUSCRN,198) NRREAD,NRWRIT,NRERR
 198    FORMAT(//' ===  END OF DATA READING  ==='//
     $           ' NR OF EVENTS READ:',I6,'   NR OF EVENTS WRITTEN:',
     $      I6,'   NR OF READ ERRORS ',I2,//)
C
 295  IF(NEVW.GT.0) WRITE(JUSCRN,298) NRUNW,NEVW
 298  FORMAT(' LAST EVENT WRITTEN (RUN,EVENT): ',2I8)
C
      IF(IERCAL.NE.0) WRITE(6,2641) IERCAL
2641  FORMAT('  **** WARNING ------>>>',I6,' CALIBRATION ERRORS ENCOUNTE
     $RED. PLEASE CONTACT P.STEFFEN OR J.OLSSON !',//)
C
      WRITE(JUSCRN,194)
  194 FORMAT(' NUMBER OF CALLS TO USER FOR EACH INDEX VALUE:'/)
      DO  193  IU = 1,10
        WRITE(JUSCRN,195) IU,IUSERC(IU)
  195   FORMAT(1X,I6,I12)
  193 CONTINUE
C                            PRINT MUON STATISTICS
      CALL MUFINI
C                            FINAL CALL TO "USER"
      INDUSR = 100
      CALL USER(INDUSR)
C
      END
