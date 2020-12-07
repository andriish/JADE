C   12/03/84 504221644  MEMBER NAME  TAGMRK   (S)           FORTRAN
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
       SUBROUTINE TAGMRK(*)
C
C
C  AUTHOR   :  A. J. FINCH
C
C       MOD :  J. M. NYE  - DOESNT RETURN 1 IF ATAG BANK NOT THERE
C                           - ALSO USES SMEARING DATE FOR MC DATA IF
C                             NO ATAG BANK.                 24/05/84
C
C  LAST MOD :  18/06/84  J. NYE  - SETS IFLMRK = -1 WHEN CALLED
C
C
C
C THIS ROUTINE SETS THE VALUE OF 'MARK' IN CWORK TO INDICATE WHICH
C TAGGER THIS IS - VALUE OF MARK          TAGGER
C                  =============          ======
C                       1                 1981/2
C                       2                 1983...
C
C IT DOES A RETURN 1 IF 'HEAD'  BANK DOESNT EXIST
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
       IMPLICIT INTEGER*2 (H)
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
C
C
C
C------------ C O M M O N    C W O R K   F O R   T A G A N -------------
C
C
       COMMON/CWORK/MARK,IFLMRK,IMC,NCLST,NNEI,
     *              ISTMZ,ISTPZ,IENDMZ,IENDPZ,
     *              SIGX,SIGY,SIGEN,
     *              CAND(3),CLUS(9,2),CMAP(10,9),
     *              SADC(32,2),CATAG(192)
C
C
C CWORK - WORKSPACE USED ONLY ONCE PER EVENT FOR INTERNAL PROCESSING
C ==================================================================
C
C MARK   ->  WHICH 'MARK' OF TAGGER - 1 = 1981,2
C                                   - 2 = 1983 ONWARDS
C
C IFLMRK ->  SET TO '1' BY TAGMRK
C
C IMC    ->  SET TO '1' BY TAGMRK IF MC DATA
C
C CATAG  ->  CONTAINS THE ADC CONTENTS UNPACKED FROM ATAG
C
C SADC   ->  COMMON FOR ADC'S AFTER SORTING  (SORT 1)
C
C CMAP(I,1...9) ->  ADDRESS OF ADC'S IN CLUSTER I,SORT23 PUTS THESE IN
C                   ORDER OF ENERGY.
C
C CAND(3) ->  X, Y, AND ENERGY OF A FOUND CLUSTER IN AFTER CLSPS
C
C SIGX,SIGY,SIGEN ->  ERROR ON X, Y, AND ENERGY AFTER CLSPS
C
C CLUS(9,2) ->  ADC ADDRESS AND CONTENTS OF CLUSTERS - SORTED BY ENERGY
C
C NCLST   ->  NUMBER OF CLUSTERS THIS END
C ISTMZ   ->  POINTER TO START OF -Z DATA IN CATAG ( ALWAYS  1       )
C ISTPZ   ->  POINTER TO START OF +Z DATA IN CATAG ( EITHER 33 OR 25 )
C IENDMZ  ->  POINTER TO   END OF -Z DATA IN CATAG ( EITHER 32 OR 24 )
C IENDPZ  ->  POINTER TO   END OF +Z DATA IN CATAG ( EITHER 64 OR 48 )
C
C A.J.FINCH 24/2/84
C MODIFIED 12/3/84 CATAG PUT TO END AND INCREASED TO 192
C  TO ALLOW IT TO BE USED FOR 1979,80 TAGGER IN GRAPHICS
C LAST MOD : J. NYE  30/05/84  RE-ORGANIZED INCLUDING IFLMRK
C
C
C-----------------------------------------------------------------------
C
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
C
       LOGICAL FIRST / .TRUE. /
C
C
C-------------------------- C O D E ------------------------------------
C
C
C
C---                                     GET HEAD AND ATAG POINTERS
C
C
       INHEAD = IDATA( IBLN('HEAD') )
       INATAG = IDATA( IBLN('ATAG') )
       IF ( INHEAD .LE. 0 ) GOTO   1
C
C---                                     GET HEAD BANK DATA
C
       IHHEAD = INHEAD + INHEAD
       NRUNTG = HDATA(IHHEAD + 10)
       NDATE  = HDATA(IHHEAD +  8)
C
       IMC = 0
       IF ( NRUNTG .GT. 0 ) GOTO 200
C
C
C--------------------------------------- MONTE CARLO DATA ONLY ---------
C
C---                                     NRUNTG = 0 => MONTE CARLO DATA
C---                                     SET FLAG FOR TAGKAL ETC.
C
       IF ( NRUNTG .EQ. 0 ) IMC = 1
C
C---                                     IF ATAG BANK EXISTS THEN USE
C---                                     WORD SET BY MC SIMULATION
C
C
       IF ( INATAG .LE. 0 ) GOTO   2
       MCMARK = HDATA( 2 * INATAG + 2 )
       IF ( MCMARK .EQ. 0 ) NRUNTG =  5000
       IF ( MCMARK .EQ. 1 ) NRUNTG = 10000
       IF ( MCMARK .EQ. 2 ) NRUNTG = 15000
       GOTO 200
C
C---                                     ELSE USE SMEARING DATE
C
    2  CONTINUE
       NRUNTG = 10000
       IF ( NDATE .LE. 1980 ) NRUNTG =  5000
       IF ( NDATE .GE. 1983 ) NRUNTG = 15000
C
C
C--------------------------------------- FOR BOTH REAL AND MC DATA -----
C
C
  200  CONTINUE
C
C
C---                                     SET VALUE OF 'MARK'
C
       IF   (NRUNTG.LT. 6000)                           MARK = 0
       IF ( (NRUNTG.GE. 6000) .AND. (NRUNTG.LT.12948) ) MARK = 1
       IF   (NRUNTG.GE.12948)                           MARK = 2
C
C
C---                                     SET IFLMRK = -1 TO FLAG THAT
C---                                     TAGMRK HAS BEEN CALLED
C
       IFLMRK = -1
       FIRST  = .FALSE.
       RETURN
C
C---------------------------------------- R E T U R N   1 --------------
C
C                                        RETURN 1 IF NO HEAD BANK
C
    1  RETURN 1
       END
