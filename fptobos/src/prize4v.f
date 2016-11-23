      SUBROUTINE PRIZE4V(IPZE4V)
*
*   Print the content of the ZE4V bank, with pointer IPZE4V
*   IPPATR points to the 4th word of the BOS header, i.e. to LENGTH
*   (this is BOS convention)   IPZE4V is a pointer in array IEVENT
*      Format, see JCN 99
*
*   Last Update  15.12.2005   J.O.
*
C********** MACRO CEVENT **************
C   Event Buffer
C   The size corresponds to 2 MByte event size, maximum
C
      COMMON /CEVENT/IEVENT(525000)
      DIMENSION REVENT(525000)
      EQUIVALENCE (IEVENT(1),REVENT(1))
*=====================================
*
      CHARACTER CHZE4V*4,CHASCI*4
*
*   J.Olsson   15.12.2005
*
* ----------------  CODE  ----------------------
*
      CHZE4V = CHASCI(IEVENT(IPZE4V-3))
      II = IPZE4V
*
      CALL TWOIN1(IEVENT(II+7),IHALF1,IHALF2)
      IRUN = IHALF1
      IEVN = IHALF2
*
      WRITE(6,1001) CHZE4V,IEVENT(II-2),IEVENT(II-1),IEVENT(II),
     $              IRUN,IEVN
 1001 FORMAT(' ',A4,' ',2I6,'    Length ',I5,' JADE Run/evnt ',2I8)
      WRITE(6,1002)
 1002 FORMAT(' ================================================',
     $       '================')
*
      CALL TWOIN1(IEVENT(II+1),IHALF1,IHALF2)
      WRITE(6,1003) IHALF1,IHALF2
 1003 FORMAT(' Header length ',I8,'    Words per vertex ',I8)
      LH = IHALF1
      LV = IHALF2
*
      CALL TWOIN1(IEVENT(II+2),IHALF1,IHALF2)
      WRITE(6,1004) IHALF1,IHALF2
 1004 FORMAT(' Nr.  vertices ',I8,'   Length of MC info ',I8)
      NV = IHALF1
      LMC = IHALF2
*
      CALL TWOIN1(IEVENT(II+3),IHALF1,IHALF2)
      WRITE(6,1005) IHALF1,IHALF2
 1005 FORMAT(' Len gentrksec ',I8,'  Total nr. of parts ',I8)
      LT = IHALF1
      NPARTS = IHALF2
*
      CALL TWOIN1(IEVENT(II+4),IHALF1,IHALF2)
      WRITE(6,1006) IHALF1,IHALF2
 1006 FORMAT(' Len chatrksec ',I8,'   Nr. charged parts ',I8)
      LCH = IHALF1
      NCH = IHALF2
*
      CALL TWOIN1(IEVENT(II+5),IHALF1,IHALF2)
      WRITE(6,1007) IHALF1,IHALF2
 1007 FORMAT(' Len neutrksec ',I8,'   Nr. neutral parts ',I8)
      LPH = IHALF1
      NPH = IHALF2
*
      CALL TWOIN1(IEVENT(II+6),IHALF1,IHALF2)
      WRITE(6,1008) IHALF1,IHALF2
 1008 FORMAT(' Len prvtrksec ',I8,'   Nr. priv. partics ',I8)
      LPR = IHALF1
      NPR = IHALF2
*
      II = II + 7
*
*  conversion to IEEE FP
*
      DO 2001 J = 1,23
      CALL CNVIBM3E(IEVENT(II+J))
 2001 CONTINUE
*
      WRITE(6,1009) REVENT(II+1)
 1009 FORMAT(' BEAM Energy (GeV) ',F12.6)
*
      WRITE(6,10091)
10091 FORMAT(' Spericity axis (1-3),   Sphericity ')
      WRITE(6,10081) (REVENT(II+J),J=2,5)
10081 FORMAT(' ',4(1X,F12.6))
*
      WRITE(6,10092)
10092 FORMAT(' 2nd spher.axis (1-3),   Value ')
      WRITE(6,10082) (REVENT(II+J),J=6,9)
10082 FORMAT(' ',4(1X,F12.6))
*
      WRITE(6,10093)
10093 FORMAT(' 3rd spher.axis (1-3),   Value ')
      WRITE(6,10083) (REVENT(II+J),J=10,13)
10083 FORMAT(' ',4(1X,F12.6))
*
      WRITE(6,10094)
10094 FORMAT('    Thrust axis (1-3),   Thrust ')
      WRITE(6,10084) (REVENT(II+J),J=14,17)
10084 FORMAT(' ',4(1X,F12.6))
*
      WRITE(6,10095)
10095 FORMAT(' Acoplanar.axis (1-3),   Value ')
      WRITE(6,10085) (REVENT(II+J),J=18,21)
10085 FORMAT(' ',4(1X,F12.6))
*
      WRITE(6,10096)
10096 FORMAT(' (two) Unused words,     MCREDU-flag ')
      WRITE(6,10086) (REVENT(II+J),J=22,23),IEVENT(II+24)
10086 FORMAT(' ',2(1X,F12.6),I10)
*
*   end of header
*
      II = II + 24
*
*   Vertex section
*
      NVLV = NV*LV
      IF(NV.GT.0) THEN
        NVLV = NV*LV
        IF(LV.NE.3) THEN
          WRITE(6,2234) LV
 2234     FORMAT(' WARNING !!   LV_vertex_info NOT 3, but ',I3)
        ENDIF
*
*  conversion to IEEE FP
*
        DO 2002 J = 1,NVLV
        CALL CNVIBM3E(IEVENT(II+J))
 2002   CONTINUE
*        
        DO 2003 IV = 1,NV
        WRITE(6,10097) IV
10097   FORMAT(' x y z  of vertex nr.',I6)
        WRITE(6,10087) (REVENT(II+(IV-1)*LV+J),J=1,LV)
10087   FORMAT(' ',3(1X,F12.6))      
 2003   CONTINUE
*
      ENDIF
*
      II = II + NVLV
*
*  MC special section
*
      IF(LMC.GT.0) THEN      
        CALL TWOIN1(IEVENT(II+1),IHALF1,IHALF2)
        WRITE(6,10077) IHALF1,IHALF2
10077   FORMAT(' Nr. orig_jets (partons)',I8,' orig.q_flavour ',I6)
        NQRK = IHALF1
*
        IF(NQRK.GT.0) THEN
          LQRK = NQRK*4
*
*  conversion to IEEE FP
*
          DO 2004 J = 1,LQRK
          CALL CNVIBM3E(IEVENT(II+J))
 2004  CONTINUE
*          
          DO 2005  IQ = 1,NQRK
          WRITE(6,10067) IQ
10067     FORMAT(' px py pz E   of parton nr.',I6)
          WRITE(6,10066) (REVENT(II+(IQ-1)*4+J),J=1,4)
10066     FORMAT(' ',4(1X,F12.6))      
 2005     CONTINUE
*          
        ENDIF     
      ENDIF
*
*           END of General Section
*
      II = IPZE4V + LH
*
      IF(NPARTS.GT.0) THEN
        IPART = 0
 3000   CONTINUE
        IPART = IPART + 1
        IF(IPART.GT.NPARTS) GO TO 9999        
        WRITE(6,10057) IPART
10057   FORMAT(' Total particle nr. ',I5)
        WRITE(6,10051) 
10051   FORMAT(' ========================')
*
*  conversion to IEEE FP
*
        DO 2006 J = 1,3
        CALL CNVIBM3E(IEVENT(II+J))
 2006   CONTINUE
*        
        WRITE(6,1029)
 1029 FORMAT(' Dir.cosines at DCA of run vertex')
        WRITE(6,1028) (REVENT(II+J),J=1,3)
 1028   FORMAT(' ',3(1X,F12.6))
*
        CALL TWOIN1(IEVENT(II+4),IHALF1,IHALF2)
        WRITE(6,1025) IHALF1,IHALF2
 1025   FORMAT('  0/PATR_nr of PHOT_partn',I8,
     $         ' Part.code*100 + id.type',I5)
*
        CALL TWOIN1(IEVENT(II+5),IHALF1,IHALF2)
        WRITE(6,1026) IHALF1,IHALF2
 1026   FORMAT('  0/Vertex_nr of part._orig ',I8,
     $         '  0/2nd_vertex_nr ',I5)
*
*
        DO 2007 J = 6,7
        CALL CNVIBM3E(IEVENT(II+J))
 2007   CONTINUE
*
        WRITE(6,1031)
 1031   FORMAT(' P_t and Charge ')
        WRITE(6,1032) (REVENT(II+J),J=6,7)
 1032   FORMAT(' ',2(1X,F12.6))
*
        CALL TWOIN1(IEVENT(II+8),IHALF1,IHALF2)
        WRITE(6,1033) IHALF1,IHALF2
 1033   FORMAT('  0/PALL_nr (MC only)',I8,
     $         '  Traceback code (MC only) ',I8)
*
        CALL TWOIN1(IEVENT(II+9),IHALF1,IHALF2)
        WRITE(6,1034) IHALF1,IHALF2
 1034   FORMAT('  0/TPTR_nr ',I8,
     $         '  Filling mode flag of track ',I8)
        MODFIL = IHALF2
*
*   Get length of remaining track info for IPART
*
        IF(MODFIL.EQ.-1) LENNN = LT
        IF(MODFIL.EQ. 2) THEN 
          LENNN = LT + LPR
*
*   Note:  LPR is not considered in this routine
*
          WRITE(6,2221) 
 2221     FORMAT(' PRIZE4V WARNING!!!  Private track area neglected!')  
        ENDIF
* 
        IF(MODFIL.EQ.-1.OR.MODFIL.EQ.2) THEN
          II = II + LENNN
          GO TO 3000
        ENDIF
*
        II = II + LT
        IF(MODFIL.EQ.1) THEN
*                              Charged tracks here
*
*  conversion to IEEE FP
*
          DO 2011 J = 1,3
          CALL CNVIBM3E(IEVENT(II+J))
 2011     CONTINUE
*                    
          WRITE(6,1109)
 1109     FORMAT(' ass_LG_clust_energy  error  0/corr_energy ')   
          WRITE(6,11082) (REVENT(II+J),J=1,3)
11082     FORMAT(' ',3(1X,F12.6))
*
          CALL TWOIN1(IEVENT(II+4),IHALF1,IHALF2)
          WRITE(6,1133) IHALF1,IHALF2
 1133     FORMAT('  Detector Flag (TP_conv.) ',I6,
     $           '  1st_ass_LG_clusnr + 100*2nd_clusnr ',I8)
*
          CALL TWOIN1(IEVENT(II+5),IHALF1,IHALF2)
          WRITE(6,1134) IHALF1,IHALF2
 1134     FORMAT('  PATR_track_nr ',I6,
     $           '  Nrhits_r_phi + 100*Nrhits_r_z ',I8)
*  Note order of r_phi and r_z   Opposite to JCN99, note in MPI')
*
*  conversion to IEEE FP
*
          DO 2012 J = 6,11
          CALL CNVIBM3E(IEVENT(II+J))
 2012  CONTINUE
*
          WRITE(6,1209)
 1209     FORMAT(' xyz_pnt of DCA to runvtx   dE/dx/error    Rmin ')
          WRITE(6,11081) (REVENT(II+J),J=6,11)
11081     FORMAT(' ',6(1X,F12.6))
*
          WRITE(6,1233) IEVENT(II+12)
 1233     FORMAT('  0 / Muon Quality Flag ',I6)
          II = II + LCH
        ENDIF
*
        IF(MODFIL.EQ.0) THEN
*                              Charged tracks here
*
*  conversion to IEEE FP
*
          DO 2013 J = 1,3
          CALL CNVIBM3E(IEVENT(II+J))
 2013     CONTINUE
*                    
          WRITE(6,1309)
 1309     FORMAT(' LG_clus_energy/error   0/corr_clust_energy ')
          WRITE(6,13081) (REVENT(II+J),J=1,3)
13081     FORMAT(' ',3(1X,F12.6))
*
          CALL TWOIN1(IEVENT(II+4),IHALF1,IHALF2)
          WRITE(6,1433) IHALF1,IHALF2
 1433     FORMAT('  LG_Detector_Flag (0,+1,-1) ',I6,
     $           '  LG_cluster_nr ',I6)
*
          CALL TWOIN1(IEVENT(II+5),IHALF1,IHALF2)
          WRITE(6,1434) IHALF1,IHALF2
 1434     FORMAT('  Nr.LG_blocks_ass_totracks ',I6,
     $           '  Nr.LG_blocks_incluster ',I6)
*
          II = II + LPH
        ENDIF
*
        GO TO 3000
*
      ENDIF
*
*-----------------------
 9999 CONTINUE
      RETURN
      END
