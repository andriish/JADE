C   01/11/84 705201518  MEMBER NAME  MENU     (S)        M  FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE MENU
C-----------------------------------------------------------------------
C
C        SENDS LIST OF AVAILABLE COMMANDS TO SCANNER.
C
C-----------------------------------------------------------------------
C
      CONTINUE ! CALL CLRCON PMF 03/12/99
C
C
      CALL TRMOUT(80,'      --- G E N E R A L ---^')
C
      CALL TRMOUT(80,
     +'HELP    Detailed information on all commands.^')
      CALL TRMOUT(80,
     +'MENU    Lists available commands.^')
      CALL TRMOUT(80,
     +'NEWS    Lists recent news about graphics.^')
C
      CALL TRMOUT(80,'      --- V I E W S ---^')
C
      CALL TRMOUT(80,
     +'RA      Displays Central Detectors in R-PHI view.^')
      CALL TRMOUT(80,
     +'RB      Displays Central Detectors and LG in R-PHI view.^')
      CALL TRMOUT(80,
     +'RC      Displays Central Detectors, LG and Muon Filter in R-PHI.^
     +')
C
      CALL TRMOUT(80,
     +'ZXA     Displays Central Detectors in Z-X view.^')
      CALL TRMOUT(80,
     +'ZXB     Displays Central Detectors and LG in Z-X view.^')
      CALL TRMOUT(80,
     +'ZXC     Displays Central Detectors, LG and Muon Filter in Z-X.^')
      CALL TRMOUT(80,
     +'ZXD     Displays whole detector in Z-X view.^')
C
      CALL TRMOUT(80,
     +'ZYA     Displays Central Detectors in Z-Y view.^')
      CALL TRMOUT(80,
     +'ZYB     Displays Central Detectors and LG in Z-Y view.^')
      CALL TRMOUT(80,
     +'ZYC     Displays Central Detectors, LG and Muon Filter in Z-Y.^')
      CALL TRMOUT(80,
     +'ZYD     Displays whole detector in Z-Y view.^')
C
      CALL TRMOUT(80,
     +'FW      Displays overall view of Forward Detector.^')
      CALL TRMOUT(80,
     +'RU      Displays rolled-out view of Lead Glass and Forward Detect
     +or.^')
      CALL TRMOUT(80,
     +'RZ      Displays rolled-out view of the Z Chamber (ZC).^')
      CALL TRMOUT(80,
     +'VC      Displays magnified view of the Vertex Chamber.^')
C
      CALL TRMOUT(80,
     +'CYL     Displays perspective view of detector.^')
      CALL TRMOUT(80,
     +'FWMU    Displays Forward Muon Counters.^')
      CALL TRMOUT(80,
     +'VRX     Displays vertex region, R-FI view.^')
      CALL TRMOUT(80,
     +'VRZX    Displays vertex region, Z-X view.^')
      CALL TRMOUT(80,
     +'VRZY    Displays vertex region, Z-Y view.^')
      CALL TRMOUT(80,
     +'STVW    Displays current standard view. Especially if magnified
     + (S).^')
C
      CALL TRMOUT(80,'      --- R E S U L T S ---^')
C
      CALL TRMOUT(80,
     +'RES     Displays ID and LG analysis results.^')
      CALL TRMOUT(80,
     +'VRES    Like RES but with vertex extrapolation.^')
      CALL TRMOUT(80,
     +'MUPT    Displays muon analysis results and can re-analyse.^')
      CALL TRMOUT(80,
     +'MUONS   Displays good muons.^')
      CALL TRMOUT(80,
     +'TR      Displays ID hits with various options. TR1 removes mirror
     + hits.^')
      CALL TRMOUT(80,
     +'ZFIT    Displays results of z-s-fits for given tracks.^')
      CALL TRMOUT(80,
     +'TRUE    Displays original Monte Carlo 4-vectors.^')
      CALL TRMOUT(80,
     +'VX      Displays vertex results.^')
C
      CALL TRMOUT(80,
     +'DEDX    Displays dE/dx results, several options.^')
      CALL TRMOUT(80,
     +'TOF     Displays TOF results, several options.^')
      CALL TRMOUT(80,
     +'QP      Displays Q-plots with several options.^')
      CALL TRMOUT(80,
     +'AX      Displays the event jet-axes.^')
C
      CALL TRMOUT(80,
     +'ZV      Displays Z vertex histogram with your assistance.^')
      CALL TRMOUT(80,
     +'FAMP    Displays results of FAMP-found tracks.^')
      CALL TRMOUT(80,
     +'ND50    Displays results of NORD 50-found tracks.^')
      CALL TRMOUT(80,
     +'VAC     Displays amplitudes on signal wires in any cell of Vertex
     + Chamber^')
      CALL TRMOUT(80,
     +'J68K    Displays amplitudes on signal wires in any cell of Jet Ch
     +amber   ^')
      CALL TRMOUT(80,
     +'FADC    Displays Jet Chamber test wire Flash-ADC values.^')
      CALL TRMOUT(80,
     +'ZTRG    Displays Z-trigger Flash-ADC values (test data only).^')
      CALL TRMOUT(80,
     +'CLUS    Displays Lead Glass clusters. RES is often better.^')
      CALL TRMOUT(80,
     +'MUR2    Like RES but shows full muon info in track list.^')
C
      CALL TRMOUT(80,'      --- E X T R A S ---^')
C
      CALL TRMOUT(80,
     +'DET     Displays the detector in current view.^')
      CALL TRMOUT(80,
     +'PRO     Displays projections of the 2 orthogonal views.^')
      CALL TRMOUT(80,
     +'EC      Displays End Cap hits in R-FI views^')
      CALL TRMOUT(80,
     +'FC      Displays Tagging System hits in RFI views^')
      CALL TRMOUT(80,
     +'TRLG    Displays Lead Glass trigger conditions^')
      CALL TRMOUT(80,
     +'TRG2    Displays T2 trigger conditions.^')
      CALL TRMOUT(80,
     +'BL      Displays dead,killed and spinning LG blocks.^')
      CALL TRMOUT(80,
     +'COM     Add comment to the picture; appears on the hard copy.^')
C
      CALL TRMOUT(80,'      --- C O N T R O L ---^')
C
      CALL TRMOUT(80,
     +'N       Goes to next event WITHOUT writing (NN).^')
      CALL TRMOUT(80,
     +'CSTL    Changes the "stop flag" settings in USER  (LEVELS).^')
      CALL TRMOUT(80,
     +'CSTV    Changes the standard view and auto-display of event (CS).
     +^')
      CALL TRMOUT(80,
     +'CDTL    Changes any one of the many drawing flags  (OPT).^')
      CALL TRMOUT(80,
     +'CDST    Displays status of the drawing "CDTL" flags  (STAT).^')
C
      CALL TRMOUT(80,
     +'WRIT    Writes current event and goes to next.^')
      CALL TRMOUT(80,
     +'MORE    Switches to a new input dataset. The dsname is prompted f
     +or  (NEW).^')
      CALL TRMOUT(80,
     +'STOP    Stops the program  (EXIT, END, QUIT).^')
      CALL TRMOUT(80,
     +'FIND    Locates event by Run and Event number which are prompted
     +for.^')
C
      CALL TRMOUT(80,
     +'JOYS    Changes scale and/or origin of display. CNTRL-E ends inpu
     +t  (ZOOM)^')
      CALL TRMOUT(80,
     +'RS      Returns to standard scale and origin  (RESET).^')
      CALL TRMOUT(80,
     +'PATR    Selects number of PATR bank to be used.^')
      CALL TRMOUT(80,
     +'JETC    Selects number of JETC bank to be used.^')
      CALL TRMOUT(80,
     +'VTXC    Selects number of VTXC bank to be used.^')
      CALL TRMOUT(80,
     +'C       Continues to next USER level with "stop flag" set.^')
      CALL TRMOUT(80,
     +'JUMP    Causes a jump to specified SUPERVISOR level.^')
C
      CALL TRMOUT(80,'      --- M A C R O S ---^')
C
      CALL TRMOUT(80,
     +'MACRO   Creates a macro command sequence.^')
      CALL TRMOUT(80,
     +'EDITMAC Edits   a user-defined macro.^')
      CALL TRMOUT(80,
     +'DELMAC  Deletes a user-defined macro.^')
      CALL TRMOUT(80,
     +'RENAMAC Renames a user-defined macro.^')
C
      CALL TRMOUT(80,'      --- O T H E R S ---^')
C
      CALL TRMOUT(80,
     +'H       Generates a Hard Copy of the display  (Q).^')
      CALL TRMOUT(80,
     +'HX      Generates an External Hard Copy  (QX).^')
      CALL TRMOUT(80,
     +'HLP     Generates a Laser Printer Hard Copy  (LASER).^')
      CALL TRMOUT(80,
     +'MASS    Computes effective mass of a group of particles.^')
      CALL TRMOUT(80,
     +'SPVA    Users own command, no action in JADEZ.^')
C
      CALL TRMOUT(80,
     +'BW      Writes out BOS banks to screen  (PRINT).^')
      CALL TRMOUT(80,
     +'BDLS    Deletes BOS banks as requested  (DELETE).^')
      CALL TRMOUT(80,
     +'DRAW    Invokes drawing routine for circles, lines and points.^')
      CALL TRMOUT(80,
     +'PICK    Returns coordinates of point selected by joystick.^')
      CALL TRMOUT(80,
     +'GVTX    Performs vertex fit of chosen tracks  (VFIT).^')
      CALL TRMOUT(80,
     +'NWCL    Recalibrates Lead Glass and JETC data  (RECAL).^')
      CALL TRMOUT(80,
     +'EDIT    Invokes PATREC editor subsystem.^')
      CALL TRMOUT(80,
     +'RET     Return to PATREC editor subsystem.^')
      CALL TRMOUT(80,
     +'SAVE    Saves PATREC /CWORK/ onto a scratch file.^')
C
      RETURN
      END
