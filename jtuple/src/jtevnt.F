      subroutine jtevnt( ievt )
      implicit none
      integer ievt

#include "bos.inc"
#include "jtffr.inc"
#include "jtstat.inc"
#include "jtjtpl.inc"

C     Local declarations:
      real vdot
      integer itkdm
      parameter( itkdm=5 )
      real mpi, elgmin
      parameter( mpi=0.140, elgmin=0.350 )
      logical lacc, lhad, lpart
      integer nphead, nph2, nppatr, nplgcl, npvect, nppall, nword
      integer i, j, ntrak, ntr, ipstrt, ipend, nwptrk
     &     , nhitrp, nhitrz, ip1, ip2, ip3, nwpcl, nlgcl
     &     , iconn, nclst, ntrkvx, ntrkgd, ntrklg, ntrkt, ndotn
     &     , iebee, ntrvx, npatr, ierr, ntrkmt, ihchge(maxtrk)
     &     , ihtype(maxtrk), iflav, nvect, nhad, npart
     &     , ipchge(maxtrk), iptype(maxtrk), ipar(maxtrk)
     &     , ipdec(maxtrk), npall, ipart, npisr, imtst(maxtrk), icall
     &     , itrkt(maxtrk)
      real ptrak(itkdm,maxtrk), plgcl(itkdm,maxtrk)
     &     , ppatr(itkdm,maxtrk), ptrkmt(itkdm,maxtrk)
     &     , phad(itkdm,maxtrk), ppart(itkdm,maxtrk)
     &     , ppall(itkdm,maxtrk), ppisr(itkdm,maxtrk)
      real r(3), rdoca(3), pmom(3), charge, cap, rmin, phim, sig, ptot
     &     , eclus, ecorr, dzds, pxy, eee1, eee2, eeb
     &     , rdot, psum(4), pmiss, thrval(3), thrvec(3,3), costt, minv
     &     , minvh, etotlg, geclus, pzbal, evis, zvtrk, etest, etot
     &     , etotmt

      data icall / 0 /

C     Increment call counter:
      icall= icall + 1

C     Run and event number:
      nphead= iw( ibln( 'HEAD' ) )
      if( nphead.gt.0 ) then
         nph2= 2*nphead
         irun= hw(nph2+10)
         ievnt= hw(nph2+11)
         ebeam= hw(nph2+29)
         ebeam= ebeam/1000.0
      else
         print *, 'JTEVNT: BOS bank HEAD not present'
         irun= 0
         ievnt= 0
      endif

C     Message:
      if( mod(ievt,50).eq.0 ) 
     &     print *, 'JTEVNT: process event (run, event)'
     &     , ievt, ' (', irun, ', ', ievnt, ')'

C     Write selected BOS banks to file (a la ze4vjb):
      if( lbankw .and. icall.le.nbankw .and. nphead.gt.0 ) then
         write( iobank, '(2x,a,6i8)' ) 'Date', (hw(nph2+i), i= 3, 8 )
         write( iobank, '(3(2x,a,i8))' ) 'Run', hw(nph2+10)
     &        , ' Event', hw(nph2+11), ' Type:', hw(nph2+12) 
         write( iobank, '(2x,a,i8)' ) 'Ebeam', hw(nph2+29)
         call showb( iobank, cbank )
      endif

C     Get PATR bank, loop over tracks and fill arrays:
      ntrk= 0
      nmttrk= 0
      npatr= 0
      ntrkgd= 0
      ntrkvx= 0
      ntrklg= 0
      nppatr= iw( ibln( 'PATR' ) )
      if( nppatr.gt.0 ) then
C     Check number of tracks in PATR bank:
         ntr= iw( nppatr + 2 )
         call hfill( 100, float(ntr), 0.0, 1.0 )
         if( ntr.le.0 ) then
            print *, 'JTEVNT: ntr <= 0, no track processing event '
     &           , irun, ievnt
            goto 100
         endif
         if( icall.le.nwvect ) 
     &        print *, 'JTEVNT: selected patr tracks event:'
     &        , irun, ievnt
C     Calculate event vertex:
         call ze4vvx( r, ntrvx )
         zvert= r(3)
C     Loop over tracks in PATR bank:
         nword= iw(nppatr+3)
         ipstrt= iw(nppatr+1) + nppatr
         ipend= ipstrt + (ntr-1)*nword
         do i= ipstrt, ipend, nword
C     Calculate track cut variables:
            ntottk= ntottk + 1
            nhitrp= iw(i+24)
            nhitrz= iw(i+33)
            zvtrk= rw(i+31)
            call prtoci( i, cap, rmin, phim, sig )
            call crdoca( i, r, pmom, rdoca, charge )
            if( lpedro ) then
               rmin= sqrt( rdoca(1)**2 + rdoca(2)**2 )
            endif
            ptot= sqrt( pmom(1)**2 + pmom(2)**2 + pmom(3)**2 )
            pxy= sqrt( pmom(1)**2 + pmom(2)**2 )
            call hfill( 120, float(nhitrp), 0.0, 1.0 )
            call hfill( 121, float(nhitrz), 0.0, 1.0 )
            call hfill( 122, zvtrk, 0.0, 1.0 )
            call hfill( 123, rmin, 0.0, 1.0 )
            call hfill( 124, ptot, 0.0, 1.0 )
            call hfill( 125, pxy, 0.0, 1.0 )
C     Select tracks and count rejected tracks in a strange way a la ze4vpk:
C     Reject tracks with ptot > 4/3ebeam in addition, but for ze4v
C     compatibility reject tracks with ptot > ebeam.  In "Pedro" mode
C     accept such tracks but set their momentum to 0.5*ebeam
            lacc= .true.
            if( nhitrp.lt.nhrpct ) then
               lacc= .false.
               nrejrp= nrejrp + 1
            elseif( nhitrz.lt.nhrzct ) then
               lacc= .false.
               nrejrz= nrejrz + 1
            elseif( abs(zvtrk).gt.zvct ) then
               lacc= .false.
               nrejzv= nrejzv + 1
            elseif( abs(rmin).gt.rminct ) then
               lacc= .false.
               nrejrm= nrejrm + 1
            elseif( ptot.lt.pcut ) then
               lacc= .false.
               nrejp= nrejp + 1
            elseif( ptot.gt.ebeam ) then
               if( lze4v ) then
                  if( lpedro ) then
                     do j= 1, 3
                        pmom(j)= pmom(j)*ebeam*0.5/ptot
                     enddo
                     ptot= ebeam*0.5
                  else
                     lacc= .false.
                     nrejp= nrejp + 1
                  endif
               elseif( ptot.gt.4.0/3.0*ebeam ) then
                  lacc= .false.
                  nrejp= nrejp + 1
               endif
            endif
            if( lacc ) then
               npatr= npatr + 1
               ntrk= ntrk + 1
               if( npatr.le.maxtrk ) then
                  call pxcopv( 3, pmom, ppatr(1,npatr) )
                  ppatr(4,npatr)= sqrt( ptot**2 + mpi**2 )
                  call pxcopv( 3, pmom, ptrk(1,ntrk) )
                  ichg(ntrk)= nint(charge)
                  nhcj(ntrk)= nhitrp
                  z0(ntrk)= zvtrk
                  d0(ntrk)= rmin
                  id02(ntrk)= 1
                  if( icall.le.nwvect ) then
                     print 101, (ppatr(j,npatr),j=1,4), nint(charge)
     &                    , rmin
                  endif
               else
                  print *, 'JTEVNT: more than ', maxtrk, 
     &                 ' selected tracks in event ', irun, ievnt
               endif
C     Count "vertex" and "good" tracks for event selection:
               if( abs(rmin).lt.30.0 ) then
                  ntrkvx= ntrkvx + 1
                  if( .not.lpedro ) itrkt(ntrkvx)= npatr
               endif

c               if( pxy.gt.0.500 .and. nhitrp.gt.24 ) ntrkgd= ntrkgd + 1

               if( lpedro ) then
                  if( pxy.gt.0.500 .and. (nhitrp.ge.24 
     &                 .or. nhitrz.ge.24) ) ntrkgd= ntrkgd + 1
                  if( (nhitrp.ge.24 .or. nhitrz.ge.24) ) then
                     ntrklg= ntrklg + 1
                     itrkt(ntrklg)= npatr
                  endif
               else
                  if( pxy.gt.0.500 .and. nhitrp.gt.24 ) 
     &                 ntrkgd= ntrkgd + 1
               endif

            endif
         enddo
         call hfill( 101, float(npatr), 0.0, 1.0 )
      else
         print *, 'JTEVNT: BOS bank PATR not present event ', irun
     &        , ievnt
      endif
 100  continue
 101  format( 4(1x,f6.3),1x,i2,1x,f6.3 )

C     Fill ntuple variables for track based event selection variables:
      inttr= ntrkvx
      iltr= ntrkgd
      call hfill( 108, float(ntrkvx), 0.0, 1.0 )
      call hfill( 109, float(ntrkgd), 0.0, 1.0 )

C     In case of 4 tracks check for 1- vs 3-prong configurations:
C     npatr, ntrkvx or ntrkgd? Choose "vertex" tracks:
      itauct= 1
      ntrkt= ntrkvx
      if( lpedro ) ntrkt= ntrklg
c      if( ntrkvx.eq.4 ) then
      if( ntrkt.eq.4 ) then
         do i= 1, 4
            ndotn= 0
            do j= 1, 4
               if( i.ne.j ) then
                  rdot= vdot( ppatr(1,itrkt(i)), 
     &                        ppatr(1,itrkt(j)), 3 )
                  if( rdot.lt.0.0 ) ndotn= ndotn + 1
               endif
            enddo
            if( ndotn.eq.3 ) then
               itauct= 0
               goto 300
            endif
         enddo
      endif
 300  continue

C     Get LGCL bank and select clusters:
      eee1= 0.0
      eee2= 0.0
      eeb= 0.0
      etotlg= 0.0
      etot= 0.0
      etotmt= 0.0
      nlgcl= 0
      nclus= 0
      nmtcls= 0
      nmtkil= 0
      nplgcl= iw( ibln( 'LGCL' ) )
      if( nplgcl.gt.0 ) then
C     Pointers into LGCL bank sections:
         ip1= iw(nplgcl+1)
         ip2= iw(nplgcl+2)
         ip3= iw(nplgcl+3)
C     Check number of clusters in bank:
         nclst= iw(nplgcl+ip1+2)
         call hfill( 102, float(nclst), 0.0, 1.0 )
         if( nclst.le.0 ) then
            print *, 'JTEVNT: nclst <= 0, no cluster processing event '
     &           , irun, ievnt
            goto 200
         endif
         if( icall.le.nwvect )
     &        print *, 'JTEVNT: selected lgcl clusters event:'
     &        , irun, ievnt
C     Get "general information" for cross checks:
         geclus= rw(nplgcl+ip1+6)
C     Loop over clusters (watch japanese pointer handling JCN 14D):
         nword= iw(nplgcl+ip1+20)
         ipstrt= nplgcl + ip3 - 1
         ipend= ipstrt + (nclst-1)*nword
         do i= ipstrt, ipend, nword
C     Energy and track connection info:
            ntotcl= ntotcl + 1
            eclus= rw(i+2)
            etotlg= etotlg + eclus
            iconn= iw(i+8)
            call hfill( 132, eclus, 0.0, 1.0 )
C     Cluster energy correction a la ze4vpk for connected tracks incl.
C     path length dependence for barrel clusters (JADE cluster matching):
            dzds= 1.0
            if( iw(i+1).eq.0 ) dzds= 1.0/sqrt(1.0-rw(i+11)**2)
            ecorr= eclus - elgmin*(iconn/100)*dzds
            if( ecorr.gt.0.0 ) call hfill( 133, ecorr, 0.0, 1.0 )
C     Select clusters before matching (eclus).  However, remove clusters
C     with ecorr < elgct (matched clusters) from cluster array for
C     compatibility with ze4v-based data analysis if demanded:
            etest= eclus
            if( lze4v ) etest= ecorr
            if( etest.gt.elgct ) then
               nlgcl= nlgcl + 1
               nclus= nclus + 1
               etot= etot + eclus
               if( ecorr.gt.elgct ) etotmt= etotmt + ecorr
               do j= 1, 3
                  plgcl(j,nlgcl)= rw(i+8+j)*eclus
                  pclus(j,nlgcl)= plgcl(j,nlgcl)
               enddo
               plgcl(4,nlgcl)= eclus
               imtst(nlgcl)= 0
C     Matched clusters with corrected energy, store index into
C     array plgcl and correction factor for surviving clusters,
C     for rejected clusters store index:
               if( iconn.ne.0 ) then
                  if( ecorr.ge.elgct ) then
                     nmtcls= nmtcls + 1
                     imtcls(nmtcls)= nlgcl
                     mtscfc(nmtcls)= ecorr/eclus
                     imtst(nclus)= nmtcls
                  else
                     nmtkil= nmtkil + 1
                     imtkil(nmtkil)= nlgcl
                     imtst(nclus)= -1
                  endif
               endif
               if( icall.le.nwvect ) then
                  print 201, (plgcl(j,nlgcl),j=1,4), ecorr, iw(i+1)
               endif
            else
               nrejlg= nrejlg + 1
            endif
C     Energy sums in barrel and endcaps for event selection, sum 
C     uncorrected cluster energies for clusters selected after matching
C     (akward, but corresponds to JADE ze4v based event selection):
            if( ecorr.ge.elgct ) then
               iebee= iw(i+1)
               if( iebee.eq.-1 ) then
                  eee1= eee1 + eclus
               elseif( iebee.eq.0 ) then
                  eeb= eeb + eclus
               elseif( iebee.eq.1 ) then
                  eee2= eee2 + eclus
               else
                  print *, 'JTEVNT: funny value of iebee= ', iebee,
     &                 ' in event ', irun, ievnt
               endif
            endif
         enddo
         if( icall.le.nwvect )
     &        print *, 'JTEVT: LGCL Etot: ', geclus, etotlg
         call hfill( 103, float(nlgcl), 0.0, 1.0 )
         call hfill( 104, float(nmtcls), 0.0, 1.0 )
      else
         print *, 'JTEVNT: BOS bank LGCL not present event ', irun
     &        , ievnt
      endif
 200  continue
 201  format( 5(1x,f6.3),1x,i3 )

C     Fill ntuple with cluster based event selection variables:
      eb= eeb
      ee(1)= eee1
      ee(2)= eee2
      call hfill( 105, eee1, 0.0, 1.0 )
      call hfill( 106, eee2, 0.0, 1.0 )
      call hfill( 107, eeb, 0.0, 1.0 )
      call hfill( 130, etot, 0.0, 1.0 )
      call hfill( 131, etotmt, 0.0, 1.0 )

C     Fill arrays with all selected particles before and after JADE 
C     cluster matching and calculate 4-momentum sum from MT list:
      call pxcopa( 1, npatr, 4, itkdm, ppatr, itkdm, ptrak )
      call pxcopa( 1, npatr, 4, itkdm, ppatr, itkdm, ptrkmt )
      call pxcopa( 1, nlgcl, 4, itkdm, plgcl, itkdm, ptrak(1,npatr+1) )
      ntrak= npatr + nlgcl
      ntrkmt= npatr
      do i= 1, nlgcl
         if( imtst(i).eq.0 ) then
            ntrkmt= ntrkmt + 1
            call pxcopv( 4, plgcl(1,i), ptrkmt(1,ntrkmt) )
         elseif( imtst(i).gt.0 .and. imtst(i).le.nlgcl ) then
            ntrkmt= ntrkmt + 1
            do j= 1, 4
               ptrkmt(j,ntrkmt)= plgcl(j,i)*mtscfc(imtst(i))
            enddo
         endif
      enddo
C     Fill event 4-mom in ntuple:
      call vzero( psum, 4 )
      do i= 1, ntrkmt
         call pxaddv( 4, ptrkmt(1,i), psum, psum )
      enddo
      call pxcopv( 4, psum, pgce )

      print *, 'PGCE', ievnt, (pgce(i),i=1,4)

C     cos(Theta_T), put thrust vector in ntuple:
      if( ntrkmt.ge.2 ) then
         call pxlth4( ntrkmt, itkdm, ptrkmt, thrval, thrvec, ierr )
         if( ierr.eq.0 ) then
            call pxcopv( 3, thrvec(1,3), tvectc )
         else
            print *, 'JTEVNT: pxlth4 failed, reject event ', irun, ievnt
            call vzero( tvectc, 3 )
         endif
      endif

C     Do event selection and count statistics:
      call jtevsl( lacc )

C     Tracks and all clusters:
      call qqevsh( itkdm, ntrak, ptrak
     &     ,tdtc, tmadtc, tmidtc, mhdtc, mldtc, btdtc, bwdtc
     &     ,cpdtc, dpdtc, sdtc, adtc, acpdtc )
      call qqjets( itkdm, ntrak, ptrak, ijade, maxjet, nxjetc, yedtc )
      call qqjets( itkdm, ntrak, ptrak, idurhm, maxjet, nxjdtc, yddtc )
      if( langl ) call qqangl( bzdtc, kswdtc, nrdtc, c34dtc )
      call qqjets( itkdm, ntrak, ptrak, icambr, maxjet, nxjctc, ycdtc )
      call qqcone( itkdm, ntrak, ptrak, maxjet, nbc, xcr, xce
     &     , 2.0*ebeam, njcedtc, njcrdtc )

C     Tracks and clusters after JADE matching:
      call qqevsh( itkdm, ntrkmt, ptrkmt
     &     ,tdmt, tmadmt, tmidmt, mhdmt, mldmt, btdmt, bwdmt
     &     ,cpdmt, dpdmt, sdmt, admt, acpdmt )
      call qqjets( itkdm, ntrkmt, ptrkmt, ijade, maxjet, nxjemt, yedmt )
      call qqjets( itkdm, ntrkmt, ptrkmt, idurhm, maxjet, nxjdmt
     &     , yddmt )
      if( langl ) call qqangl( bzdmt, kswdmt, nrdmt, c34dmt )
      call qqjets( itkdm, ntrkmt, ptrkmt, icambr, maxjet, nxjcmt
     &     , ycdmt )
      call qqcone( itkdm, ntrkmt, ptrkmt, maxjet, nbc, xcr, xce
     &     , 2.0*ebeam, njcedmt, njcrdmt )

C     Tracks only:
      call qqevsh( itkdm, npatr, ppatr
     &     ,tdt, tmadt, tmidt, mhdt, mldt, btdt, bwdt
     &     ,cpdt, dpdt, sdt, adt, acpdt )
      call qqjets( itkdm, npatr, ppatr, ijade, maxjet, nxjet, yedt )
      call qqjets( itkdm, npatr, ppatr, idurhm, maxjet, nxjdt, yddt )
      if( langl ) call qqangl( bzdt, kswdt, nrdt, c34dt )
      call qqjets( itkdm, npatr, ppatr, icambr, maxjet, nxjct, ycdt )
      call qqcone( itkdm, npatr, ppatr, maxjet, nbc, xcr, xce, 2.0*ebeam
     &     , njcedt, njcrdt )

C     Clusters only:
      call qqevsh( itkdm, nlgcl, plgcl
     &     ,tdc, tmadc, tmidc, mhdc, mldc, btdc, bwdc
     &     ,cpdc, dpdc, sdc, adc, acpdc )
      call qqjets( itkdm, nlgcl, plgcl, ijade, maxjet, nxjec, yedc )
      call qqjets( itkdm, nlgcl, plgcl, idurhm, maxjet, nxjdc, yddc )
      if( langl ) call qqangl( bzdc, kswdc, nrdc, c34dc )
      call qqjets( itkdm, nlgcl, plgcl, icambr, maxjet, nxjcc, ycdc )
      call qqcone( itkdm, nlgcl, plgcl, maxjet, nbc, xcr, xce, 2.0*ebeam
     &     , njcedc, njcrdc )

C     Monte Carlo part:

C     Get VECT bank, contains all "stable" particles before
C     decays in the detector and without neutrinos:
      nhad= 0
      call vzero( psum,4 )
      npvect= iw(ibln( 'VECT' ))
      if( npvect.gt.0 ) then
C     Flavour:
         iflav= abs(iw(npvect+9))
C     Get number of entries and loop:
         nvect= iw(npvect+4)
         if( nvect.le.0 ) then
            print *, 'JTEVNT: nvect <= 0, no hadron processing event '
     &           , irun, ievnt
            goto 400
         endif
         nword= iw(npvect+2)
         ip1= iw(npvect+1)
         ipstrt= npvect + ip1
         ipend= ipstrt + (nvect-1)*nword
         if( icall.le.nwvect ) 
     &        print *, 'JTEVNT: VECT bank event ', irun, ievnt
         do i= ipstrt, ipend, nword
            nhad= nhad+1
            do j= 1, 4
               phad(j,nhad)= rw(i+j)
               psum(j)= psum(j) + rw(i+j)
            enddo
            ihchge(nhad)= iw(i+6)
            ihtype(nhad)= iw(i+7)
            if( icall.le.nwvect ) then
               print 310, nhad, (phad(j,nhad),j=1,4)
     &              , ihchge(nhad), ihtype(nhad)
            endif
         enddo
      else
         print *, 'JTEVNT: BOS bank VECT not present event ', irun
     &        , ievnt
      endif
      if( icall.le.nwvect ) print 320, (psum(i),i=1,4), iflav
 310  format(i3,4(1x,f7.3),2(1x,i3))
 320  format(3x,4(1x,f7.3),1x,i3)
 400  continue

C     PALL bank:
      npart= 0
      nhad= 0
      npisr= 0
      nppall= iw( ibln( 'PALL' ) )
      nprimf= 0
      if( nppall.gt.0 ) then
C     Flavour:
         iflav= abs(iw(nppall+9))
C     Get number of entries and loop:
         npall= iw(nppall+4)
         if( npall.le.0 ) then
            print *, 'JTEVNT: npall <= 0, no particle processing',
     &           'event ', irun, ievnt
            goto 500
         endif
         if( icall.le.nwpall ) then
            print *, 'JTEVNT: PALL bank event ', irun, ievnt
            print *, 
     &           'i    px      py      pz      E       m       q',
     &           '    typ   ipar idec'
         endif
         nword= iw(nppall+2)
         ip1= iw(nppall+1)
         ipstrt= nppall + ip1
         ipend= ipstrt + (npall-1)*nword
         lhad= .false.
         lpart= .false.
         npall= 0
         do i= ipstrt, ipend, nword
            npall= npall + 1
C     Get charge, type and decay info:
            ipchge(npall)= iw(i+6)
            iptype(npall)= iw(i+7)
            ipar(npall)= iw(i+8)
            ipdec(npall)= iw(i+9)
            do j= 1, 5
               ppall(j,npall)= rw(i+j)
            enddo
C     Get primary fermions which come from (Z0/gamma)^*:
            if( int(iptype(npall)/100).eq.5 
     &           .and. ipar(npall).gt.0 ) then
               if( iptype(ipar(npall)).eq.100023 ) then
                  nprimf= nprimf + 1
                  if( nprimf.le.maxprf ) then
                     iferid(nprimf)= iptype(npall)-500
                     call pxcopv( 4, ppall(1,npall), primf(1,nprimf) )
                  endif
               endif
            endif
C     Collect ISR:
            if( ipar(npall).eq.0 .and. iptype(npall).eq.1 ) then
               npisr= npisr + 1
               call pxcopv( 4, ppall(1,npall), ppisr(1,npisr) )
            endif
C     Set flags for selection of final state partons incl. FSR 
C     and "stable" hadrons:
            if( iptype(npall).eq.100023 .and. ipdec(npall).gt.0 )
     &           lpart= .true.
c            if( iptype(npall).eq.100091 .or. 
c     &           iptype(npall).eq.100092 ) then
            if( iptype(npall).ne.100023 .and. iptype(npall).ne.1 
     &           .and. int(iptype(npall)/100).ne.5 ) then
               lhad= .true.
               lpart= .false.
            endif
C     Partons incl. FSR have a parent, ISR are orphans.
C     Also add FSR to hadron list and collect ISR.
C     Fill ID info according to modern scheme:
            if( lpart .and. iptype(npall).ne.100023 ) then
               if( ipar(npall).gt.0 ) then
                  if( iptype(ipar(npall)).ne.100023 ) then
                     npart= npart + 1
                     if( int(iptype(npall)/100).eq.5 ) then
                        ilucp(npart)= iptype(npall)-500
                        if( ilucp(npart).eq.0 ) ilucp(npart)= 21
                     elseif( iptype(npall).eq.1 ) then
                        ilucp(npart)= 22
                     endif
                     call pxcopv( 4, ppall(1,npall), ppart(1,npart) )
                     if( iptype(npall).eq.1 ) then
                        nhad= nhad + 1
                        call pxcopv( 4, ppall(1,npall), phad(1,nhad) )
                        iluch(nhad)= 22
                     endif
                  endif
               endif
            endif
C     Hadrons which don't have a pointer to decay products are stable:
            if( lhad .and. ipdec(npall).eq.0 ) then
               nhad= nhad + 1
               call pxcopv( 4, ppall(1,npall), phad(1,nhad) )
               iluch(nhad)= iptype(npall)
            endif
c     Print PALL bank contents:
            if( icall.le.nwpall ) then
               print 410, npall, (ppall(j,npall),j=1,5)
     &              , ipchge(npall), iptype(npall)
     &              , ipar(npall), ipdec(npall)
            endif
         enddo
C     Copy 4-vectors of partons and hadrons to ntuple and
C     calculate 4-mom sums:
         ntrkp= npart
         call pxcopa( 1, npart, 4, itkdm, ppart, 4, ptrkp )
         ntrkh= nhad
         call pxcopa( 1, nhad, 4, itkdm, phad, 4, ptrkh )
         call vzero( psum, 4 )
         do i= 1, npart
            call pxaddv( 4, ppart(1,i), psum, psum )
         enddo
         minv= sqrt( psum(4)**2-psum(1)**2-psum(2)**2-psum(3)**2 )
         if( icall.le.nwpall ) print 420, (psum(i),i=1,4), minv
     &        , '(partons)'
         do i= 1, npisr
            call pxaddv( 4, ppisr(1,i), psum, psum )
         enddo
         minv= sqrt( psum(4)**2-psum(1)**2-psum(2)**2-psum(3)**2 )
         if( icall.le.nwpall ) print 420, (psum(i),i=1,4), minv
     &        , '(partons+ISR)'
         call vzero( psum, 4 )
         do i= 1, nhad
            call pxaddv( 4, phad(1,i), psum, psum )
         enddo
C     Get 4-vector from hadron system for ntuple:
         call pxcopv( 4, psum, pisr )
         minvh= sqrt( psum(4)**2-psum(1)**2-psum(2)**2-psum(3)**2 )
         if( icall.le.nwpall ) print 420, (psum(i),i=1,4), minvh
     &        , '(hadrons)'
         do i= 1, npisr
            call pxaddv( 4, ppisr(1,i), psum, psum )
         enddo
         minv= sqrt( psum(4)**2-psum(1)**2-psum(2)**2-psum(3)**2 )
         if( icall.le.nwpall ) print 420, (psum(i),i=1,4), minv
     &        , '(hadrons+ISR)'
      else
         print *, 'JTEVNT: BOS bank PALL not present event ', irun
     &        , ievnt
      endif
 410  format(i3,5(1x,f7.3),1x,i3,1x,i7,2(1x,i4))
 420  format(3x,5(1x,f7.3),1x,a15)
 500  continue

C     Count non-radiative events:
      inonr= 0
      if( minvh.gt.2.0*ebeam-1.0 ) then
         nnonr= nnonr + 1
         inonr= 1
         if( lacc ) ndnonr= ndnonr + 1
      endif

C     Parton-level observables:
      call qqevsh( itkdm, npart, ppart
     &     ,tp, tmap, tmip, mhp, mlp, btp, bwp
     &     ,cpp, dpp, sp, ap, acpp )
      call qqjets( itkdm, npart, ppart, ijade, maxjet, nxjep, yep )
      call qqjets( itkdm, npart, ppart, idurhm, maxjet, nxjdp, ydp )
      if( langl ) call qqangl( bzp, kswp, nrp, c34p )
      call qqjets( itkdm, npart, ppart, icambr, maxjet, nxjcp, ycp )
      call qqcone( itkdm, npart, ppart, maxjet, nbc, xcr, xce
     &     , 2.0*ebeam, njcep, njcrp )

C     Hadron-level observables:
      call qqevsh( itkdm, nhad, phad
     &     ,th, tmah, tmih, mhh, mlh, bth, bwh
     &     ,cph, dph, sh, ah, acph )
      call qqjets( itkdm, nhad, phad, ijade, maxjet, nxjeh, yeh )
      call qqjets( itkdm, nhad, phad, idurhm, maxjet, nxjdh, ydh )
      if( langl ) call qqangl( bzh, kswh, nrh, c34h )
      call qqjets( itkdm, nhad, phad, icambr, maxjet, nxjch, ych )
      call qqcone( itkdm, nhad, phad, maxjet, nbc, xcr, xce
     &     , 2.0*ebeam, njceh, njcrh )


C     Commit this event to ntuple:
      call hfnt( 10 )

C     The End:
 999  return
      end
