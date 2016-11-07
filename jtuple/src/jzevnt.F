      subroutine jzevnt
      implicit none
C
C Fill JADE ntuple from ze4v data
C

#include "xdata.inc"
#include "jtjtpl.inc"
#include "jtstat.inc"
#include "jzffr.inc"

C     Local declarations:
      real vdot

      real mpi
      parameter( mpi=0.140 )

      integer itkdm
      parameter( itkdm=5 )

      logical lacc
      integer i, j, ierr
      integer npatr, nlgcl, ntrkmt, nhitrp, nhitrz, itrkt(maxtrk)
     &     , ntrkvx, ntrkgd, ntrklg, ntrkt, iebee, ndotn, ntrak

      real ppatr(itkdm,maxtrk), plgcl(itkdm,maxtrk)
     &     , ptrkmt(itkdm,maxtrk), psum(4), ptrak(itkdm,maxtrk)
      real zvtrk, rmin, ptot, pxy, eclus, ecorr, eee1, eee2, eeb, rdot
     &     , etot, etotmt
      real thrvec(3,3), thrval(3)

C     General part, some variables are computed from tracks/clusters
C     and are added later:
      irun= ixrun
      ievnt= ixevent
      ebeam= xebeam
      zvert= 999.
      if( ixnver.ge.1 ) zvert= xverz(1)

C     Particle section:
      npatr= 0
      ntrk= 0
      ntrkvx= 0
      ntrkgd= 0
      ntrklg= 0
      nlgcl= 0
      nclus= 0
      nmtcls= 0
      nmtkil= 0
      nmttrk= 0
      eee1= 0.0
      eee2= 0.0
      eeb= 0.0
      etot= 0.0
      etotmt= 0.0
      do i= 1, ixnp
         
C     Tracks:
         if( ixptf(i).eq.1 ) then
            ntottk= ntottk + 1
C     Number of hits in r-phi and r-z according to ze4vpk packing:
            nhitrp= mod(ixhits(i),100)
            nhitrz= int(ixhits(i)/100)
C     zvtrk not present, use xdz(i) instead:
            zvtrk= xdz(i)
            if( lpedro ) then
               rmin= sqrt(xdx(i)**2+xdy(i)**2)
            else
               rmin= xrmin(i)
            endif
            ptot= xp(i)
            pxy= sqrt( xex(i)**2 + xey(i)**2 )*xp(i)
            call hfill( 120, float(nhitrp), 0.0, 1.0 )
            call hfill( 121, float(nhitrz), 0.0, 1.0 )
            call hfill( 122, zvtrk, 0.0, 1.0 )
            call hfill( 123, rmin, 0.0, 1.0 )
            call hfill( 124, ptot, 0.0, 1.0 )
            call hfill( 125, pxy, 0.0, 1.0 )
C     Select tracks and count rejected tracks in a strange way a la ze4vpk.
C     Catch tracks set to ptot= 0.5*ebeam by ze4vpk, they had ptot>ebeam;
C     so rejecting tracks with ptot > 4/3*ebeam (see jtevent.F) does not
C     make sense here:
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
            elseif( abs(ptot-0.5*ebeam).lt.1.0e-5 
     &              .and. .not.lpedro ) then
               lacc= .false.
               nrejpp= nrejpp + 1
            elseif( ptot.lt.pcut ) then
               lacc= .false.
               nrejp= nrejp + 1
            endif
            if( lacc ) then
               npatr= npatr + 1
               if( npatr.le.maxtrk ) then
                  ntrk= npatr
                  ppatr(1,npatr)= xex(i)*xp(i)
                  ppatr(2,npatr)= xey(i)*xp(i)
                  ppatr(3,npatr)= xez(i)*xp(i)
                  ppatr(4,npatr)= sqrt( xp(i)**2 + mpi**2 )
                  call pxcopv( 3, ppatr(1,npatr), ptrk(1,ntrk) )
                  ichg(ntrk)= nint(xch(i))
                  nhcj(ntrk)= nhitrp
                  d0(ntrk)= rmin
                  z0(ntrk)= zvtrk
                  id02(ntrk)= 1
               else
                  print *, 'JZEVNT: npatr > maxtrk in run, event: ',
     &                 irun, ievnt, npatr, maxtrk
               endif
C     Count "vertex" and "good" tracks for event selection:
               if( abs(rmin).lt.30.0 ) then
                  ntrkvx= ntrkvx + 1
                  if( .not.lpedro ) itrkt(ntrkvx)= npatr
               endif
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
            
C     Cluster:
         elseif( ixptf(i).eq.0 ) then
C     Cluster energies before and after JADE matching:
            eclus= xecl(i)
            ecorr= xp(i)
            ntotcl= ntotcl + 1
            call hfill( 132, eclus, 0.0, 1.0 )
            call hfill( 133, ecorr, 0.0, 1.0 )
            if( eclus.gt.elgct ) then
               nlgcl= nlgcl + 1
               if( nlgcl.le.maxtrk ) then
                  nclus= nlgcl
                  plgcl(1,nlgcl)= xex(i)*eclus
                  plgcl(2,nlgcl)= xey(i)*eclus
                  plgcl(3,nlgcl)= xez(i)*eclus
                  plgcl(4,nlgcl)= eclus
                  call pxcopv( 3, plgcl(1,nlgcl), pclus(1,nclus) )
C     Get JADE matching info:
                  if( ecorr.lt.eclus ) then
                     nmtcls= nmtcls + 1
                     imtcls(nmtcls)= nclus
                     mtscfc(nmtcls)= ecorr/eclus
                  endif
               else
                  print *, 'JZEVNT: nlgcl > maxtrk in run, event: ',
     &                 irun, ievnt, nlgcl, maxtrk
               endif
               etot= etot + eclus
               etotmt= etotmt + ecorr
C     Energy sums in barrel and endcaps for event selection, note that
C     the uncorrected cluster energies are summed:
               iebee= ixlgp(i)
               if( iebee.eq.-1 ) then
                  eee1= eee1 + eclus
               elseif( iebee.eq.0 .or. iebee.eq.100 ) then
                  eeb= eeb + eclus
               elseif( iebee.eq.1 ) then
                  eee2= eee2 + eclus
               else
                  print *, 'JZEVNT: funny value of iebee= ', iebee,
     &                 ' in event ', irun, ievnt
               endif
            else
               nrejlg= nrejlg + 1
            endif
 
         endif
      enddo
      ntkd02= ntrk

C     Fill histos:
      call hfill( 101, float(npatr), 0.0, 1.0 )
      call hfill( 103, float(nlgcl), 0.0, 1.0 )
      call hfill( 104, float(nmtcls), 0.0, 1.0 )

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
c                  rdot= vdot( ppatr(1,itrkvx(i)), 
c     &                        ppatr(1,itrkvx(j)), 3 )
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
C     cluster matching:
      call pxcopa( 1, npatr, 4, itkdm, ppatr, itkdm, ptrak )
      call pxcopa( 1, nlgcl, 4, itkdm, plgcl, itkdm, ptrak(1,npatr+1) )
      call pxcopa( 1, npatr, 4, itkdm, ppatr, itkdm, ptrkmt )
      call pxcopa( 1, nlgcl, 4, itkdm, plgcl, itkdm, ptrkmt(1,npatr+1) )
      ntrak= npatr + nlgcl
      ntrkmt= npatr + nlgcl
      do i= 1, nmtcls
         do j= 1, 4
            ptrkmt(j,npatr+imtcls(i))= 
     &           ptrkmt(j,npatr+imtcls(i))*mtscfc(i)
         enddo
      enddo

C     Fill event 4-mom in ntuple, calculate 4-momentum sum from MT list:
      call vzero( psum, 4 )
      do i= 1, ntrkmt
         call pxaddv( 4, ptrkmt(1,i), psum, psum )
      enddo
      call pxcopv( 4, psum, pgce )

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

C     Commit this event to ntuple:
      call hfnt( 10 )

C     The End:
      return
      end
