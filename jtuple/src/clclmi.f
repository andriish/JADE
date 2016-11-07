      real function cntda()
      implicit none
      integer i
      include 'cntmc.inc'
      vector vrsl
      vector vrsh
      vector vrunl
      vector vrunh
      vector vda
      do i= 1, 6
        if( 2.0*ebeam.gt.vrsl(i) .and. 2.0*ebeam.lt.vrsh(i) .and.
     &      irun.ge.vrunl(i) .and. irun.le.vrunh(i) .and.
     &      itkmh.eq.1 ) then
          vda(i)= vda(i) + 1.0
        endif
      enddo
      return
      end
      real function cntmc( imc )
      implicit none
      integer imc
      include 'cntmc.inc'
      vector vmc
      vector vmcs
      vector vmcl
      vector vmcb
      vector vmcsl
      vector vmcsb
      if( itkmh.eq.1 ) then
        vmcs(imc)= vmcs(imc) + 1.0
        if( abs(iferid(1)).eq.5 ) then
          vmcsb(imc)= vmcsb(imc) + 1.0
        else
          vmcsl(imc)= vmcsl(imc) + 1.0
        endif
      endif
      vmc(imc)= vmc(imc) + 1.0
      if( abs(iferid(1)).eq.5 ) then
        vmcb(imc)= vmcb(imc) + 1.0
      else
        vmcl(imc)= vmcl(imc) + 1.0
      endif
      return
      end
