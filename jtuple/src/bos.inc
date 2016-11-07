C -*- mode: fortran -*-
C BOS memory declarations:
      integer mbos
      parameter( mbos=80000 )
      integer iw(mbos)
      integer*2 hw(mbos*2)
      real rw(mbos)
      common /bcs/ iw
      equivalence( hw(1), iw(1), rw(1) )
C BOS functions:
      integer ibln
