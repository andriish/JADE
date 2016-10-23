C   14/10/82            MEMBER NAME  MEMBERS1 (S)           FORTRAN77
      0.   COMMON ACS




      END
#include "acs.for"
#include "bscom1.for"
      1.   STANDARD PROGRAMS


           SUBPROGRAM   ENTRIES

      1.1  BINT         BINT1  BSPC  BRNM
      1.2  IBLN         -
      1.3  BCRE         -
      1.4  CCRE         -
      1.5  BLOC         -
      1.6  CLOC         -
      1.7  BCHM         BCHF  BSTR BMRT
      1.8  BDAR         -
      1.9  BPOS         CPOS  BPRL
      1.10 BNXT         -
      1.11 CNXT         -
      1.12 BMLT         BSAW  BDEF  BSAT  BSAD
      1.13 BDLS         BPRS
      1.14 BDLM         BDLG  BPRM  BSLW  BSLC   BSLT
      1.15 BGAR         -
      1.16 BDMP         BDMPF
      1.17 BSTA         -
      1.18 DUMP0C       -
      1.19 BCHL         BMVE
      1.20 CCHL         CMVE
      1.21 BGAC         -
      1.22 BSLS         BSLR
      1.23 INDAR
      END
#include "bint.for"
#include "ibln.for"
#include "bcre.for"
#include "ccre.for"
#include "bloc.for"
#include "cloc.for"
#include "bchm.for"
#include "bdar.for"
#include "bpos.for"
#include "bnxt.for"
#include "cnxt.for"
#include "bmlt.for"
#include "bdls.for"
#include "bdlm.for"
#include "bgar.for"
#include "bdmp.for"
#include "bsta.for"
#include "dump0c.for"
#include "bchl.for"
#include "cchl.for"
#include "bgac.for"
#include "bsls.for"
#include "indar.for"
      2.   CARD INPUT PROGRAM

           SUBPROGRAM   ENTRIES

      2.1  BREADC       -
      2.2  READHL       -
      END
#include "breadc.for"
#include "readhl.for"
      3.   INPUT-OUTPUT PROGRAMS

           SUBPROGRAM   ENTRIES

      3.1  BREAD        -
      3.2  CREAD        -
      3.3  BWRITE       -
      3.4  BRDS         -
      3.5  BWRS         BWRO
      3.6  BFRD         BFWR
      3.7  BINP         BFRR
      3.8  BOUTP        BORDP
      3.9  BSEQ         BSEW  BSET  BSEL
      3.10 RDVAR        WRVAR WRFIX
      END
#include "bread.for"
#include "cread.for"
#include "bwrite.for"
#include "brds.for"
#include "bwrs.for"
#include "bfrd.for"
#include "binp.for"
#include "boutp.for"
#include "bseq.for"
#include "rdvar.for"
      4.   DIRECT ACCESS PROGRAMS


           SUBPROGRAM   ENTRIES

      4.1  BLOCDA       BDLSDA BCREDA BCHKDA
      4.2  BINLDA       -
      4.3  BLOADA       -
      4.4  PODA         CHDIR
      4.5  DADA         -
      4.6  RDDA         WRDA   RDSQ   WRSQ
      4.7  BPASDA

      END
#include "blocda.for"
#include "binlda.for"
#include "bloada.for"
#include "poda.for"
#include "dada.for"
#include "rdda.for"
#include "bpasda.for"
      5.   APPLICATION PROGRAMS



      END
#include "ucond.for"
#include "pcond.for"
#include "utabl.for"
#include "qtabl.for"
#include "ptabl.for"
#include "uhist.for"
#include "defst.for"
#include "phist.for"
#include "ucorr.for"
#include "ccor.for"
#include "pcorr.for"
#include "ustos.for"
#include "pstor.for"
#include "tocont.for"
#include "ptext.for"
#include "dpeak.for"
#include "hmean.for"
#include "pmean.for"
#include "ppeak.for"
#include "umean.for"
#include "qmean.for"
#include "qpeak.for"
#include "ussto.for"
#include "utabi.for"
#include "uwp2.for"
      6.   NON-STANDRAD PROGRAMS


      3.10 ASRD         ASVB  ASVBS ASCL

      END
#include "testwr.for"
#include "lsbcom.for"
      7.  NON-BOS PROGRAMS



      END
#include "uwp.for"
#include "bfmt.for"
#include "iaf.for"
#include "itoda.for"
#include "pvert.for"
#include "sort4.for"
#include "sortir.for"
#include "vall.for"
