C----------------------------------------------------------------------
C-
C-   Created   12-JAN-1993   Andrew G. Brandt
C-   Updated   06-MAY-1993   Andrew G. Brandt Add ESCALE and IESYS
C-   Updated   24-SEP-1993   Andrew G. Brandt Add new switches for RV11
C-   Updated   18-MAR-1994   Andrew G. Brandt Add D0MDST allow 64 trigs
C-   Updated   01-NOV-1994   Andrew G. Brandt Update for CW. No INC_JET
C-   Updated   06-DEC-1995   Andrew G. Brandt Add UNFILTERED, DO_AIDA, DO_LUM
C-   Updated   4-FEB-1996    Bob Hirosky   add l0 vertex switch for qual cuts
C-   Updated   02-MAR-1996   Andrew G. Brandt Add MUONWD
C-   Updated   06-MAR-1996   Andrew G. Brandt Add NPACK PACK for PBD switches
C-   Updated   06-MAR-1996   Andrew G. Brandt Add EXCLU_NAME
C-
C----------------------------------------------------------------------
C
C Number of allowed QCD triggers  (7 cone sizes allowed)
C
      INTEGER NTRGMX,NCMAX,NPKMAX
      PARAMETER(NTRGMX=64)
      PARAMETER(NCMAX=7)
      PARAMETER(NPKMAX=10)
C      
      COMMON /QCD_NTUP_INFO/UPATH,DO_L1L2,
     &                  DO_ELC,DO_PHO,
     &                  DO_FULL_JET,MC_DATA,ALL_JET,
     &                  GAP_JET,PNUTWD,VERTWD,GLOBWD,BADTST,
     &                  ESCALE,NJREQ_ALL,
     &                  NTRIG,QCD_TRIG(NTRGMX),
     &                  NCONES,RCONES(NCMAX),NOMERG,NNEIGH,
     &                  JTCSON,NOQCD,NNOQCD,NOQCD_TRIG(NTRGMX),
     &                  D0MDST,L1WD,ETMIN,MINB,
     &                  DO_USER_BOOK,DO_USER_FILL,
     &                  UNFILTERED,DO_AIDA,DO_LUM,
     &                  QNT_DATA,MUONWD,NPACK,PACK(NPKMAX),
     &                  EXCLU_NAME
C
      INTEGER PNUTWD,VERTWD,GLOBWD,BADTST,NJREQ_ALL,
     &        NTRIG,NCONES,NNOQCD,L1WD,MUONWD,NPACK
      CHARACTER*4 UPATH
      CHARACTER*32 QCD_TRIG,NOQCD_TRIG,PACK
      LOGICAL DO_L1L2,DO_ELC,DO_PHO,
     &        DO_FULL_JET,MC_DATA,ALL_JET,GAP_JET,ESCALE,
     &        NOMERG,NNEIGH,JTCSON,NOQCD,D0MDST,MINB,
     &        DO_USER_BOOK,DO_USER_FILL,
     &        UNFILTERED,DO_AIDA,DO_LUM,QNT_DATA,
     &        EXCLU_NAME
      REAL RCONES,ETMIN
C
