      FUNCTION QCD_GET_RCP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check QCD_NTUP.rcp file to do QCD_NTUP
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   24-JAN-1993  Andrew G. Brandt
C-   Updated   06-MAY-1993  Andrew G. Brandt add ESCALE and IESYS
C-   Updated   24-SEP-1993  Andrew G. Brandt Add new switches for RV11
C-   Updated   16-NOV-1993  Andrew G. Brandt Add NUTCOR switch
C-   Updated   28-FEB-1994  Andrew G. Brandt Remove NUTCOR IESYS ES cone check
C-                                           add GAP_JET
C-   Updated   01-NOV-1994  Andrew G. Brandt Update for CW. No INC_JET
C-   Updated   06-DEC-1995  Andrew G. Brandt Add UNFILTERED, DO_AIDA, DO_LUM
C-   Updated   02-MAR-1996  Andrew G. Brandt Add QNT_DATA (bob), MUONWD
C-   Updated   02-MAR-1996  Andrew G. Brandt Add EXCLU_NAME
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:QCD_EVT_INFO.INC/LIST'
      INCLUDE 'D0$INC:QCD_NTUP_INFO.INC/LIST'
      INTEGER NCHR,IER
      LOGICAL QCD_GET_RCP
C----------------------------------------------------------------------
C
C First get RCP file
C
      QCD_GET_RCP=.TRUE.
      CALL INRCP('QCD_NTUP_RCP',IER)
      IF(IER.NE.0) CALL ERRMSG('QCD_NTUP_RCP','QCD_NTUP_RCP',
     &    'RCP FILE NOT PRESENT','W')
      CALL EZPICK('QCD_NTUP_RCP')
C
      CALL EZGETS('UPATH',1, UPATH,NCHR,IER)
      IF ( IER.LT.0) THEN
        CALL ERRMSG('QCD_NTUP_RCP','UPATH',
     &      'NO VARIABLE FOUND','F')
      END IF
C
      CALL EZGET('NJREQ_ALL', NJREQ_ALL,IER)
      IF ( IER.LT.0) THEN
        CALL ERRMSG('QCD_NTUP_RCP','NJREQ_ALL',
     &      'NO VARIABLE FOUND','W')
      END IF
C
      CALL EZGET('ALL_JET', ALL_JET,IER)
      IF ( IER.LT.0) THEN
        CALL ERRMSG('QCD_NTUP_RCP','ALL_JET',
     &      'NO VARIABLE FOUND','W')
        ALL_JET=.FALSE.
      END IF
C
      CALL EZGET('ETMIN', ETMIN,IER)
      IF ( IER.LT.0) THEN
        CALL ERRMSG('QCD_NTUP_RCP','ETMIN',
     &      'NO VARIABLE FOUND','W')
        ETMIN=0.
      END IF
C
C
      CALL EZGET('D0MDST', D0MDST,IER)
      IF ( IER.LT.0) THEN
        CALL ERRMSG('QCD_NTUP_RCP','D0MDST',
     &      'NO VARIABLE FOUND','W')
        D0MDST=.FALSE.
      END IF
C
      CALL EZGET('GAP_JET', GAP_JET,IER)
      IF ( IER.LT.0) THEN
        CALL ERRMSG('QCD_NTUP_RCP','GAP_JET',
     &      'NO VARIABLE FOUND','W')
        GAP_JET=.FALSE.
      END IF
C
      CALL EZGET('MINB', MINB,IER)
      IF ( IER.LT.0) THEN
        CALL ERRMSG('QCD_NTUP_RCP','MINB',
     &      'NO VARIABLE FOUND','W')
        MINB=.FALSE.
      END IF
C
      CALL EZGET('DO_USER_BOOK', DO_USER_BOOK,IER)
      IF ( IER.LT.0) THEN
        CALL ERRMSG('QCD_NTUP_RCP','DO_USER_BOOK',
     &      'NO VARIABLE FOUND','W')
        DO_USER_BOOK=.FALSE.
      END IF
C
      CALL EZGET('DO_USER_FILL', DO_USER_FILL,IER)
      IF ( IER.LT.0) THEN
        CALL ERRMSG('QCD_NTUP_RCP','DO_USER_FILL',
     &      'NO VARIABLE FOUND','W')
        DO_USER_FILL=.FALSE.
      END IF
C
C Make sure that NJREQ_ALL is consistent
C
      IF(.NOT.MINB) THEN
        IF(NJREQ_ALL.LE.0) NJREQ_ALL=4
      END IF
C
C Make sure that Ntuple is requested
C
      IF(.NOT.ALL_JET.AND..NOT.GAP_JET) THEN
        CALL ERRMSG('QCD_GET_RCP','QCD_GET_RCP',
     &              'ALL_JET and GAP_JET are false','F')
      END IF
C
      CALL EZGET('DO_L1L2', DO_L1L2,IER)
      IF ( IER.LT.0) THEN
        CALL ERRMSG('QCD_NTUP_RCP','DO_L1L2',
     &      'NO VARIABLE FOUND','W')
        DO_L1L2=.FALSE.
      END IF
C
      CALL EZGET('L1WD', L1WD,IER)
      IF ( IER.LT.0) THEN
        CALL ERRMSG('QCD_NTUP_RCP','L1WD',
     &      'NO VARIABLE FOUND','W')
        IF(IRUN.EQ.1) THEN
          L1WD=1
        ELSE
          L1WD=2
        END IF
      END IF
C
      CALL EZGET('DO_LUM', DO_LUM,IER)
      IF ( IER.LT.0) THEN
        CALL ERRMSG('QCD_NTUP_RCP','DO_LUM',
     &      'NO VARIABLE FOUND--SET TO TRUE','W')
        DO_LUM=.TRUE.
      END IF
C
      CALL EZGET('DO_ELC', DO_ELC,IER)
      IF ( IER.LT.0) THEN
        CALL ERRMSG('QCD_NTUP_RCP','DO_ELC',
     &      'NO VARIABLE FOUND','W')
        DO_ELC=.FALSE.
      END IF
C
      CALL EZGET('DO_PHO', DO_PHO,IER)
      IF ( IER.LT.0) THEN
        CALL ERRMSG('QCD_NTUP_RCP','DO_PHO',
     &      'NO VARIABLE FOUND','W')
        DO_PHO=.FALSE.
      END IF
C
      CALL EZGET('DO_FULL_JET', DO_FULL_JET,IER)
      IF ( IER.LT.0) THEN
        CALL ERRMSG('QCD_NTUP_RCP','DO_FULL_JET',
     &      'NO VARIABLE FOUND','W')
        DO_FULL_JET=.FALSE.
      END IF
C
      CALL EZGET('DO_AIDA', DO_AIDA,IER)
      IF ( IER.LT.0) THEN
        CALL ERRMSG('QCD_NTUP_RCP','DO_AIDA',
     &      'NO VARIABLE FOUND','W')
        DO_AIDA=.FALSE.
      END IF
C
      CALL EZGET('MC_DATA', MC_DATA,IER)
      IF ( IER.LT.0) THEN
        CALL ERRMSG('QCD_NTUP_RCP','MC_DATA',
     &      'NO VARIABLE FOUND','W')
        MC_DATA=.FALSE.
      END IF
C
      CALL EZGET('UNFILTERED', UNFILTERED,IER)
      IF ( IER.LT.0) THEN
        CALL ERRMSG('QCD_NTUP_RCP','UNFILTERED',
     &      'NO VARIABLE FOUND','W')
        IF (MC_DATA) THEN
          UNFILTERED=.TRUE.
        ELSE
          UNFILTERED=.FALSE.
        END IF
      ELSE IF (.NOT.MC_DATA.AND.UNFILTERED) THEN
        CALL ERRMSG('QCD_NTUP_RCP','UNFILTERED',
     &      'ARE YOU SURE YOU WANT TO SKIP FILTERING FOR REAL DATA','W')
      END IF
C
      CALL EZGET('PNUTWD', PNUTWD,IER)
      IF ( IER.LT.0) THEN
        CALL ERRMSG('QCD_NTUP_RCP','PNUTWD',
     &      'NO VARIABLE FOUND','W')
        PNUTWD=0
      END IF
C
      CALL EZGET('MUONWD', MUONWD,IER)
      IF ( IER.LT.0) THEN
        CALL ERRMSG('QCD_NTUP_RCP','MUONWD',
     &      'NO VARIABLE FOUND','W')
        MUONWD=0
      END IF
C
      CALL EZGET('VERTWD', VERTWD,IER)
      IF ( IER.LT.0) THEN
        CALL ERRMSG('QCD_NTUP_RCP','VERTWD',
     &      'NO VARIABLE FOUND','W')
        VERTWD=0
      END IF
C
      CALL EZGET('GLOBWD', GLOBWD,IER)
      IF ( IER.LT.0) THEN
        CALL ERRMSG('QCD_NTUP_RCP','DO_GLOB',
     &      'NO VARIABLE FOUND','W')
        GLOBWD=0
      END IF
C
      CALL EZGET('BADTST', BADTST,IER)
      IF ( IER.LT.0) THEN
        CALL ERRMSG('QCD_NTUP_RCP','BADTST',
     &      'NO VARIABLE FOUND','W')
        BADTST=0
      END IF
C
      CALL EZGET('QNT_DATA', QNT_DATA,IER)
      IF ( IER.LT.0) THEN
        CALL ERRMSG('QCD_NTUP_RCP','QNT_DATA',
     &      'NO VARIABLE FOUND','W')
        QNT_DATA=0
      END IF
C
      CALL EZGET('ESCALE', ESCALE,IER)
      IF ( IER.LT.0) THEN
        CALL ERRMSG('QCD_NTUP_RCP','ESCALE',
     &      'NO VARIABLE FOUND--set to false','W')
        ESCALE=.FALSE.
      END IF
C
      CALL EZGET('NCONES', NCONES,IER)
      IF ( IER.LT.0) THEN
        CALL ERRMSG('QCD_NTUP_RCP','NCONES',
     &      'NO VARIABLE FOUND','W')
        NCONES=0
      END IF
C
      CALL EZGET('RCONES', RCONES,IER)
      IF ( IER.LT.0) THEN
        CALL ERRMSG('QCD_NTUP_RCP','RCONES',
     &      'NO VARIABLE FOUND','W')
      END IF
C
      CALL EZGET('NOMERG', NOMERG,IER)
      IF ( IER.LT.0) THEN
        CALL ERRMSG('QCD_NTUP_RCP','NOMERG',
     &      'NO VARIABLE FOUND','W')
        NOMERG=.FALSE.
      END IF
C
      CALL EZGET('NNEIGH', NNEIGH,IER)
      IF ( IER.LT.0) THEN
        CALL ERRMSG('QCD_NTUP_RCP','NNEIGH',
     &      'NO VARIABLE FOUND','W')
        NNEIGH=.FALSE.
      END IF
C
      CALL EZGET('JTCSON', JTCSON,IER)
      IF ( IER.LT.0) THEN
        CALL ERRMSG('QCD_NTUP_RCP','JTCSON',
     &      'NO VARIABLE FOUND','W')
        JTCSON=.FALSE.
      END IF
C
      CALL EZGET('NOQCD', NOQCD,IER)
      IF ( IER.LT.0) THEN
        CALL ERRMSG('QCD_NTUP_RCP','NOQCD',
     &      'NO VARIABLE FOUND','W')
        NOQCD=.FALSE.
      END IF
C
      CALL EZGET('EXCLU_NAME', EXCLU_NAME,IER)
      IF ( IER.LT.0) THEN
        CALL ERRMSG('QCD_NTUP_RCP','EXCLU_NAME',
     &      'NO VARIABLE FOUND','W')
        EXCLU_NAME=.FALSE.
      END IF
C
C number of triggers is NTRIG and names are in QCD_TRIG array
C
      CALL EZ_GET_CHARS('QCD_TRIG',NTRIG,QCD_TRIG,IER)
      IF ( IER.LT.0) THEN
        CALL ERRMSG('QCD_NTUP_RCP','QCD_TRIG',
     &      'NO VARIABLE FOUND','W')
        NTRIG = 0
      END IF
C
C number of triggers is NNOQCD and names are in NOQCD_TRIG array
C
      CALL EZ_GET_CHARS('NOQCD_TRIG',NNOQCD,NOQCD_TRIG,IER)
      IF ( IER.LT.0) THEN
        CALL ERRMSG('QCD_NTUP_RCP','NOQCD_TRIG',
     &      'NO VARIABLE FOUND','W')
        NNOQCD = 0
      END IF
C
C number of extra packages to turn on is NPACK and names are in PACK array
C
      CALL EZ_GET_CHARS('PACKAGES_ON',NPACK,PACK,IER)
      IF ( IER.LT.0) THEN
        CALL ERRMSG('QCD_NTUP_RCP',' PACKAGES_ON ',
     &      'NO VARIABLE FOUND','W')
        NPACK = 0
      END IF
C
      CALL EZRSET
  999 RETURN
      END
