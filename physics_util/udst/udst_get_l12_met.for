      SUBROUTINE UDST_GET_L12_MET(L1MET,L1METPHI,L1SET,L2MET,L2METPHI,
     &  L2SET)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get L1 and L2 met,metphi,sum et for UDST
C-
C-   Inputs  : NONE
C-   Outputs : L1MET,L1METPHI,L1SET,L2MET,L2METPHI,L2SET
C-   Controls:
C-
C-   Created  17-DEC-1993   Ian Adam - Code taken from original UDST_GET_MET
C-   Updated   9-APR-1995   Ulrich Heintz - some bug fixes
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:ESUM.PARAMS'
      INTEGER FLAG,IER,NFOUND(ID_ALL:LAST_TYPE)
      REAL    ET,ETA,ETAD,PHI
      REAL    L1MET,L1METPHI,L1SET
      REAL    L2MET,L2METPHI,L2SET
      CHARACTER*4 TYPE
C----------------------------------------------------------------------
      TYPE='TRGR'

      CALL GTESUM_COUNTS (TYPE,NFOUND,IER)
      IF(NFOUND(ID_ETMISS).GT.0)THEN
        CALL GTESUM(TYPE,ID_ETMISS,1,ET,ETA,ETAD,PHI,FLAG,IER)
        L1MET=ET
        L1METPHI=PHI
      ELSE
        L1MET=0.
        L1METPHI=0.
      ENDIF
      IF(NFOUND(ID_ETSUM).GT.0)THEN
        CALL GTESUM(TYPE,ID_ETSUM,1,ET,ETA,ETAD,PHI,FLAG,IER)
        L1SET=ET
      ELSE
        L1SET=0.
      ENDIF

      TYPE='FILT'

      CALL GTESUM_COUNTS (TYPE,NFOUND,IER)
      IF(NFOUND(ID_ETMISS).GT.0)THEN
        CALL GTESUM(TYPE,ID_ETMISS,1,ET,ETA,ETAD,PHI,FLAG,IER)
        L2MET=ET
        L2METPHI=PHI
      ELSE
        L2MET=0.
        L2METPHI=0.
      ENDIF
      IF(NFOUND(ID_ETSUM).NE.1)THEN
        CALL GTESUM(TYPE,ID_ETSUM,1,ET,ETA,ETAD,PHI,FLAG,IER)
        L2SET=ET
      ELSE
        L2SET=0.
      ENDIF

  999 RETURN
      END
