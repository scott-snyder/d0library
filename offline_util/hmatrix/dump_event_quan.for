      SUBROUTINE DUMP_EVENT_QUAN(UNIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : DUMP EVENT QUANTITES TO UNIT
C-
C-   Inputs  : UNIT = UNIT NUMBER
C-   Outputs :
C-   Controls:
C-
C-   Created  11-SEP-1991   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:EVENT_QUAN.INC'
      INTEGER UNIT
      INTEGER LOCAL_RUN,LOCAL_EVENT,IR,IE
      INTEGER RUNNO,EVONUM
C----------------------------------------------------------------------
C
      LOCAL_RUN = RUNNO()
      LOCAL_EVENT = EVONUM()
C
      IE=IQ(LHEAD+7)                    ! event number
      IR=IQ(LHEAD+8)                    ! run number
C
      WRITE(UNIT,2)LOCAL_RUN,LOCAL_EVENT,IR,IE
    2 FORMAT(//,
     &    ' LOCAL RUN NUMBER :', I10, ' LOCAL EVENT NUMBER : ',I10,/,
     &    ' RUN NUMBER :', I10, 'EVENT NUMBER : ',I10)
C
      WRITE(UNIT,3)TRMASS,TOP_LIKE,MET_D0,NJETS,P6_CLUST,
     &  ELECTRON,PHOTON,MUON
C
    3 FORMAT(' W TRANSVERSE MASS, TOP_LIKELIHOOD ',2F15.7,/,
     &  ' MISSING ET ',F15.7,' NUMBER OF JETS ',I12,/,
     &  ' EM cluster 4 vector ',4F15.7,/,
     &  ' EM ET, EM Isolation ', 2F15.7,/,
     &  ' Electron,photon,muon flags ',3L2)
  999 RETURN
      END
