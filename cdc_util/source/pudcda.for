      SUBROUTINE PUDCDA ( ILAYD, ISECD )
C======================================================================
C
C   Purpose and Methods :  Eliminates the unused portion of
C                          bank DCDA after it has been filled.
C
C
C-   Created   4-FEB-1988   Ghita Rahal-Callot   
C
C======================================================================
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:CDCLNK.INC/LIST'
      INTEGER ILAYD,ISECD,NHITS,NWDCDA
      INTEGER NPTWHT,NWDSHT,NHMXV,MXDCDA
C
      IF(LDCDA(ISECD,ILAYD).NE.0) THEN
         MXDCDA = IQ (LDCDA (ISECD,ILAYD) - 1 )
         NHITS  = IQ (LDCDA (ISECD,ILAYD) + 1 )
         NPTWHT = IQ (LDCDA (ISECD,ILAYD) + 2 )
         NWDSHT = IQ (LDCDA (ISECD,ILAYD) + 3 )
         NWDCDA = 3 + NPTWHT*2 + NWDSHT*NHITS
         CALL MZPUSH (IXCOM,  LDCDA(ISECD,ILAYD), 0, NWDCDA-MXDCDA, 'R')
      ENDIF
C
      RETURN
      END
