      SUBROUTINE PUDSEC ( ILAYD, ISECD )
C======================================================================
C
C   Purpose and Methods :  Eliminates the unused portion of
C                          bank DSEC after it has been filled.
C
C
C-   Updated  12-FEB-1988   Ghita Rahal-Callot : Datas for the Push are
C-                          taken from the bank itself    
C
C======================================================================
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:CDCLNK.INC/LIST'
      INTEGER ILAYD,ISECD,NHITS,NWDSEC
      INTEGER NPTWHT,NWDSHT,MXDSEC
C
      IF(LDSEC(ISECD,ILAYD).NE.0) THEN
         MXDSEC = IQ (LDSEC (ISECD,ILAYD) - 1 )
         NHITS  = IQ (LDSEC (ISECD,ILAYD) + 1 )
         NPTWHT = IQ (LDSEC (ISECD,ILAYD) + 2 )
         NWDSHT = IQ (LDSEC (ISECD,ILAYD) + 3 )
         NWDSEC = 3 + NPTWHT*2 + NWDSHT*NHITS
         CALL MZPUSH (IXCOM,  LDSEC(ISECD,ILAYD), 0, NWDSEC-MXDSEC, 'R')
      ENDIF
C
      RETURN
      END
