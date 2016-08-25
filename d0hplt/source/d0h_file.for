      SUBROUTINE D0H_FILE(HID,HROUT_FILE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create a output file name for the HBOOK4
C-                         histograms based on run number and a seqential
C-                         number
C-
C-   Inputs  : HID - Histogram number to be outputted, 0 to output all hists
C-   Outputs : HROUT_FILE - File name where histograms are to be written
C-   Controls: 
C-
C-   Created   8-JUN-1990   Sharon Hagopian
C-   Updated  19-APR-1991   Chip Stewart - REMOVED SPAWN STUFF- ADDED USR$OUT
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER HID
      CHARACTER*(*) HROUT_FILE      
      INTEGER NRUN,RUNNO
      INTEGER N1,N2,ISTAT,LSPAWN
      INTEGER II,JJ,KK,STATUS,LSTRING,TRNLNM
      CHARACTER*16 CRUN
      CHARACTER*80 STRING
C----------------------------------------------------------------------
      NRUN=RUNNO()
      CALL STRINT('RUN',NRUN,CRUN,N1)
      IF(HID.EQ.0)THEN
        HROUT_FILE=CRUN(1:N1)
        N2=N1
      ELSE
        CRUN=CRUN(1:N1)//'_'
        CALL STRINT(CRUN(1:N1+1),HID,HROUT_FILE,N2)
      ENDIF
      HROUT_FILE=HROUT_FILE(1:N2)//'.HST'
      STATUS = TRNLNM('USR$OUT',STRING,LSTRING) 
      IF ( STATUS.ne.0 ) THEN                            
        HROUT_FILE = 'USR$OUT:'//HROUT_FILE(1:N2+4)
      ENDIF
  999 RETURN
      END
