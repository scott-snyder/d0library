      SUBROUTINE BKICDH(LICDH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : This routine books the ICDH bank.
C-                              This bank contains hit information from the
C-                              intercryostat detector. The ICDH bank is placed
C-                              under GHIT.
C-
C-   Inputs  : None
C-   Outputs : LICDH the link to the booked ICDH bank
C-   Controls: None
C-
C-   Created:  3-NOV-1988   Z. WOLF
C-   Updated: 27-FEB-1989   Z. Wolf
C-   Updated   9-AUG-1989   Alan M. Jonckheere   Put on standard header
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
C--   I/O
      INTEGER LICDH,NVERS,NWTRK
C
C--   ICDH BANK PARAMETERS
      PARAMETER (NVERS=1)
      PARAMETER (NWTRK=15)
C
C--   ZEBRA BANKS
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZICDH.LINK/LIST'
C
C--   GEANT UNITS
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
C
C--   INTERNAL VARIABLES
      INTEGER GZICDH
      INTEGER IFORM
      INTEGER LGHIT,GZGHIT
C----------------------------------------------------------------------
C
C--   SEE IF ICDH ALREADY EXISTS
      LICDH=GZICDH()
      IF(LICDH.GT.0)GO TO 999
C
C--   FORMAT ICDH BANK
      CALL MZFORM('ICDH','-I',IFORM)
C
C--   GET LINK TO SUPPORTING GHIT BANK
      LGHIT=GZGHIT()
      IF(LGHIT.LE.0)CALL BKGHIT(LGHIT)
      IF(LGHIT.LE.0)THEN
        WRITE(LOUT,*)'BKICDH--> CAN NOT ESTABLISH LGHIT'
        RETURN
      END IF
C
C--   BOOK THE ICDH BANK UNDER GHIT
      CALL MZBOOK(IXMAIN,LICDH,LGHIT,-IZICDH,'ICDH',0,0,3000,IFORM,0)
C
C--   CHECK
      IF(LICDH.LE.0)GO TO 999
C
C--   FILL THE ICDH BANK PARAMETERS
      IQ(LICDH+1)=NVERS
      IQ(LICDH+2)=NWTRK
C
  999 RETURN
      END
