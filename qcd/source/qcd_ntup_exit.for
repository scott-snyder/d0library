      FUNCTION QCD_NTUP_EXIT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :Close the NTUPLE files.
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   14-JAN-1993   Andrew Brandt (based on J. Yu QCD_EXIT)
C-   Modified  03-MAR-1994   Andrew Brandt to fix unsolvable bug
C-   Modified  01-NOV-1994   Andrew Brandt replace w/Iain's CW routine
C-   
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL QCD_NTUP_EXIT
      INTEGER ICYCLE
C
      QCD_NTUP_EXIT=.TRUE.
C
      CALL HLDIR('//PAWC',' ')
      CALL HLDIR('//NTUPLE',' ')
      CALL HCDIR('//NTUPLE',' ')
      CALL HROUT(0,ICYCLE,' ')
      CALL HREND('NTUPLE')
      CLOSE(1)
  999 RETURN
      END
