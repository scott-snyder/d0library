      SUBROUTINE DETUCA
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Definition of hit and
C-                         digitization parameters for the Central
C-                         calorimeter
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  1985 (?)      Rajendran Raja
C-                          A.M.Jonckheere
C-   Updated  13-SEP-1988   Rajendran Raja
C-   Updated  10-DEC-1988   Stuart Fuess  Separate sets for each Layer 
C-   Updated  27-DEC-1988   Stuart Fuess  Better names for sets 
C-   Updated  28-DEC-1988   Stuart Fuess  Combine volumes into larger sets
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
      INTEGER ISET,IDET
C----------------------------------------------------------------------
C  Establish detector sets for each of the CC sub-systems
C----------------------------------------------------------------------
      CALL SETDET('IUSET_CCEM',SCAL,ISET,IDET)
      CALL SETDET('IUSET_CCFH',SCAL,ISET,IDET)
      CALL SETDET('IUSET_CCCH',SCAL,ISET,IDET)
      CALL SETDET('IUSET_CC_MASSLESS_GAP',SCAL,ISET,IDET)
      RETURN
      END
