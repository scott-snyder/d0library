      LOGICAL FUNCTION INID0
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read geometry database containing global and
C-   general parameters.
C-
C-   Inputs  : None
C-   Outputs : None
C-
C-   Created  ??-???-????   R.RAJA
C-   Updated  29-JUL-1987   A.M.Jonckkhere
C-   Updated  20-MAY-1988   Ghita Rahal-Callot : add the general STP_file
C-                                               ( beam pipes, flanges,..)
C-   Updated  15-NOV-1988   Ghita Rahal-Callot : add the path to GEAN
C-   Updated   5-MAR-1989   Alan M. Jonckheere : add check on read err
C-   
C-   Updated   5-JUN-1989   Harrison B. Prosper  
C-   Made into program-builder interface function.
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IERR
      INCLUDE 'D0$INC:GCUNIT.INC'
C----------------------------------------------------------------------
      INID0 = .TRUE.
C
C ****  Put Geometry of the Beam Pipes, MVOL, MCAL, MCEN in /ZEBSTP/
C
      CALL D0ISTP ( 'GEN_STPFILE',IERR )
      IF ( IERR.NE.0 ) THEN
        CALL ERRMSG
     &    ('D0GEANT','INID0','Error opening geometry file','F')
      ENDIF
C
  999 RETURN
      END
