      SUBROUTINE STDOUT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :       Do standard output
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  April 1987   S. Protopopescu
C-   Updated  18-JUN-1992   K. Wyatt Merritt   Comment out print version
C-                           of HBOOK histograms; send histograms only
C-                           to RZ file
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER OUT,ERR,SSUNIT
C---------------------------------------------------------------
C
      CALL JOBSUM
      CALL USRSSM
      OUT=SSUNIT()
      CALL HOUTPU(OUT)
C     CALL HERMES(OUT)
      CALL HLDIR('//PAWC','T')
C     CALL HPDIR('//PAWC','T')
      CALL SSCLOS
C
      RETURN
      END

