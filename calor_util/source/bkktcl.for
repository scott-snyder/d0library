      SUBROUTINE BKKTCL( LKTCL, NP )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book and fill the KTCL bank
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  21-FEB-1995   Richard V. Astur
C-   Updated   1-OCT-1995   Gordon Watts, Brad Abbott
C-                          Reformatted: NH decreased from 3 to 2, 
C-                          # words per precluster increased from 1 to 2
C-   Updated   4-OCT-1995   Dhiman Chakraborty   
C-                          Moved KTCL from LQ(LHEAD-15) to LQ(LANLS-6)
C-   Updated   6-OCT-1995   Dhiman Chakraborty   
C-                          Increase data header part (NH) from 2 words to 3.
C-   Updated  20-OCT-1995   Dhiman Chakraborty   
C-                          Replace GZANLS by BKANLS
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZKTCL.LINK/LIST'
      INTEGER LKTCL, NP, IVER, NH        ! NP = # of preclusters
      INTEGER NDATA,NS,NL,IOH
      PARAMETER( NS = 1)
      PARAMETER( NL = NS)
      INTEGER LANLS
      SAVE IOH
      DATA IVER / 1 /                     ! version number
      DATA NH   / 3 /                     ! # of header words
      LOGICAL FIRST
      DATA FIRST / .TRUE. /
C----------------------------------------------------------------------
C-
C---  Initialize
C-
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL MZFORM('KTCL','3I -I',IOH)
      ENDIF
C
C--- Book it if not there, else just get pointer
C
      LKTCL = 0
      CALL BKANLS(LANLS)
      IF(LANLS.LE.0) THEN
        CALL ERRMSG('ANLS bank not formed','BKKTCL',
     &    'Cannot book KTCL without ANLS','W')
        GOTO 999
      ENDIF
C
      NDATA = NH + 2*NP
      CALL MZBOOK(IXMAIN,LKTCL,LANLS,-IZKTCL,'KTCL',NS,NL,NDATA,IOH,0)
c
c  put these here for now -- but they are not in the proper format!
c  
      IQ(LKTCL+1) = IVER
      IQ(LKTCL+2) = NP
C
  999 RETURN
      END
