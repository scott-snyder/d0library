      SUBROUTINE BKJNEP(LJETS,LJNEP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      book a JNEP bank
C-   
C-   Inputs  :
C-     LJETS = pointer to the supporting bank 
C-   Outputs : 
C-     LJNEP = pointer to the created bank 
C-
C-   Created  22-NOV-1991 Dhiman Chakraborty
C-   Updated  17-MAY-1993   Harrison B. Prosper  
C-    Add full error matrix - Version 2 
C-   Updated  23-OCT-1993 R. Astur
C-    Add reference link to VCOR
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LJNEP
      INTEGER LJETS
      INTEGER IXIO
      INTEGER GZJETS
      INTEGER NDATA
      PARAMETER( NDATA = 26 )
      INTEGER NL,NS
      PARAMETER( NL = 2 )
      PARAMETER( NS = 1 )
C
C--   ZEBRA BANKS
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZJNEP.LINK/LIST'
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
C--   INITIALIZE
C
      LJNEP = 0
      IF(FIRST)THEN
C
        CALL MZFORM('JNEP','1I 13F 1I 10F 1I',IXIO)  ! Describe Bank format
C
        FIRST = .FALSE.
      ENDIF
C
      IF(LJETS.EQ.0) THEN
        CALL ERRMSG('CALORIMETER','BKJNEP',
     &    'JETS BANK DOES NOT EXIST ' ,'W')
C
      ELSE
        CALL MZBOOK
     &  (IXMAIN,LJNEP,LJETS,-IZJNEP,'JNEP',NL,NS,NDATA,IXIO,0)
        IQ(LJNEP+1)=3   ! version number
      ENDIF
C
  999 RETURN
      END
