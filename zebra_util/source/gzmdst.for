C VAX/DEC CMS REPLACEMENT HISTORY, Element GZMDST.FOR
C *2    27-FEB-1994 22:18:42 MEENA "Richard V. Astur: GZ for MDST - updated to work with GZCAEQ"
C *1    27-OCT-1991 23:19:32 NGRAF "Andrew Milder: Get pointer to MicroDST bank"
C VAX/DEC CMS REPLACEMENT HISTORY, Element GZMDST.FOR
      INTEGER FUNCTION GZMDST()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the link to the MDST bank 
C-
C-   Returned value  : Link to MDST bank
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  19-AUG-1991   Andrew J. Milder
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZANLS,LANLS
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZMDST.LINK'
      CHARACTER*4 BNAME
C----------------------------------------------------------------------
      GZMDST = 0
      LANLS = GZANLS()
      IF (LANLS.LE.0) THEN
        CALL ERRMSG('CALORIMETER','GZMDST',
     &    ' ANLS BANK DOES NOT EXIST ' ,'W')
        GO TO 999
      ENDIF
      GZMDST = LQ(LANLS-IZMDST)
C
C: This link is sometimes held by D0 mdst banks
C
      IF ( GZMDST .GT. 0 ) THEN
        CALL UHTOC(IQ(GZMDST-4), 4, BNAME, 4)
        IF ( BNAME .NE. 'MDST' ) GZMDST = 0
      ENDIF
  999 RETURN
      END
