C DEC/CMS REPLACEMENT HISTORY, Element SAGTHT.FOR
C *1    29-MAY-1991 16:45:01 ABACHI "OLEG EROSHIN: New SAMUS routines"
C DEC/CMS REPLACEMENT HISTORY, Element SAGTHT.FOR
C DEC/CMS REPLACEMENT HISTORY, Element SAGTHT.FOR
C *1    29-MAY-1991 16:45:01 ABACHI "OLEG EROSHIN: New SAMUS routines"
C DEC/CMS REPLACEMENT HISTORY, Element SAGTHT.FOR
      INTEGER FUNCTION SAGTHT(NPL,PT,VT,DIF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get number of nearest  hit for line
C-                         in NPL plane
C-
C-   Returned value  : hit number or -1
C-   Inputs  : 
C-             NPL   -   plane number
C-             PT    -   point on line
C-             VT    -   direction cosine 
C-             DIF   -   maximum distance between hit and line
C-   Outputs : 
C-   Controls: 
C-
C-   Created  27-MAR-1991   O.Eroshin
C-   Updated  02-DEC-1993   O.Eroshin  Add message 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZSACL.LINK'
C----------------------------------------------------------------------
      LOGICAL  SAHIT
      INTEGER  I,NPL,MPL,LHIT,GZSAHH
      REAL     SALINE,DIF,DIST,DISL,PT(3),VT(3),TP(3,2)
      CHARACTER*32 MESSID,CALLER
      CHARACTER*80 MESSAG
C----------------------------------------------------------------------
      SAGTHT = -1
C
      MPL     = IQ(GZSAHH()+NPL)
      IF (MPL.LE.0)                     RETURN
      LHIT    = LQ(LQ(GZSAHH()-NPL)-IZSACL)
      MPL     = IQ(LHIT-1)/15
C
      DISL    = 1.E20
C
C......   Loop on all hits on plane
C
      DO 1 I=1,MPL
        IF (.NOT.SAHIT(LHIT,1))         GO TO 1
        DIST  = SALINE(Q(LHIT+6),PT,Q(LHIT+9),VT,TP(1,1),TP(1,2))
        IF (DIST. GT. 1.E5)             THEN
          MESSID = ' SAGTHT: illegal distance '
          CALLER = 'SAGTHT'
          CALL ERRMSG(MESSID,CALLER,MESSAG,'W')
        END IF
        IF (DIST.LT.DIF)                THEN
          SAGTHT = I
          DIF    = DIST
        END IF
        IF (DIST.GT.DISL)               RETURN
        DISL     = DIST
    1 LHIT       = LHIT+15
C
  999 RETURN
      END
