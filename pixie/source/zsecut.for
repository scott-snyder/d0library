      SUBROUTINE ZSECUT(ENRG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Determine a color acording to the energy 
C-      If the device is not color it will give different line styles.  
C-      This routine will allow PXCOLN to perform the JCOLRO or JLSTYL
C-      calls for the user.
C-
C-   Inputs  :   ENRG   - Energy to be display
C-               
C-   Created   5-JUN-1989   LUPE ROSAS
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    ENRG, ENGMIN(4), ENGMAX(4)
      INTEGER INDX, KCOLOR,KINTEN,KFILL,KSTYL
C----------------------------------------------------------------------
      DATA ENGMIN/0.,35.,70.,105./
      DATA ENGMAX/35.,70.,105.,9999./
C----------------------------------------------------------------------
      IF (ENRG.LE.ENGMIN(2)) THEN  
        INDX = 4     ! purple 
      ELSEIF(ENRG.LE.ENGMAX(2)) THEN
        INDX = 6     ! blue 
      ELSEIF(ENRG.LE.ENGMAX(3)) THEN
          INDX = 13  ! red 
      ELSE
          INDX = 16  ! yellow 
      ENDIF
      CALL PXCOLN('CDC',INDX,4,.TRUE.,KCOLOR,KINTEN,KFILL,KSTYL)
  999 RETURN
      END
