      SUBROUTINE DGN(ID,IEVENT,BUFFER,STATUS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return a sub-set of values from the row-wise
C-   ntuple with identifier ID based on a previously defined sub-set of
C-   fields. The definition is performed by DGN_BEGIN.
C-
C-   Inputs  : ID         [I]   Id of row-wise ntuple
C-             IEVENT     [I]   Event (that is, row) 
C-             
C-   Outputs : BUFFER(*)  [R]   Array to receive values.
C-             STATUS     [I]   0 = OK
C-   Controls: 
C-
C-   Created  10-MAR-1995   Harrison B. Prosper
C-   Updated  25-JUN-1995   Harrison B. Prosper  
C-    Fix XSCALE bug 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ID, IEVENT, STATUS
      REAL    BUFFER(*)
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:DGNCOM.INC'
C----------------------------------------------------------------------
      INTEGER I, J
      LOGICAL FOUND
C----------------------------------------------------------------------
C
C ****  Crash if DGN_BEGIN not yet called
C
      IF ( .NOT. DONE_BEGIN ) THEN
        CALL ERRMSG('BAD_START','DGN','Please call DGN_BEGIN first','F')
      ENDIF
C
C ****  Select correct inputs
C
      I = 0
      CALL HGN(ID,I,IEVENT,XTUPLE,STATUS)
      IF ( STATUS .NE. 0 ) THEN
        GOTO 999
      ENDIF
C
      DO I =  1, NFIELD
        CALL LOCSTR(FIELD(I),TAG,NTAG,FOUND,J)
        IF ( FOUND ) THEN
          BUFFER(I) = XTUPLE(ITAG(J))
          IF ( XSCALE(I).NE.0 ) THEN 
            BUFFER(I) = XTUPLE(ITAG(J)) / XSCALE(I)
          ENDIF
        ENDIF
      ENDDO
  999 RETURN
      END
