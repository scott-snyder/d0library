      SUBROUTINE EZ_MODIFY_ARRAY(RCP_NAME,NEW_ARRAY,LINES,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Modifyes an existing RCP element by a 
C-   by an NEW one.  This element can be an array, if it is LINES 
C-   should have the '\ARRAY array_name' as the first element and 
C-   '\END' as the last.
C-   
C-   Note: It is assume that new values that are CHARACTER type are going 
C-   to be input with a double quote around them.
C-   
C-   Inputs  : RCP_NAME [C*]: Name of RCP array
C-             NEW_ARRAY[C*]: New array values as though they are in a
C-                            ascii rcp file.  
C-             LINES    [I ]: Number of lines taken by the NEW_ARRAY
C-                            in the RCP file
C-   Outputs : IER      [I ]: Non zero on error
C-
C-   Created 12-JUN-1991   Lupe Howell - This routine is based on 
C-                         EZ_REPLACE_ARAY by Rajendran Raja
C-   Updated  22-NOV-1991   Lupe Howell  Setting IER code to 0 if no error 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      CHARACTER*(*) RCP_NAME,NEW_ARRAY(*)
      INTEGER LINES,IER
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:SRCP.DEF'
C----------------------------------------------------------------------
      INTEGER TRULEN,I,IERR,TL
C----------------------------------------------------------------------
      IER = 0
C
C ****  Delete old bank
C
      CALL EZDELETE(RCP_NAME,IERR)
      IF ( IERR.EQ.0 .OR. IERR.EQ.EZS_PARAM_NOTFOUND ) THEN
C
C ****  Add all the elements of the new array
C
        DO I = 1 , LINES
          TL = TRULEN(NEW_ARRAY(I))
          TL = MAX(1,TL)
          CALL EZADD(NEW_ARRAY(I)(1:TL),1,IERR)
        ENDDO
C
C ****  Add the \END to end the array 
C
        IER = IERR
        CALL EZEND
        IF(IERR.NE.0.AND.IERR.NE.EZS_BANK_EXTENDED)THEN
          CALL ERRMSG(' EZ_PACKAGE','EZ_MODIFY_ARRAY',
     &    ' Error adding to bank','W')
          IER = IERR
        ELSE
          IER = EZS_SUCCESS    ! No error
        ENDIF
      ELSE
        CALL ERRMSG(' EZ_PACKAGE','EZ_MODIFY_ARRAY',
     &    ' Error deleting array','W')
      ENDIF
  999 RETURN
      END
