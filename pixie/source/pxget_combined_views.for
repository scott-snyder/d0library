      SUBROUTINE PXGET_COMBINED_VIEWS(ARRAY_CHOSEN,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Displays the combined views available in current
C-   RCp file and allows the user to select one returning the 
C-
C-   Inputs  : None
C-   Outputs : ARRAY_CHOSEN [C*]: Array chosen by the user
C-   Controls: IER - 0 - Okay
C-                   1 - No combined views found in this RCP file
C-                   2 - No selction was made
C-   Created   4-APR-1991   Lupe Howell
C-   Updated   7-JAN-1992   Lupe Howell  Modify call to PX_DISPLAY_ITEMS 
C-   Updated  19-MAR-1992   Lupe Howell  Fix heading displays 
C-   Updated   2-NOV-1992   Lupe Howell  Add call to DISPLAY_ITEMS 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) ARRAY_CHOSEN
      INTEGER IER
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:PIXIE.DEF'
C----------------------------------------------------------------------
      INTEGER NCOMBVIEWS,NEXT,I,J,K,ALEN,ARRAYNUM,ITYPE
      CHARACTER*40 FOUND_ARRAYS,COMBINE_ARRAY(MAXCPAR),COMB_REM(MAXCPAR)
      CHARACTER*40 REMARK,CVAL,CURR_RCP
      CHARACTER*80 STRING
C----------------------------------------------------------------------
C
C ****  Get the all the combine view arrays' names
C
      IER = 0
      NCOMBVIEWS = 0
      NEXT = 0
      DO WHILE ( NEXT .NE. -1 )
        CALL EZGET_NEXT_NAME(FOUND_ARRAYS,NEXT)
        CALL WORD(FOUND_ARRAYS,I,J,ALEN)
        IF ( FOUND_ARRAYS(J:J) .EQ. '%' ) THEN
          NCOMBVIEWS = NCOMBVIEWS + 1
          COMBINE_ARRAY(NCOMBVIEWS) = FOUND_ARRAYS(I:J)
        ENDIF
      ENDDO
C
C ****  Get option from user
C
      IF ( NCOMBVIEWS .LE. 0 ) THEN
        CALL STAMSG(' No COMBINED VIEWS in current package',.TRUE.)
        IER = 1
        GOTO 999
      ENDIF
C
C ****  Set the remarks for the combined arrays if any
C
      I = 0
      DO WHILE ( I .LT. NCOMBVIEWS )
        I = I + 1
        CALL EZ_GET_ARRAY ! Getting remarks of combined views
     &    (COMBINE_ARRAY(I),'%TITLE',1,J,CVAL,ITYPE,REMARK,IER)
        COMB_REM(I) = REMARK
      ENDDO
C
C ****  Display combined views available and let user pick
C
      CALL EZTELL(CURR_RCP,I)
      STRING = 'COMBINED VIEWS FOR '//CURR_RCP
      CALL SWORDS(STRING,I,J,K)
      CALL DISPLAY_ITEMS
     &  (NCOMBVIEWS,COMBINE_ARRAY,COMB_REM,STRING(I:J),ARRAYNUM)
      IF ( ARRAYNUM .EQ. 0 ) THEN
        ARRAY_CHOSEN = ' '
        IER = 2
      ELSE
        ARRAY_CHOSEN = COMBINE_ARRAY(ARRAYNUM)
      ENDIF
  999 RETURN
      END
