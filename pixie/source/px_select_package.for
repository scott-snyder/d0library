      SUBROUTINE PX_SELECT_PACKAGE(PACKAGES,TOTPACK,OUTNUM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : This routine allows you to select one of the
C-   pacakges available in the system, or given by an array.
C-
C-   Inputs  : PACKAGES [C*(*)]: Array containing the list of packages to
C-                               display for selection.
C-                               PACKAGES(1)=' ' It will display ALL the
C-                               available packages.
C-             TOTPACK      [I]: Total number of elements in PACKAGES
C-
C-   Outputs : OUTNUM       [I]: Index corresponding to the selected package
C-                               If 0 no selction made.
C-
C-   Created  13-MAR-1991   Lupe Howell
C-   Updated   9-OCT-1991   Lupe Howell  New parameters
C-   Updated   7-JAN-1992   Lupe Howell  Modify call to PX_DISPLAY_ITEMS
C-   Updated  20-MAR-1992   Lupe Howell  More meaning ful menu messages 
C-   Updated   1-APR-1992   Lupe Howell  PX_CHECK_PICK entry created 
C-   Updated   3-NOV-1992   Lupe Howell  Add DISPLAY_ITEMS 
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
      CHARACTER*(*) PACKAGES(*)
      CHARACTER*(*) PICKED_BANK
      INTEGER TOTPACK,OUTNUM
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:PXCOMK.INC'
C----------------------------------------------------------------------
      INTEGER I,J,K,L,FLEN,LPACK,TOTAL_LIST
      CHARACTER*40 RCPFILE,CHOSEN_BANK,PACKAGE_LIST(MAXPAK)
      CHARACTER*40 MODE,LAST_MODE,PACK_REM(MAXPAK)
      LOGICAL PICK_FLAG,FIRST,CURRENT_PACKAGES,PICKED,FLAG
C
      DATA PICK_FLAG/.FALSE./
      DATA FIRST/.TRUE./
      DATA LAST_MODE /'0'/
C----------------------------------------------------------------------
      SAVE PICK_FLAG,LAST_MODE,PICKED
C----------------------------------------------------------------------
C
C ****  Checking selection packages
C
      CURRENT_PACKAGES =  PACKAGES(1) .EQ. '  '
C
C ****  Set up packages option.  If current packages requested
C ****  get all the pacakges available else use the input parameter
C ****  list
C
      IF ( CURRENT_PACKAGES ) THEN
        DO I = 1, NPACKAGE
          PACKAGE_LIST(I) = PACKAGE(I)
        ENDDO
        TOTAL_LIST = NPACKAGE + 1
        PACKAGE_LIST(TOTAL_LIST) = 'SYSTEM'
      ELSE
        DO I = 1, TOTPACK
          PACKAGE_LIST(I) = PACKAGES(I)
        ENDDO
        TOTAL_LIST = TOTPACK
      ENDIF
C
C ****  Set the remarks for the packages
C
      DO I = 1, TOTAL_LIST
        CALL WORD(PACKAGE_LIST(I),J,K,L)
        PACK_REM(I) = 'Select Package '//PACKAGE_LIST(I)(1:L)
      ENDDO
C
C ****  Get option from screens if there are more than
C ****  one option to choose from
C
      IF ( TOTAL_LIST .GT. 1 ) THEN
        CALL DISPLAY_ITEMS
     &    (TOTAL_LIST,PACKAGE_LIST,PACK_REM,'SELECT PACKAGE',OUTNUM)
      ELSE
        OUTNUM = 1
      ENDIF
      PICKED = .FALSE.
      IF ( OUTNUM .NE. 0 ) THEN
C
C ****  Picking the chosen bank and setting PICK_FLAG on
C
        CALL WORD(PACKAGE_LIST(OUTNUM),I,J,LPACK)
        CHOSEN_BANK = 'PX_'//PACKAGE_LIST(OUTNUM)(1:LPACK)//'_RCP'
        CALL WORD(CHOSEN_BANK,I,J,LPACK)
        CALL EZTELL(RCPFILE,FLEN)
C
        IF ( RCPFILE(1:FLEN) .NE. CHOSEN_BANK(1:FLEN) ) THEN
          IF ( PICK_FLAG ) THEN
            CALL EZRSET                 ! Resetting previous RCP file
          ENDIF
          CALL EZPICK(CHOSEN_BANK(1:LPACK))
          PICK_FLAG = .TRUE.
          PICKED = .TRUE.
        ENDIF
      ENDIF
  999 RETURN
C
      ENTRY PX_RESET_SELECT_PACKAGE
      IF ( PICK_FLAG ) THEN
        PICK_FLAG = .FALSE.
        CALL EZRSET
      ENDIF
      RETURN
C
      ENTRY PX_SET_PACKAGE(PICKED_BANK)
      CALL WORD(PICKED_BANK,I,J,LPACK)
      CALL EZTELL(RCPFILE,FLEN)
      IF ( RCPFILE(1:FLEN) .NE. PICKED_BANK(1:FLEN) ) THEN
        IF ( PICK_FLAG ) THEN
          CALL EZRSET                 ! Resetting previous RCP file
        ENDIF
        CALL EZPICK(PICKED_BANK(1:LPACK))
        PICK_FLAG = .TRUE.
      ENDIF
      RETURN
C
      ENTRY PX_CHECK_PICK(FLAG)
      IF ( PICKED ) THEN
        FLAG = .TRUE.
      ELSE
        FLAG = .FALSE.
      ENDIF
      RETURN
      END
