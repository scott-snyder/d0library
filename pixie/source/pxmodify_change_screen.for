      SUBROUTINE PXMODIFY_CHANGE_SCREEN(CURRENT_RCP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Activates a requested RCP file.  If the current RCP
C-   file matches the requested one it will do nothing, otherwise it will EZRSET
C-   the current RCP file and it will read the requested one returning the name
C-   of the bank 
C-
C-   Inputs  : CURRENT_RCP [C*]: Name of the current RCP bank 
C-   
C-   Outputs : CURRENT_RCP [C*]: Name of the current RCP bank
C-
C-   Created  27-NOV-1991   Lupe Howell
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) CURRENT_RCP
C
      CHARACTER*80 FILENAME,BANK_NAME
      INTEGER FILSIZ,I,J,CL,BLEN,IER
C----------------------------------------------------------------------
      CALL WORD(CURRENT_RCP,I,J,CL)
C
C ****  Display the available RCP files to choose 
C ****  the one to activate
C
      CALL PUZSEL(FILENAME,FILSIZ)
      IF( FILSIZ .EQ. 0 ) GOTO 999
C
C ****  Interpreting the file name requested
C
      I = INDEX(FILENAME,'.RCP')
      J = INDEX(FILENAME,':')
      IF ( I .GT. 0 ) THEN
        FILSIZ = I - 1
      ENDIF
      IF ( J .EQ. 0 ) THEN
        J = INDEX(FILENAME,']')
      ENDIF
      BANK_NAME = FILENAME(J+1:FILSIZ)//'_RCP'
      FILENAME = FILENAME(1:FILSIZ)//'.RCP'
      FILSIZ = FILSIZ + 4
      BLEN = FILSIZ - J
C
C ****  If the file chosen is the same as the current RCP file 
C ****  exit the routine othewise EZRSET the current RCP file
C
      IF ( CURRENT_RCP(1:CL) .EQ. BANK_NAME(1:BLEN) )THEN
        GOTO 999
      ELSE
        CALL EZRSET
      ENDIF
C
C ****  Reading file requested 
C
      CALL INTMSG(' Reading file: '//FILENAME)
      CALL INRCP(FILENAME,IER)
      IF ( IER .NE. 0 )THEN
        CALL INTMSG(' Problems acessing file '//FILENAME(1:FILSIZ))
        GOTO 999
      ENDIF
C
C ****  Picking selected RCP file
C
      CALL EZPICK(BANK_NAME(1:BLEN))
C
C ****  Filling common block with selected file
C
      CALL PXBUILD_READ(BANK_NAME(1:BLEN))
C
C ****  Sending out the name of the selected bank
C
      CURRENT_RCP = BANK_NAME(1:BLEN)
  999 RETURN
      END
