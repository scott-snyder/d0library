      SUBROUTINE CAPSTP (FILNAM,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read Calorimeter CAPACITANCE banks into ZEBSTP. 
C-                              Modeled on CDISTP.FOR by
C-                              Olivier and Ghita Callot
C-
C-   Inputs  : FILNAM [C*]      Input file name
C-   Outputs : IERR [I]          0 if open was succesful
C-
C-   Created 31-JUL-1991   Jan Guida, Chip Stewart   
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL OK
      INTEGER LSTPC
      CHARACTER*(*) FILNAM
      INTEGER LUNIN,L,LBANK,IERR, LCCPH
C
      INCLUDE 'D0$LINKS:IZSTPC.LINK'
      INCLUDE 'D0$LINKS:IZSCAL.LINK'
      INCLUDE 'D0$LINKS:IZCCPH.LINK'
      INCLUDE 'D0$LINKS:IZCCPT.LINK'
      INCLUDE 'D0$LINKS:IZCCPC.LINK'
      INCLUDE 'D0$LINKS:IZCCUA.LINK'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      OK=.TRUE.
C     
      IF(FIRST) THEN   ! do this only once
C
        CALL INZSTP      ! initialize ZEBSTP
C
C ****  Check for existence of STPC bank
C
        LSTPC = LC ( LSTPH - IZSTPC )
        IF ( LSTPC .LE. 0 ) THEN
C
          CALL CONSTP                       ! Construct STP headers
C
C ****  Create SCAL bank
C
          CALL BKSCAL ('STPC',LSCAL)
C
C ****  Create CCPH bank
C
          CALL BKCCPH (0,LCCPH)
C
        ENDIF
C
C ****  Open the requested file
C
        CALL GTUNIT(80,LUNIN,IERR)
        L = LEN (FILNAM)
        CALL D0OPEN (LUNIN,FILNAM(1:L),'IU',OK)
        IF ( .NOT. OK ) THEN
          IERR = -1
          GOTO 999
        ELSE
          IERR= 0
        END IF
C
C ****  Declare file to ZEBRA
C
        CALL FZFILE( LUNIN, 0, 'I' )
C
C ****  Read in data banks
C
        LSTPC = LC ( LSTPH - IZSTPC )
        LSCAL = LC ( LSTPC - IZSCAL )
        IF (LSCAL.LE.0)  CALL BKSCAL ('STPC',LSCAL)
        LCCPH = LC ( LSCAL - IZCCPH )
        IF (LCCPH.LE.0) CALL BKCCPH(0,LCCPH)
        IF (INDEX(FILNAM,'CCPT').GT.0) THEN
          CALL FZIN  ( LUNIN, IDVSTP, LCCPH, -IZCCPT, ' ', 0, 0 )
        ELSE IF (INDEX(FILNAM,'CCPC').GT.0) THEN
          CALL FZIN  ( LUNIN, IDVSTP, LCCPH, -IZCCPC, ' ', 0, 0 )
        ELSE IF (INDEX(FILNAM,'CCUA').GT.0) THEN
          CALL FZIN  ( LUNIN, IDVSTP, LCCPH, -IZCCUA, ' ', 0, 0 )
        ELSE
          CALL ERRMSG('BAD CAP BANK FILE','CAPSTP',
     &      'NO FILE FETCHED','W')
          IERR = -1
          GOTO 999
        END IF
C
C ****  Terminate and Close file
C
        CALL FZENDI( LUNIN, 'TU' )
        CLOSE (UNIT=LUNIN)
C
C ****  Check for  bank
C
        LSTPC = LC( LSTPH - IZSTPC )
        LSCAL = LC( LSTPC - IZSCAL )
        IF ( LSCAL .LE. 0 ) THEN
          CALL ERRMSG ('NO CAP BANKS','CAPSTP','LSCAL is ZERO !!!!','F')
        ENDIF
C
        LCCPH = LC( LSCAL - IZCCPH )
        IF ( LCCPH .LE. 0 ) THEN
          CALL ERRMSG ('NO CAP BANKS','CAPSTP','LCCPH is ZERO !!!!','F')
        ENDIF
C
        IF (INDEX(FILNAM,'CCPT').GT.0) THEN
          LBANK = LC( LCCPH - IZCCPT )
        ELSE IF (INDEX(FILNAM,'CCPC').GT.0) THEN
          LBANK = LC( LCCPH - IZCCPC )
        ELSE IF (INDEX(FILNAM,'CCUA').GT.0) THEN
          LBANK = LC( LCCPH - IZCCUA )
        END IF
C
        IF ( LBANK .LE. 0 ) THEN
          CALL ERRMSG ('NO CAP BANKS','CAPSTP','ADDRESS is ZERO !','F')
        ENDIF
C
        FIRST=.FALSE.
      ENDIF
C
  999 RETURN
      END
