      SUBROUTINE CSFSTP (FILNAM,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read Calorimeter Sampling Fraction 
C-                         banks into ZEBSTP. 
C-
C-   Inputs  : FILNAM [C]  Input file name
C-   Outputs : IERR   [I]  0 if open was succesful
C-
C-   Created 21-SEP-1990   Chip Stewart   
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL OK
      CHARACTER*(*) FILNAM
      INTEGER LUNIN,L,LBANK,IERR, LCSFH
C
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
        CALL FZIN  ( LUNIN, IDVSTP, LCSFH, 2, ' ', 0, 0 )
C
C ****  Check for  bank
C
        IF ( LCSFH .LE. 0 ) THEN
          CALL ERRMSG ('CAL','CSFSTP','LCSFH is ZERO !!!!','F')
        ELSE
          CALL GZCSFH_INIT(LCSFH)
        ENDIF
C
        FIRST=.FALSE.
C
C ****  Terminate and Close file
C
        CALL FZENDI( LUNIN, 'TU' )
        CLOSE (UNIT=LUNIN)
        CALL RLUNIT(80,LUNIN,IERR)
      ENDIF
C
  999 RETURN
      END
