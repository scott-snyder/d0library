      SUBROUTINE CADSTP (FILNAM,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read Calorimeter ADDRESS TABLE banks into ZEBSTP. 
C-                              Modeled on CDISTP.FOR by
C-                              Olivier and Ghita Callot
C-
C-   Inputs  : FILNAM [C*]      Input file name
C-   Outputs : IERR [I]          0 if open was succesful
C-
C-   Created 21-SEP-1990   Chip Stewart   
C-   Updated  30-APR-1992   Chip Stewart   - no IF (FIRST)
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL OK
      INTEGER LSTPC
      CHARACTER*(*) FILNAM
      INTEGER LUNIN,L,LBANK,IERR, LCADT
C
      INCLUDE 'D0$LINKS:IZSTPC.LINK'
      INCLUDE 'D0$LINKS:IZSCAL.LINK'
      INCLUDE 'D0$LINKS:IZCPDH.LINK'
      INCLUDE 'D0$LINKS:IZCGNH.LINK'
      INCLUDE 'D0$LINKS:IZCGEH.LINK'
      INCLUDE 'D0$LINKS:IZCADT.LINK'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      OK=.TRUE.
C     
      IF(.NOT.FIRST) THEN   ! 
        CALL ERRMSG(' OVERWRITE EXISTING CAD_STPFILE ',
     &    'CADSTP',FILNAM,'I')
        CALL ERRMSG(' OVERWRITE EXISTING CAD_STPFILE ',
     &    'CADSTP',FILNAM,'S')
      END IF
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
C ****  Check for existence of STPC, SCAL, CGEH banks
C
      LSTPC = LC ( LSTPH - IZSTPC )
      IF (LSTPC .LE. 0) CALL CONSTP                  ! Construct STP headers
      LSCAL = LC ( LSTPC - IZSCAL )
      IF (LSCAL.LE.0)   CALL BKSCAL('STPC',LSCAL)    ! ****  Create SCAL bank
      LCGEH = LC ( LSCAL - IZCGEH )
      IF (LCGEH.LE.0)   CALL BKCGEH('STPC',LCGEH)    ! ****  Create CGEH bank
      IF (IC(LCGEH-2).LT.IZCADT) THEN
C NOT ENOUGH LINKS FOR CADT
        CALL ERRMSG('CGEH TOO FEW LINKS','CADSTP',' ABORT ','F')
      END IF
C
C ****  Read in data banks
C
      CALL FZIN  ( LUNIN, IDVSTP, LCGEH, -IZCADT, ' ', 0, 0 )
C
C ****  Terminate and Close file
C
      CALL FZENDI( LUNIN, 'TU' )
      CLOSE (UNIT=LUNIN)
      CALL RLUNIT(80,LUNIN,IERR)
C
C ****  Check for  bank
C
      LSTPC = LC( LSTPH - IZSTPC )
      LSCAL = LC( LSTPC - IZSCAL )
      IF ( LSCAL .LE. 0 ) THEN
        CALL ERRMSG ('CAL','CADSTP','LSCAL is ZERO !!!!','F')
      ENDIF
C
      LCADT = LC( LCGEH - IZCADT )
      IF ( LCADT .LE. 0 ) THEN
        CALL ERRMSG ('CAL','CADSTP','LCADT is ZERO !!!!','F')
      ENDIF
C
      FIRST=.FALSE.
C
  999 RETURN
      END
