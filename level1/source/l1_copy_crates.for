      SUBROUTINE L1_COPY_CRATES ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Copy the Level 0 crate from the input event 
C-                         (found in TRGR bank one down in the linear chain) 
C-                         to the output TRGR bank.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created   3-JUN-1993   Philippe Laurens - MSU L1 Trigger
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:L1SIM_CONTROL.INC'
C
      INTEGER LTRGR, LTRGR_OLD
      INTEGER LTRGR_OLD_LEVEL0, LTRGR_OLD_L0_END 
      INTEGER L0_CRATE_LENGTH
      INTEGER NEW_L0_OFFSET
C
      INTEGER I
C
      INTEGER      L0_CRATE_ID, TRAILER_LENGTH 
      PARAMETER    ( L0_CRATE_ID = 1, TRAILER_LENGTH = 4 )
C
      INTEGER  GZTRGR, GZFIND_CRATE, GZFIND_CRATE_TRAILER_WAS
      EXTERNAL GZTRGR, GZFIND_CRATE, GZFIND_CRATE_TRAILER_WAS
C-----------------------------------------------------------------------
C    Are we suppposed to look for and copy the old L0 crate 
C    
      IF ( COPY_L0_CRATE .EQV. .FALSE. ) GOTO 999
C
C    Find The Original Trgr Bank As The Second In The Linear Chain
C
      LTRGR = GZTRGR ()
      LTRGR_OLD = LQ(LTRGR)
C
      IF ( LTRGR_OLD .LE. 0 ) THEN
        CALL ERRMSG( 'Copy Level 0 Crate', 'L1_COPY_CRATES',
     &               ' No TRGR bank in input event', 'W')
        GOTO 999
      ENDIF
C
C    Find The Original Level 0 Crate 
C    Begining, Trailer and length
C
      LTRGR_OLD_LEVEL0 = GZFIND_CRATE ( 'TRGR', LTRGR_OLD, L0_CRATE_ID )
      IF ( LTRGR_OLD_LEVEL0 .LE. 0 ) THEN
        CALL ERRMSG( 'Copy Level 0 Crate', 'L1_COPY_CRATES',
     &               ' could not find L0 Crate in input event', 'W')
        GOTO 999
      ENDIF
C
      LTRGR_OLD_L0_END = GZFIND_CRATE_TRAILER_WAS () 
      IF ( LTRGR_OLD_L0_END .LE. 0 ) THEN
        CALL ERRMSG( 'Copy Level 0 Crate', 'L1_COPY_CRATES',
     &               ' could not find end of L0 crate in input event', 
     &               'W')
        GOTO 999
      ENDIF
C      
      L0_CRATE_LENGTH = LTRGR_OLD_L0_END 
     &                - LTRGR_OLD_LEVEL0 
     &                + TRAILER_LENGTH
C
C      Extend The Trgr Bank 
C      (The Data cable trailer has not yet been written at this time)
C
      CALL L1UTIL_TRGR_EXTENSION ( LTRGR, L0_CRATE_LENGTH, 
     &                             NEW_L0_OFFSET ) 
C
C    Redo all the locating work in case the extension of the TRGR bank 
C    caused a garbage collection
C
      LTRGR = GZTRGR ()
      LTRGR_OLD = LQ(LTRGR)
      LTRGR_OLD_LEVEL0 = GZFIND_CRATE ( 'TRGR', LTRGR_OLD, L0_CRATE_ID )
C      
C   Copy The Level 0 Crate Data And Trailer
C
      DO I = 1 , L0_CRATE_LENGTH 
        IQ( LTRGR + NEW_L0_OFFSET + I ) = IQ( LTRGR_OLD_LEVEL0 + I - 1) 
      ENDDO
C
C----------------------------------------------------------------------
  999 RETURN
      END
