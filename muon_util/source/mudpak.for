      SUBROUTINE MUDPAK(IERR)
C     ====================================================
C     THIS ROUTINE UNPACKS MUD1 AND FILLS MUHT MUHP AND MUOF
C
C     Output : IERR  (0 if everything is OK)
C
CC    CREATED:  7/93    M. Fortner
C
C     ======================================================
      IMPLICIT NONE
      INTEGER IERR
      INTEGER DUM1,DUM2,DUM3,DUM4
      INTEGER LCRATE,LNEXT,ICRID,IVRUN,JERR
      INTEGER GZMUHP,GZMUOF
      EXTERNAL GZMUHP,GZMUOF
C     ======================================================
C
CC              Initialize
      IERR=0
      JERR=0
      LCRATE = 0
      IF (GZMUHP(0).NE.0.OR.GZMUOF(0).NE.0) GOTO 999  ! Banks already unpacked
      CALL MUHTFL(1,DUM1,DUM2)
      CALL MUOFFL(1,DUM1,DUM2,DUM3)
      CALL MUHPFL(1,DUM1,DUM2,DUM3,DUM4)
C
CC              Loop over crates and fill pointer banks
 1000 CONTINUE
C
CC    -- get header and trailer for one crate
      CALL MUDCHK(LCRATE,LNEXT,ICRID,IVRUN,JERR)
      IF (JERR.LT.0) THEN
          IERR=JERR
          CALL MUHTFL(5,JERR,DUM2)
          GO TO 998    ! FATAL ERRORS
      ENDIF
      IF (JERR.GT.0) THEN            ! Trailer/header mismatch
          IERR=JERR+10*ICRID
      ENDIF
C
CC    --  unpack crate data
      IF(IVRUN.LE.0) THEN
          CALL MUDU1A(LCRATE)       ! Run 1A data format
      ELSE
          CALL MUDU1B(LCRATE)       ! Run 1B data format
      END IF
      LCRATE = LNEXT
      IF (LCRATE.NE.0) GOTO 1000
C
  998 CONTINUE
C
CC    --  compress pointer banks
      CALL MUHTFL(99,DUM1,DUM2)
      CALL MUOFFL(99,DUM1,DUM2,DUM3)
      CALL MUHPFL(99,DUM1,DUM2,DUM3,DUM4)
C
  999 RETURN
      END
