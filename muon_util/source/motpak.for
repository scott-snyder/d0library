      SUBROUTINE MOTPAK(IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Unpack Muon OTC crates in TRGR bank
C-
C-   Inputs  : 
C-   Outputs : ierr = 0 if everything is OK
C-   Controls: 
C-
C-   Created   28-JAN-1994   M. Fortner
C-   Modified  10/94 MF make errors non-fatal
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
C
      INTEGER IERR
      INTEGER LTRGR,LCRATE,LTRAIL,ICRID,ICRATE,DUMMY(2),NMOTR
      CHARACTER*32 MESSID,CALLER
      CHARACTER*80 MESSAG
      INTEGER GZMTRG,GZTRGR,MUDVER
      EXTERNAL GZMTRG,GZTRGR,MUDVER
      DATA MESSAG/' '/,CALLER/'MOTPAK'/
C
C              Initialize
C
      IERR=0
      LCRATE = 0
      LTRGR = GZTRGR()
      IF (LTRGR.EQ.0) THEN
          IF (MUDVER(0).GE.10) GOTO 999
          IERR = 11
          MESSID = 'MOTPAK: TRGR bank not found'
          CALL ERRMSG(MESSID,CALLER,MESSAG,'W')
          GOTO 999
      ENDIF
      IF (GZMTRG(0).NE.0) GOTO 999  
      LTRAIL = LTRGR + IQ(LTRGR-1) - 16 - 4
      CALL MOTMOD(1,DUMMY(1),DUMMY(2))
C
C              Loop over crates and fill MTRG,MOTR banks
C
      DO WHILE (LTRAIL.GT.LTRGR)
          LCRATE = LTRAIL - IQ(LTRAIL+1) + 4
          IF (LCRATE.LT.LTRGR) THEN
              IERR = 12
              MESSID = 'MOTPAK: TRGR bank pointer error'
              CALL ERRMSG(MESSID,CALLER,MESSAG,'W')
              GOTO 998
          ENDIF
          ICRID = IAND(IQ(LTRAIL+2),255)
          ICRATE = ICRID/10 - 1
          IF (ICRATE.GE.1.AND.ICRATE.LE.5) THEN
              CALL MTRGFL(LCRATE,ICRATE,LTRAIL)
          ENDIF
          LTRAIL = LCRATE - 4
      END DO
C
  998 CONTINUE
C
C       Compress bank
C
      NMOTR = -1
      CALL MOTRFL(DUMMY,DUMMY,DUMMY,DUMMY,NMOTR)
C
  999 RETURN
      END
