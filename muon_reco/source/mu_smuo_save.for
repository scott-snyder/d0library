	SUBROUTINE MU_SMUO_SAVE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Save the muon geometry constants
C-	Intended for use with MU_SMEAR package
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER IUNIT,IERR,GZSMUO
      LOGICAL OK
C
          CALL GTUNIT(789,IUNIT,IERR)
          IF(IERR.EQ.0) THEN
            CALL D0OPEN(IUNIT,'SMUO_SAVE_GEOM.OUT','OU',OK)
            CALL FZFILE(IUNIT,0,'O')
            LSMUO=GZSMUO('STPC')
            IF(LSMUO.NE.0) THEN
              CALL FZOUT(IUNIT,IDVSTP,LSMUO,1,' ',1,0,0)
              CALL INTMSG(' SMUO_SAVE_GEOM.OUT written to disk')
            ENDIF
            CALL FZENDO(IUNIT,'T')
            CLOSE(UNIT=IUNIT)
            CALL RLUNIT(789,IUNIT,IERR)
          ELSE
            CALL INTMSG(' MU_SMUO_SAVE:GTUNIT error ')
          ENDIF

  999 RETURN
      END
