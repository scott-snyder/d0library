      SUBROUTINE MU_MAC_INPUT(LMODNO,MACHIT)
C-----------------------------------------------------------------
C  Fill one module with hits in MAC format
C  Assumes existence of MUHT, MUOF and MUD1 banks
C  Created 7-90  M. Fortner
C  Checked 9-28-91 K. Bazizi
C  Fix for corrupted IWADD data 5-15-92 K. Bazizi
C
C  LMODNO is LPMUOF(MODNO), where MODNO is the Phil Martin number
C  MACHIT is array of pad latch hits using columns 2-25
C
C-----------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LMODNO,KDIR,MACHIT(26,4)
      INTEGER I,J,JMUD1
      INTEGER NMOD,NRAW,IMUD1,NPROC,IMUOH,NHPL0,NHPL1,NHPL2,NHPL3
      INTEGER IWADD,IEPHA,IEPHB,IDR1,IDR2,IOPHA,IOPHB,IDELT1,IDELT2
      INTEGER NPLN,NWIR,IERR,IODD,IEVEN
C
C               Initialize MACHIT array
      DO I=1,4
        DO J=1,26
          MACHIT(J,I)=0
        ENDDO
      ENDDO
C
C               Get location of module in MUD1
      CALL GTMUOF(LMODNO,NMOD,NRAW,IMUD1,NPROC,IMUOH,NHPL0,
     &              NHPL1,NHPL2,NHPL3)
C
C               Loop over hits in this module
      DO I=1,NRAW
        JMUD1=IMUD1+(I-1)*9-1
        CALL GTMUDA(JMUD1,IWADD,IEPHA,IEPHB,IDR1,IDR2,IOPHA,IOPHB,
     &                IDELT1,IDELT2)
C
C               decode cell address
        CALL MUADD(IWADD,NMOD,NPLN,NWIR,IERR)
        IF(IERR.EQ.0) THEN
          CALL MULTCH(IWADD,IODD,IEVEN)
          IF (IEVEN.EQ.1) MACHIT(NWIR+2,NPLN+1)=1
          IF (IODD.EQ.1) MACHIT(NWIR+3,NPLN+1)=1
          IF(IEVEN.EQ.0.AND.IODD.EQ.0) THEN
            IF (IEPHA.GT.IOPHA.OR.IEPHB.GT.IOPHB) THEN
              MACHIT(NWIR+2,NPLN+1)=1
            ELSE
              MACHIT(NWIR+3,NPLN+1)=1
            ENDIF
          ENDIF
        ENDIF
      ENDDO
C
      RETURN
      END
