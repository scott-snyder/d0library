      SUBROUTINE VTMH_READ(OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Replace VTX time banks (VTMH on down) with
C-                         structure read from external ZEBRA file.
C-
C-   Inputs  : filename from RCP
C-   Outputs : OK: .TRUE. if all goes well
C-
C-   Created  19-OCT-1992   Peter M. Grudberg
C-   Updated  28-JAN-1993   Liang-ping Chen (P.G) ZSHUNT chains of VDTM  
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZVTMH.LINK'
C
      INTEGER IER, LUN, LEN, GZVTMH, IUSER
      INTEGER LVTMH_OLD, LVTMW, LVTMW_OLD, LAYER, SECTOR
      INTEGER NITEMS, NWIRES, NSECTS, OFFSET, LVDTM
      CHARACTER*60 FILENAME
      CHARACTER*10 CHOPT
      LOGICAL OK, FIRST, OPENED
      DATA IUSER / 666 /
      DATA FIRST / .TRUE. /
C----------------------------------------------------------------------
      OK = .TRUE.
      IF ( .NOT. FIRST ) GO TO 999
      FIRST = .FALSE.
C
      CALL EZPICK('VTRAKS_RCP')
      CALL EZGETS('VTMHFILE',1,FILENAME,LEN,IER)
      CALL EZRSET
C
      CALL GTUNIT(IUSER,LUN,IER)
      CALL D0OPEN(LUN,FILENAME,'IX',OPENED)
      IF ( .NOT. OPENED ) THEN
        CALL ERRMSG('File open error','VTMH_READ',
     &    'Unable to open VTMH file','W')
        OK = .FALSE.
      ENDIF
      CALL XZRECL(LEN,CHOPT)
      CALL FZFILE(LUN,LEN,CHOPT)
C
C ****  Read in new VTMH structure; this will create a linear structure of VTMH
C ****  banks.  Copy the electrostatic category info from the old VTMW banks to
C ****  the new ones, and shunt the VDTM banks to hang under the new VTMW banks.
C ****  Finally, drop the old VTMH structure.
C
      CALL FZIN(LUN,IDVSTP,LSVTX,-IZVTMH,' ',0,0)
      LVTMH = GZVTMH()
      IF ( LVTMH .GT. 0 ) LVTMH_OLD = LC(LVTMH)
      IF ( LVTMH .GT. 0 .AND. LVTMH_OLD .GT. 0 ) THEN
        DO LAYER = 0, 2
          LVTMW = LC(LVTMH-1-LAYER)
          LVTMW_OLD = LC(LVTMH_OLD-1-LAYER)
          NITEMS = IC(LVTMW+3)
          NWIRES = IC(LVTMW+4)
          NSECTS = IC(LVTMW+5)
          OFFSET = NITEMS*NWIRES*NSECTS + 6
          DO SECTOR = 0, NSECTS - 1
            IC(LVTMW+OFFSET+SECTOR) = IC(LVTMW_OLD+OFFSET+SECTOR)
            LVDTM = LC(LVTMW_OLD-1-SECTOR)
            IF ( LVDTM .GT. 0 )
     &          CALL ZSHUNT(IXSTP,LVDTM,LVTMW,-SECTOR-1,1)
          ENDDO
        ENDDO
        CALL MZDROP(IXSTP,LVTMH_OLD,' ')
        LVTMH = GZVTMH() ! Make sure link is correct in ZEBSTP
      ENDIF
C
      CALL FZENDI(LUN,'QT')
      CLOSE(LUN)
      CALL RLUNIT(IUSER,LUN,IER)
C
  999 RETURN
      END
