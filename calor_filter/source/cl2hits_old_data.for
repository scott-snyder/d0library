      SUBROUTINE CL2HITS_OLD_DATA(SFTVSN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : force special address translation for old data to
C-      be available for CL2 table building, before have read in data to see
C-      that we have this type of data
C-
C-   Inputs  : CADT banks; SFTVSN to use
C-   Outputs : modified CADT banks
C-   Controls: 
C-
C-   Created   5-JUN-1991   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:STP_ZLINKA.INC'   ! for CADT_FIX
      INTEGER CADT_LINK(2,0:5)
      INTEGER SFTVSN                    ! software version to use
      INTEGER NCAD,NCRATE,GZCADT,LCADT
C----------------------------------------------------------------------
C
C ****  SETUP CADT LOOK-UP TABLE
C
      LCADT = GZCADT ()
      DO NCAD = 1, 2
        DO NCRATE = 0, 5
C
C ****  Reserve link in STP_ZLINKA for link to ncad,ncrate
C
          CALL STP_GSLINK('CUNPAK',CADT_LINK(NCAD,NCRATE) )
          STP_LSLINK(CADT_LINK(NCAD,NCRATE)) = LCADT
          IF (LCADT.LE.0) THEN
            CALL ERRMSG('CL2HITS','CL2_OLD_DATA',
     &          'CADT CHAIN BAD','F')
            GOTO 999
          END IF
          LCADT = LC(LCADT)
        END DO
      END DO
      CALL CADT_FORCE_FIX(CADT_LINK,SFTVSN)   ! after link areas established
  999 RETURN
      END
