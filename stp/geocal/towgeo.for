      SUBROUTINE TOWGEO
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C     This Program creates a Zebra bank structure for the calorimeter
C     geometry static parameters
C
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created   19-JAN-1987  Steve Kahn
C-   Updated   7-APR-1989   Steve Kahn
C-                          -- now a subroutine in CAL_POSTGEO
C-   Updated   2-MAY-1989   Chip Stewart   - SET LQSCAL & LQCGEH
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CLINKS.INC'
      INTEGER GZCGEH
C------------------------------------------------------------------------
C
C ****  SET LQSCAL & LQCGEH
C
      CALL MZLINK(IXSTP,'/CLINKS/',LQSTPH,LQCLYR,LQSTPH)    ! protect
C                                        ! link area
      LQCGEH = GZCGEH()
C
      IF(LQCGEH.LE.0) THEN
        CALL ERRMSG(' CAWSTP ',' TOWGEO ',' CGEH NOT FOUND ','F')
        GOTO 999
      END IF
C
C
      CALL CMAT                 ! Create Material banks that will be used
C
      CALL CREG                 ! Create detector region bank
C
      CALL CLGI                 ! Create inactive cylinder banks
C
      CALL CLGA                 ! Create active cylinder banks
C
      CALL CRYO                 ! Create cryostat description banks
C
      CALL CLAY                 ! Create mechanical layer banks
C
      CALL CPLA                 ! Create plate description banks
C
      CALL CENP                 ! Create Endplate description bank
C
      CALL CSCN                 ! Create Scintillator description bank
C
      CALL CABSRB               ! Create Absorber description banks for Scint
C
      CALL CEDP                 ! Create tower header bank
C
      CALL CETA                 ! Create constant eta banks
C
      CALL CLYR                 ! Create read out cell banks
C
  999 RETURN
      END
