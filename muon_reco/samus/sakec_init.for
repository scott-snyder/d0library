      SUBROUTINE SAKEC_INIT(INIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Read startup D0 EC (EM & IH) settings
C-
C-   Inputs  :    INIT    --  flag: INIT.eq.0 -> don't read STP etc.
C-   Outputs :
C-   Controls:
C-
C-   Created  19-MAR-1994   Igor V. Mandrichenko
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    CRYO_LEN
      PARAMETER   (CRYO_LEN=10.)
      INCLUDE 'D0$INC:SAKFIT.INC'
      INTEGER J,ILAYER,IETA,IPHI,OKAY,INIT,ISIDE
      REAL    X,Y,Z
      REAL  RAD_LEN,ABS_LEN
C----------------------------------------------------------------------
c      REAL  RADL_ECEM(4),RADL_ECIH(5)
c      DATA  RADL_ECEM/0.3, 2.6, 7.9, 9.3/
c      DATA  RADL_ECIH/4*30.45,34.32/
C----------------------------------------------------------------------
      IF(MUKA_CONST_INIT.NE.0) RETURN           ! Already initialized
      IF(INIT.ne.0) THEN
c        CALL MZEBRA (0)                   ! INITIALIZE ZEBRA
c        CALL INZCOM (2)                   ! Initialize /ZEBCOM/
c        CALL INZLNK                       ! initialize ZLINKA
c        CALL INPAWC                       ! initialize HBOOK4
         call CALOR_INI
      END IF
C<<
      IETA = -25          ! Eta = -2.5, crosses ECEM and ECIH
      IPHI = 1
      DO ISIDE = 1,2
        J = 1
        EC_TOTLEN(ISIDE) = 0.
C
C ****  Make ECEM
C
        DO  ILAYER = 1,3
          CALL CELXYZ(IETA, IPHI, ILAYER, X, Y, Z, OKAY)
          IF( OKAY.EQ.0 ) THEN
            EC_ZMED(J,ISIDE) = Z
            CALL CAL_DEPTH(IETA, ILAYER, EC_LEN(J,ISIDE),
     +            RAD_LEN, ABS_LEN, OKAY)
            EC_TOTLEN(ISIDE) = EC_TOTLEN(ISIDE) + EC_LEN(J,ISIDE)
            J = J+1
          END IF
        END DO
C-
C- Layers 4-6 are actually sublayers of 3'th layer,
C- they have the same Z space.
C-
        ILAYER = 7
        CALL CELXYZ(IETA, IPHI, ILAYER, X, Y, Z, OKAY)
        IF( OKAY.EQ.0 ) THEN
          EC_ZMED(J,ISIDE) = Z
          CALL CAL_DEPTH(IETA, ILAYER, EC_LEN(J,ISIDE),
     +            RAD_LEN, ABS_LEN, OKAY)
          EC_TOTLEN(ISIDE) = EC_TOTLEN(ISIDE) + EC_LEN(J,ISIDE)
          J = J+1
        END IF
C
C ****  Make ECIH
C
        DO  ILAYER = 11,15
          CALL CELXYZ(IETA, IPHI, ILAYER, X, Y, Z, OKAY)
          IF( OKAY.EQ.0 ) THEN
            EC_ZMED(J,ISIDE) = Z
            CALL CAL_DEPTH(IETA, ILAYER, EC_LEN(J,ISIDE),
     +            RAD_LEN, ABS_LEN, OKAY)
            EC_TOTLEN(ISIDE) = EC_TOTLEN(ISIDE) + EC_LEN(J,ISIDE)
            J = J+1
          END IF
        END DO
        IETA = -IETA
C<<
        EC_NMOD(ISIDE) = J-1
C<<
      END DO          ! Side = 1,2
C<<
  999 RETURN
      END
