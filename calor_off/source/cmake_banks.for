      SUBROUTINE CMAKE_BANKS(ELECTRON,ECLUS,DCLA,NZTRAKS,ETRANS,
     &                        PHI_LO,PHI_HI,THETA_LO,THETA_HI,LEVEL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : MAKES AND FILLS PPHO OR PELC BANKS
C-
C-   Inputs  : ELECTRON = .TRUE. PELC BANK BOOKED AND FILLED
C-                      = .FALSE. PPHO BANK BOOKED AND FILLED
C-             LZTRAK_ELECTRON  = LINK TO ZTRAK PERTAINING TO ELECTRON
C-   Outputs : 
C-   Controls: 
C-
C-   Created   6-APR-1990   Rajendran Raja
C-   Updated  11-APR-1994   Qizhong Li-Demarteau  update # of PELC/PPHO
C-                                                in PARH bank 
C-   Updated  28-FEB-1995   Meenakshi Narain  Increase size of pelc and ppho
C-                          (version 6 and above) by 4 words 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      LOGICAL ELECTRON,DO_HMATRIX
      REAL DCLA,NZTRAKS,ETRANS,ENERGY_MAX,ECLUS
      REAL PHI_LO,PHI_HI,THETA_LO,THETA_HI
      INTEGER LEVEL
C----------------------------------------------------------------------
      IF ( ELECTRON ) THEN
C
        CALL BKPELC(LPARH,35,LPELC)
        CALL PELCFL(DCLA,NZTRAKS,ETRANS,ECLUS)
        IQ(LPARH+3) = IQ(LPARH+3) + 1
C
C ****  Now book and fill subsidiary banks
C
        CALL BKHMTE(LPELC,LHMTE)
        CALL HMTEFL(LHMTE,LEVEL,PHI_LO,PHI_HI,THETA_LO,THETA_HI)
C
      ELSE
C
        CALL BKPPHO(LPARH,35,LPPHO)
        CALL PPHOFL(ETRANS,ECLUS)
        IQ(LPARH+4) = IQ(LPARH+4) + 1
C
C ****  Now book and fill subsidiary banks
C
        CALL BKHMTP(LPPHO,LHMTP)
        CALL HMTPFL(LHMTP,LEVEL,PHI_LO,PHI_HI,THETA_LO,THETA_HI)
C
      ENDIF
C
      END
