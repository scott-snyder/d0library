      SUBROUTINE GET_TRD_COR_HVT
     &  (PLANE,WIRE,CORRECTION,HVA,HVP,HVW,ERROR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : calculates high voltage corrections
C-
C-   Inputs  : PLANE            integer   1,2,3 for anodes. 4,5,6 for cathodes
C-             WIRE             integer   in [1,256]
C-
C-   Outputs : CORRECTION       real
C-             HVA,HVP,HVW      real      high voltage anode,potential,window
C-             ERROR            integer   0=OK
C-                                        1=correction not required in TRD.RCP
C-                                        2=plane>3 (cathodes)
C-                                        3=old run without HV information
C-                                        4=TROP bank not found
C-                                        5=error during TRD_COR_HV
C-                                        6=data base error
C-   Controls: TRD.RCP
C-
C-   Created  15-JAN-1993   Alain PLUQUET
C-   Updated  22-SEP-1993   A. Zylberstejn  Updated for 512 cells in layer 3 
C-   Updated  21-OCT-1994   Alain PLUQUET correct bug (wrong POT HV number) 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZTROP.LINK'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER PLANE,WIRE,ERROR,IER,DBM_FIRST_RUN,NHVA,NHVP,NHVW
      INTEGER SECTOR,LTROP,STATUS,JBYT
      REAL CORRECTION,HVA,HVP,HVW
      LOGICAL FIRST,DO_CORRECTION,READ_DBMON,RUN1A
      DATA FIRST /.TRUE./
      IF (FIRST) THEN
        FIRST=.FALSE.
        CALL EZPICK('TRD_RCP')
        CALL EZGET('COR_HVT',DO_CORRECTION,IER)
        CALL EZGET('READ_DBMON',READ_DBMON,IER)
        CALL EZGET('DBM_FIRST_RUN',DBM_FIRST_RUN,IER)
        CALL EZRSET
      ENDIF
      IF(READ_DBMON.AND.IQ(LHEAD+12).GT.DBM_FIRST_RUN) THEN
        LTROP=LC(LTGEN-IZTROP)
        IF(LTROP.GE.0)THEN
c         STATUS=JBYT(IC(LTROP+2),5,4)
          STATUS=0 ! until TROP bank is released
          IF (STATUS.EQ.0) THEN
            SECTOR=(WIRE-1)/16+1
            IF(PLANE.EQ.1) THEN
              NHVA=SECTOR
            ELSEIF(PLANE.EQ.2) THEN
              NHVA=(SECTOR-1)/2+17
            ELSEIF(PLANE.EQ.3) THEN
              IF (.NOT.RUN1A()) SECTOR=(WIRE-1)/32+1
              NHVA=(SECTOR-1)/2+25
            ENDIF
            NHVP=3*((SECTOR-1)/8)+32+PLANE
            NHVW=NHVP+8
            HVA=1000.*C(LTROP+5+NHVA)
            HVP=1000.*C(LTROP+5+NHVP)
            HVW=1000.*C(LTROP+5+NHVW)
            IF(DO_CORRECTION) THEN
              IF (PLANE.LE.3) THEN
                CALL TRD_CORHV(CORRECTION,PLANE,HVA,HVP,IER)
                IF (IER.NE.0) THEN
                  CORRECTION=1.
                  ERROR=5
                ELSE
                  ERROR=0
                ENDIF
              ELSE
                CORRECTION=1.
                ERROR=2 
              ENDIF
            ELSE
              CORRECTION=1.
              ERROR=1 
            ENDIF
          ELSE
            CORRECTION=1.
            ERROR=6 
          ENDIF
        ELSE
          CORRECTION=1.
          ERROR=4 
        ENDIF
      ELSE
        CORRECTION=1.
        ERROR=3 
      ENDIF
      END
