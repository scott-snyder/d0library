      LOGICAL FUNCTION COMPUTE_EM_QUALITY
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : get electron/photon quality and
C-                         update PELC/PPHO banks.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  27-OCT-1992   Meenakshi Narain
C-   Updated   5-FEB-1994   Meenakshi Narain  : protect links in ZLINKA 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
      INTEGER TRK,STATUS,IER,LCLUS,VERSION
      INTEGER GZPELC,GZPPHO,ILINK
      LOGICAL ELECT,OK
C----------------------------------------------------------------------
      COMPUTE_EM_QUALITY = .TRUE.
C... reserve a link or two...
      CALL GSLINK('CEMLINK',ILINK)
C
C ****  get electron/photon quality
C
      LCLUS = GZPELC()
      LSLINK(ILINK) = LCLUS
      TRK   = 1
      ELECT = .TRUE.
      IF (LCLUS.EQ.0) THEN
        LCLUS = GZPPHO()
        LSLINK(ILINK) = LCLUS
        TRK   = 0
        ELECT = .FALSE.
      ENDIF
      DO WHILE (LSLINK(ILINK).NE.0)
        VERSION = IQ(LSLINK(ILINK)+1)
        CALL CLEANEM(LSLINK(ILINK),TRK,OK,STATUS)
        IF (VERSION.EQ.1) THEN
            IQ(LSLINK(ILINK)+20) = STATUS
        ELSE IF (VERSION.EQ.2) THEN
          IF (ELECT) THEN
            IQ(LSLINK(ILINK)+20) = STATUS
          ELSE
            IQ(LSLINK(ILINK)+23) = STATUS
          ENDIF
        ELSE
          IQ(LSLINK(ILINK)+30) = STATUS
        ENDIF
        LSLINK(ILINK) = LQ(LSLINK(ILINK))
        IF (LSLINK(ILINK).EQ.0..AND.ELECT) THEN
          LSLINK(ILINK) = GZPPHO()
          TRK   = 0
          ELECT = .FALSE.
        ENDIF
      END DO
      CALL RSLINK('CEMLINK',ILINK)
  999 RETURN
      END
