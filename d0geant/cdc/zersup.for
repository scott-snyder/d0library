      SUBROUTINE ZERSUP (DTTYPE, IBIMAX, IFLASH, NCLUS, ILEAD, ITRAIL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Performs the zero-suppression:
C-                         For the 512 bins data of one Flash ADC, finds
C-                         the NCLUS clusters, and stores the Leading
C-                         edge and the trailing edge of each one.
C-
C-   Inputs  :    DTTYPE = detector type (0:VTX; 1:CDC; 2:FDC)
C-                IBIMAX = mixmum bin number of FADC
C-                IFLASH = data for one Flash ADC
C-
C-   Outputs :    NCLUS  = # of clusters found
C-                ILEAD  = Leading edge for each cluster
C-                ITRAIL = Trailing edge for each cluster
C-
C-   Created  11-AUG-1987   Ghita Rahal-Callot : Adapted for the
C-                          Monte-carlo from C.Klopfenstein routine
C-                          CDRHIT.
C-            09-sep-1987   G. Rahal-Callot : correct LENGTH
C-   Updated  12-FEB-1988   Ghita Rahal-Callot   : correct for the clusters
C-                          with a trailing edge at the end of the FADC 
C-   Updated  15-JUN-1989   Qizhong Li-Demarteau  correct it according to
C-                          "Zero-suppression Chip Description 08/25/88"
C-   Updated  07-NOV-1989   Peter Grudberg - reorganize routine to speed up
C-                          processing
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZZEROP.INC'
C
C
      INTEGER DTTYPE
      INTEGER IFLASH( 0:* )
      INTEGER NCLUS, IBIMAX
      INTEGER ILEAD( * ), ITRAIL( * )
C
      LOGICAL PULSE
      INTEGER IBIN, ISTART
      INTEGER IDIF, IDIF0, LENGTH
C
C----------------------------------------------------------------------
C
C ****  Loop on all the channels of the Flash ADC
C
      NCLUS = 0
      PULSE = .FALSE.
      IBIN = 0
      DO WHILE ( IBIN .LT. IBIMAX - 2 )
        IF ( .NOT. PULSE ) THEN
C
C ****  Search for leading edge (note: BINTH2 = BINTH1)
C
          IF ( IFLASH(IBIN+2) .GE. BINTH2(DTTYPE) ) THEN
            IF ( IFLASH(IBIN+1) .GE. BINTH2(DTTYPE) .AND.
     &           IFLASH(IBIN) .GE. BINTH2(DTTYPE) ) THEN
              PULSE = .TRUE.
            ELSEIF ( IFLASH(IBIN+1) .GT. BINTH1(DTTYPE) .AND.
     &               IFLASH(IBIN) .GT. BINTH1(DTTYPE) ) THEN
              IDIF0 = IFLASH(IBIN+1) - IFLASH(IBIN)
              IDIF  = IFLASH(IBIN+2) - IFLASH(IBIN+1)
              IF ( IDIF0 .GT. DIFTH1(DTTYPE) .AND.
     &             IDIF  .GT. DIFTH1(DTTYPE) ) THEN
                PULSE = .TRUE.
              ELSE
                IBIN = IBIN + 1
                GO TO 100
              ENDIF
            ELSE
              IBIN = IBIN + 1
              GO TO 100
            ENDIF
          ELSEIF ( IFLASH(IBIN+2) .GT. BINTH1(DTTYPE) ) THEN
            IF ( IFLASH(IBIN+1) .GT. BINTH1(DTTYPE) .AND.
     &           IFLASH(IBIN) .GT. BINTH1(DTTYPE) ) THEN
              IDIF0 = IFLASH(IBIN+1) - IFLASH(IBIN)
              IDIF  = IFLASH(IBIN+2) - IFLASH(IBIN+1)
              IF ( IDIF0 .GT. DIFTH1(DTTYPE) .AND.
     &             IDIF  .GT. DIFTH1(DTTYPE) ) THEN
                PULSE = .TRUE.
              ELSE
                IBIN = IBIN + 1
                GO TO 100
              ENDIF
            ELSE
              IBIN = IBIN + 1
              GO TO 100
            ENDIF
          ELSE
            IBIN = IBIN + 3
            GO TO 100
          ENDIF
C
C ****  Leading edge found: increment cluster counter, save address of
C ****  beginning
C
          ISTART = IBIN
          NCLUS = NCLUS + 1
          ILEAD(NCLUS) = MAX(ISTART-4, 1)
        ENDIF
C
        IF ( PULSE ) THEN
C
C ****  Search for trailing edge (note: BINTH4 <= BINTH3)
C
          IF ( IFLASH(IBIN) .LT. BINTH4(DTTYPE) ) THEN
            IF ( IFLASH(IBIN+1) .LT. BINTH4(DTTYPE) .AND.
     &           IFLASH(IBIN+2) .LT. BINTH4(DTTYPE) ) THEN
              PULSE = .FALSE.
            ELSEIF ( IFLASH(IBIN+1) .LT. BINTH3(DTTYPE) .AND.
     &               IFLASH(IBIN+2) .LT. BINTH3(DTTYPE) ) THEN
              IDIF0 = IFLASH(IBIN+1) - IFLASH(IBIN)
              IDIF  = IFLASH(IBIN+2) - IFLASH(IBIN+1)
              IF ( IDIF0 .GT. DIFTH2(DTTYPE) .AND.
     &             IDIF0 .LT. DIFTH3(DTTYPE) .AND.
     &             IDIF  .GT. DIFTH2(DTTYPE) .AND.
     &             IDIF  .LT. DIFTH3(DTTYPE) ) THEN
                PULSE = .FALSE.
              ELSE
                IBIN = IBIN + 1
                GO TO 100
              ENDIF
            ELSE
              IBIN = IBIN + 1
              GO TO 100
            ENDIF
          ELSEIF ( IFLASH(IBIN) .LT. BINTH3(DTTYPE) ) THEN
            IF ( IFLASH(IBIN+1) .LT. BINTH3(DTTYPE) .AND.
     &           IFLASH(IBIN+2) .LT. BINTH3(DTTYPE) ) THEN
              IDIF0 = IFLASH(IBIN+1) - IFLASH(IBIN)
              IDIF  = IFLASH(IBIN+2) - IFLASH(IBIN+1)
              IF ( IDIF0 .GT. DIFTH2(DTTYPE) .AND.
     &             IDIF0 .LT. DIFTH3(DTTYPE) .AND.
     &             IDIF  .GT. DIFTH2(DTTYPE) .AND.
     &             IDIF  .LT. DIFTH3(DTTYPE) ) THEN
                PULSE = .FALSE.
              ELSE
                IBIN = IBIN + 1
                GO TO 100
              ENDIF
            ELSE
              IBIN = IBIN + 1
              GO TO 100
            ENDIF
          ELSE
            IBIN = IBIN + 1
            GO TO 100
          ENDIF
        ENDIF
C
C ****  Trailing edge found:  make the length of the cluster a multiple of 4
C ****  and save the address ITRAIL
C
        LENGTH = IBIN - ILEAD(NCLUS) + 1
        LENGTH = 4 * ((LENGTH-1)/4 + 1)
        ITRAIL(NCLUS) = MIN(ILEAD(NCLUS) + LENGTH - 1,
     &                      IBIMAX - 1)
        ILEAD(NCLUS) = ITRAIL(NCLUS) - LENGTH + 1
  100   CONTINUE
      ENDDO
C
C ****  Trailing edge not found for the last cluster
C ****  Put the trailing edge value to the last time-slice, and push the
C ****  leading edge in order to have a length multiple of 4
C
      IF ( PULSE ) THEN
        ITRAIL(NCLUS) = IBIMAX - 1
        LENGTH = ITRAIL(NCLUS) - ILEAD(NCLUS) + 1
        LENGTH = 4 * ((LENGTH-1)/4 + 1)
        ILEAD(NCLUS) = ITRAIL(NCLUS) - LENGTH + 1
      ENDIF
  999 RETURN
      END
