C----------------------------------------------------------------------
C-
C-   COMMON /VZDATA/ is for communication between z-strip data
C-                   digitization routines.
C-   Contents:
C-      NZDATA = total # of zstrip pulses
C-      NZDTMX = max # of pulses that can be stored.
C-              If NZDATA > NZDTMX, data is lost
C-      ZDAT_SORT = integer on which the data is sorted
C-      ZDAT_INDX = index array giving order of hits sorted on ZDAT_SORT
C-      ZDAT_ADDR = array of z-strip addresses
C-      ZDAT_TIME = array of pulse times
C-      ZDAT_PHGT = array of pulse heights
C-      ZDAT_PWID = array of pulse widths
C-      ZDAT_TRAK = array of track ids
C-
C-   Created  10-NOV-1989   Peter Grudberg
C-
C----------------------------------------------------------------------
      INTEGER NZDATA, MZDTMX, NZDTMX
      PARAMETER ( MZDTMX = 16000 )
      INTEGER ZDAT_SORT(MZDTMX), ZDAT_INDX(MZDTMX), ZDAT_ADDR(MZDTMX)
      INTEGER ZDAT_TRAK(MZDTMX)
      REAL ZDAT_TIME(MZDTMX), ZDAT_PHGT(MZDTMX), ZDAT_PWID(MZDTMX)
      COMMON /VZDATA/ NZDATA, NZDTMX, ZDAT_SORT, ZDAT_INDX, ZDAT_ADDR,
     &                ZDAT_TIME, ZDAT_PHGT, ZDAT_PWID, ZDAT_TRAK
