C----------------------------------------------------------------------
C-
C-   COMMON /VZHITS/ is for communication between z-strip hit
C-                   digitization routines.
C-   Contents:
C-      NZHITS = total # of zstrip hits
C-      NZHTMX = max # of hits that can be stored.
C-              If NZHITS > NZHTMX, data is lost
C-      ZHIT_SORT = integer on which the data is sorted
C-      ZHIT_INDX = index array giving order of hits sorted on ZHIT_SORT
C-      ZHIT_ADDR = array of z-strip addresses
C-      ZHIT_TIME = array of hit times
C-      ZHIT_PHGT = array of pulse heights
C-      ZHIT_PWID = array of pulse widths
C-      ZHIT_TRAK = array of track ids
C-      ZHIT_STRP = array of floating strip numbers
C-
C-   Created  10-NOV-1989   Peter Grudberg
C-
C----------------------------------------------------------------------
      INTEGER NZHITS, MZHTMX, NZHTMX
      PARAMETER ( MZHTMX = 3200 )
      INTEGER ZHIT_SORT(MZHTMX), ZHIT_INDX(MZHTMX), ZHIT_ADDR(MZHTMX)
      INTEGER ZHIT_TRAK(MZHTMX)
      REAL ZHIT_TIME(MZHTMX), ZHIT_PHGT(MZHTMX)
      REAL ZHIT_PWID(MZHTMX), ZHIT_STRP(MZHTMX)
      COMMON /VZHITS/ NZHITS, NZHTMX, ZHIT_SORT, ZHIT_INDX, ZHIT_ADDR,
     &                ZHIT_TIME, ZHIT_PHGT, ZHIT_PWID, ZHIT_TRAK,
     &                ZHIT_STRP
