      SUBROUTINE BLVZLA
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create and fill the ZEBRA structure 
C-                         VTXH -- VZLA from hits stored in the common
C-                         VZHITS.  This routine handles the two z strip 
C-                         layers associated with one wire layer. 
C-
C-   Inputs  : Hits stored in VZHITS
C-     Contents of VZHITS:
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
C-        ZLAYER in VTLOCA is the inner of the two proceesed layers
C-
C-   Outputs : VZLA is filled
C-   Controls: none
C-
C-   Created  16-NOV-1989   Peter Grudberg
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GCUNIT.INC'
      INCLUDE 'D0$INC:VZHITS.INC'
      INCLUDE 'D0$INC:VTLOCA.INC'
C
      INTEGER NWDSHT
      PARAMETER ( NWDSHT = 11 )
      INTEGER IHIT, IORD, IZHIT(NWDSHT), IZLAY, LAYNXT, INOUT
      REAL ZHIT(NWDSHT)
      EQUIVALENCE (ZHIT, IZHIT)
C----------------------------------------------------------------------
      IF ( NZHITS .LE. 0 ) GO TO 999
      IF ( NZHITS .GT. NZHTMX ) THEN
        WRITE(LOUT,*) ' **** BLVZLA: Too many hits, excess lost'
        NZHITS = NZHTMX
      ENDIF
C
C ****  Sort the hits on ZHIT_SORT:
C
      CALL SORTZV(ZHIT_SORT,ZHIT_INDX,NZHITS,-1,0,0)
C
C ****  Process the hits in order
C
      IHIT = 1
      DO INOUT = 0, 1
        IZLAY = ZLAYER + INOUT
        LAYNXT = IZLAY
        DO WHILE ( IHIT .LE. NZHITS .AND. LAYNXT .EQ. IZLAY )
          IORD = ZHIT_INDX(IHIT)
          IZHIT(1)  = ZHIT_ADDR(IORD)   ! packed address
          ZHIT(2)   = ZHIT_STRP(IORD)   ! floating strip #
          ZHIT(3)   = 0.3               ! error in strip #
          ZHIT(4)   = ZHIT_TIME(IORD)   ! drift time
          ZHIT(5)   = 7.0               ! drift time error (ns)
          ZHIT(6)   = ZHIT_PHGT(IORD)   ! pulse area
          ZHIT(7)   = SQRT(ZHIT(6))     ! pulse area error
          ZHIT(8)   = ZHIT(6)           ! peak, same as area for now
          ZHIT(9)   = ZHIT_PWID(IORD)   ! pulse width (ns)
          IZHIT(10) = 0                 ! status word
          IZHIT(11) = ZHIT_TRAK(IORD)   ! track id
C
C ****  Load hit into VZLA
C
          CALL LDVZLA( ZHIT, IZLAY )
C
C ****  Prepare for next hit
C
          IHIT = IHIT + 1
          IF ( IHIT .LE. NZHITS ) THEN
            LAYNXT = IBITS( ZHIT_ADDR(ZHIT_INDX(IHIT)), 9, 3)
          ENDIF
        ENDDO
C
C ****  All done with layer; trim unused portion of VZLA bank
C
        CALL PUVZLA( IZLAY )
C
      ENDDO
C
  999 RETURN
      END
