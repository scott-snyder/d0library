      SUBROUTINE BLVZDA
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create and fill the ZEBRA structure 
C-                         VTXH -- VZLA -- VZDA from hits stored in the common
C-                         VZDATA.  This routine handles the two z strip 
C-                         layers associated with one wire layer. 
C-
C-   Inputs  : Hits stored in VZDATA
C-     Contents of VZDATA:
C-      NZDATA = total # of zstrip data
C-      NZDTMX = max # of data that can be stored.
C-              If NZDATA > NZDTMX, data is lost
C-      ZDAT_SORT = integer on which the data is sorted
C-      ZDAT_INDX = index array giving order of data sorted on ZDAT_SORT
C-      ZDAT_ADDR = array of z-strip addresses
C-      ZDAT_TIME = array of hit times
C-      ZDAT_PHGT = array of pulse heights
C-      ZDAT_PWID = array of pulse widths
C-      ZDAT_TRAK = array of track ids
C-
C-        ZLAYER in VTLOCA is the inner of the two proceesed layers
C-
C-   Outputs : VZLA is filled
C-   Controls: called by MKVTXS
C-
C-   Created  16-NOV-1989   Peter Grudberg
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GCUNIT.INC'
      INCLUDE 'D0$INC:VZDATA.INC'
      INCLUDE 'D0$INC:VTLOCA.INC'
C
      LOGICAL NOMORE, ENDLAZ
      INTEGER POINT, NHITZ, IZLAY, IADDR
C----------------------------------------------------------------------
      IF ( NZDATA .LE. 0 ) GO TO 999
      IF ( NZDATA .GT. NZDTMX ) THEN
        WRITE(LOUT,*) ' **** BLVZDA: Too many pulses, excess lost'
        NZDATA = NZDTMX
      ENDIF
C
C ****  Sort the data on ZDAT_SORT:
C ****  The pulses are sorted in order of ascending logical address, and within
C ****  that order, are sorted in time order.
C
      CALL SORTZV(ZDAT_SORT,ZDAT_INDX,NZDATA,-1,0,0)
C
C ****  Process the pulses channel by channel, in channel order.
C ****  NXTZST returns the pointer to the first hit (pointing to the index
C ****  array) and the number of hits on the next z strip channel.  Continue
C ****  processing until there are no more hits.
C
      NOMORE = .FALSE.
      DO WHILE ( .NOT. NOMORE )
        CALL NXTZST(POINT, NHITZ, IZLAY, ENDLAZ)
        IF ( NHITZ .GT. 0 ) THEN
          IADDR = ZDAT_ADDR( ZDAT_INDX(POINT) )
          CALL FIVZDA(IADDR, POINT, NHITZ)
C
C ****  Push bank after last hit channel in layer
C
          IF ( ENDLAZ ) CALL PUVZDA( IZLAY )
        ELSE
          NOMORE = .TRUE.
        ENDIF
      ENDDO
  999 RETURN
      END
