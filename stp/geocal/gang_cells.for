      SUBROUTINE GANG_CELLS(IETA, IPHI, IDEPTH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : THIS SUBROUTINE REMOVES THE 'SLAVE' CELL
C-        THAT IS TO BE GANGED TO ANOTHER CELL.  IT HAS SUCH CHANGES
C-        HARDWIRED INTO IT.  IT WILL ATTACT THE ZEBRA BANKS OF THE
C-        SLAVE CELL (AS SUB-CELLS) TO THE MASTER CELL.  IT THEN
C-        DROPS THE SLAVE CELL.
C-
C-   Inputs  :     IETA      eta of slave cell
C-                 IPHI      phi of slave cell
C-                 IDEPTH    layer of slave cell
C-   Outputs : 
C-   Controls:     through Zebra banks
C-   Zebra banks added:      CLYR
C-   Zebra banks dropped:    CLYR
C-
C-   Created  15-DEC-1989   Stephen Kahn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CLINKS.INC'
      INCLUDE 'D0$INC:LCLYR.INC'
      INCLUDE 'D0$PARAMS:CLYR.PARAMS'
      INCLUDE 'D0$LINKS:IZCLYR.LINK'
      INCLUDE 'D0$LINKS:IZSCLR.LINK'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
C
      INTEGER IETA, IPHI, IDEPTH, JBYT, LZLAST, JDEPTH, LZBYT, KPHI
      INTEGER LERR
      DATA LERR / 6 /
C
      IF( IETA.EQ.15 .AND. IDEPTH.EQ.MXLYCH) THEN     ! cell 15/17 to be
                                        ! ganged to 15/15
        JDEPTH = MNLYCH                 ! jdepth=15 (master cell)
        JQCLYR = LC(LQCETA-IZCLYR-JDEPTH+1)      ! master cell
        LQCLYR = LC(LQCETA-IZCLYR-IDEPTH+1)      ! slave cell
  100   IF(LQCLYR .EQ. 0) GO TO 150     ! loop on subcell banks
        IF( JBYT(iC(LQCLYR), JBSBCL, NBSBCL) .EQ. 0) THEN
          KQCLYR = JQCLYR
  120     KQCLYR = LZBYT(IXSTP, KQCLYR, 0, JBSBCL, NBSBCL)
          IF( KQCLYR .EQ. 0) THEN       ! error
            WRITE( LERR,*) 'NO MATCH UP OF GANGED CELLS ', IPHI
            GO TO 130
          END IF
          KPHI = JBYT(IC(KQCLYR+ICELID), JBIPHI, NBIPHI)
          IF ( KPHI .NE. IPHI ) GO TO 120
          CALL ZSHUNT(IXSTP,LQCLYR,KQCLYR,-IZSCLR,0)
                                        ! copy slave CRSCEL bank
                                        ! 1st link of master
        END IF
  130   LQCLYR = LC(LQCLYR)
        GO TO 100
  150   CONTINUE
        LQCLYR = LC(LQCETA-IZCLYR-IDEPTH+1)
        CALL ZSHUNT(IXSTP,LQCLYR,JQCLYR,0,1)
                                        ! copy slave linear bank
                                        ! structure to end of master
      END IF        
C----------------------------------------------------------------------
  999 RETURN
      END
