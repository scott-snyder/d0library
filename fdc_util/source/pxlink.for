      SUBROUTINE PXLINK
     &  (IOFSET,DRIFT,Z,NHITS,WIRBEG,WIREND,MXPASS,NLK,LK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create links in USER bank area from hits.
C-     Make two-hit links in one phi sector of Forward Drift Chamber
C-     Loop over active hits & build links. Active hits are hits in 
C-     current sector which have not been used in a segment.
C-
C-   Inputs  : IOFSET = Link offset in LFLOC bank structure
C-             DRIFT,Z = Drift and Z values for each hit
C-             NHITS(0:NBPSEN-1) = Number of hits on each wire
C-             WIRBEG,WIREND = First and last sense wire to use.
C-             MXPASS = Max gap allowed in chain + 1, max num passes
C-             NLK = Number of links for each wire for each pass.
C-             LK = Location of first link for each wire for each pass.
C-   Outputs : none, fill LINK banks
C-
C-   Created   6-AUG-1990   Jeffrey Bantly
C-   Updated  29-APR-1991   Jeffrey Bantly  use new RCP,PARAMS 
C-   Updated  13-JUN-1991   Jeffrey Bantly  links must go towards adj sector 
C-   Updated  25-NOV-1991   Robert E. Avery  Use Z array (eliminate DELTAZ).
C-   Updated  29-MAY-1992   Susan K. Blessing  Add NLK and LK to call
C-    to drastically reduce time spent in PXTREE.
C-   Updated  29-JUN-1993   Susan K. Blessing  Use stand alone bank FLOC
C-    rather than USER bank.
C-   Updated 5-MAR-1994  Rcy C. Thatcher - Fixed so D0entry picks up name
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INCLUDE 'D0$INC:FLOCAL.INC/LIST'
C
      INTEGER IOFSET                    ! Link offset in USER bank for LINKs
      REAL    DRIFT(0:NBPSEN-1,MX_HIT_WIRE*2) ! Drift Dists for hits (wire,hit)
      REAL    Z(0:NBPSEN-1,MX_HIT_WIRE*2) ! Z location of hits (wire,hit)
      INTEGER NHITS(0:NBPSEN-1)         ! Number of hits per wire
      INTEGER WIRBEG,WIREND             ! Beginning and ending wires to use
      INTEGER MXPASS                    ! Maximum gap allowed in one link
C
      INTEGER IDIR                      ! Direction of adjacent sector
      INTEGER IPASS                     ! Current max allowed gap in wires
      INTEGER NPASS                     ! IPASS with sign of IDIR
      INTEGER ILINK                     ! link counter
      INTEGER LLINK                     ! Link of current LINK bank
      INTEGER LNKLOC                    ! Location in LINK of current link
      INTEGER IXLINK                    ! MZFORM value of LINK bank format
      INTEGER NWORD                     ! Max number of words in LINK bank
      INTEGER WIRE1,WIRE2               ! Current wire, wire NPASS away
      INTEGER IHIT1,IHIT2               ! Current hits on wire1,wire2
      INTEGER ISTEP                     ! Step direction to next wire
      INTEGER ICALL                     ! initialization check
      INTEGER IER                       ! error check
      INTEGER N,NLK(0:NBPSEN-1,2),LK(0:NBPSEN-1,2)
      INTEGER PREV_WIRE1,PREV_IPASS
C
      REAL    SL                        ! slope dY/dZ of current hits
      REAL    AVSL                      ! average slope of cur hit on wire1
      REAL    TOLSL                     ! (RCP) max slope of link allowed
C
      SAVE ICALL,IXLINK,TOLSL,NWORD
      DATA ICALL/0/
C----------------------------------------------------------------------
C
      IF (ICALL.EQ.0) THEN
        CALL MZFORM('LINK','/4I 1F 5I',IXLINK)
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('TOLSL',TOLSL,IER)
        CALL EZRSET
        NWORD = (10*MAXLNK)
        ICALL = 1
      END IF
C
C  Initialize variables and USER bank link 
C
      N = 0
      PREV_WIRE1 = -1
      PREV_IPASS = -1
      CALL VZERO(NLK,2*NBPSEN)
      CALL VZERO(LK,2*NBPSEN)
      ILINK = 0
      IF (LFLOC.LE.5) GOTO 999
      IDIR = 1
      IF (WIRBEG.GT.WIREND) IDIR = -1
      IF (IOFSET.LT.0) IDIR = -1*IDIR
      IOFSET = ABS(IOFSET)
      ISTEP = 1
      IF (WIRBEG.GT.WIREND) ISTEP = -1
C
C  Loop over passes from 1 to MXPASS, where the IPASS value is the
C  current gap size between links.
C
      DO 100 IPASS = 1,MXPASS
        NPASS = IPASS*ISTEP
C
C  Loop over wires from first with a hit to wire one IPASS gap before last
C  wire with hit.
C
        DO 200 WIRE1 = WIRBEG,WIREND-IPASS,ISTEP
          IF (WIRE1.LT.0) GO TO 200
          WIRE2 = WIRE1+NPASS             ! Wire2 automatically set by cur gap
          IF (WIRE2.LT.0) GO TO 200
          IF (NHITS(WIRE1).LE.0) GO TO 200    ! Skip if no hits on start wire 
          IF (NHITS(WIRE2).LE.0) GO TO 200    ! Skip if no hits on end wire
C
C  Loop over all pairs of hits on the two wires and save those pairs that 
C  link together as a link in the current LINK bank.  
C
          DO 300 IHIT1 = 1,NHITS(WIRE1)
            AVSL = 0.01*ABS(DRIFT(WIRE1,IHIT1))
C
            DO 500 IHIT2 = 1,NHITS(WIRE2)
              SL = (DRIFT(WIRE2,IHIT2)-DRIFT(WIRE1,IHIT1))
     &               / ABS(Z(WIRE2,IHIT2)-Z(WIRE1,IHIT1))
              IF (SL*IDIR .LT. 0.0) GOTO 500  ! slope must go towards
C                                               ! other sector.
C
              IF ((ABS(SL)-AVSL).LT.TOLSL) THEN ! slope within tolerance
C
C  Store link information in LINK bank for the sector.
C
                ILINK = ILINK+1
                LLINK = LQ(LFLOC-IOFSET)
                IF (LLINK.LE.0) THEN     ! Create ZEBRA bank LINK (link bank)
                  CALL MZBOOK(IXMAIN,LLINK,LFLOC,-IOFSET,'LINK',0,0,
     &              NWORD,IXLINK,0)
                END IF
                LLINK = LQ(LFLOC-IOFSET)
                LNKLOC = LLINK + (ILINK-1)*10
                IF ((LLINK+IQ(LLINK-1)-10) .LT. LNKLOC) THEN
                  CALL MZPUSH(IXCOM,LLINK,0,20*10,'I')
                  LLINK = LQ(LFLOC-IOFSET)
                  LNKLOC = LLINK + (ILINK-1)*10
                END IF
C
C  Put link data into bank.
C
                IQ(LNKLOC+1) = IHIT1    ! hit on start wire
                IQ(LNKLOC+2) = IHIT2    ! hit on end wire
                IQ(LNKLOC+3) = WIRE1    ! start wire
                IQ(LNKLOC+4) = WIRE2    ! end wire
                Q (LNKLOC+5) = SL       ! slope = dy/dx
                IQ(LNKLOC+6) = 0        ! number of branch links (1-4)
                IQ(LNKLOC+7) = 0        ! \ 
                IQ(LNKLOC+8) = 0        !  branching link information 
                IQ(LNKLOC+9) = 0        !  filled by PXTREE
                IQ(LNKLOC+10) = 0       ! /
                N = N + 1
                NLK(WIRE1,IPASS) = NLK(WIRE1,IPASS) + 1
                IF (WIRE1.NE.PREV_WIRE1.OR.IPASS.NE.PREV_IPASS) THEN
                  LK(WIRE1,IPASS) = N
                  PREV_WIRE1 = WIRE1
                  PREV_IPASS = IPASS
                END IF
              END IF
  500       CONTINUE                    ! End loop over IHIT2
  300     CONTINUE                      ! End loop over IHIT1
  200   CONTINUE                        ! End loop over WIRE1
C
        IF (ILINK.GT.MAXLNK) THEN       ! Too many links accruing
          MXPASS = IPASS                ! Reset MXPASS to reflect cut
          GOTO 50                       ! Exit out of IPASS loop
        END IF
C
  100 CONTINUE                          ! End loop over IPASS.
C
   50 CONTINUE
C
      IQ(LFLOC+IOFSET) = ILINK          ! Record the number of links
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
