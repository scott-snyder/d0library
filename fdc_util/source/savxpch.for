      SUBROUTINE SAVXPCH(IOFSET,IDEPTH,LINK,IPASS)
C------------------------------------------------------------------------
C
C  Purpose and Methods: Store chain found by subroutine CLIMB
C                       in Zebra bank CHAI.
C
C  Input: IOFSET = Link offset to current LINK bank link in USER bank
C         IDEPTH = Number of links on chain.
C         LINK   = Array of links on chain.
C         IPASS  = Number of allowed missing wires, i.e.
C                  For IPASS=1 store chains with no missing wires,
C                      IPASS=2                   1  missing wire,etc.
C
C-   Created  xx-DEC-1988   Daria Zieminska
C-   Updated  28-FEB-1990   Jeffrey Bantly  clean up
C-   Updated   8-NOV-1990   Jeffrey Bantly  add max num segs option 
C-   Updated  29-APR-1991   Jeffrey Bantly  use new RCP,PARAMS 
C-   Updated  13-JUN-1991   Jeffrey Bantly  cleanup, add comments 
C-   Updated  29-JUN-1993   Susan K. Blessing  Use stand alone bank FLOC
C-    rather than USER bank.
C
C------------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INCLUDE 'D0$INC:FLOCAL.INC/LIST'
C
      INTEGER IOFSET                    ! link offset to LINK bank
      INTEGER IDEPTH                    ! number of links on chain
      INTEGER LINK(NBPSEN-1)            ! array of links on chain
      INTEGER IPASS                     ! pass, number of allowed
C                                       ! missing wires
C
      INTEGER IGAP                      ! gap in wires in current link
      INTEGER ILINK                     ! current link
      INTEGER IWR1                      ! first wire in current link
      INTEGER LCHAI                     ! CHAI bank link
      INTEGER LLINK                     ! LINK bank link
      INTEGER LOCL                      ! current location in LINK bank
      INTEGER LSTART                    ! current location in CHAI bank
      INTEGER NCHAIN                    ! number of chains in CHAI
      INTEGER NHIT                      ! number of hits on chain
      INTEGER NWORDS                    ! number of words in CHAI bank
      PARAMETER (NWORDS=NBPSEN*MXNSEG+1)
      INTEGER ICALL                     ! initialization check
      INTEGER IER                       ! error check
C
      LOGICAL NEW                       ! true if current chain is new
C
      SAVE ICALL
      DATA ICALL /0/
C-----------------------------------------------------------------------
      IF (ICALL.EQ.0) THEN
        ICALL=1
      ENDIF
      NHIT=IDEPTH+1                   ! number of hits on chain
C
C  Fetch CHAI bank links and create ZEBRA bank CHAI if not present
C
      LCHAI=LQ(LFLOC-2)
      IF (LCHAI.LE.0) THEN
        CALL MZBOOK(IXMAIN,LCHAI,LFLOC,-2,'CHAI',0,0,NWORDS,2,0)
      END IF
      NCHAIN=IQ(LCHAI+1)
      IF (NCHAIN.GE.MXNSEG) GO TO 999   ! do not exceed array sizes
C
C  Check to make sure chain has not been stored before by looping over links
C  and looking for a gap between wires corresponding to current pass size.
C
      NEW=.FALSE.
      LLINK=LQ(LFLOC-IOFSET)
      DO 100 ILINK=1,IDEPTH
        LOCL=LLINK+(LINK(ILINK)-1)*10
        IGAP=IQ(LOCL+4)-IQ(LOCL+3)
        IF (ILINK.EQ.1) THEN            ! first link checks
          IWR1=IQ(LOCL+3)
C                                       ! is first wire or first link
C                                       ! gap equal to the pass size
          IF (IOFSET.EQ.1) THEN
            IF (IWR1.EQ.IPASS-1.OR.ABS(IGAP).EQ.IPASS) THEN
              NEW=.TRUE.
              GO TO 300
            END IF
          ELSEIF (IOFSET.GT.1) THEN
            IF (IWR1.EQ.NBPSEN-IPASS.OR.ABS(IGAP).EQ.IPASS) THEN
              NEW=.TRUE.
              GO TO 300
            END IF
          ENDIF
        ELSE                            ! remaining links
C                                       ! is gap equal to current pass size
          IF (ABS(IGAP).EQ.IPASS) THEN
            NEW=.TRUE.
            GO TO 300
          END IF
        END IF
  100 CONTINUE                          ! End loop over ILINK
  300 CONTINUE
      IF (.NOT. NEW) GO TO 999
C
C  Store chain as its links on the chain, if new.
C
      IQ(LCHAI+1)=IQ(LCHAI+1)+1             ! increment number of chains
      NCHAIN=IQ(LCHAI+1)
      LSTART=LCHAI+1+(NCHAIN-1)*16
      IQ(LSTART+1)=NHIT
      DO 200 ILINK=1,IDEPTH
        IQ(LSTART+1+ILINK)=LINK(ILINK)      ! links on chain
  200 CONTINUE
C-------------------------------------------------------------------------
  999 RETURN
      END
