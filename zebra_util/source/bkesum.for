      SUBROUTINE BKESUM(STYP,NOBJ,LESUM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book ESUM
C-                          It also maintains the link area for the 'FILT'
C-                          version of the bank
C-
C-   Inputs  : STYP = CHARARCTER*4  Summary Type 'FILT','TRGR','RECO','ISAE'
C-             NOBJ = # of objects to allocate space for. If bank is already
C-                    booked, increase size by this # of objects
C-             also initializes the link area for this bank on first call
C-   Outputs : LESUM= new pointer to ESUM
C-   Controls:
C-
C-   Created  10-DEC-1991   Richard V. Astur
C-   Updated  11-DEC-1991   James T. Linnemann  new size, zeroing, and
C-                                              link handling
C-   Updated   8-JAN-1992   James T. Linnemann  -> ESUM, linear chain
C-   Updated  15-MAR-1992   R.V. Astur : increase size, add jet types
C-   Modified 25-JUN-1992   U. Heintz: drop existing ESUM bank if it is 
C-                                     not of version ESUM_VERSION. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NR
      PARAMETER (NR = 6 ) !per object
      INTEGER NFIX
      PARAMETER( NFIX = 30 )  !fixed header
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:L2ESUMLK.INC'
      INCLUDE 'D0$LINKS:IZESUM.LINK'
      INCLUDE 'D0$PARAMS:ESUM.PARAMS'
      CHARACTER*(*) STYP
      INTEGER NOBJ, LESUM, LSUP, NSIZ
      INTEGER IOH, GZHSUM,GZESUM, ISTYP
      LOGICAL FIRST
      SAVE FIRST, IOH
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        CALL MZLINK(IXCOM,'/L2ESUMLK/',L2ESUM,L2ESUM,L2ESUM)  ! link area
        CALL MZFORM('ESUM','29I 1H/1I 1B 4F',IOH)
        FIRST = .FALSE.
      END IF
      LESUM = GZESUM(STYP)
      IF( (LESUM.GT.0).AND.(IQ(LESUM+1).NE.ESUM_VERSION) )THEN ! if old version 
        CALL MZDROP(IXCOM,LESUM,'.')                    ! drop the bank
        LESUM = GZESUM(STYP)                  ! get new pointer (should be =0)
      ENDIF
      IF ( LESUM .LE. 0 ) THEN  ! need to book the bank
C...get support if necessary
        LSUP = GZHSUM()
        IF (LSUP .LE. 0) CALL BKHSUM( LSUP )
        IF (LSUP .LE. 0) THEN
          CALL ERRMSG('Can''t book','BKESUM','Can''t book HSUM','E')
          GO TO 999
        END IF
C...initial booking; set fixed part of bank to zero
        NSIZ = NR*NOBJ + NFIX
        CALL MZBOOK(IXMAIN,LESUM,LSUP,-IZESUM,'ESUM',0,0,NSIZ,IOH,NFIX)
        IQ( LESUM + 1 ) = ESUM_VERSION  ! version number
        IQ( LESUM + 2 ) = NFIX    ! fixed length part
        IQ( LESUM + 3 ) = NR      ! Repetition size
        CALL UCTOH (STYP,ISTYP,4,4)
        IQ( LESUM + NFIX ) = ISTYP   ! summary type
      ELSE  ! expand bank by NOBJ objects
        CALL MZPUSH(IXCOM, LESUM, 0, NR*NOBJ,' ')
      END IF
      IF ( LESUM .LE. 0 ) THEN     !double check
        CALL ERRMSG('BKESUM', 'BKESUM',
     &    'Booking or extension of ESUM failed','F')
      END IF
C...initialize link area for FILT version of the bank
      IF (STYP.EQ.'FILT') L2ESUM = LESUM
  999 RETURN
      END
