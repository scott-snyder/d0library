      SUBROUTINE MU_CCT_CF_BOT(A,B,C,JBITS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Tests CCT trigger for Central bottom octants.
C-
C-   Inputs  : A(12),B(20),C(20) - Input coarse centroids (already or'ed by 4)
C-
C-   Outputs : JBITS(12) - CCT trigger output
C-
C-   Controls: None
C-
C-   Created     SEP-1991   M. Fortner
C-   Updated  04-SEP-1991   K. Bazizi
C-                  Use a new logic for the CENTRAL muon system which
C-      is derived from a detailed study of 10,000 single muon tracks
C-      with Pt=5 and 50GeV run through D0GEANT
C-   Updated     NOV-1992   K. Bazizi, G.Lima
C-        Complete reevaluation of CCT logic (version 2):
C-      * Allows for A*B and A*C combinations only where B and C layers
C-        have big cracks, except in the punchthrough dominated regions.
C-      * No B*C combinations allowed.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER BC(12),A(12),B(20),C(20),JBITS(12),I
      INTEGER IDIM
      PARAMETER (IDIM=70)
      INTEGER IROAD(IDIM),ILOG,IVERS,IER
      CHARACTER*80 STRING

      DATA ILOG/2/
      DATA IVERS/2/

      LOGICAL FIRST
      DATA FIRST/.TRUE./

C..  Get trigger logic number 
      IF (FIRST) THEN
        FIRST=.FALSE.

        CALL EZPICK('MUSIM_RCP')
        CALL EZERR(IER)     ! Check if error
        IF(IER.NE.0) THEN
          CALL EZGET_ERROR_TEXT(IER,STRING)
          CALL ERRMSG(' EZPICK ERR','MU_CCT_CF_BOT',STRING,'F')
          GOTO 999
        ENDIF
C
        CALL EZGET('WAM_CCT_VERS',IVERS,IER)
        IF(IER.NE.0) THEN
          CALL EZGET_ERROR_TEXT(IER,STRING)
          CALL ERRMSG(' WAM_CCT_VERS','MU_CCT_CF_BOT',STRING,'F')
          GOTO 999
        ENDIF
C
        CALL EZRSET()
      ENDIF
C
C.. Calculate first stage pal logic
      IF ( IVERS.EQ.1 ) THEN
        BC(1) = B(3)
        BC(2) = B(4)
        BC(3) = C(1)        + C(2)        + C(3)
        BC(4) = B(5)        + C(3)        + C(4)
        BC(5) = B(6)
        BC(6) = B(7)*C(5)   + B(7)*C(6)   + B(8) *C(7)  + B(8) *C(8)
        BC(7) = B(14)*C(16) + B(14)*C(15) + B(13)*C(14) + B(13)*C(13)
        BC(8) = B(15)
        BC(9) = B(16)       + C(18)       + C(17)
        BC(10)= C(20)       + C(19)       + C(18)
        BC(11)= B(17)
        BC(12)= B(18)
      ENDIF
      IF ( IVERS.EQ.2 ) THEN
        BC(1) = B(2)        + B(3)        + B(4)
        BC(2) = B(4)
        BC(3) = C(1)        + C(2)        + C(3)
        BC(4) = B(5)        + C(2)        + C(3)        + C(4)
        BC(5) = B(6)        + B(7)
        BC(6) = B(7)        + B(8)        + C(13) 
        BC(7) = B(14)       + B(13)       + C(8)  
        BC(8) = B(15)       + B(14)
        BC(9) = B(16)       + C(19)       + C(18)       + C(17)
        BC(10)= C(20)       + C(19)       + C(18)
        BC(11)= B(17)
        BC(12)= B(19)       + B(18)       + B(17)
      ENDIF

C.. Renormalize BC(1,2,..,12) to 1 for use in the logic below
      DO I=1,12
        IF(BC(I).GT.0) BC(I)=1
      ENDDO

C.. Second stage pal logic
      DO I=1,12
        JBITS(I)=0
        IF ((A(I)*BC(I)).NE.0) JBITS(I)=1
      ENDDO
      IF ( IVERS.EQ.1 ) THEN
        JBITS(6) = 0
        IF (BC(6).NE.0) JBITS(6) = 1
        JBITS(7) = 0
        IF (BC(7).NE.0) JBITS(7) = 1
      ENDIF

C--
      DO I=1,12
        IF(JBITS(I).NE.0) CALL HFILL(1040+ILOG,FLOAT(I),0.,1.)
      ENDDO

C.. compute different roads independently for monitoring
      DO I=1,IDIM
        IROAD(I)=0
      ENDDO

      IROAD(2)   = A(1) *B(2)
      IROAD(3)   = A(1) *B(3)
      IROAD(4)   = A(1) *B(4)

      IROAD(8)   = A(2) *B(4)

      IROAD(12)  = A(3)       *C(1)
      IROAD(13)  = A(3)       *C(2)
      IROAD(14)  = A(3)       *C(3)

      IROAD(18)  = A(4) *B(5)
      IROAD(19)  = A(4)       *C(2)
      IROAD(20)  = A(4)       *C(3)
      IROAD(21)  = A(4)       *C(4)

      IROAD(25)  = A(5) *B(6)
      IROAD(26)  = A(5) *B(7)

      IROAD(30)  = A(6) *B(7)       
      IROAD(31)  = A(6) *B(8)
      IROAD(32)  = A(6) *C(13)     

      IROAD(36)  = A(7) *C(8)
      IROAD(37)  = A(7) *B(13)
      IROAD(38)  = A(7) *B(14)

      IROAD(42)  = A( 8)*B(14)
      IROAD(43)  = A( 8)*B(15)

      IROAD(47)  = A( 9)      *C(17)
      IROAD(48)  = A( 9)      *C(18)
      IROAD(49)  = A( 9)      *C(19)
      IROAD(50)  = A( 9)*B(16)

      IROAD(54)  = A(10)      *C(18)
      IROAD(55)  = A(10)      *C(19)
      IROAD(56)  = A(10)      *C(20)

      IROAD(60)  = A(11)*B(17)

      IROAD(64)  = A(12)*B(17)
      IROAD(65)  = A(12)*B(18)
      IROAD(66)  = A(12)*B(19)


      DO I=1,IDIM
        IF(IROAD(I).NE.0) CALL HFILL(1030+ILOG,FLOAT(I),0.,1.)
      ENDDO

C      CALL WAMUS_ROADS(1200,A,B,C,JBITS)

  999 RETURN
      END
