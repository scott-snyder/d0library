      SUBROUTINE ISA_NUS_SUM(NUS_SUM,OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-        sum over neutrino 4-vectors in ISAJET
C-   
C-   Outputs : 
C-   NUS_SUM(4)= 4-vector sum over neutrinos
C-   OK = false if no ISAJET banks available
C-
C-   Created  27-NOV-1991   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL NUS_SUM(4)
      LOGICAL OK
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZISP1.LINK'
      INTEGER LISAL,GZISAL,ID,I,LISV1,GZISV1,LISP1
C----------------------------------------------------------------------
C
      OK=.FALSE.
      LISAL=GZISAL()
      DO I=1,4
        NUS_SUM(I)=.0001   ! do not set exactly to 0 to avoid underflows
      ENDDO
C
      IF ( LISAL.GT.0 ) THEN  ! loop over lepton banks if available
        OK=.TRUE.
  10    CONTINUE
        ID=IABS(IQ(LISAL+1))
        IF(ID.EQ.11.OR.ID.EQ.13.OR.ID.EQ.15) THEN
          DO I=1,4
            NUS_SUM(I)=NUS_SUM(I)+Q(LISAL+I+1)
          ENDDO
        ENDIF
        LISAL=LQ(LISAL)
        IF(LISAL.GT.0) GOTO 10
C
      ELSE                   ! loop over vertices and particles
        LISV1=GZISV1()
        IF(LISV1.GT.0) THEN
          OK=.TRUE.
  20      LISP1=LISV1-IZISP1
          IF(LISP1.GT.0) THEN
  21        ID=IABS(IQ(LISP1+1))
            IF(ID.EQ.11.OR.ID.EQ.13.OR.ID.EQ.15) THEN
              DO I=1,4
                NUS_SUM(I)=NUS_SUM(I)+Q(LISP1+I+1)
              ENDDO
              LISP1=LQ(LISP1)
              IF(LISP1.GT.0) GOTO 21
            ENDIF
            LISV1=LQ(LISV1)
            IF(LISV1.GT.0) GOTO 20
          ENDIF
        ENDIF
      ENDIF  
  999 RETURN
      END
