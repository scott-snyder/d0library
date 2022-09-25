      SUBROUTINE GET_PJET_TYPE(LPJET,LADDR,ITYPE,PARTONID,WEIGHT,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : For the specified PJET try to classify the
C-   partonic jet according its parentage; that is, according to which
C-   primary parton lies in its history: B, Bbar, W+, W-, T, Tbar, other.
C-   The WEIGHT is the fraction of the PJET energy carried by the partons
C-   linked to one of the above primary partons. Note: ISAJ contains the
C-   T, Tbar, W+ and W- while ISAQ contains the B and Bbar.
C-
C-   Inputs  : LPJET    [I]     Address of PJET bank
C-
C-   Outputs : LADDR    [I]     Address of primary parton bank
C-             ITYPE    [I]     Jet Type code (1..7)
C-             PARTONID [I]     ISAJET particle-id or zero for ITYPE = 7
C-             WEIGHT   [R]     Weight associated with classification
C-             IER      [I]     0 -- OK
C-   Controls:
C-
C-   Created   5-JAN-1991   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LPJET
      INTEGER LADDR
      INTEGER ITYPE
      INTEGER PARTONID
      REAL    WEIGHT
      INTEGER IER
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZPJPT.LINK'
      INCLUDE 'D0$PARAMS:ISAJET_CODES.DEF'
C----------------------------------------------------------------------
      INTEGER LPJPT,LISAQ,LISAJ,LLADD
      INTEGER I,J,JTYPE,NPARTON,LINK,LUN
      INTEGER BT,BB,WP,WM,TP,TB,OTHER,PARTID
      INTEGER NMAX,MAXPAR
      PARAMETER( NMAX   = 7 )
      PARAMETER( MAXPAR = 100 )
      INTEGER PARTON,PARTON_ID(NMAX),LOC(NMAX)
      REAL    BIGGEST,ENERGY,PJET_ENERGY,SUM(NMAX),WT
      CHARACTER*8 NAMEISAQ(MAXPAR),NAMEISAJ(MAXPAR),LABEL,NAME
C----------------------------------------------------------------------
      LOGICAL FIRST
      SAVE FIRST,NAMEISAQ,NAMEISAJ,PARTID,WT
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IER = 0
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        BT = 1
        BB = 2
        WP = 3
        WM = 4
        TP = 5
        TB = 6
        OTHER = 7
        PARTON_ID(BT) = BOTTOM
        PARTON_ID(BB) =-BOTTOM
        PARTON_ID(WP) = W_BOSON
        PARTON_ID(WM) =-W_BOSON
        PARTON_ID(TP) = TOP
        PARTON_ID(TB) =-TOP
        PARTON_ID(OTHER) = 0
      ENDIF
C
C ****  Check address
C
      IF ( LPJET.LE. 0 ) THEN
        CALL ERRMSG('CALORIMETER','GET_PJET_TYPE','LPJET is ZERO','W')
        IER = -1
        GOTO 999
      ENDIF
C
C ****  Get address of pointer bank
C
      LPJPT = LQ(LPJET-IZPJPT)
      IF ( LPJPT.LE. 0 ) THEN
        CALL ERRMSG('CALORIMETER','GET_PJET_TYPE','LPJPT is ZERO','W')
        IER = -1
        GOTO 999
      ENDIF
C
C ****  Loop over ISAQ banks (parton banks) pointed to by PJPT
C
      CALL UZERO(SUM,1,NMAX)
      CALL vZERO_i(LOC,NMAX)
C
      NPARTON = IQ(LPJPT-3) - 1         ! Number of reference links
      NPARTON = MIN(NPARTON,MAXPAR)
C
      DO LINK =  1,NPARTON
        LISAQ = LQ(LPJPT-1-LINK)
        IF ( LISAQ .GT. 0 ) THEN
          PARTON = IQ(LISAQ+1)
          LLADD  = LISAQ
          NAMEISAQ(LINK) = LABEL(PARTON)
C
C ****  Found ISAQ bank; Check if this is a bottom quark
C
          IF     ( PARTON .EQ.  BOTTOM ) THEN
            JTYPE = BT
          ELSEIF ( PARTON .EQ. -BOTTOM ) THEN
            JTYPE = BB
          ELSE
C
C ****  This is NOT a bottom quark so move to ISAJ
C
            LISAJ = LQ(LISAQ-1)
            IF ( LISAJ .GT. 0 ) THEN
              PARTON = IQ(LISAJ+1)
              LLADD  = LISAJ
C
              IF     ( PARTON .EQ.  W_BOSON ) THEN
                JTYPE = WP
              ELSEIF ( PARTON .EQ. -W_BOSON ) THEN
                JTYPE = WM
              ELSEIF ( PARTON .EQ.  TOP ) THEN
                JTYPE = TP
              ELSEIF ( PARTON .EQ. -TOP ) THEN
                JTYPE = TB
              ELSE
                JTYPE = OTHER
                LLADD = 0
              ENDIF
            ELSE
              JTYPE = OTHER
              LLADD = 0
            ENDIF
          ENDIF
C
C ****  Sum energy according to type
C
          ENERGY     = Q(LISAQ+5)
          SUM(JTYPE) = SUM(JTYPE) + ENERGY
          LOC(JTYPE) = LLADD
C
C ****  Get ISAJ label
C
          LISAJ = LQ(LISAQ-1)
          IF ( LISAJ .GT. 0 ) THEN
            NAMEISAJ(LINK) = LABEL(IQ(LISAJ+1))
          ELSE
            NAMEISAJ(LINK) = 'No ISAJ'
          ENDIF
C
        ELSE
          CALL ERRMSG('CALORIMETER','GET_PJET_TYPE',
     &      'Reference link in PJPT to ISAQ is ZERO','W')
        ENDIF
      ENDDO
C
C ****  Now classify according to largest weight
C
      BIGGEST = 0.0
      J = 1
      DO I =  1,NMAX
        IF ( SUM(I) .GT. BIGGEST ) THEN
          BIGGEST = SUM(I)
          J = I
        ENDIF
      ENDDO
C
      PJET_ENERGY = Q(LPJET+6)
      ITYPE    = J
      PARTONID = PARTON_ID(J)
      WEIGHT   = BIGGEST/PJET_ENERGY
      LADDR    = LOC(J)
C
      PARTID = PARTONID
      WT     = WEIGHT
      RETURN
C
      ENTRY DUMP_PJET_TYPE(LUN,LPJET)
      WRITE(LUN,1000) (Q(LPJET+I),I=2,7),
     &                 Q(LPJET+10),Q(LPJET+8),Q(LPJET+9)
      WRITE(LUN,FMT='(1X,2X,''Parton CONTENT'')')
      WRITE(LUN,FMT='(1X,2X,7A8)') (NAMEISAQ(I),I=1,NPARTON)
      WRITE(LUN,FMT='(1X,2X,''Parton PARENT'')')
      WRITE(LUN,FMT='(1X,2X,7A8)') (NAMEISAJ(I),I=1,NPARTON)
      IF ( PARTID .NE. 0 ) THEN
        NAME = LABEL(PARTID)
      ELSE
        NAME = 'Other'
      ENDIF
      WRITE(LUN,FMT=
     &  '(1X,2X,''PJET TYPE : '',A8,2X,''Weight    : '',F5.3)') NAME,WT
      WRITE(LUN,FMT='('' '')')
C
  999 RETURN
 1000 FORMAT(1X,2X,'PJET',/,
     &  1X,6X,'Pt',6X,'Px',6X,'Py',6X,'Pz',7X,'E',
     &  4X,'Mass',5X,'Eta',5X,'Phi',3X,'Theta',/,
     &  1X,9F8.2)
      END
