      SUBROUTINE VSECHT( LAYER, SECTOR )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Do hitfinding for one sector of wire data
C-
C-   Inputs  : LAYER, SECTOR: location in VTX
C-             Data in banks CDDN, N=1,..,4
C-
C-   Outputs : Fills VWDA with raw hits
C-             Processed hits stored in VSEC
C-             Hit counters in VLAY and VTXH updated
C-
C-   Controls: none
C-
C-   Created  13-FEB-1989   Peter Grudberg
C-   Updated   8-APR-1991   Peter Grudberg  Handle real + mc data 
C-   Updated   4-NOV-1991   Peter M. Grudberg  Remove TYPE arg in call 
C-                                             to VHTCHK 
C-   Updated  12-MAY-1992   Peter Grudberg  Fix TRGOFF bug 
C-   Updated   4-AUG-1992   P.Grudberg/S.Zinchenko  Dont use TRGOFF for MC data 
C-   Updated   9-FEB-1994   Liang-Ping Chen get rid of the unused word ZFLAG,  
C-                                          IF ENDIF statements, and  
C-                                          calls VTRGTM only when it's needed
C-   Updated   9-FEB-1994   Liang-Ping Chen call CDH1_TO_HITLST if CDH1 exists
C-   Updated  30-AUG-1994   Liang-ping Chen Remove DUMDAT() array in CALL VTPULS   
C-
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:VTXLNK.INC'
      INCLUDE 'D0$LINKS:IZCDD1.LINK'
      INTEGER LAYER, SECTOR, LABEL
      INTEGER ISTAT
C
      INTEGER WIRE, MXWIRE, END, IER
      INTEGER LFADC
      PARAMETER( MXWIRE = 7 )           ! Starting from 0
      PARAMETER( LFADC=512 )
      REAL TRTIME, TRGOFF
C
      LOGICAL DONE
      INTEGER NHITS, NHITSC(4), MXHTOT, NWVWDA
      PARAMETER ( MXHTOT = 500 )        ! Maximum # of hits in sector
      PARAMETER ( NWVWDA = 8 )
      INTEGER MXTIMP
      PARAMETER ( MXTIMP = 5 )
      INTEGER NPULSE(0:7,0:1)
      INTEGER GZCDH1
      INTEGER KPVSEC, KPVWDA, I
      INTEGER RUN, RUNSAV, EVT, EVTSAV
      REAL HITLST(NWVWDA,MXHTOT)
      LOGICAL FIRST, TRGFLG, USE_DEF, MCDATA
      LOGICAL CDD1_EXIST, CDH1_EXIST
      DATA FIRST / .TRUE. /
      DATA RUNSAV, EVTSAV / -1, -1 /
C----------------------------------------------------------------------
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('VTRAKS_RCP')
        CALL EZGET('USE_DEFAULT', USE_DEF, IER)
        IF ( USE_DEF ) THEN
          TRGFLG = .FALSE. 
          IF ( IQ(LHEAD+1) .GT. 1000 ) THEN
            MCDATA = .TRUE.            ! MC data
          ELSE
            MCDATA = .FALSE.           ! Real data (collider run)
          ENDIF
        ELSE
          CALL EZGET('TRGFLG', TRGFLG, IER)
        ENDIF
        CALL EZGET('TRGOFF', TRGOFF, IER)
        CALL EZRSET
      ENDIF
      
C
      NHITS = 0
C
      CALL VZERO( NHITSC, 4 )
      CALL VZERO( NPULSE, 16 )
C
      CDD1_EXIST = LQ(LHEAD - IZCDD1) .GT. 0
      CDH1_EXIST = GZCDH1() .GT. 0
      
      DO WIRE = 0, MXWIRE
        DO END = 0, 1
          LABEL = LAYER*512 + SECTOR*16 + WIRE*2 + END
          IF ( NHITS .EQ. MXHTOT ) GO TO 100
C
C **** This builds the Pulse list in HITLST array (with a maximum number
C **** of hits for the whole sector)
C
          IF (CDD1_EXIST) THEN
            CALL VTPULS( LABEL,NPULSE(WIRE,END),                 
     &                 HITLST(1,NHITS+1), MXHTOT-NHITS )
          
          ELSEIF (CDH1_EXIST) THEN
            CALL CDH1_TO_HITLST(LABEL,NPULSE(WIRE,END),                 
     &                 HITLST(1,NHITS+1), MXHTOT-NHITS, ISTAT)
          ENDIF
          NHITS = NHITS + NPULSE(WIRE,END)
        ENDDO
      ENDDO

  100 CONTINUE
C
C ****  Set bit in VTXH corresponding to LAYER, SECTOR indicating that
C ****  hitfinding has been done
C
      CALL VHTCHK(LAYER, SECTOR, 1, DONE)
      IF ( NHITS .EQ. 0 ) GO TO 999
C
      CALL BKVSEC( LAYER, SECTOR, NHITS, KPVSEC )
      CALL BKVWDA( LAYER, SECTOR, NHITS, KPVWDA )
C
C **** Correct for trigger time
C
      IF ( TRGFLG ) THEN
C
        IF ( MCDATA .OR. .NOT. TRGFLG ) THEN
          TRTIME = 0.0                    ! MONTE CARLO
        ELSE
C
C ****  Subtract trigger time for real data (cosmic data uses asynchronous
C ****  trigger).  Note:  trigger offset (TRGOFF) is subtracted within VTRGTM
C  
          CALL EVNTID(RUN,EVT)
          IF (RUN.NE.RUNSAV .OR.EVT.NE.EVTSAV) THEN
            CALL VTRGTM(TRTIME)
            RUNSAV=RUN
            EVTSAV=EVT  
          ENDIF 
        ENDIF
        DO I = 1, NHITS
          HITLST(2,I) = HITLST(2,I) - TRTIME
        ENDDO
      ELSEIF ( .NOT. MCDATA ) THEN
        DO I = 1, NHITS
          HITLST(2,I) = HITLST(2,I) - TRGOFF
        ENDDO
      ENDIF
C
      CALL ZFVWDA( LAYER, SECTOR, HITLST, NPULSE )
C
C **** Now combine hits on the two ends into a single hit and fill VSEC
C
      CALL VCOORD( LAYER, SECTOR, NHITSC)
      CALL PUVSEC( LAYER, SECTOR )                !  Remove extra words
C
C **** Now update hit counters in VLAY and VTXH
C
      DO I = 1, 4
        IQ(LVLAY(LAYER)+I)= IQ(LVLAY(LAYER)+I) + NHITSC(I)
      ENDDO
      IQ(LVTXH+1)=IQ(LVTXH+1) + NHITSC(1)             
      IQ(LVTXH+2)=IQ(LVTXH+2) + NHITSC(1)
  999 RETURN
      END

