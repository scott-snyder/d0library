      SUBROUTINE OBJECT_JET1(JET_TYPE,IDX, NMAX, ARRAY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns JET information for Top analysis
C-                         Has to have PARTICLE_SELECT package
C-                         WILL REMOVE objects from jets listed in
C-                         REMOVE_ELEC_JETS, REMOVE_PHOTON_JETS in
C-                         particle_select_rcp
C-
C-   Inputs  : JET_TYPE =  'TOP_JETS' etc
C-             IDX      [I]   Object Number
C-   Outputs : ARRAY(*) [R]   Array of quantities
C-
C-              ARRAY(1..9)
C-                            PX, PY, PZ, E, ET, ETA, PHI, DET_ETA, QUALITY
C-                    I = 9
C-   X1         ARRAY(I+1)    Sig**2(Ex)
C-              ARRAY(I+2)    Sig**2(Ey)
C-              ARRAY(I+3)    RMS Eta width
C-              ARRAY(I+4)    RMS Phi width
C-              ARRAY(I+5)    Fraction of EM Et
C-              ARRAY(I+6)    Flag for merging/splitting
C-              ARRAY(I+7)    Number of cells above threshold (1GeV def)
C-              ARRAY(I+8)    Fraction of ICD/MG Et(EtICD+EtMG/TOTAL_ET)
C-              ARRAY(I+9)    Fraction of CH Et (Et CH/TOTAL_ET)
C-              ARRAY(I+10)   Ratio of hottest to next-hottest cell
C-              ARRAY(I+11)   Number of CDC tracks in jet cone
C-              ARRAY(I+12)   Number of TOWERS comprising 90% of jet Et
C-              ARRAY(I+13)   ConeSize (-1.0 for Nearest Neighbor)
C-              ARRAY(I+14)   Phi Difference between MET and JET (PNUT(2))
C-              ARRAY(I+15)   Spare
C-              ARRAY(I+16)   Energy Correction Flag (I)
C-              ARRAY(I+17)   Sig**2(Ez)
C-              ARRAY(I+18)   dExdEy
C-              ARRAY(I+19)   dExdEz
C-   X20        ARRAY(I+20)   dEydEz
C-
C-   Controls:
C-
C-   Created:   4-NOV-1994   Rajendran Raja  BASED ON OBJECT_JETS
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) JET_TYPE
      INTEGER IDX, NMAX
      INTEGER NJETS,NJETINF
      REAL    ARRAY(*)
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAPH.DEF'
      INCLUDE 'D0$PARAMS:GM_PARAMS.DEF'
      INCLUDE 'D0$LINKS:IZJNEP.LINK'
C----------------------------------------------------------------------
      INTEGER  JET_SORT
      INTEGER  LBANK, LCAPH 
      INTEGER LVERT, GZVERT,  BANKLENGTH
      REAL    ZVTX, THETA
      REAL    MPHI, PHI_DIFFERENCE
C----------------------------------------------------------------------
      INTEGER MINNUM
      PARAMETER( MINNUM =  9 )
      INTEGER MAXBUF
      PARAMETER( MAXBUF = MINNUM + 20 )
C----------------------------------------------------------------------
      INTEGER NN
      INTEGER IBUFFER(MAXBUF)
      REAL    BUFFER(MAXBUF)
      EQUIVALENCE (IBUFFER(1), BUFFER(1))
C----------------------------------------------------------------------
C
C will allow this many max jets of any type
C
      INTEGER MAX_JETS
      PARAMETER( MAX_JETS = 20 )
      INTEGER JET_LINKS(MAX_JETS)
C
C----------------------------------------------------------------------
C
C ****  OBJECT: JET
C
      NN = MIN(NMAX, MAXBUF)
      CALL VZERO(BUFFER, NN)
C
      IF(NMAX .GT. MAXBUF) THEN
        CALL ERRMSG('OBJECT_JET1','OBJECT_JET1',
     &    'Too much info requested. Will set to MAXBUF','I')
      ENDIF
C
      CALL GTSLINK_ONE(JET_TYPE,IDX,LBANK)
      BANKLENGTH = IQ(LBANK-1)
      CALL UCOPY(Q(LBANK+2), BUFFER(IPX), 6)
      BUFFER(IETA) = Q(LBANK+9)
      BUFFER(IPHI) = Q(LBANK+8)
      LVERT = GZVERT(1)
      ZVTX = 0.
      IF(LVERT.GT.0) ZVTX = Q(LVERT+5)
      THETA = Q(LBANK+7)
      CALL DET_ETA(ZVTX, THETA, BUFFER(IDETA))
C
C ****  OTHER
C
      CALL UCOPY(Q(LBANK+10), BUFFER(IX1), 5)
      BUFFER(IX6) = FLOAT(IQ(LBANK+15)) ! split/merge flag is an integer
C
C ****  Start of protection from reading off end of bank
C
      IF ( BANKLENGTH .GE. 16 ) THEN
        BUFFER(IX7) = FLOAT(IQ(LBANK+16)) ! number of cells is an integer
        IF ( BANKLENGTH .GE. 19) THEN

          CALL UCOPY(Q(LBANK+17), BUFFER(IX8), 3)
C
C ****  Add Cone Size
C
          IF ( BANKLENGTH .GE. 21 ) THEN
            BUFFER(IX11) = FLOAT(IQ(LBANK+20))
            BUFFER(IX12) = FLOAT(IQ(LBANK+21))
C
C ****  Add rest of covariance matrix
C
            IF ( BANKLENGTH .GE. 26 ) THEN
              BUFFER(IX16) = Q(LBANK+26)  ! Correction Status Flag
              CALL UCOPY(Q(LBANK+22), BUFFER(IX17), 4)
            ENDIF                              ! if banklength .ge. 26
          ENDIF                                ! if banklength .ge. 21
        ENDIF                                  ! if banklength .ge. 19
      ENDIF                                    ! if banklength .ge. 16
C
      LCAPH = LQ(LBANK+1)           !Origin link to CAPH
      JET_SORT = IQ(LCAPH+K_ALGORITHM)
      IF ( JET_SORT .EQ. A_CONE_JET ) THEN
        BUFFER(IX13) = Q(LCAPH+K_BASE+1)
      ELSE
        BUFFER(IX13) = -1.0
      ENDIF
C
      BUFFER(IX14) = PHI_DIFFERENCE(MPHI, Q(LBANK+8))
C
      CALL UCOPY(BUFFER, ARRAY, NN)
  999 RETURN
C
C**************************************************************************
C
      ENTRY NOBJ_JETS1(JET_TYPE,NJETS,NJETINF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Used in top analysis
C-
C-   Inputs  : JET_TYPE =  'TOP_JETS' etc
C-
C-   Outputs  :  NJETS     Number of jets
C-               NJETINF    Number of information per jets
C-   Outputs :
C-   Controls:
C-
C-   Created   9-JUN-1994   Rajendran Raja
C-
C----------------------------------------------------------------------
      CALL GTSLINK(JET_TYPE,MAX_JETS,NJETS,
     &  JET_LINKS)
      NJETINF = MAXBUF
C----------------------------------------------------------------------
      RETURN
      END
