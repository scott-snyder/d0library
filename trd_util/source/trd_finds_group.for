      INTEGER FUNCTION TRD_FINDS_GROUP()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finds groups of coded adjacent wires in each TRD
C-                         layer.
C-   Inputs  :
C-   Outputs : Returns 0 if no overflow in the group finding,
C-             returns -1 if too many groups,
C-             returns Cl_number if too many wires in a group.
C-   Controls:
C-
C-   Created  24-MAY-1992   JF Glicenstein
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C      INCLUDE 'USR$ROOT1:[DUCROS.CALIBOF.NEW]TRD_GROUPS.INC'
c      INCLUDE 'USR$ROOT1:[DUCROS.CALIBOF.NEW]TRD_GROUP.INC'
c      INCLUDE 'D0$INC:TRD_GROUP.INC'
      INCLUDE 'D0$PARAMS:TRD_NB_OF_WIRES.PARAMS'
C----------------------------------------------------------------------
C-
C-   Created  24-MAY-1992   JFG
C-                          Information on clustering in TRD layers
C-                          Maximum 30 groups of maximum 50 hits (?)
C- NGROUP_PER_LAYER(L):number of  grouped set of wires found in layer L
C- NWIRE_PER_GROUP(NW,L): number of wires in group NW in layer L
C- LISTW_INCL(List,NW,L): list of wires for group NW in layer L
C----------------------------------------------------------------------
      INTEGER NWRMAX,NCLMAX
      INTEGER NLM,NTM
      PARAMETER (NLM=6)
      PARAMETER (NWRMAX = 50)
      PARAMETER (NCLMAX = 30)
      PARAMETER (NTM=NLM*NTOT_WIRE_TRD)
      COMMON /TRD_GROUPS/ NGROUP_PER_LAYER(NLM),NWIRE_PER_GROUP
     &(NCLMAX,NLM),LISTW_INCL(NWRMAX,NCLMAX,NLM),GOOD_WIRE(NTM)
      INTEGER NGROUP_PER_LAYER,NWIRE_PER_GROUP,LISTW_INCL
      LOGICAL GOOD_WIRE
      INCLUDE 'D0$INC:TRHITW.INC'
      INTEGER K1,K2,K3,K4,NHITS,LAYER,CL_N,NWIR
      INTEGER WIRE_LEFT(30),WIRE_RITE(30)
      TRD_FINDS_GROUP = 0
C
C ****
C
C      DO 10 K1 = 1,2 ! loop over anode, cathode
      K1=1 ! impose only anodes
      DO 20 K2 = 1,3 !loop over layers
        LAYER = K2+3*(K1-1)
        NHITS = NBTHIT(K2,K1) !number of hits in layer
        CL_N = 0
C
C ****  if hit, then get surounding wire numbers that were hit
C
        IF (NHITS.GT.1) THEN
          WIRE_LEFT(1) = NUMTWH(1,K2,K1)
          WIRE_RITE(1) = NUMTWH(1,K2,K1)
          CL_N = 1
        ENDIF
        CALL VZERO(NWIRE_PER_GROUP(1,LAYER),NCLMAX)
C       Starts group search.
        DO 30 K3 = 1,NHITS
          IF ((NUMTWH(K3,K2,K1)-WIRE_RITE(CL_N)).GE.2) THEN
            CL_N = CL_N + 1
            IF (CL_N.GT.NCLMAX) THEN
              TRD_FINDS_GROUP = - 1
              GOTO 999
            ENDIF
            WIRE_LEFT(CL_N)             = NUMTWH(K3,K2,K1)
          ENDIF
          NWIRE_PER_GROUP(CL_N,LAYER) = NWIRE_PER_GROUP(CL_N,
     &        LAYER)
     &        +1
          NWIR                        = NWIRE_PER_GROUP(CL_N,LAYER)
          IF (NWIR.GT.NWRMAX) THEN
            TRD_FINDS_GROUP = CL_N
            GOTO 999
          ENDIF
          LISTW_INCL(NWIR,CL_N,LAYER) = NUMTWH(K3,K2,K1)
          WIRE_RITE(CL_N)             = NUMTWH(K3,K2,K1)
   30   CONTINUE
        NGROUP_PER_LAYER(LAYER)    = CL_N
C        Check if the last group is identical with the first.
        IF (WIRE_RITE(CL_N).EQ.256.AND.CL_N.GT.1) THEN
          IF (WIRE_LEFT(1).EQ.1) THEN
            NGROUP_PER_LAYER(LAYER)  = NGROUP_PER_LAYER(LAYER) - 1
            NWIRE_PER_GROUP(1,LAYER) = NWIRE_PER_GROUP(CL_N,LAYER)
     &          +
     &          NWIRE_PER_GROUP(1,LAYER)
            NWIR                       = NWIRE_PER_GROUP(1,LAYER)
            IF (NWIR.GT.NWRMAX) THEN
              TRD_FINDS_GROUP = 1
              GOTO 999
            ENDIF
            DO 40 K4 = 1,NWIRE_PER_GROUP(CL_N,LAYER)
              LISTW_INCL(NWIR+K4,CL_N,LAYER) = LISTW_INCL(K4,CL_N,
     &            LAYER)
              LISTW_INCL(K4,CL_N,LAYER)      = 0
   40       CONTINUE
            NWIRE_PER_GROUP(CL_N,LAYER)    = 0
          ENDIF
        ENDIF
   20 CONTINUE
   10 CONTINUE
  999 RETURN
      END
