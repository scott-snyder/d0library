      SUBROUTINE ESUMFL(STYP,ID,PT,ETAIN,ETADETIN,PHIIN,QUAL_BITS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill ESUM bank. Create or increase size if needed.
C-                         Bank holds unique objects where unique is defined
C-                         by bins in eta,phi and binsize = the object's
C-                         resolution. See ESUM.PARAMS. New objects at the
C-                         same position replace the old object if their Et is
C-                         higher.  Apply a threshold cut to the objects.
C-                         In case of replacement, the QUAL_BITS is ORed with
C-                         the previous QUAL_BITS, so that it contains the
C-                         record of all cut bits applied and passed.
C-
C-   Inputs  : STYP     [C*4] summary bank type: 'FILT' 'TRGR' 'RECO' 'ISAE'
C-             ID       [I] id of object; see ESUM.PARAMS
C-             PT       [R] Et or PT of object
C-             ETAIN    [R] Physics eta of object
C-             ETADETIN [R] Detector eta of object
C-             PHI      [R] phi of object
C-             QUAL_BITS    32-bit word of tool-defined bit information
C-            note: for a VERTEX, x,y,z are in ETAIN,THETAIN,PHI
C-   Outputs :
C-   Controls:
C-
C-   Created  10-DEC-1991   Richard V. Astur
C-   Updated  11-DEC-1991   James T. Linnemann  keep sums by type; tuning
C-   Updated   6-JAN-1992   James T. Linnemann  ESUM: add  Threshold, STYP
C-   Updated  14-APR-1992   RVA: Fix bug in how Esum defines 'unique' objects
C-   Updated  19-JUN-1992   RVA: Go to version 3 of Esum bank
C-   Updated   3-AUG-1992   A. Boehnlein: only use res code for FILT and TRGR
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:ESUM.PARAMS'
      INCLUDE 'D0$INC:L2ESUMLK.INC'       ! Link area for FILT version only
      REAL PT, ETAIN, ETADETIN, PHIIN, PHI, ETA, ETADET
      CHARACTER*(*) STYP
      INTEGER ID,QUAL_BITS,IETA,IPHI,I,LESUM,GZESUM,POINT,NFIX,NR
      REAL OBJ_RESOLVE( ID_ALL : LAST_TYPE )    !size of pixel for object
      REAL OBJ_THRESH ( ID_ALL : LAST_TYPE )    !threshold for object
      SAVE OBJ_RESOLVE, OBJ_THRESH
      CHARACTER*10 OBJ_NAME( ID_ALL : LAST_TYPE ) !object names
      INTEGER MORE_OBJ
      PARAMETER( MORE_OBJ = 24 )   ! # of objects to add per call to BKESUM
      INTEGER IOBJ,NUM_OBJ,ND_NEED  ! This object; # so far; words to hold
      INTEGER QUALITY
      LOGICAL FIRST,RESOLVED
      REAL    A,B
      SAVE FIRST
      DATA FIRST/.TRUE./, NUM_OBJ/0/
C----------------------------------------------------------------------
C...statement functions
C...if objects sep by exactly resolution, say they're separated (eg L1 TT)
      RESOLVED(A,B) = ABS(A-B) .GT. ( OBJ_RESOLVE(ID) - 1.E-3)
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        CALL ESUM_INIT( OBJ_RESOLVE, OBJ_THRESH, OBJ_NAME )
        CALL BKESUM( STYP, MORE_OBJ, LESUM )  !book a bank; all have same format
        NFIX = IQ( LESUM + 2)    !size of fixed part of bank
        NR   = IQ( LESUM + 3)    !size of repeating part of bank
        FIRST = .FALSE.
      ENDIF
C
C: Check ID
      IF ( (ID .LT. 0) .OR. (ID .GT. LAST_TYPE ) ) THEN
        CALL ERRMSG('Illegal ESUM id','ESUMFL', 'Object ID is illegal',
     &    'W')
        GOTO 999
      END IF
C
C: Check if we need to book the bank for this summary type
      IF (STYP.EQ.'FILT') THEN
        LESUM = L2ESUM  ! get from link area initially filled by BKESUM
      ELSE
        LESUM = GZESUM(STYP)
      ENDIF
      IF ( LESUM.LE.0 ) CALL BKESUM( STYP, MORE_OBJ, LESUM )  !book new bank
C
C: apply threshold
      IF (PT.LT.OBJ_THRESH(ID)) GO TO 999
C
C: get eta,phi position (actually x,y,z if this is a vertex)
      ETA = ETAIN
      ETADET = ETADETIN
      PHI = PHIIN
C
      NUM_OBJ = IQ( LESUM + 4)     !get each time, just to be safe
      QUALITY = QUAL_BITS          !new value
C: Check if the object is at the same position as an old one of the same type
C: for FILT and TRGR
      IF(STYP.EQ.'FILT'.OR.STYP.EQ.'TRGR') THEN
        DO I = 1, NUM_OBJ ! Cycle over known objects
          POINT = (I-1)*NR + NFIX + LESUM
          IF ( IQ( POINT + JESUM_ID ) .EQ. ID ) THEN
            IF ( .NOT.RESOLVED(ETADET, Q(POINT + JESUM_ETA_DET)) ) THEN
              IF(.NOT.RESOLVED(PHI,    Q(POINT + JESUM_PHI)    ) ) THEN
C...object turns up in same place, within resolution
C...OR all bits of objects within this resolution, whether new or old object
                QUALITY = IOR(IQ(POINT + JESUM_FLAG),QUAL_BITS)
                IQ(POINT + JESUM_FLAG) = QUALITY
                IF (PT.GT.Q(POINT + JESUM_PT)) THEN
                  IOBJ = I  !same place but higher Pt: overwrite old object
                  GO TO 100
                ELSE
                  GO TO 999 !older one had same or higher Pt: keep old object
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        END DO
      ENDIF
C
C: found a new object: increment counts
      NUM_OBJ = NUM_OBJ + 1
      ND_NEED = NUM_OBJ*NR + NFIX
      IF ( ND_NEED.GT.IQ(LESUM-1) ) CALL BKESUM( STYP,MORE_OBJ,LESUM )
      IQ( LESUM + 4 ) = NUM_OBJ                          !global count
      IQ( LESUM + 5 + ID) = IQ( LESUM + 5 + ID) + 1 !count per type
      IOBJ = NUM_OBJ  ! This is a new object
C
C: At this point, IOBJ points to the object we wish to record
  100 CONTINUE
      POINT = ( IOBJ - 1 ) * NR + NFIX + LESUM
      IQ(POINT + JESUM_ID    ) = ID
      Q( POINT + JESUM_PT    ) = PT
      Q( POINT + JESUM_ETA   ) = ETA
      Q( POINT + JESUM_ETA_DET ) = ETADET
      Q( POINT + JESUM_PHI   ) = PHI
      IQ(POINT + JESUM_FLAG  ) = QUALITY
C
  999 RETURN
      END
