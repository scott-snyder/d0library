      SUBROUTINE GTESUM_SORT (STYP,IDWANT,NWANT,IORDER,WORK,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get Index of objects of a given type from ESUM bank
C-                          Sorted by descending Pt order
C-    Inputs:
C-             STYP     [C*4]   Sumary Type: 'FILT','TRGR','RECO','ISAE'
C-             IDWANT   [I]     ID of object(s) to look for:
C-                              See ESUM.PARAMS for known object types:
C-                              ID_ALL, ID_VERTEX, ID_JET, ID_MUON, ID_ELECTRON
C-                              ID_PHOTON, ID_ETMISS, ID_ETSUM,ID_TAU
C-             NWANT    [I]     Maximum # of objects to return (actual number
C-                                found can be obtained by GTESUM_TOTALS)
C-
C-    Outputs: IORDER(NWANT) [I] Index of objects to use with GTESUM:
C-                                If 3 jets of Pt = 7, 5, 10 are found
C-                                IORDER = 3, 1, 2
C-             WORK(NWANT)  [F]  Working array (copied from ESUM)
C-                
C-              NOTE: IORDER and WORK must be dimensioned NWANT or greater
C-                              created the object
C-             IER      [I]     Error code; 0 --- OK
C-                              -1 --- No ESUM bank of this type found
C-                              -2 --- no objects of this type found 
C-                              -3 --- More than NWANT objects found of this
C-                                      type: Those found are sorted, but not
C-                                      guaranteed to contain the highest ET
C-                                      object
C-      Calls CERNLIB M101 SORTZV
C-   Controls: none
C-   Created  6-JAN-1992   James T. Linnemann   
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:ESUM.PARAMS'
      CHARACTER*(*) STYP
      INTEGER IDWANT, NWANT
      INTEGER IORDER( NWANT ) 
      REAL WORK( NWANT ) 
      INTEGER IER, LESUM, GZESUM, POINT, I, NGOT
      INTEGER NFIX,NR,NUM_OBJ
      INTEGER IREAL,IDESCEND,IALL
      PARAMETER( IREAL = 1 )      ! sort is on real numbers
      PARAMETER( IDESCEND =  1 )  ! sort in descending order
      PARAMETER( IALL = 0 )       ! sort every element found
C----------------------------------------------------------------------
C: Set error flag
      IER = 0                   ! OK
C: Get link
      LESUM = GZESUM(STYP)
      IF ( LESUM .LE. 0 ) THEN
        IER = -1  !   bank not found
        GO TO 999
      END IF
      NFIX = IQ( LESUM + 2)
      NR   = IQ( LESUM + 3)
      NUM_OBJ = IQ( LESUM + 4)
      NGOT = 0
C: Look for objects desired
      DO I = 1, NUM_OBJ ! Cycle over known objects
        POINT = (I-1)*NR + NFIX + LESUM
        IF ( IDWANT.EQ.IQ(POINT+JESUM_ID) )THEN
          IF ( NGOT.LT.NWANT) THEN
            NGOT = NGOT + 1
            WORK( NGOT )= Q( POINT + JESUM_PT )
          ELSE
            IER = -3  !not enough space to save all objects requested
          ENDIF
        ENDIF
      END DO
      IF (NGOT.LE.0) THEN
        IER = -2    ! no objects of this type found
      ELSE
        CALL SORTZV(WORK,IORDER,NGOT,IREAL,IDESCEND,IALL)
      ENDIF
  999 RETURN
      END
