      SUBROUTINE CGL1PHT
C-  !  Generate Level-1 to PHysics indices Table
C----------------------------------------------------------------------
C-
C-   CGL1PHT = (Calorimeter) Generate L-1 dbi to PHysics Table.
C-
C-   Purpose and Methods : Fills the common block /L1OL/ with the required
C-                         IETAC, IPHIC values. /L1OL/ is a table relating
C-                         level-1 trigger table index values to their
C-                         IETAC, IPHIC counterparts. CDBIPH does the work.
C-
C-   Inputs  : None.
C-
C-   Outputs : The common block table /L1OL/
C-
C-   Controls: None.
C-
C-   Created  20-DEC-1988   Dale A. Ross, MSU
C-   Updated  17-Mar-1992   Herbert Greenlee
C-      Removed machine blocks
C-
C----------------------------------------------------------------------
*
      IMPLICIT NONE!
*
      INCLUDE 'D0$PARAMS:L1PHP.PARAMS/LIST'
      INCLUDE 'D0$INC:L1OL.INC/list' ! Contains the array L1OL, and the
C                                           varible FINDBI.
*
C     Local Variables:
*
        INTEGER  CRATE,IADDRL,IADDRH        !  The hex address variables.
        LOGICAL  CCMGXST,ECMGXST,ICDXST,OR  ! Existance flags.
        INTEGER  IETAC,IPHIC                !  L-2 eta,phi indices
        LOGICAL  ITSOK          !  "Is it ok?"; that is, the return from
C                               !   call.
C        INTEGER  L1DINX
C        INTEGER  L1ETAC,L1PHIC  !  L-1 eta,phi indexes.
        INTEGER  L1INX          !  Level-1 INdeX (count variable).
        LOGICAL  MARKED         !  Marks the the place in L1INX where,
                                !  after which, there exist no real
                                !  trigger towers.
C        CHARACTER*25 TAG        !  <nothing special>
*
*
C     Begin the Main Event!
C     _____________________
*
      MARKED = .FALSE.
      DO 100 L1INX= 0,(NETAL1*NPHIL1*2-1)  !  The number of EMTT's (=1536)
*
         CALL CDBIPH(L1INX,IETAC,IPHIC,CRATE,IADDRL,IADDRH,ITSOK)
         IF (ITSOK) THEN
               L1OL(L1INX,1) = IETAC
               L1OL(L1INX,2) = IPHIC
               L1OL(L1INX,3) = CRATE
               L1OL(L1INX,4) = IADDRL
               L1OL(L1INX,5) = IADDRH
               CALL CL2_GAPXST(IETAC,IPHIC,CCMGXST,ECMGXST,ICDXST,OR)
               IF (OR) THEN
                 L1OL(L1INX,6) = -1 ! = .TRUE. on a VAX.
               ELSE
                 L1OL(L1INX,6) =  0 ! = .FALSE. on a VAX.
               ENDIF
            ELSE
               L1OL(L1INX,1) = 0  ! Code of nonexistance.
               L1OL(L1INX,2) = 0
               L1OL(L1INX,3) = 0
               L1OL(L1INX,4) = 0
               L1OL(L1INX,5) = 0
               L1OL(L1INX,6) = 0 ! = .FALSE. on a VAX.
               IF (.NOT.MARKED) THEN  ! Make note of the final good
                  FINDBI = L1INX-1   ! entry in the table. The number is
                  MARKED = .TRUE.   ! is not used in this table generator,
               ENDIF               ! but is used in routine that do depend
C                                 ! on this table (such as CGL1HXR).
         ENDIF
*
*
C
C        The following code is used for testing pourposes. In particular the
C        current code was used to check to see that the inverse routines mapped
C        correctly. Otherwise, at some future date the code in 'D' may be
C        removed. DR.
C
C        CALL CPHTT(IETAC,IPHIC,L1ETAC,L1PHIC,ITSOK)
C        CALL CTTDBI(L1ETAC,L1PHIC,L1DINX)
*
*
C        IF (L1INX .NE. L1DINX) TAG='  HEAR IS A DIFFERENCE!'
C        Type *,l1inx,ietac,iphic,l1dinx,tag,itsok
C        TYPE *,L1INX,IETAC,IPHIC
C        TAG = ' '
*
100   CONTINUE  !  END Do
*
      END
