      SUBROUTINE CMNMXHX(IETACL,IPHICL,IETAC,IPHIC,
     &                  CRATE,IADDRL,IADDRH,ITSOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : ...is to determine which (IETAC,IPHIC) 
C-                         combination from the IETACL, IPHICL lists
C-                         will translate to the smallest and largest
C-                         hex address values (sans the last 6 bits).
C-
C-   Inputs  : IETACL and IPHICL provide lists of ofline eta and phi
C-             indexs.
C-
C-   Outputs : IETAC and IPHIC give the offline eta,phi index that 
C-             has the smallest hex address translation;
C-             CRATE and IADDRL is the corosponding hex address that begins the
C-             the tower. IADDRH is the beginning address of the last ROT 
C-             in the trigger tower. ITSOK reports if there was an error in the 
C-             determination. Note that a TT lies entirely within a crate.
C-             Thus only one crate number has to be returned.
C-
C-   Controls: None
C-
C-   Created   5-JAN-1989   Dale A. Ross, MSU
C-
C----------------------------------------------------------------------
*
*
C     Variable Declarations:
*
      IMPLICIT NONE!
*
C       Passed Variables:
*
           INTEGER  IETACL(2),IPHICL(2)   !   OffLine ETA [,PHI]
                                          !   List(s) that contain
                                          !   the canadates for the minimum
                                          !   hex address.
           INTEGER  IETAC,IPHIC ! OffLine Eta [Phi] MINimum vaues
                                ! determined.
           INTEGER  CRATE       ! hex system crate and 16-bit address.
           INTEGER  IADDRL,IADDRH ! IADDRLow, IADDRHigh in the TT.
           LOGICAL  ITSOK       ! IT'S OK is .TRUE. when the conversion is ok
*
C       Local Variables:
*
           INTEGER  ELIX,PLIX  !  Eta [,Phi] List IndeX; simply a loop var.
           INTEGER  ELIXMA,PLIXMA ! elix [,plix] MAximum: a loop constant.
           INTEGER  IADDR      !  Hex address, low 16 bits.
           LOGICAL  ITISOK     !  Return-condition flag.
           INTEGER  MASK       !  bits 2-30 are set high; rest set 0.
           INTEGER  RFMNHX,RFMXHX !  ReFernece MiN/MaX HeX
           INTEGER  TMPHEX     !  TeMPoarary HEX: stores hex addess.
           PARAMETER (MASK=2147483584)    !  = 7FFFFFC0 (hex)
*
C     Begin Main Code!
*
      RFMNHX = 2147483647  !  = 7FFFFFFF (hex)  The largest positive integer
      RFMXHX = 0
      ITSOK  = .FALSE.     ! ...untill otherwise pardoned...
*
      IF (IETACL(2) .NE. 0) THEN
          ELIXMA = 2
        ELSE
          ELIXMA = 1
      ENDIF
      IF (IPHICL(2) .NE. 0) THEN
          PLIXMA = 2
        ELSE
          PLIXMA = 1
      ENDIF
*
      DO ELIX = 1,ELIXMA
         DO PLIX = 1,PLIXMA
C           ...convert one (ietac,iphic) pair to hex format
            CALL CPHHEX(IETACL(ELIX),IPHICL(PLIX),CRATE,IADDR,ITISOK)
            TMPHEX = IAND(IADDR,MASK)  !  not interested in bottom 6 bits
            IF (ITISOK) THEN
                  IF (TMPHEX.LE.RFMNHX) THEN
                         IETAC  = IETACL(ELIX)
                         IPHIC  = IPHICL(PLIX)
                         RFMNHX = TMPHEX
                         ITSOK  = .TRUE.
                  ENDIF
                  IF (TMPHEX.GE.RFMXHX) RFMXHX = TMPHEX
            ENDIF
         END DO  !  plix
      END DO  !  elix
      IF (ITSOK) THEN
            IADDRL = RFMNHX
            IADDRH = RFMXHX
         ELSE
            CRATE  = 0
            IADDRL = 0
            IADDRH = 0
      ENDIF
*
*
  999 RETURN
      END
