      SUBROUTINE CL2_GAP_ADR(TYPE,IETAC,IPHIC,CRATE,IADDR,ITSOK)
C----------------------------------------------------------------------
C-
C-   CL2_GAP_ADR = (Calorimeter) L-2 Central Calorimeter 
C-                 massless/icd  GAP ADdRess
C-
C-   Purpose and Methods : For a paticular (IETAC,IPHIC) location we
C-                         return the hex address of the Gap
C-                         there.
C-
C-   Inputs  : IETAC,IPHIC specifies the location.
C-
C-   Outputs : CRATE and IADDR give the full hex address of the MG or ICD.
C-             ITSOK returns status .FALSE. if there is no gap here.
C-
C-   Controls: TYPE specifies the type of address to return.
C-                      Parameters for these codes are found in 
C-                      D0$PARAMS:CAL_LEVEL2.PARAMS
C-             TYPE = 1 for the "first massless gap layer" as defined
C-                      in CAL_OFF$PARAMS:CAL_OFFLINE.PARAMS. Currently it
C-                      is for the CCMG.  Parameter name is CCGPTP
C-             TYPE = 2 for the ICD which, according to
C-                      CAL_OFF$PARAMS:CAL_OFFLINE.PARAMS is LYICD.
C-                                        Parameter name is ICDGPTP
C-             TYPE = 3 for the "last massless gap layer" as defined
C-                      in CAL_OFF$PARAMS:CAL_OFFLINE.PARAMS. Currently it
C-                      is for the ECMG.  Parameter name is ECGPTP.
C-
C-   Created  23-AUG-1989   Dale A. Ross, MSU
C-
C----------------------------------------------------------------------
*
      IMPLICIT NONE!
*
C     Passed Variables:
*
         INTEGER  IETAC,IPHIC   ! Give the L-2 loacation.
         INTEGER  CRATE,IADDR   ! Is thehex address specification.
         LOGICAL  ITSOK         ! Status flag.
         INTEGER  TYPE          ! The address type to return.
*
C     Local Variables:
*
         INTEGER  ADC,BLS,ROTOW,DEPTH,ICOND,SCALE,NEGLIM
         PARAMETER (SCALE=0,NEGLIM=0) ! Thease last two bits not considered.
         LOGICAL  CCMGXST ! = CC MG eXiST.
         LOGICAL  ECMGXST ! = EC MG eXiST.
         LOGICAL  ICDXST  ! = ICD eXiST.
         INTEGER  LYCCMG  ! CC MG LaYer.
         INTEGER  LYECMG  ! EC MG LaYer.
         LOGICAL  OR      ! = ccmgxst .OR. ecmgxst .OR. icdxst
*
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS/LIST'
      PARAMETER (LYCCMG=MNLYMG)  ! MNLYMG is the fist MG layer.
C                                  This parameter is found in
C                                  D0$PARAMS:CAL_OFFLINE.params.
      PARAMETER (LYECMG=MXLYMG)  ! MXLYMG is the fist MG layer.
C                                  This parameter is found in
C                                  D0$PARAMS:CAL_OFFLINE.params.
*
*
*
C     The Machines Core:
C     _______________________________________________________
C     _______________________________________________________
*
      ITSOK = .TRUE.
      CALL CL2_GAPXST(IETAC,IPHIC,CCMGXST,ECMGXST,ICDXST,OR)
      IF (TYPE .EQ. 1 .AND. CCMGXST) THEN
         CALL CPHAD(IETAC,IPHIC,LYCCMG,CRATE,ADC,BLS,ROTOW,
     &              DEPTH,ICOND)
         CALL CADPAK(ADC,BLS,ROTOW,DEPTH,SCALE,NEGLIM,IADDR)
      ELSE IF (TYPE .EQ. 2 .AND. ICDXST) THEN
         CALL CPHAD(IETAC,IPHIC,LYICD,CRATE,ADC,BLS,ROTOW,
     &              DEPTH,ICOND)
         CALL CADPAK(ADC,BLS,ROTOW,DEPTH,SCALE,NEGLIM,IADDR)
      ELSE IF (TYPE .EQ. 3 .AND. ECMGXST) THEN
         CALL CPHAD(IETAC,IPHIC,LYECMG,CRATE,ADC,BLS,ROTOW,
     &              DEPTH,ICOND)
         CALL CADPAK(ADC,BLS,ROTOW,DEPTH,SCALE,NEGLIM,IADDR)
      ELSE
         ITSOK = .FALSE.
      ENDIF
*
999   RETURN
      END
