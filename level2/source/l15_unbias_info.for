      SUBROUTINE L15_UNBIAS_INFO(UNBIAS,L1_MASK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : see whether L1.5 send event as unbiased sample
C-
C-   Inputs  : TRGR bank
C-   Outputs : UNBIAS = .TRUE. if this is an unbiased event sent by L1.5
C-             L1_MASK mask of bits 0:31 in VAX order, 
C-              says which L1 bits are on because of unbiased L1.5
C-   Controls: 
C-
C-   Created  17-NOV-1993   James T. Linnemann
C-   Updated  14-FEB-1994   R. J. Genik II   
C-        NOTE: this routine is written as if there will be only one crate,
C-        the addition of another would force modification anyway to the
C-        way the UNBIAS Mask is handled.
C-        Also: Calling this routine provokes a check of the L15 Data Valid
C-        Longword. If an invalid condition is detected, a warning is
C-        issued via Errmsg.
C----------------------------------------------------------------------
C-   Updated  14-FEB-1994   R. J. Genik II   Parameters hard coded
C-   for now, pending decisions and responses on proper location.
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:L15CALDBB_DATA_BLOCK.PARAMS'
      INCLUDE 'D0$PARAMS:L15_UNBIAS_INFO.PARAMS'
      LOGICAL UNBIAS, L15_Data_Valid
      INTEGER L15_HEADER_PASS(L15CAL_HEADER_LENGTH)
      INTEGER CYCLE_PERFORMED,FW_TERM_EARLY
      INTEGER L1_MASK, I_Current_Term, Ierror,
     +  L15_Unbias_Terms_Mask, L1_Fired_Mask, LHEAD_TRGR_BANK, GZTRGR
      Integer L15_Term_Mask(L15_Term_Beg:L15_Term_End)
C----------------------------------------------------------------------
      L1_MASK = 0 ! Initialize to Zero
      L1_Fired_Mask = 0
C
C ****  Load the L1 bit masks of the L15 terms
C
      Call EZPICK('AND_OR') ! L2 param definitions from coor
      Call EzGetA('AND_OR_USED', I_AND_OR_USED_SKIP + L15_Term_Beg,
     +    I_AND_OR_USED_SKIP + L15_Term_End, 1, ! 1 = Step Value
     +    L15_Term_Mask, Ierror)
      If (Ierror.ne.0)
     +   Call Errmsg('EZGETA Failed', 'L15_UNBIAS_INFO_INIT',
     +    'Cannot Read AND_OR_USED parameters', 'F')
      Call EzRset
C
C ****  Read Data Validity longword
C
      CALL L15EXTRACT_L15CAL_HEADER(1,L15_HEADER_PASS,L15_Data_Valid,
     &           CYCLE_PERFORMED,FW_TERM_EARLY,L15_Unbias_Terms_Mask)
C
      If (L15_Data_Valid) Then !Retrieve the Unbias Mask
C
C ****  Retrive the L15 terms mask which are on if the term was made unbias
C ****  by PASS_1_OF_n
C
        If (L15_Unbias_Terms_Mask.NE.0) Then ! Avoid unneccesary
                                             ! processing...
C
C ****  OR all L1 bits which might have fired and allowed the unbias L1
C ****  term 
C
          Do I_Current_Term = L15_Term_Beg, L15_Term_End
            If(Btest(L15_Unbias_Terms_Mask, I_Current_Term))
     +        L1_Mask = IOR(L1_Mask, L15_Term_Mask(I_Current_Term))
          Enddo
C
C ****  Retrieve the mask of L1 bits which actually fired for this event
C ****  from the TRGR bank. There is no reason to pass back an unbias flag
C ****  in an L1 bit if it didn't fire this event; therefore, AND it with
C ****  the possible unbias mask
C
          LHEAD_TRGR_BANK = GZTRGR() ! Get TRGR bank pointer
          L1_Fired_Mask = IQ(LHEAD_TRGR_BANK + L1_Fired_Mask_Offset)
          L1_Mask = IAND(L1_Mask, L1_Fired_Mask) 
        Endif
      Else ! L15 CalTrig Data was invalid
        Call Errmsg('L15 Data Invalid', 'L15_UNBIAS_INFO', 
     &    'L15 DATA NOT VALID','W')
      Endif
C
C ****  Set unbias flag (Any bits on)
C
  100 UNBIAS = (L1_MASK.NE.0)
  999 RETURN
      END
