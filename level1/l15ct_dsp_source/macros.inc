******************************************************************************
*                                                                            *
*   File:  Macros.Inc                                   Date:  1-JUN-1994    *
*                                                                            *
*  "Meaningful" Register Usage                                               *
*  ---------------------------                                               *
*  AR3: Pointer to Object List                              Input            *
*  AR4: Base of Peripheral Bus                              Input            *
*  DP:  Base of Global SRAM                                 Input            *
*  R2:  Object Count                                        Input            *
*  R3:  1x2 EM    Et Sum                                    Input            *
*  R4:  Tool Acept/Reject Flag                              Output           *
*  R5:  3x3 Total Et Sum                                    Input            *
*                                                                            *
*  "Destroyable" Register Usage                                              *
*  ----------------------------                                              *
*  R10, R11, AR7                                                             *
*                                                                            *
*  Module Description                                                        *
*  ==================                                                        *
*                                                                            *
*  This file contains macros used in the Local and Global DSP Programs.      *
*                                                                            *
*                                                                            *
******************************************************************************

******************************************************************************
*                                                                            *
*   Macro:  SETUP_DMA                                                        *
*                                                                            *
*   Arguments:    Chan              integer in the range [0..5] defining DMA *
*                                   Channel (and associated Comm Port)       *
*                                   Number                                   *
*                                                                            *
*                 Link_Pointer_Loc  memory address of the value to be loaded *
*                                   into the specified DMA Channel Link Ptr  *
*                                                                            *
*                 Synch_Type_Loc    memory address of the value to be "ORed" *
*                                   into the DIE Register                    *
*                                                                            *
*                 ISR_Loc           memory address of the value to be        *
*                                   in the IVT slot corresponding to the     *
*                                   DMAINT(x) for this DMA Channel           *
*                                                                            *
*   Purpose of macro                                                         *
*   ----------------                                                         *
*                                                                            *
*   This macro programs the DMA Channel corresponding to some type of        *
*   data transfer (for example, ERPB Data Input or Local-to-Global Transfer) *
*                                                                            *
*   Programming each DMA Channel is done with the following steps:           *
*                                                                            *
*   First, I will halt the DMA Channel    corresponding to a type of         *
*   transfer                                                                 *
*                                                                            *
*   Then I will program this DMA Channel to perform a "Dummy" DMA Transfer   *
*   (copy one word from the first location in Dual Port memory back to the   *
*   first location in Dual Port memory).  The real purpose of this "Dummy"   *
*   transfer is to cause the DMA Channel to auto-initialize.  The "Dummy"    *
*   list uses the Link Pointer register to point at the "Real" list which    *
*   is stored in Global SRAM.                                                *
*                                                                            *
*   The DMA Channel will then be ready to follow its "Real" list.  The       *
*   "Real" list will read 32 Longwords from the Comm Port corresponding      *
*   to the appropriate type of data.                After finishing          *
*   the transfer, it will auto-initialize.  The "Real" list uses the Link    *
*   Pointer register to point back at itself.                                *
*                                                                            *
*   i.e., schematically:                                                     *
*                                                                            *
*       ----------------------        ----------------------                 *
*       |  "Dummy" DMA List  |    .-->|  "Real"  DMA List  |<---.            *
*       |--------------------|    |   |--------------------|    |            *
*       | Control Register   |    |   | Control Register   |    |            *
*       |--------------------|    |   |--------------------|    |            *
*       |        .           |    |   |        .           |    |            *
*       |        .           |    |   |        .           |    |            *
*       |        .           |    |   |        .           |    |            *
*       |--------------------|    |   |--------------------|    |            *
*       | Link Pointer       |----'   | Link Pointer       |----'            *
*       ----------------------        ----------------------                 *
*                                                                            *
*   After all of the transfers specified in the "Real" list have been        *
*   completed, the DMA Channel will interrupt the CPU (using the appropriate *
*   DMAINTR signal).                                                         *
*                                                                            *
******************************************************************************

SETUP_DMA   .macro  Chan, Link_Pointer_Loc, Synch_Type_Loc, ISR_Loc


        LDI     @DMA_Halt_Word_Loc, R11                 ; Use R11 to halt DMA
        STI     R11, *+AR4(Disp_to_DMA_:Chan:_Control)  ; Channel

        LDI     @Dummy_DMA_Source_Loc, R11              ; Use R11 to set up the
        STI     R11, *+AR4(Disp_to_DMA_:Chan:_Source)   ; Source Address
                                                        ; Register for the
                                                        ; Dummy DMA Transfer

        LDI     @Dummy_DMA_Source_Index_Loc, R11
        STI     R11, *+AR4(Disp_to_DMA_:Chan:_Source_Index)
                                                        ; Use R11 to set up the
                                                        ; Source Address Index
                                                        ; Register for the
                                                        ; Dummy DMA Transfer

        LDI     @Dummy_DMA_Counter_Loc, R11             ; Use R11 to set up the
        STI     R11, *+AR4(Disp_to_DMA_:Chan:_Counter)  ; Transfer Counter
                                                        ; Register for the
                                                        ; Dummy DMA Transfer

        LDI     @Dummy_DMA_Destination_Loc, R11
        STI     R11, *+AR4(Disp_to_DMA_:Chan:_Destination)
                                                        ; Use R11 to set up the
                                                        ; Destination Address
                                                        ; Register for the
                                                        ; Dummy DMA Transfer

        LDI     @Dummy_DMA_Dest_Index_Loc, R11
        STI     R11, *+AR4(Disp_to_DMA_:Chan:_Dest_Index)
                                                        ; Use R11 to set up the
                                                        ; Destination Address
                                                        ; Index Register for
                                                        ; the Dummy DMA Transfer

        LDI     :Link_Pointer_Loc:, R11
        STI     R11, *+AR4(Disp_to_DMA_:Chan:_Link_Pointer)
                                                        ; Use R11 to set up the
                                                        ; Link Pointer
                                                        ; Register for the
                                                        ; Dummy DMA Transfer
                                                        ; This points to the
                                                        ; beginning of the
                                                        ; "Real" DMA List.

        LDI     @DMA_Start_Initialize_Loc, R11          ; Use R11 to
        STI     R11, *+AR4(Disp_to_DMA_:Chan:_Control)  ; Start the Dummy
                                                        ; Transfer

******************************************************************************
*   Now the DMA Channel has begun transferring the "Dummy" List              *
*   It will not execute the "Real" List until the synchronization            *
*   condition have been met.                                                 *
*                                                                            *
*   Now set up the IIE register to allow  DMAINTx     to interrupt           *
*   the CPU upon completion of the "Real" List transfer, and insert the      *
*   vector for the DMAINTx Interrupt Service Routine into the Interrupt      *
*   Vector Table                                                             *
*                                                                            *
*   Also enable the appropriate synchronization for DMA Channel #x           *
******************************************************************************

        LDI     :ISR_Loc:, R11                          ; Use AR7 and R11 to
        LDEP    IVTP, AR7                               ; map the Interrupt
        STI     R11, *+AR7(Disp_to_DMAINT:Chan:_Vector) ; Service Routine for
                                                        ; the completion of one
                                                        ; component of the
                                                        ; Local to Global
                                                        ; DMA Transfer to the
                                                        ; ISR for DMAINTx

        OR      @Enable_DMAINT:Chan:_Loc, IIE           ; Enable DMAINTx to
                                                        ; generate an internal
                                                        ; interrupt

        OR      :Synch_Type_Loc:, DIE                   ; Program
                                                        ; the source of the
                                                        ; Synchronization
                                                        ; for DMA Channel #x

        .endm

******************************************************************************
*   End of macro    SETUP_DMA                                                *
******************************************************************************

******************************************************************************
*   Macro:  SETUP_CRC_PORT                                                   *
*                                                                            *
*   Arguments:    Port              integer in the set {0..5}, indicating    *
*                                   which Comm Port to program.              *
*                                                                            *
*   Purpose of macro                                                         *
*   ----------------                                                         *
*                                                                            *
*   This macro programs the Comm Port Control Register corresponding to      *
*   a CRC Input Comm Port.  It performs the following actions:               *
*                                                                            *
*   1. Halt the Output Channel corresponding to this CRC Input Comm Port.    *
*      Recall the the DSP never outputs data to the CRC.  Halting this       *
*      Output Channels guarantees that the DSP will never request the        *
*      token from the CRC (which is unable to release the token)             *
*                                                                            *
*   2. Check the CPCR to verify that the token has been released to the      *
*      CRC.  If the token has not been released then we will write an        *
*      Error   code into the Com_Port_Status_Loc.                            *
*                                                                            *
*   3. Check the CPCR to verify that both the Input and Output FIFOs are     *
*      empty.  If either FIFO is not empty then we will write an Error       *
*      code into the Com_Port_Status_Loc.                                    *
*                                                                            *
*                                                                            *
*   Recall the format of the Com_Port_Status_Loc  (which is compatible with  *
*   the Status to TCC standard, so this word can be written to the TCC       *
*   by another module if desired):                                           *
*                                                                            *
*                     D        D   D        D   D          D   D       D     *
*                     3        2   2        1   1          0   0       0     *
*                     1        4   3        6   5          8   7       0     *
*                   |------------|------------|--------------|----------|    *
*                   | Comm Port  | Reserved   | Reserved     | Error    |    *
*                   | ID         |            |              | Code     |    *
*                   |---------------------------------------------------|    *
*                                                                            *
*   Recall the possible Error Codes:                                         *
*                                                                            *
*       241:  CRC Comm Port has not given up its token                       *
*       242:  CRC Comm Port does not have empty FIFOs                        *
******************************************************************************

SETUP_CRC_PORT  .macro  Port

        LDI   Halt_Com_Port_Output_Channel, R11             ; Use R11 and
                                                            ; AR4 to halt
        STI   R11, *+AR4(Disp_to_ComPort_:Port:_Control)    ; the Output
                                                            ; Channel for
                                                            ; Port #x

        LDI   Status_OK, R10                                ; Load the
                                                            ; code for
                                                            ; Status OK
                                                            ; into R10

        LDI   *+AR4(Disp_to_ComPort_:Port:_Control), R11    ; Use AR4 and
                                                            ; R11 to check
        AND   Com_Port_Direction_Mask, R11                  ; the Com Port
                                                            ; Direction bit.
        CMPI  Com_Port_Input_Mode, R11                      ; If Port #x
                                                            ; is NOT in Input
        LDIne CRC_Port_Token_Error, R10                     ; mode then
                                                            ; load an Error
                                                            ; code into R10

        LDI   *+AR4(Disp_to_ComPort_:Port:_Control), R11    ; Use AR4 and
                                                            ; R11 to check
        AND   Com_Port_FIFO_Depth_Mask, R11                 ; the Com Port
                                                            ; FIFO depths.
        CMPI  Com_Port_FIFOs_Empty, R11                     ; If either FIFO
                                                            ; is NOT empty,
        LDIne CRC_Port_FIFO_Error, R10                      ; then load an
                                                            ; Error   code
                                                            ; into R10

        CMPI  Status_OK, R10                                ; See if the
                                                            ; OK code is
        Beq   CRC_Port_Exit_Point?                          ; still in R10.
                                                            ; If it is, then
                                                            ; branch to the
                                                            ; end of the
                                                            ; macro (do not
                                                            ; write results
                                                            ; to the Status
                                                            ; longword)

                                                   ; if we get to this point,
                                                   ; some Comm Port problem
                                                   ; has been found.  Set
                                                   ; an Error  code in
                                                   ; the Status longword

        LDI   :Port:, R11                                   ; MERGE the
                                                            ; Comm Port ID
        MB3   R11, R10                                      ; into R10
                                                            ; (which also
        STI   R10, @Com_Port_Status_Loc                     ; contains the
                                                            ; Status code)
                                                            ; and write R10
                                                            ; into the Comm
                                                            ; Port Status
                                                            ; longword

CRC_Port_Exit_Point?

        .endm

******************************************************************************
*   End of macro    SETUP_CRC_PORT                                           *
******************************************************************************


******************************************************************************
*   Macro:  SETUP_LG_PREV_PORT                                               *
*                                                                            *
*   Arguments:    Port              integer in the set {0..5}, indicating    *
*                                   which Comm Port to program.              *
*                                                                            *
*   Purpose of macro                                                         *
*   ----------------                                                         *
*                                                                            *
*   This macro programs the Comm Port Control Register corresponding to      *
*   a Local to Global from Previous DSP Transfer.  It performs the following *
*   actions:                                                                 *
*                                                                            *
*   1. Halt the Output Channel corresponding to this LG from Prev Com Port.  *
*      Recall the the DSP never outputs data to the "previous" DSP.  Halting *
*      this Output Channel guarantees that the DSP will never try to feed    *
*      data back to the Previous DSP (which will never read it).             *
*                                                                            *
*   2. Check the CPCR to verify that both the Input and Output FIFOs are     *
*      empty.  If either FIFO is not empty then we will write an Error       *
*      code into the Com_Port_Status_Loc.                                    *
*                                                                            *
*   Note that this macro does not check the direction of this Comm Port.     *
*   This is because this Comm Port may not have been requested to give up    *
*   the token by the time this macro has been called.                        *
*                                                                            *
*   Recall the format of the Com_Port_Status_Loc  (which is compatible with  *
*   the Status to TCC standard, so this word can be written to the TCC       *
*   by another module if desired):                                           *
*                                                                            *
*                     D        D   D        D   D          D   D       D     *
*                     3        2   2        1   1          0   0       0     *
*                     1        4   3        6   5          8   7       0     *
*                   |------------|------------|--------------|----------|    *
*                   | Comm Port  | Reserved   | Reserved     | Error    |    *
*                   | ID         |            |              | Code     |    *
*                   |---------------------------------------------------|    *
*                                                                            *
*   Recall the possible Error   Codes:                                       *
*                                                                            *
*       243:  Local to Global from Prev_DSP Port does not have empty FIFOs   *
******************************************************************************

SETUP_LG_PREV_PORT .macro  Port

        LDI   Halt_Com_Port_Output_Channel, R11             ; Use R11 and
                                                            ; AR4 to halt
        STI   R11, *+AR4(Disp_to_ComPort_:Port:_Control)    ; the Output
                                                            ; Channel for
                                                            ; Port #x

        LDI   Status_OK, R10                                ; Load the
                                                            ; code for
                                                            ; Status OK
                                                            ; into R10

        LDI   *+AR4(Disp_to_ComPort_:Port:_Control), R11    ; Use AR4 and
                                                            ; R11 to check
        AND   Com_Port_FIFO_Depth_Mask, R11                 ; the Com Port
                                                            ; FIFO depths.
        CMPI  Com_Port_FIFOs_Empty, R11                     ; If either FIFO
                                                            ; is NOT empty,
        LDIne LG_Prev_Port_FIFO_Error, R10                  ; then load an
                                                            ; Error   code
                                                            ; into R10

        CMPI  Status_OK, R10                                ; See if the
                                                            ; OK code is
        Beq   LG_Prev_Port_Exit_Point?                      ; still in R10.
                                                            ; If it is, then
                                                            ; branch to the
                                                            ; end of the
                                                            ; macro (do not
                                                            ; write results
                                                            ; to the Status
                                                            ; longword)

                                                   ; if we get to this point,
                                                   ; some Comm Port problem
                                                   ; has been found.  Set
                                                   ; an Error  code in
                                                   ; the Status longword

        LDI   :Port:, R11                                   ; MERGE the
                                                            ; Comm Port ID
        MB3   R11, R10                                      ; into R10
                                                            ; (which also
        STI   R10, @Com_Port_Status_Loc                     ; contains the
                                                            ; Status code)
                                                            ; and write R10
                                                            ; into the Comm
                                                            ; Port Status
                                                            ; longword

LG_Prev_Port_Exit_Point?

        .endm

******************************************************************************
*   End of macro    SETUP_LG_PREV_PORT                                       *
******************************************************************************


******************************************************************************
*   Macro:  SETUP_LG_NEXT_PORT                                               *
*                                                                            *
*   Arguments:    Port              integer in the set {0..5}, indicating    *
*                                   which Comm Port to program.              *
*                                                                            *
*   Purpose of macro                                                         *
*   ----------------                                                         *
*                                                                            *
*   This macro programs the Comm Port Control Register corresponding to      *
*   a Local to Global to Next DSP Transfer.  It performs the following       *
*   actions:                                                                 *
*                                                                            *
*   1. Halt the Input Channel corresponding to this LG to Next DSP Com Port. *
*      Recall the the DSP never receives data from the "next" DSP. Halting   *
*      this Input Channel guarantees that this DSP will never accept data    *
*      from the "next" DSP.                                                  *
*                                                                            *
*   2. Check the CPCR to verify that both the Input and Output FIFOs are     *
*      empty.  If either FIFO is not empty then we will write an Error       *
*      code into the Com_Port_Status_Loc.                                    *
*                                                                            *
*   Note that this macro does not check the direction of this Comm Port.     *
*   This is because this Comm Port may not have requested                    *
*   the token by the time this macro has been called.                        *
*                                                                            *
*   Recall the format of the Com_Port_Status_Loc  (which is compatible with  *
*   the Status to TCC standard, so this word can be written to the TCC       *
*   by another module if desired):                                           *
*                                                                            *
*                     D        D   D        D   D          D   D       D     *
*                     3        2   2        1   1          0   0       0     *
*                     1        4   3        6   5          8   7       0     *
*                   |------------|------------|--------------|----------|    *
*                   | Comm Port  | Reserved   | Reserved     | Error    |    *
*                   | ID         |            |              | Code     |    *
*                   |---------------------------------------------------|    *
*                                                                            *
*   Recall the possible Error   Codes:                                       *
*                                                                            *
*       244:  Local to Global to Next_DSP Port does not have empty FIFOs     *
******************************************************************************

SETUP_LG_NEXT_PORT .macro  Port

        LDI   Halt_Com_Port_Input_Channel, R11              ; Use R11 and
                                                            ; AR4 to halt
        STI   R11, *+AR4(Disp_to_ComPort_:Port:_Control)    ; the Input
                                                            ; Channel for
                                                            ; Port #x

        LDI   Status_OK, R10                                ; Load the
                                                            ; code for
                                                            ; Status OK
                                                            ; into R10

        LDI   *+AR4(Disp_to_ComPort_:Port:_Control), R11    ; Use AR4 and
                                                            ; R11 to check
        AND   Com_Port_FIFO_Depth_Mask, R11                 ; the Com Port
                                                            ; FIFO depths.
        CMPI  Com_Port_FIFOs_Empty, R11                     ; If either FIFO
                                                            ; is NOT empty,
        LDIne LG_Next_Port_FIFO_Error, R10                  ; then load an
                                                            ; Error   code
                                                            ; into R10

        CMPI  Status_OK, R10                                ; See if the
                                                            ; OK code is
        Beq   LG_Next_Port_Exit_Point?                      ; still in R10.
                                                            ; If it is, then
                                                            ; branch to the
                                                            ; end of the
                                                            ; macro (do not
                                                            ; write results
                                                            ; to the Status
                                                            ; longword)

                                                   ; if we get to this point,
                                                   ; some Comm Port problem
                                                   ; has been found.  Set
                                                   ; an Error  code in
                                                   ; the Status longword

        LDI   :Port:, R11                                   ; MERGE the
                                                            ; Comm Port ID
        MB3   R11, R10                                      ; into R10
                                                            ; (which also
        STI   R10, @Com_Port_Status_Loc                     ; contains the
                                                            ; Status code)
                                                            ; and write R10
                                                            ; into the Comm
                                                            ; Port Status
                                                            ; longword
LG_Next_Port_Exit_Point?

        .endm

******************************************************************************
*   End of macro    SETUP_LG_NEXT_PORT                                       *
******************************************************************************


******************************************************************************
*                                                                            *
*   Macro:  SETUP_MFP_TO_NEXT_DMA                                            *
*                                                                            *
*   Arguments:    Chan              integer in the set {0..5} indicating     *
*                                   DMA Channel (and associated Comm Port)   *
*                                   used for Local-to-Global-to-Next DSP     *
*                                   Transfer                                 *
*                                                                            *
*   Purpose of macro                                                         *
*   ----------------                                                         *
*                                                                            *
*   This macro sets up the DMA of Mark and Force Pass data to the Next       *
*   DSP in the Local-to-Global transfer chain.  This macro is used in        *
*   all Local DSPs.  This macro is expanded in each L_DSP_xx module,         *
*   in a routine which is called by the L_Scan module whenever a Mark        *
*   and Force Pass event flows through the L1.5 Cal Trig.  Recall that       *
*   this information is derived from the Wakeup Word.                        *
*                                                                            *
*       To set up the MFP Data to the "next" DSP                             *
*       ----------------------------------------                             *
*                                                                            *
*   Recall that the DMA Channel #x is HALTED when this routine is called,    *
*   and has not re-initialized itself for the Local to Global DMA Transfer.  *
*   Therefore we must first RESET the channel, then load all of the          *
*   DMA Channel #x Registers just as if the DMA Channel was re-initializing. *
*                                                                            *
*   We store values DIFFERENT from the normal auto-initialize values in the  *
*   following registers:                                                     *
*                                                                            *
*       DMA Channel #x Control Register                                      *
*       DMA Channel #x Link Pointer Register                                 *
*                                                                            *
*   All of the other values loaded are the same as the normal auto-          *
*   initialize values, but must be loaded again here since the normal        *
*   auto-initialize has not occured (and the DMA Channel has been HALTED).   *
*                                                                            *
*   When the Mark and Force Pass List is finished, the DMA Channel auto-     *
*   initializes itself to point to the "normal" ObjectList (i.e. we don't    *
*   call another "re-setup the DMA Channel for normal operation" routine)    *
******************************************************************************

SETUP_MFP_TO_NEXT_DMA   .macro    Chan

        LDI     @DMA_Halt_Word_Loc, R11                 ; Use R11 to halt DMA
        STI     R11, *+AR4(Disp_to_DMA_:Chan:_Control)  ; Channel #x

        LDI     @LG_Next_DMA_Source_Loc,R11             ; Use R11 to set up the
        STI     R11, *+AR4(Disp_to_DMA_:Chan:_Source)   ; Source Address
                                                        ; Register for the
                                                        ; Local to Global
                                                        ; to "next" DSP
                                                        ; DMA Transfer (i.e
                                                        ; the beginning of this
                                                        ; LDSP's ObjectList)
                                                        ; over Channel #x

        LDI     @LG_Next_DMA_Source_Index_Loc,R11
        STI     R11, *+AR4(Disp_to_DMA_:Chan:_Source_Index)
                                                        ; Use R11 to set up the
                                                        ; Source Address Index
                                                        ; Register for the
                                                        ; Local to Global to
                                                        ; "next" DSP DMA
                                                        ; Transfer over
                                                        ; Channel #x

        LDI     @LG_Next_DMA_Counter_Loc,R11            ; Use R11 to set up the
        STI     R11, *+AR4(Disp_to_DMA_:Chan:_Counter)  ; Transfer Counter
                                                        ; Register for the
                                                        ; Local to Global to
                                                        ; "next" DSP DMA
                                                        ; Transfer over
                                                        ; Channel #x

        LDI     @LG_Next_DMA_Destination_Loc,R11
        STI     R11, *+AR4(Disp_to_DMA_:Chan:_Destination)
                                                        ; Use R11 to set up the
                                                        ; Destination Address
                                                        ; Register for the
                                                        ; Local to Global to
                                                        ; "next" DSP DMA
                                                        ; Transfer over
                                                        ; Channel #x

        LDI     @LG_Next_DMA_Dest_Index_Loc,R11
        STI     R11, *+AR4(Disp_to_DMA_:Chan:_Dest_Index)
                                                        ; Use R11 to set up the
                                                        ; Destination Address
                                                        ; Index Register for
                                                        ; the Local to Global
                                                        ; to "next" DSP
                                                        ; DMA Transfer
                                                        ; Channel #x

        LDI     @LG_MFP_Next_DMA_List_Handle,R11
        STI     R11, *+AR4(Disp_to_DMA_:Chan:_Link_Pointer)
                                                        ; Use R11 to set up the
                                                        ; Link Pointer
                                                        ; Register for the
                                                        ; Local to Global to
                                                        ; "next" DSP DMA
                                                        ; Transfer on Channel
                                                        ; #x.  When the normal
                                                        ; Identified Object
                                                        ; List is transferred,
                                                        ; the DMA Channel will
                                                        ; auto-initialize
                                                        ; pointing to the Mark
                                                        ; and Force Pass Data

        LDI     @DMA_Start_LG_to_Next_w_MFP_Loc,R11     ; Use R11 to
        STI     R11, *+AR4(Disp_to_DMA_:Chan:_Control)  ; program the Control
                                                        ; Register for the
                                                        ; Local to Global
                                                        ; to "next" DSP
                                                        ; DMA Transfer

        .endm

******************************************************************************
*   End of macro    SETUP_MFP_TO_NEXT_DMA                                    *
******************************************************************************


******************************************************************************
*   Macro:  SETUP_MFP_FROM_SINGLE_PREV_DMA                                   *
*                                                                            *
*   Arguments:    Chan              integer in the range {0..5} indicating   *
*                                   DMA Channel (and associated Comm Port)   *
*                                   for the Local-to-Global-from-Previous    *
*                                   transfer from LDSP #{A,B, or C}4         *
*                                                                            *
*   Purpose of macro                                                         *
*   ----------------                                                         *
*                                                                            *
*   This macro sets up the DMA of Mark and Force Pass data from the Previous *
*   DSP in the Local-to-Global transfer chain.  This macro is used ONLY      *
*   in Local DSPs which receive Previous data on a single Comm Port.  That   *
*   is, LDSPs A3, B3, and C3, receiving data from LDSPs A4, B4, and C4       *
*   respectively.  This macro  is expanded in the appropriate                *
*   L_DSP_xx modules, in a routine which is called by the L_Scan module      *
*   whenever a mark and Force Pass Event flows through the L1.5 Cal Trig.    *
*   Recall that this information is derived from the Wakeup Word.            *
*                                                                            *
*     To set up the MFP data from the Previous DSP                           *
*     --------------------------------------------                           *
*                                                                            *
*   Recall that the DMA Channel #x is not HALTED when this routine is called *
*   and has re-initialized itself for the Local to Global DMA Transfer.      *
*   Therefore we need only change the values stored in the following         *
*   registers:                                                               *
*                                                                            *
*   We store values DIFFERENT from the normal auto-initialize values in the  *
*   following registers:                                                     *
*                                                                            *
*       DMA Channel #x Control Register                                      *
*       DMA Channel #x Link Pointer Register                                 *
*                                                                            *
*   All of the other registers remain programmed with the normal auto-       *
*   initialize values, and need not be modified here since the normal        *
*   auto-initialize has already occured.                                     *
*                                                                            *
*   When the Mark and Force Pass List is finished, the DMA Channel auto-     *
*   initializes itself to point to the "normal" ObjectList (i.e. we don't    *
*   call another "re-setup the DMA Channel for normal operation" routine)    *
******************************************************************************

SETUP_MFP_FROM_SINGLE_PREV_DMA  .macro  Chan

        LDI     @LG_MFP_Pr_4_DMA_List_Handle,R11
        STI     R11, *+AR4(Disp_to_DMA_:Chan:_Link_Pointer)
                                                        ; Use R11 to set up the
                                                        ; Link Pointer
                                                        ; Register for the
                                                        ; Local to Global
                                                        ; from Previous DSP
                                                        ; Transfer on Channel
                                                        ; #X.  When the normal
                                                        ; Identified Object
                                                        ; List is transferred,
                                                        ; the DMA Channel will
                                                        ; auto-initialize
                                                        ; pointing to the Mark
                                                        ; and Force Pass Data

        LDI     @DMA_Start_LG_fr_Prev_w_MFP_Loc,R11     ; Use R11 to
        STI     R11, *+AR4(Disp_to_DMA_:Chan:_Control)  ; program the Control
                                                        ; Register for the
                                                        ; Local to Global
                                                        ; Transfer

        .endm

******************************************************************************
*   End of macro    SETUP_MFP_FROM_SINGLE_PREV_DMA                           *
******************************************************************************


******************************************************************************
*   Macro:  SETUP_MFP_FROM_DOUBLE_PREV_DMA                                   *
*                                                                            *
*   Arguments:    Chan_1            integer in the range {0..5} indicating   *
*                                   DMA Channel (and associated Comm Port)   *
*                                   for the Local-to-Global-from-Previous    *
*                                   Transfer from LDSP #{A or C}1            *
*                                                                            *
*   Arguments:    Chan_3            integer in the range {0..5} indicating   *
*                                   DMA Channel (and associated Comm Port)   *
*                                   for the Local-to-Global-from-Previous    *
*                                   Transfer from LDSP #{A or C}3            *
*                                                                            *
*   Purpose of macro                                                         *
*   ----------------                                                         *
*                                                                            *
*   This macro sets up the DMA of Mark and Force Pass data from the Previous *
*   DSPs in the Local-to-Global transfer chain.  This macro is used ONLY     *
*   in Local DSPs which receive Previous data on two Comm Ports.  That is    *
*   LDSPs A2 and B2, receiving data from both A1 and A3, or C1 and C3,       *
*   respectively.  This macro is expanded in the appropriate                 *
*   L_DSP_xx modules, in a routine which is called by the L_Scan module      *
*   whenever a mark and Force Pass Event flows through the L1.5 Cal Trig.    *
*   Recall that this information is derived from the Wakeup Word.            *
*                                                                            *
*     To set up the MFP data from the Previous DSPs                          *
*     --------------------------------------------                           *
*                                                                            *
*   Recall that the DMA Channels #x/y are not HALTED when we enter this      *
*   routine, and have re-initialized themselves for the LG DMA Transfer.     *
*   Therefore we need only change the values stored in the following         *
*   registers:                                                               *
*                                                                            *
*   We store values DIFFERENT from the normal auto-initialize values in the  *
*   following registers:                                                     *
*                                                                            *
*       DMA Channel #x/y Control Register                                    *
*       DMA Channel #x/y Link Pointer Register                               *
*                                                                            *
*   All of the other registers remain programmed with the normal auto-       *
*   initialize values, and need not be modified here since the normal        *
*   auto-initialize has already occured.                                     *
*                                                                            *
*   When the Mark and Force Pass List is finished, the DMA Channel auto-     *
*   initializes itself to point to the "normal" ObjectList (i.e. we don't    *
*   call another "re-setup the DMA Channel for normal operation" routine)    *
******************************************************************************

SETUP_MFP_FROM_DOUBLE_PREV_DMA  .macro  Chan_1, Chan_3

        LDI     @LG_MFP_Pr_1_DMA_List_Handle,R11
        STI     R11, *+AR4(Disp_to_DMA_:Chan_1:_Link_Pointer)
                                                        ; Use R11 to set up the
                                                        ; Link Pointer
                                                        ; Register for the
                                                        ; Local to Global
                                                        ; from Previous DSP
                                                        ; Transfer on Channel
                                                        ; #X.  When the normal
                                                        ; Identified Object
                                                        ; List is transferred,
                                                        ; the DMA Channel will
                                                        ; auto-initialize
                                                        ; pointing to the Mark
                                                        ; and Force Pass Data

        LDI     @DMA_Start_LG_fr_Prev_w_MFP_Loc,R11       ; Use R11 to
        STI     R11, *+AR4(Disp_to_DMA_:Chan_1:_Control)  ; program the Control
                                                          ; Register for the
                                                          ; Local to Global
                                                          ; Transfer

        LDI     @LG_MFP_Pr_3_DMA_List_Handle,R11
        STI     R11, *+AR4(Disp_to_DMA_:Chan_3:_Link_Pointer)
                                                        ; Use R11 to set up the
                                                        ; Link Pointer
                                                        ; Register for the
                                                        ; Local to Global
                                                        ; from Previous DSP
                                                        ; Transfer on Channel
                                                        ; #y.  When the normal
                                                        ; Identified Object
                                                        ; List is transferred,
                                                        ; the DMA Channel will
                                                        ; auto-initialize
                                                        ; pointing to the Mark
                                                        ; and Force Pass Data

        LDI     @DMA_Start_LG_fr_Prev_w_MFP_Loc,R11       ; Use R11 to
        STI     R11, *+AR4(Disp_to_DMA_:Chan_3:_Control)  ; program the Control
                                                          ; Register for the
                                                          ; Local to Global
                                                          ; Transfer

        .endm

******************************************************************************
*   End of macro    SETUP_MFP_FROM_DOUBLE_PREV_DMA                           *
******************************************************************************


******************************************************************************
*   Macro:  WAIT_FOR_PREV_DMA                                                *
*                                                                            *
*   Arguments:    NONE                                                       *
*                                                                            *
*   Purpose of macro                                                         *
*   ----------------                                                         *
*                                                                            *
*   This macro waits for the DMA transfer of Previous LDSP data to complete. *
*   It is used ONLY in Local DSPs which receive data either from a single    *
*   Comm Port or a pair of Comm Ports (i.e. LDSPs A2, A3, B3, C2, and C3).   *
*   This macro is expanded in the appropriate L_DSP_xx modules, in a         *
*   routine which is called by the L_Scan module for all events.             *
******************************************************************************

WAIT_FOR_PREV_DMA   .macro
                                                        ; note that the ISR
                                                        ; which sets this flag
                                                        ; will be called during
                                                        ; this loop.  This will
                                                        ; not cause an error
                                                        ; no matter which
                                                        ; instruction is
                                                        ; interrupted.

        LDI     @LG_Xfr_from_Prev_Status_Loc,R11        ; Get the flag which
                                                        ; indicates whether the
                                                        ; Local-to-Global xfr
                                                        ; from the "prev" DSP
                                                        ; is complete (this
                                                        ; flag is set in
                                                        ; an ISR)

        CMPI    LG_Xfr_Complete_Flag, R11               ; See if the transfer
                                                        ; is complete

        Bne     Wait_for_Previous_DSP_Data              ; If the transfer
                                                        ; is not complete,
                                                        ; then continue
                                                        ; waiting for
                                                        ; the transfer to
                                                        ; finish.



        .endm

******************************************************************************
*   End of macro    WAIT_FOR_PREV_DMA                                        *
******************************************************************************


******************************************************************************
*                                                                            *
*   Macro:  START_TOOL                                                       *
*                                                                            *
*   Arguments:    none                                                       *
*                                                                            *
*   Purpose of macro                                                         *
*   ----------------                                                         *
*                                                                            *
*   This macro performs the "Start of Tool" processing for any Eta Ring.     *
*                                                                            *
*   It PUSHES the Status Register.                                           *
*                                                                            *
******************************************************************************

START_TOOL      .macro

         PUSH   ST                         ; Save the Status Register.

         LDI    Tool_Accept_Flag, R4       ; Set the "Accept/Reject this
                                           ; Seed" flag (in R4) to "Accept."
         .endm

******************************************************************************
*   End of macro    START_TOOL                                               *
******************************************************************************


******************************************************************************
*   Macro:  COMPARE_1X2                                                      *
*                                                                            *
*   Arguments:    None                                                       *
*                                                                            *
*   Purpose of macro                                                         *
*   ----------------                                                         *
*                                                                            *
*   This macro compares the 1x2 EM Et Sum (stored in R3) to the 1x2 EM Et    *
*   Threshold (stored in EM_Et_1x2_Threshold_Loc), and modifies the          *
*   Tool Accept/Reject Flag (stored in R4) appropriately.                    *
*                                                                            *
*   If the 1x2 EM Et Sum is >= 1x2 EM Et Threshold then this macro exits     *
*   without modifying the flag (note that the Flag is initialized with the   *
*   "Accept" value).                                                         *
*                                                                            *
*   If the 1x2 EM Et Sum is < 1x2 EM Et Threshold then this macro sets the   *
*   Tool Accept/Reject Flag to "Reject."                                     *
******************************************************************************

COMPARE_1X2     .macro

         CMPI   @EM_Et_1x2_Threshold_Loc, R3      ; Compare the EM Et 1x2
                                                  ; Sum (in R3) to the
                                                  ; EM Et 1x2 Threshold

         LDIlt  Tool_Reject_Flag, R4              ; If the Sum is strictly
                                                  ; less than (using a
                                                  ; SIGNED compare) the
                                                  ; Threshold, then set
                                                  ; the Accept/Reject Flag
                                                  ; to "Reject"

         .endm

******************************************************************************
*   End of macro    COMPARE_1X2                                              *
******************************************************************************


******************************************************************************
*   Macro:  COMPARE_RATIO                                                    *
*                                                                            *
*   Arguments:    None                                                       *
*                                                                            *
*   Purpose of macro                                                         *
*   ----------------                                                         *
*                                                                            *
*   This macro makes the "ratio threshold cut"                               *
*                                                                            *
*   | EM_Et_1x2_Sum  |  ??                                                   *
*   | -------------- |  >=  EM_vs_Tot_Ratio_Threshold                        *
*   | Tot_Et_3x3_Sum |  ??                                                   *
*                                                                            *
*                                                                            *
*   and modifies the Tool Accept/Reject Flag (stored in R4) appropriately.   *
*                                                                            *
*   If the actual ratio is >= Ratio Threshold then this macro exits without  *
*   modifying the flag (note that the Flag is initialized with the "Accept"  *
*   value.                                                                   *
*                                                                            *
*   If the actual ratio is < Ratio Threshold then this macro sets the        *
*   Tool Accept/Reject Flag to "Reject."                                     *
*                                                                            *
*   Note that division is costly, so the comparison is actually performed    *
*   as:                                                                      *
*                     ??                                                     *
*    |EM_Et_1x2_Sum|  >= |Tot_Et_3x3_Sum|* EM_vs_Tot_Ratio_Threshold         *
*                     ??                                                     *
*                                                                            *
*   Also note that the EM Et 1x2 Sum and Tot Et 3x3 Sum are INTEGERS,        *
*   while the EM_vs_Tot_Ratio is a FLOATING POINT.  This macro produces      *
*   a floating point version of both the EM Et 1x2 Sum and the Tot Et 3x3    *
*   Sum (WITHOUT modifying the integer version which will be included in     *
*   the Local Object List if the Tool Accepts the seed).                     *
******************************************************************************

COMPARE_RATIO   .macro

         FLOAT  R3, R11                     ; Put a floating-point version
                                            ; of the 1x2 EM Et Sum in R11

         ABSF   R11, R9                     ; Put the absolute value of
                                            ; the floating point 1x2 EM Et
                                            ; Sum into R9

         FLOAT  R5, R11                     ; Put a floating-point version
                                            ; of the 3x3 Tot Et Sum in R11

         ABSF   R11, R10                    ; Put the absolute value of
                                            ; the floating point 3x3 Tot
                                            ; Et Sum into R10

         MPYF   @EM_vs_Tot_Ratio_Threshold_Loc, R10   ; Multiply the Tot
                                                      ; Et 3x3 Sum by the
                                                      ; Ratio Threshold

         CMPF   R10, R9                     ; Compare the floating-poing
                                            ; 1x2 EM Et Sum to the
                                            ; Ratio_Threshold * 3x3 Tot Sum.

         LDIlt  Tool_Reject_Flag, R4        ; If the 1x2 EM Et Sum is
                                            ; strictly less than the
                                            ; Ratio_Threshold * 3x3 Tot Sum,
                                            ; then set the Tool Accept/
                                            ; Reject Flag to "Reject."

         .endm

******************************************************************************
*   End of macro  COMPARE_RATIO                                              *
******************************************************************************


******************************************************************************
*   Macro:  TOOL_ACCEPT_REJECT                                               *
*                                                                            *
*   Arguments:    Ring              character string in the set              *
*                                   {Nplus2, NPlus3, Nplus4, Nplus5}         *
*                                   indicating Eta Ring                      *
*                                                                            *
*   Purpose of macro                                                         *
*   ----------------                                                         *
*                                                                            *
*   This macro chooses whether to Accept or Reject this Seed.  It looks at   *
*   the Tool Accept/Reject Flag (stored in R4) and the Flags byte of the     *
*   Wake Up Word (stored in R7) to make this decision.                       *
*                                                                            *
*   The decision is made as follows:                                         *
*                                                                            *
*     IF this Object has been Accepted, THEN branch to the                   *
*       "Write_Object_Entry"  label                                          *
*                                                                            *
*     ELSE IF this is a MFP Event, THEN branch to "Write_Object_Entry"       *
*       (note that R4 = 0  if this Object is to be Accepted,                 *
*                     = FF if this Object is to be Rejected.                 *
*        Therefore, R4 is exactly what needs to be put into the              *
*        Real Data vs. Mark and Force Pass Data Flag in the 2nd longword     *
*        of the Object Entry).                                               *
*                                                                            *
*     ELSE (i.e. this Object has been Rejected and the event type is Real)   *
*       branch back to the Scan Loop.                                        *
*                                                                            *
*   Otherwise, this macro branches to the label "Pass_This_Tool_Ring."       *
******************************************************************************

TOOL_ACCEPT_REJECT  .macro  Ring

         CMPI   Tool_Accept_Flag, R4            ; Does the Accept/Reject
                                                ; Flag say "Accept?"

         BeqD   Write_Object_Entry              ; If so, make a delayed
                                                ; branch to the Write Obj.
                                                ; routine.  The next 3
                                                ; instructions are also
                                                ; executed.

         NOP                                    ; Execute this instruction

         LBU1   R7, R11                         ; Get the Flags Byte
                                                ; from the Wake Up Word.

         CMPI   MFP_Wakeup_Word_Flag, R11       ; Compare the Flags Byte
                                                ; to the MFP Flag

         Beq    Write_Object_Entry              ; If this is a MFP event,
                                                ; make a branch to
                                                ; the Write Obj. routine.

                                                ; if we get to this point,
                                                ; the Seed has been
                                                ; rejected.

         BRD    Return_from_Tool_Ring_:Ring:    ; If the flag says "Reject"
                                                ; then make a delayed branch
                                                ; back to the Scan Loop.
                                                ; The next 3 instructions
                                                ; are executed

         NOP                                         ; Also perform this
                                                     ; instruction

         NOP                                         ; Also perform this
                                                     ; instruction

         POP     ST                        ; Restore the Status Register to
                                           ; the value that it had when we
                                           ; entered this Tool Code routine.

         .endm

******************************************************************************
* End of macro    TOOL_ACCEPT_REJECT                                         *
******************************************************************************


******************************************************************************
*                                                                            *
*   Macro:  TOOL_SUCCESS                                                     *
*                                                                            *
*   Arguments:    Ring              character string in the set              *
*                                   {Nplus2, NPlus3, Nplus4, Nplus5}         *
*                                   indicating Eta Ring                      *
*                                                                            *
*   Purpose of macro                                                         *
*   ----------------                                                         *
*                                                                            *
*   This macro performs the "Success" exit point operations of each of the   *
*   Tool Rings:                                                              *
*                                                                            *
*   IF                                                                       *
*        writing the data from this Object would overrun the Identified      *
*        Object List                                                         *
*   THEN                                                                     *
*        set a flag indicating that the List has been overrun, and DO NOT    *
*        attempt to write any data from this Object but instead return to    *
*        the Scan Routine                                                    *
*   ELSE                                                                     *
*        write the data from this Object to the Identified Object List       *
*        increment the Identified Object Counter                             *
*        return to the Scan Routine.                                         *
*                                                                            *
*  Recall that the 3 Longwords which describe the Identified Object are      *
*  exactly the Entry in the L1.5 Cal Trig Data Block Local DSP Section       *
*  corresponding to this Object, as well as the data transferred to the      *
*  Global DSP                                                                *
*                                                                            *
*  Recall the format of these Entries:                                       *
*                     D        D   D        D   D          D   D       D     *
*                     3        2   2        1   1          0   0       0     *
*                     1        4   3        6   5          8   7       0     *
*                   |------------|------------|--------------|----------|    *
*   1st Longword:   | Phi coord. | Eta coord. | Local Tool # | Term #   |    *
*                   | of center  | of center  |              |          |    *
*                   | Tower      | Tower      |              |          |    *
*                   |( 1..32)    | (-20..-1,  |              |          |    *
*                   |            |  +1..+20)  |              |          |    *
*                   |---------------------------------------------------|    *
*   2nd Longword:   | Object Energy           | Real or Mark | Object   |    *
*                   |                         | and Pass Data| Type     |    *
*                   | This is the 1x2 EM Et   | (00: real    | Code     |    *
*                   | Sum.                    | (FF: mark and|          |    *
*                   |                         |      force   |          |    *
*                   | units = 1/4 Gev per     |      pass)   |          |    *
*                   | count                   |              |          |    *
*                   |---------------------------------------------------|    *
*   3rd Longword:   | Phi coord. | Eta coord. | Total Et 3x3 Sum        |    *
*                   | of neighbr | of neighbr |                         |    *
*                   | Tower used | Tower used | units = 1/4 GeV per     |    *
*                   | in 1x2 EM  | in 1x2 EM  | count                   |    *
*                   | Et Sum     | Et Sum     |                         |    *
*                   | ( 1..32)   | (-20..-1,  |                         |    *
*                   |            | (+1..+20)  |                         |    *
*                   |---------------------------------------------------|    *
*                                                                            *
*                                                                            *
*  Finally I keep a running pointer (in AR3) to the next Longword to write   *
*  in the List of Identified Objects.  I use "pre-displacement increment"    *
*  because the first time we enter the routine the pointer points at the     *
*  location of the Local DSP Header longword.                                *
******************************************************************************

******************************************************************************
*  Note here some of the LSH3's and STI's could be done in parallel.  I will *
*  change the code after I have tested the non-parallel version.             *
******************************************************************************

TOOL_SUCCESS    .macro  Ring

        ADDI    1,R2                                ; Increment the Valid
                                                    ; Object Counter

        CMPI    @LDSP_Entries_per_List_Loc,R2       ; If this Object would
                                                    ; overrun the Local
        BhiD    This_Local_List_Overrun             ; List of Identified
                                                    ; Objects, then make a
                                                    ; delayed branch to the
                                                    ; overrun handling routine
                                                    ; Otherwise continue
                                                    ; writing data to the List

        SUBI3   RC,BK,R9                            ; LOAD phi into R9
                                                    ; This instruction is
                                                    ; a bit on the weird
                                                    ; side but using AR1
                                                    ; one runs into problems
                                                    ; at phi = 32 because of
                                                    ; circular addressing.
                                                    ; Recall BK = 32 and RC
                                                    ; counts down from 31 to 0
                                                    ; as phi goes up from 1 to
                                                    ; 32.

        LSH     24,R9                               ; Shift phi into the MSB
                                                    ; of R9.

        MB2    @Eta_:Ring:_Coord_Loc,R9             ; MERGE the eta coord of
                                                    ; the center tower into
                                                    ; the next to MSB of
                                                    ; R9

        MB1    @Tool_Number_Loc,R9                  ; MERGE the Tool Number
                                                    ; of this Tool into the
                                                    ; next to LSB of R9

        MB0    @Term_Number_for_This_Tool_Loc,R9    ; MERGE the Term Number
                                                    ; of the Term being
                                                    ; evaluated into the LSB
                                                    ; of R9

        STI     R9, *++AR3(1)                       ; Write this word (LW #1
                                                    ; of the 3     Longwords
                                                    ; in each entry in the
                                                    ; List of Identified
                                                    ; Objects).  BEFORE
                                                    ; storing the word
                                                    ; increment AR3 by 1.

        LSH3    16,R3,R9                            ; LOAD the 1x2 EM Sum
                                                    ; into the MSW of R9

        MB1     R4, R9                              ; MERGE the Accept/Reject
                                                    ; Flag into the next to
                                                    ; LSB of R9 (this is
                                                    ; used as the Real vs.
                                                    ; MFP flag).

        MB0    @Object_Type_Loc,R9                 ; MERGE the Object Type
                                                    ; into the LSB of R9

        STI     R9, *++AR3(1)                       ; Write this word (LW #2
                                                    ; of the 3     Longwords
                                                    ; in each entry in the
                                                    ; List of Identified
                                                    ; Objects).  BEFORE
                                                    ; storing the word
                                                    ; increment AR3 by 1.

        LDI     R5, R9                              ; LOAD the 3x3 Total
                                                    ; Et Sum into R9

         BRD     Return_from_Tool_Ring_:Ring:        ; Make an unconditional
                                                     ; delayed branch back to
                                                     ; the Trigger Tower
                                                     ; scanning routine.  The
                                                     ; next 3 instructions
                                                     ; will be executed before
                                                     ; the branch is taken.

        MH1     @Largest_Neighbor_ID_Loc, R9        ; MERGE the key to the
                                                    ; ID of the neighbor
                                                    ; TT used to calculate
                                                    ; the 1x2 EM Et Sum
                                                    ; into the 2 MSBytes
                                                    ; of R9.  This key is
                                                    ; stored in R11

        STI     R9, *++AR3(1)                       ; Write this word (LW #3
                                                    ; of the 3     Longwords
                                                    ; in each entry in the
                                                    ; List of Identified
                                                    ; Objects).  BEFORE
                                                    ; storing the word
                                                    ; increment AR3 by 1.

         POP     ST                        ; Restore the Status Register to
                                           ; the value that it had when we
                                           ; entered this Tool Code routine.

******************************************************************************
*  This is the "List Overrun" handling routine                               *
******************************************************************************

This_Local_List_Overrun:

         BRD     Return_from_Tool_Ring_:Ring:        ; Make an unconditional
                                                     ; delayed branch back to
                                                     ; the Trigger Tower
                                                     ; scanning routine.  The
                                                     ; next 3 instructions
                                                     ; will be executed before
                                                     ; the branch is taken.

         LDI     @List_Overrun_Flag_Loc,R2          ; Set    the Identified
                                                    ; Object Count to a
                                                    ; value indicating that
                                                    ; the Local List has been
                                                    ; overrun

         NOP                               ; Also execute this instruction
                                           ; before the unconditional delayed
                                           ; branch back to the Trig Tower
                                           ; scanning routine is taken.

         POP     ST                        ; Restore the Status Register to
                                           ; the value that it had when we
                                           ; entered this Tool Code routine.


         .endm

******************************************************************************
*   End of macro    TOOL_SUCCESS                                             *
******************************************************************************

