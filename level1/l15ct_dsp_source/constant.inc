****************************************************************************
*                                                                          *
*  File:  Constant.Inc                                 Date:    1-JUN-1994 *
*                                                                          *
*  Description of Module:                                                  *
*  =====================                                                   *
*                                                                          *
*  This file contains constants specific to the Local and Global DSPs.     *
*  It is divided into multiple sections:                                   *
*                                                                          *
*   (1) Term Numbers                                                       *
*   (2) Unique Tool ID's                                                   *
*   (3) Eta Coordinates                                                    *
*   (4) DSP ID's                                                           *
*   (5) Parameter Passing Data Format                                      *
*   (6) Error Codes Returned to TCC                                        *
*   (7) Status Codes Returned to 68K                                       *
*   (8) Data Block Format Constants                                        *
*   (9) Other General Purpose Constants                                    *
*  (10) Per-Trigger Tower Zero Response Data                               *
*                                                                          *
*  This file contains only constants defined with the assembler .set       *
*  directive                                                               *
*                                                                          *
****************************************************************************



******************************************************************************
*  Constants defined with the assembler .set directive                       *
*                                                                            *
*           Term Numbers                                                     *
******************************************************************************

Term_Number_0               .set      0                 ; Term Number of
                                                        ; Term # 0

Term_Number_1               .set      1                 ; Term Number of
                                                        ; Term # 1

Term_Number_2               .set      2                 ; Term Number of
                                                        ; Term # 2

Term_Number_3               .set      3                 ; Term Number of
                                                        ; Term # 3

Term_Number_4               .set      4                 ; Term Number of
                                                        ; Term # 4

Term_Number_5               .set      5                 ; Term Number of
                                                        ; Term # 5

Term_Number_6               .set      6                 ; Term Number of
                                                        ; Term # 6

Term_Number_7               .set      7                 ; Term Number of
                                                        ; Term # 7
******************************************************************************
*  Constants defined with the assembler .set directive                       *
*                                                                            *
*       Unique Tool Numbers and Tool-Related Constants                       *
******************************************************************************

EM_3x3_Tot_5x5_Tool     .set          01h       ; This is the Tool Number of
                                                ; the EM 3x3 vs. TOT 5x5 Tool

Tool_1_Derived_Const_Length .set      10        ; This is the Number of
                                                ; Derived Constants for
                                                ; Tool 1

EM_1x2_Tot_3x3_Tool     .set          02h       ; This is the Tool Number of
                                                ; the EM 1x2 vs. TOT 3x3 Tool

Tool_2_Derived_Const_Length .set       6        ; This is the Number of
                                                ; Derived Constants for
                                                ; Tool 2

******************************************************************************
*  Constants defined with the assembler .set directive                       *
*                                                                            *
*           ETA Coordinates                                                  *
******************************************************************************

Eta_Nonexistent             .set     000h               ; non-existent eta
                                                        ; coordinate for
                                                        ; eta not in the
                                                        ; range
                                                        ; {-20..-1,+1..+20}

Eta_Neg_20                  .set     0ECh               ; eta coordinate  -20

Eta_Neg_19                  .set     0EDh               ; eta coordinate  -19

Eta_Neg_18                  .set     0EEh               ; eta coordinate  -18

Eta_Neg_17                  .set     0EFh               ; eta coordinate  -17

Eta_Neg_16                  .set     0F0h               ; eta coordinate  -16

Eta_Neg_15                  .set     0F1h               ; eta coordinate  -15

Eta_Neg_14                  .set     0F2h               ; eta coordinate  -14

Eta_Neg_13                  .set     0F3h               ; eta coordinate  -13

Eta_Neg_12                  .set     0F4h               ; eta coordinate  -12

Eta_Neg_11                  .set     0F5h               ; eta coordinate  -11

Eta_Neg_10                  .set     0F6h               ; eta coordinate  -10

Eta_Neg_9                   .set     0F7h               ; eta coordinate   -9

Eta_Neg_8                   .set     0F8h               ; eta coordinate   -8

Eta_Neg_7                   .set     0F9h               ; eta coordinate   -7

Eta_Neg_6                   .set     0FAh               ; eta coordinate   -6

Eta_Neg_5                   .set     0FBh               ; eta coordinate   -5

Eta_Neg_4                   .set     0FCh               ; eta coordinate   -4

Eta_Neg_3                   .set     0FDh               ; eta coordinate   -3

Eta_Neg_2                   .set     0FEh               ; eta coordinate   -2

Eta_Neg_1                   .set     0FFh               ; eta coordinate   -1

Eta_Pos_1                   .set     001h               ; eta coordinate   +1

Eta_Pos_2                   .set     002h               ; eta coordinate   +2

Eta_Pos_3                   .set     003h               ; eta coordinate   +3

Eta_Pos_4                   .set     004h               ; eta coordinate   +4

Eta_Pos_5                   .set     005h               ; eta coordinate   +5

Eta_Pos_6                   .set     006h               ; eta coordinate   +6

Eta_Pos_7                   .set     007h               ; eta coordinate   +7

Eta_Pos_8                   .set     008h               ; eta coordinate   +8

Eta_Pos_9                   .set     009h               ; eta coordinate   +9

Eta_Pos_10                  .set     00Ah               ; eta coordinate  +10

Eta_Pos_11                  .set     00Bh               ; eta coordinate  +11

Eta_Pos_12                  .set     00Ch               ; eta coordinate  +12

Eta_Pos_13                  .set     00Dh               ; eta coordinate  +13

Eta_Pos_14                  .set     00Eh               ; eta coordinate  +14

Eta_Pos_15                  .set     00Fh               ; eta coordinate  +15

Eta_Pos_16                  .set     010h               ; eta coordinate  +16

Eta_Pos_17                  .set     011h               ; eta coordinate  +17

Eta_Pos_18                  .set     012h               ; eta coordinate  +18

Eta_Pos_19                  .set     013h               ; eta coordinate  +19

Eta_Pos_20                  .set     014h               ; eta coordinate  +20


******************************************************************************
*  Constants defined with the assembler .set directive                       *
*                                                                            *
*                 DSP IDs                                                    *
******************************************************************************

Local_DSP_A1                .set     0a1h               ; ID of LDSP Node A1

Local_DSP_A2                .set     0a2h               ; ID of LDSP Node A2

Local_DSP_A3                .set     0a3h               ; ID of LDSP Node A3

Local_DSP_A4                .set     0a4h               ; ID of LDSP Node A4

Local_DSP_B1                .set     0b1h               ; ID of LDSP Node B1

Local_DSP_B3                .set     0b3h               ; ID of LDSP Node B3

Local_DSP_B4                .set     0b4h               ; ID of LDSP Node B4

Local_DSP_C1                .set     0c1h               ; ID of LDSP Node C1

Local_DSP_C2                .set     0c2h               ; ID of LDSP Node C2

Local_DSP_C3                .set     0c3h               ; ID of LDSP Node C3

Local_DSP_C4                .set     0c4h               ; ID of LDSP Node C4

Global_DSP_B2               .set     0b2h               ; ID of GDSP Node B2

******************************************************************************
*       Constants defined with assembler .set directives                     *
*                                                                            *
*   Data format for Parameter Passing                                        *
*                                                                            *
******************************************************************************

Memory_Map_Version_Expected      .set          1        ; Specifies Memory
                                                        ; Map Version
                                                        ; 1

Number_of_Terms_Expected         .set          1        ; Specifies
                                                        ; 1
                                                        ; Term Expected

Maximum_Number_of_Terms          .set          8        ; Specifies
                                                        ; 8
                                                        ; Terms allowed
                                                        ; (at most)
                                                        ; in one Crate

Maximum_Number_of_Ref_Sets       .set         10        ; Specifies
                                                        ; 10
                                                        ; Ref Sets allowed
                                                        ; (at most)
                                                        ; in one Crate

Univ_Param_Block_Size            .set         20h       ; Size of the
                                                        ; Universal Parameter
                                                        ; Block in Dual
                                                        ; Port Memory

One_Term_Frame_Param_Block_Size  .set         20h       ; Size of the Frame
                                                        ; Parameter Block
                                                        ; for a single
                                                        ; Term

One_Term_Local_Param_Block_Size  .set         20h       ; Size of the Local
                                                        ; Parameter Block
                                                        ; for a single
                                                        ; Term

One_Term_Global_Param_Block_Size .set         20h       ; Size of the Global
                                                        ; Parameter Block
                                                        ; for a single
                                                        ; Term

One_Ref_Set_Block_Size           .set         20h       ; Size of one
                                                        ; Reference Set
                                                        ; Block in DP
                                                        ; Mem

One_DSP_Status_Block_Size        .set          8h       ; Size of one
                                                        ; DSP (Local or
                                                        ; Global) Status
                                                        ; Block to
                                                        ; either EC or TCC

TCC_68K_Status_Block_Size        .set         10h       ; Size of the
                                                        ; TCC-to-DSP and
                                                        ; 68K-to-DSP
                                                        ; Status Blocks

One_Inter_DSP_Status_Block_Size  .set          8h       ; Size of one
                                                        ; DSP-to-DSP
                                                        ; Status
                                                        ; Block

LDSP_A2_Inter_DSP_Base_Address   .set   80001ec0h       ; Base Address of
                                                        ; the "Inter-DSP"
LDSP_A3_Inter_DSP_Base_Address   .set   80001ec8h       ; Communication
                                                        ; Blocks for each
LDSP_A4_Inter_DSP_Base_Address   .set   80001ed0h       ; of the Local
                                                        ; DSPs and the
LDSP_A1_Inter_DSP_Base_Address   .set   80001ed8h       ; Global DSP

LDSP_B3_Inter_DSP_Base_Address   .set   80001ee0h

LDSP_B4_Inter_DSP_Base_Address   .set   80001ee8h

LDSP_B1_Inter_DSP_Base_Address   .set   80001ef0h

LDSP_C3_Inter_DSP_Base_Address   .set   80001ef8h

LDSP_C4_Inter_DSP_Base_Address   .set   80001f00h

LDSP_C1_Inter_DSP_Base_Address   .set   80001f08h

LDSP_C2_Inter_DSP_Base_Address   .set   80001f10h

GDSP_B2_Inter_DSP_Base_Address   .set   80001f18h


LDSP_A2_to_68K_Stat_Base_Address .set   80001f20h       ; Base Address of
                                                        ; the "Status to
LDSP_A3_to_68K_Stat_Base_Address .set   80001f28h       ; 68K Services" block
                                                        ; for each of the
LDSP_A4_to_68K_Stat_Base_Address .set   80001f30h       ; Local DSP and
                                                        ; the Global DSP
LDSP_A1_to_68K_Stat_Base_Address .set   80001f38h

LDSP_B3_to_68K_Stat_Base_Address .set   80001f40h

LDSP_B4_to_68K_Stat_Base_Address .set   80001f48h

LDSP_B1_to_68K_Stat_Base_Address .set   80001f50h

LDSP_C3_to_68K_Stat_Base_Address .set   80001f58h

LDSP_C4_to_68K_Stat_Base_Address .set   80001f60h

LDSP_C1_to_68K_Stat_Base_Address .set   80001f68h

LDSP_C2_to_68K_Stat_Base_Address .set   80001f70h

GDSP_B2_to_68K_Stat_Base_Address .set   80001f78h

Disp_to_EC_Stat                  .set          0        ; Displacement
                                                        ; to the EC
                                                        ; Status Longword
                                                        ; within a "DSP
                                                        ; to 68K Status"
                                                        ; Block


SEK_to_all_DSP_Stat_Base_Address .set   80001f80h       ; Base address
                                                        ; of the "Status
                                                        ; from the "68K
                                                        ; Services" block.
                                                        ; This one block
                                                        ; serves ALL
                                                        ; DSPs.

Disp_to_Local_WUW                .set          0        ; Displacement
                                                        ; to the Local
                                                        ; Wake Up Word
                                                        ; within the
                                                        ; "68K to all
                                                        ; DSPs Status"
                                                        ; Block

Disp_to_Global_WUW               .set          1        ; Displacement
                                                        ; to the Global
                                                        ; Wake Up Word
                                                        ; within the
                                                        ; "68K to all
                                                        ; DSPs Status"
                                                        ; Block

Disp_to_Transfer_Word            .set          2        ; Displacement
                                                        ; to the Transfer
                                                        ; Required Word
                                                        ; within the
                                                        ; "68K to all
                                                        ; DSPs Status"
                                                        ; Block

LDSP_A2_to_TCC_Stat_Base_Address .set   80001f90h       ; Base Address of
                                                        ; the "Status to
LDSP_A3_to_TCC_Stat_Base_Address .set   80001f98h       ; the TCC" block
                                                        ; for each of the
LDSP_A4_to_TCC_Stat_Base_Address .set   80001fa0h       ; Local DSP and
                                                        ; the Global DSP
LDSP_A1_to_TCC_Stat_Base_Address .set   80001fa8h

LDSP_B3_to_TCC_Stat_Base_Address .set   80001fb0h

LDSP_B4_to_TCC_Stat_Base_Address .set   80001fb8h

LDSP_B1_to_TCC_Stat_Base_Address .set   80001fc0h

LDSP_C3_to_TCC_Stat_Base_Address .set   80001fc8h

LDSP_C4_to_TCC_Stat_Base_Address .set   80001fd0h

LDSP_C1_to_TCC_Stat_Base_Address .set   80001fd8h

LDSP_C2_to_TCC_Stat_Base_Address .set   80001fe0h

GDSP_B2_to_TCC_Stat_Base_Address .set   80001fe8h


TCC_to_all_DSP_Stat_Base_Address .set   80001ff0h       ; Base address
                                                        ; of the "Status
                                                        ; from the EC
                                                        ; 68K" block.
                                                        ; This one block
                                                        ; serves ALL
                                                        ; DSPs.


******************************************************************************
*  Constants defined with the assembler .set directive                       *
*                                                                            *
*           Error Codes Returned to TCC                                      *
******************************************************************************

Status_OK                       .set    0               ; Status OK (from any
                                                        ; routine)

Local_Tool_Number_Error         .set    1               ; Error in Local Tool
                                                        ; Number (from
                                                        ; L_Params)

Number_of_Local_Params_Error    .set    2               ; Incorrect number of
                                                        ; Local Parameters
                                                        ; specified (from
                                                        ; L_Tl_Ini)

Local_Param_Range_Error         .set    3               ; Local Parameter
                                                        ; out of Range (from
                                                        ; L_Tl_Ini)

Local_Param_Block_Type_Error    .set    4               ; Incorrect Parameter
                                                        ; Block Type Flag
                                                        ; Local Parameter
                                                        ; Block (from L_Params)

Local_Term_Number_Error         .set    5               ; Term Number in
                                                        ; Local Parameter
                                                        ; Block Header did
                                                        ; not match expected
                                                        ; Term Number (from
                                                        ; L_Params)

Reference_Set_Type_Error        .set    6               ; Reference Set Type
                                                        ; did not match
                                                        ; expected Reference
                                                        ; Set Type (from
                                                        ; L_Tl_Ini)

Local_Tool_Memory_Ovf_Error     .set    21              ; Local Tool Internal
                                                        ; memory overflow
                                                        ; was sensed (from
                                                        ; L_Tl_Np%)

Global_Cand_Memory_Ovf_Error    .set    41              ; Global Tool Internal
                                                        ; memory overflow
                                                        ; was sensed during
                                                        ; candidate list
                                                        ; processing

Global_Answer_Memory_Ovf_Error  .set    61              ; Global Tool Internal
                                                        ; memory overflow
                                                        ; was sensed during
                                                        ; answer setting

Global_Tool_End_Event_Error     .set    81              ; Global Tool found
                                                        ; an error while
                                                        ; getting ready for
                                                        ; the next event

Global_Tool_Number_Error        .set    101             ; Error in Global Tool
                                                        ; Number (from
                                                        ; G_Params)

Number_of_Global_Params_Error   .set    102             ; Incorrect number of
                                                        ; Global Parameters
                                                        ; specified

Global_Param_Range_Error        .set    103             ; Global Parameter
                                                        ; out of Range

Global_Param_Block_Type_Error   .set    104             ; Incorrect Parameter
                                                        ; Block Type Flag
                                                        ; Global Parameter
                                                        ; Block (from G_Params)

Global_Term_Number_Error        .set    105             ; Term Number in
                                                        ; Global Parameter
                                                        ; Block Header did
                                                        ; not match expected
                                                        ; Term Number (from
                                                        ; G_Params)

Number_of_Frame_Params_Error    .set    121             ; Incorrect number of
                                                        ; Frame Parameters
                                                        ; specified (from
                                                        ; L_Params)

Frame_Param_Range_Error         .set    122             ; Frame Parameter
                                                        ; out of Range (from
                                                        ; L_Params)

Frame_Param_Block_Type_Error    .set    123             ; Incorrect Parameter
                                                        ; Block Type Flag in
                                                        ; Frame Parameter
                                                        ; Block (from L_Params)

Frame_Term_Number_Error         .set    124             ; Term Number in Frame
                                                        ; Parameter Block
                                                        ; Header did not
                                                        ; match expected Term
                                                        ; Number (from
                                                        ; L_Params)

Memory_Map_Version_Error        .set    141             ; Memory Map Version
                                                        ; in Universal Param
                                                        ; Header did not
                                                        ; match expected
                                                        ; Memory Map Version
                                                        ; (from L_Params)

Number_of_Defined_Terms_Error   .set    142             ; Incorrect number of
                                                        ; Terms defined (from
                                                        ; L_Params)

Number_of_Univ_Params_Error     .set    143             ; Incorrect number
                                                        ; of Universal Params
                                                        ; specified (from
                                                        ; L_Params)

Univ_Param_Range_Error          .set    144             ; Universal Parameter
                                                        ; out of Range (from
                                                        ; L_Params)

CRC_Port_Token_Error            .set    241             ; Comm Port Connected
                                                        ; to a CRC Channel
                                                        ; has not given up
                                                        ; its Token (from
                                                        ; L_DSP_xx or
                                                        ; G_DSP_B2)

CRC_Port_FIFO_Error             .set    242             ; A CRC
                                                        ; Comm Port used
                                                        ; by this DSP does
                                                        ; not have empty
                                                        ; FIFOs after
                                                        ; Initialization
                                                        ; (from L_DSP_xx
                                                        ; or G_DSP_B2)

LG_Prev_Port_FIFO_Error         .set    243             ; A LG_from_Prev
                                                        ; Comm Port used
                                                        ; by this DSP does
                                                        ; not have empty
                                                        ; FIFOs after
                                                        ; Initialization
                                                        ; (from L_DSP_xx
                                                        ; or G_DSP_B2)

LG_Next_Port_FIFO_Error         .set    244             ; A LG_to_Next
                                                        ; Comm Port used
                                                        ; by this DSP does
                                                        ; not have empty
                                                        ; FIFOs after
                                                        ; Initialization
                                                        ; (from L_DSP_xx
                                                        ; or G_DSP_B2)

******************************************************************************
*  Constants defined with the assembler .set directive                       *
*                                                                            *
*           Status Codes Returned to 68K                                     *
*                                                                            *
* Recall that the Local/Global DSP Status to 68K is passed via 16 bits of    *
* the first longword in each of the DSP_to_68K communication blocks.         *
*                                                                            *
* Each bit has a distinctive meaning which is described here                 *
******************************************************************************

Local_Looking_for_Valid_W_U_W       .set 0001h          ; This Local DSP
                                                        ; is Ready for the
                                                        ; the next (or first)
                                                        ; loop and is looking
                                                        ; for a valid
                                                        ; Wake Up Word

Local_Received_Valid_W_U_W          .set 0002h          ; This Local DSP
                                                        ; has received a
                                                        ; Valid Wake Up Word

Local_Received_all_ERPB_Data        .set 0004h          ; This Local DSP
                                                        ; has received all
                                                        ; of its ERPB Data

Local_Started_Loc_Glob_Xfr          .set 0008h          ; This Local DSP
                                                        ; has started its
                                                        ; part of the Local
                                                        ; to Global to Next
                                                        ; DSP Transfer.
                                                        ; By definition it
                                                        ; has completed its
                                                        ; Local Code and
                                                        ; has built its
                                                        ; Object List

Local_Finished_Loc_Glob_Xfr         .set 0010h          ; This Local DSP
                                                        ; has finished its
                                                        ; part(s) of the Local
                                                        ; to Global Transfer
                                                        ; (including Mark and
                                                        ; Force Pass Data
                                                        ; if applicable)

Local_Cleaned_Up_Wait_for_Synch     .set 8000h          ; This Local DSP
                                                        ; has cleaned up
                                                        ; from this Cycle
                                                        ; and is waiting
                                                        ; for all DSPs to
                                                        ; re-synchronize.
                                                        ; The EC will announce
                                                        ; this re-synching
                                                        ; by setting the
                                                        ; Wake Up Word
                                                        ; invalid



Global_Looking_for_Valid_W_U_W      .set 0001h          ; The Global DSP
                                                        ; is Ready for the
                                                        ; the next (or first)
                                                        ; loop and is looking
                                                        ; for a valid
                                                        ; Wake Up Word

Global_Received_Valid_W_U_W         .set 0002h          ; The Global DSP
                                                        ; has received a
                                                        ; Valid Wake Up Word

Global_Received_all_Local_Data      .set 0004h          ; The Global DSP
                                                        ; has received all
                                                        ; of its Data from
                                                        ; all Local DSPS.
                                                        ; This includes the
                                                        ; Mark and Force
                                                        ; Pass Data if
                                                        ; applicable

Global_Evaluated_all_Terms          .set 0008h          ; The Global DSP
                                                        ; has evaluated all
                                                        ; Terms and is now
                                                        ; providing the
                                                        ; Term Answer Word
                                                        ; to the EC

Global_Finished_Xfr_to_214          .set 0010h          ; The Global DSP
                                                        ; has finished the
                                                        ; transfer of the
                                                        ; DSP-sourced Data
                                                        ; in the Data Block
                                                        ; to the MVME214.
                                                        ; This includes
                                                        ; Mark and Force
                                                        ; Pass Data if
                                                        ; applicable

Global_Skipped_Xfr_to_214           .set 0020h          ; The Global DSP
                                                        ; has acknowledged
                                                        ; that no transfer
                                                        ; to the 214 was
                                                        ; required and
                                                        ; hence the transfer
                                                        ; was skipped.

Global_Cleaned_Up_Wait_for_Synch    .set 8000h          ; The Global DSP
                                                        ; has cleaned up
                                                        ; from this Cycle
                                                        ; and is waiting
                                                        ; for all DSPs to
                                                        ; re-synchronize.
                                                        ; The EC will announce
                                                        ; this re-synching
                                                        ; by setting the
                                                        ; Wake Up Word
                                                        ; invalid


******************************************************************************
*  Constants defined with the assembler .set directive                       *
*                                                                            *
*       Data Block Format-Related Constants                                  *
******************************************************************************

Crate_Header_Length                 .set       6        ; Specifies
                                                        ; 6
                                                        ; Longwords in
                                                        ; the Crate
                                                        ; Header

Frame_Section_Length                .set       5        ; Specifies
                                                        ; 5
                                                        ; Longwords in
                                                        ; the Frame
                                                        ; Section

Frame_Param_Section_Length          .set     145        ; Specifies
                                                        ; 145
                                                        ; Longwords in
                                                        ; the Frame
                                                        ; Parameter
                                                        ; Section.

Term_Param_Section_Length           .set     257        ; Specifies
                                                        ; 257
                                                        ; Longwords in
                                                        ; the Term
                                                        ; Parameter
                                                        ; Section

LDSP_Longwords_per_Entry            .set       3        ; Specifies
                                                        ; 3
                                                        ; Longwords per
                                                        ; Entry in Local
                                                        ; DSP List of
                                                        ; Identified Objects

LDSP_Entries_per_List               .set       8        ; Specifies
                                                        ; 8
                                                        ; Entries per
                                                        ; Local DSP List of
                                                        ; Identified
                                                        ; Objects

LDSP_ObjectList_Length .set LDSP_Longwords_per_Entry * LDSP_Entries_per_List + 1
                                                        ; Specifies the
                                                        ; length of the
                                                        ; LDSP ObjectList
                                                        ; Note that the "1"
                                                        ; refers to the
                                                        ; header which is
                                                        ; 1 longword.

LDSP_Section_Length  .set  11 * LDSP_ObjectList_Length  ; Specifies the
                                                        ; length of the
                                                        ; LDSP Section
                                                        ; in the Data
                                                        ; Block

GDSP_Longwords_per_Entry            .set       4        ; Specifies
                                                        ; 4
                                                        ; Longwords per
                                                        ; Entry in Global
                                                        ; DSP List of
                                                        ; Identified Objects

GDSP_Entries_per_List               .set       16       ; Specifies
                                                        ; 16
                                                        ; Entries per
                                                        ; Global DSP
                                                        ; List of
                                                        ; Identified Objects

GDSP_ObjectList_Length .set GDSP_Longwords_per_Entry * GDSP_Entries_per_List + 1
                                                        ; Specifies the
                                                        ; length of the
                                                        ; GDSP ObjectList
                                                        ; Note that the "1"
                                                        ; refers to the
                                                        ; header which is
                                                        ; 1 longword.

GDSP_Section_Length    .set  GDSP_ObjectList_Length     ; Specifies the
                                                        ; length of the
                                                        ; GDSP Section
                                                        ; in the Data
                                                        ; Block

One_TT_Data_Block_Size             .set        32       ; Specifies
                                                        ; 32
                                                        ; elements in
                                                        ; each TT Data
                                                        ; Block

DeBug_Type_0_Entry_Length          .set         2       ; Specifies
                                                        ; 2
                                                        ; longwords
                                                        ; per 68K
                                                        ; DeBug Data
                                                        ; Entry

MFP_Type_1_Entry_Length            .set   4 * One_TT_Data_Block_Size + 1
                                                        ; Specifies
                                                        ; the number of
                                                        ; longwords per
                                                        ; MFP Type 1
                                                        ; Entry
                                                        ; Note that the "1"
                                                        ; refers to the
                                                        ; one-longword long
                                                        ; Header.

MFP_Type_2_Entry_Length            .set   4 * One_Ref_Set_Block_Size + 2
                                                        ; Specifies
                                                        ; the number of
                                                        ; longwords per
                                                        ; MFP Type 2
                                                        ; Entry
                                                        ; Note that the "2"
                                                        ; refers to the
                                                        ; one-longword long
                                                        ; Header plus the
                                                        ; one-longword long
                                                        ; Ref Set identifier

MFP_Type_3_Tool_1_Entry_Length     .set   Tool_1_Derived_Const_Length + 1
                                                        ; Specifies
                                                        ; the number of
                                                        ; longwords per
                                                        ; MFP Type 3
                                                        ; Entry for Tool 1
                                                        ; Note that the "1"
                                                        ; refers to the
                                                        ; one-longword long
                                                        ; Header

MFP_Type_3_Tool_2_Entry_Length     .set   Tool_2_Derived_Const_Length + 1
                                                        ; Specifies
                                                        ; the number of
                                                        ; longwords per
                                                        ; MFP Type 3
                                                        ; Entry for Tool 2
                                                        ; Note that the "1"
                                                        ; refers to the
                                                        ; one-longword long
                                                        ; Header

LDSP_MFP_Section_Length  .set  MFP_Type_1_Entry_Length + MFP_Type_2_Entry_Length + MFP_Type_3_Tool_2_Entry_Length
                                                        ; Specifies
                                                        ; the number of
                                                        ; longwords per
                                                        ; Local DSP
                                                        ; Mark and Force
                                                        ; Pass Data Section

Synch_Data_Length                  .set    1            ; Specifies
                                                        ; the number of
                                                        ; longwords
                                                        ; of Synch Data
                                                        ; in the Type 4
                                                        ; Debug Entry.

DeBug_Type_4_Entry_Length          .set    Synch_Data_Length + 1
                                                        ; Specifies the
                                                        ; number of
                                                        ; longwords per
                                                        ; DeBug Type 4
                                                        ; Entry.
                                                        ; Note that the "1"
                                                        ; refers to the
                                                        ; one-longword long
                                                        ; Header

All_DSP_MFP_DeBug_Sect_Length       .set   11 * LDSP_MFP_Section_Length + DeBug_Type_4_Entry_Length
                                                        ; Specifies the
                                                        ; total length
                                                        ; of the DSP-
                                                        ; sourced part
                                                        ; of the
                                                        ; DeBug section
                                                        ; Data Block for
                                                        ; MFP events

All_DSP_Normal_DeBug_Sect_Length    .set   DeBug_Type_4_Entry_Length
                                                        ; Specifies the
                                                        ; total length
                                                        ; of the DSP-
                                                        ; sourced part
                                                        ; of the
                                                        ; DeBug section
                                                        ; Data Block for
                                                        ; normal events

******************************************************************************
*  Constants defined with the assembler .set directive                       *
*                                                                            *
*       General Purpose Constants                                            *
******************************************************************************


;    ********************************************************************
;    *          Constants related to setting up DMA Lists               *
;    ********************************************************************


DMA_Halt_Word                   .set    00000007h       ; Control word used
                                                        ; to HALT a DMA
                                                        ; Channel

DMA_Start_Word_for_Initialize   .set    00c0000bh       ; Control word used
                                                        ; to Initialize a
                                                        ; DMA Channel.
                                                        ; Specifies no synch,
                                                        ; auto-init w/o CPU,
                                                        ; DMA priority higher
                                                        ; than CPU priority,
                                                        ; and starts the DMA
                                                        ; Channel

DMA_Start_Word_for_ERPB_Input   .set    00c4004bh       ; Control word used
                                                        ; for DMA "Input from
                                                        ; ERPB" transfer.
                                                        ; Specifies READ synch,
                                                        ; auto-initialize w/o
                                                        ; CPU intervention,
                                                        ; DMA priority higher
                                                        ; than CPU priority,
                                                        ; DMA Controller will
                                                        ; generate an interrupt
                                                        ; when Transfer Counter
                                                        ; goes to 0, and starts
                                                        ; the DMA Channel

DMA_Start_Word_for_LG_Prev      .set    00c4004bh       ; Control word used
                                                        ; for DMA "Loc-Glob
                                                        ; from previous DSP"
                                                        ; transfer.
                                                        ; Specifies READ synch,
                                                        ; auto-initialize w/o
                                                        ; CPU intervention,
                                                        ; DMA priority higher
                                                        ; than CPU priority,
                                                        ; DMA Controller will
                                                        ; generate an interrupt
                                                        ; when Transfer Counter
                                                        ; goes to 0, and starts
                                                        ; the DMA Channel

DMA_Start_Word_for_LG_MFP_Prev  .set    00c0004bh       ; Control word used
                                                        ; for DMA "Loc-Glob
                                                        ; from previous DSP"
                                                        ; transfer WITH MFP
                                                        ; data Transfer to
                                                        ; follow.
                                                        ; Specifies READ synch,
                                                        ; auto-initialize w/o
                                                        ; CPU intervention,
                                                        ; DMA priority higher
                                                        ; than CPU priority,
                                                        ; DMA Controller will
                                                        ; NOT generate an
                                                        ; interrupt
                                                        ; when Transfer Counter
                                                        ; goes to 0, and starts
                                                        ; the DMA
                                                        ; Channel

DMA_Start_Word_for_MFP_Prev_End .set    00c4004bh       ; Control word used
                                                        ; for the last (and
                                                        ; only) section of
                                                        ; MFP data from the
                                                        ; Previous DSP.
                                                        ; Specifies READ synch,
                                                        ; auto-initialize w/o
                                                        ; CPU intervention,
                                                        ; DMA priority higher
                                                        ; than CPU priority,
                                                        ; DMA Controller will
                                                        ; generate an interrupt
                                                        ; when Transfer Counter
                                                        ; goes to 0, and starts
                                                        ; the DMA Channel

DMA_Start_Word_for_LG_Next      .set    0084008bh       ; Control word used
                                                        ; for DMA "Local to
                                                        ; Global to next
                                                        ; DSP" transfer
                                                        ; without MFP data.
                                                        ; Specifies WRITE sync,
                                                        ; auto-initialize w/o
                                                        ; CPU intervention,
                                                        ; DMA priority higher
                                                        ; than CPU priority,
                                                        ; DMA Controller will
                                                        ; generate an interrupt
                                                        ; when Transfer Counter
                                                        ; goes to 0, and halts
                                                        ; the DMA Channel at a
                                                        ; transfer boundary

DMA_Start_Word_for_LG_MFP_Next  .set    0080008bh       ; Control word used
                                                        ; for DMA "Local to
                                                        ; Global to next
                                                        ; DSP" transfer
                                                        ; WITH    MFP data.
                                                        ; Specifies WRITE sync,
                                                        ; auto-initialize w/o
                                                        ; CPU intervention,
                                                        ; DMA priority higher
                                                        ; than CPU priority,
                                                        ; DMA Controller will
                                                        ; NOT generate an
                                                        ; interrupt
                                                        ; when Transfer Counter
                                                        ; goes to 0, and halts
                                                        ; the DMA Channel at a
                                                        ; transfer boundary

DMA_Cont_Word_for_MFP_Next      .set    00c0008bh       ; Control word used
                                                        ; for all but the last
                                                        ; subsection of the
                                                        ; "Mark and Force Pass"
                                                        ; Transfer to the next
                                                        ; DSP" transfer.
                                                        ; Specifies WRITE synch,
                                                        ; auto-initialize w/o
                                                        ; CPU intervention,
                                                        ; DMA priority higher
                                                        ; than CPU priority,
                                                        ; DMA Controller will
                                                        ; NOT generate an
                                                        ; interrupt when
                                                        ; Transfer Counter goes
                                                        ; to 0, and starts the
                                                        ; DMA Channel

DMA_Cont_Word_for_MFP_Next_End  .set    00c4008bh       ; Control word used
                                                        ; for the last sub-
                                                        ; section of the "Mark
                                                        ; and Force Pass to
                                                        ; next DSP" transfer.
                                                        ; Specifies WRITE synch,
                                                        ; auto-initialize w/o
                                                        ; CPU intervention,
                                                        ; DMA priority higher
                                                        ; than CPU priority,
                                                        ; DMA Controller will
                                                        ; generate an interrupt
                                                        ; when Transfer Counter
                                                        ; goes to 0, and starts
                                                        ; the DMA Channel


Dummy_DMA_Source                .set    002ffbffh       ; Dummy DMA Source
                                                        ; Address

Dummy_DMA_Source_Index          .set           0h       ; Dummy DMA Source
                                                        ; Address Index

Dummy_DMA_Transfer_Counter      .set           1h       ; Dummy DMA Transfer
                                                        ; Counter

Dummy_DMA_Destination           .set    002ffbffh       ; Dummy DMA Destination
                                                        ; Address

Dummy_DMA_Dest_Index            .set           0h       ; Dummy DMA Destination
                                                        ; Address Index


;    ********************************************************************
;    *          Constants related to setting up Comm Ports              *
;    ********************************************************************


Halt_Com_Port_Input_Channel   .set  0008h       ; Setting bit 3 in a
                                                ; CPCR halts the Input
                                                ; Channel

Halt_Com_Port_Output_Channel  .set  0010h       ; Setting bit 4 in a
                                                ; CPCR halts the Output
                                                ; Channel

Com_Port_Direction_Mask       .set  0004h       ; Bit 2 in a CPCR is
                                                ; the Direction bit.

Com_Port_Input_Mode           .set  0004h       ; If bit with value 4
                                                ; in a CPCR is set, the
                                                ; Comm Port is in INPUT
                                                ; Mode

Com_Port_Output_Mode          .set  0000h       ; If bit with value 2
                                                ; in a CPCR is cleared, the
                                                ; Comm Port is in OUTPUT
                                                ; Mode

Com_Port_FIFO_Depth_Mask      .set  1fe0h       ; Bits 12..5 are the
                                                ; FIFO Depth Bits

Com_Port_FIFOs_Empty          .set  0000h       ; If all bits 12..5 are
                                                ; cleared then both
                                                ; the Input and Output
                                                ; FIFOs are empty

;    ********************************************************************
;    *          Constants Related receiving CRC Data                    *
;    ********************************************************************

One_TT_DMA_Finished     .set    00000081h       ; This is the flag
                                                ; indicating that one
                                                ; TT DMA is finished

Rack_1_EM_Tot_TT_Data   .set    00008181h       ; This is the mask of
                                                ; TT Data Received
                                                ; appropriate for a
                                                ; LDSP which only
                                                ; services "Rack 1"

Rack_2_EM_Tot_TT_Data   .set    81810000h       ; This is the mask of
                                                ; TT Data Received
                                                ; appropriate for a
                                                ; LDSP which only
                                                ; services "Rack 2"

Racks_1_2_EM_Tot_TT_Data  .set  81818181h       ; This is the mask of
                                                ; TT Data Received
                                                ; appropriate for a
                                                ; LDSP which services
                                                ; "Rack 1" and "Rack 2"

;    ********************************************************************
;    *          Constants Related receiving LG Data                     *
;    ********************************************************************

One_LG_DMA_Finished     .set        0042h       ; This is the flag
                                                ; indicating that one
                                                ; LG DMA is finished

LDSP_B134_LG_Data       .set    00424200h       ; This is the mask of
                                                ; LG Data Received
                                                ; appropriate for the
                                                ; GDSP when only
                                                ; Hydra "B" is present

All_LDSP_LG_Data        .set    42424242h       ; This is the mask of
                                                ; LG Data Received
                                                ; appropriate for
                                                ; the GDSP when all
                                                ; 3 Hydras are present


;    ********************************************************************
;    *          Constants Related to Object Lists                       *
;    ********************************************************************

Real_ObjectList_Entry_Type    .set   000h       ; This is the flag indicating
                                                ; "Real Data" in this entry
                                                ; in the Object List

MFP_ObjectList_Entry_Type     .set   0ffh       ; This is the flag indicating
                                                ; Mark and Force Pass Data
                                                ; in this entry in the
                                                ; Object List

Electron_Object_Type    .set         001h       ; This is the object type
                                                ; of electrons

ObjectList_Overrun_Flag .set         0ffh       ; This is the flag indicating
                                                ; Overrun of the List of
                                                ; Identified Objects

;    ********************************************************************
;    *          Constants Related to Local-Global Transfer              *
;    ********************************************************************

LG_Xfr_Incomplete_Flag       .set  0000h        ; This is the flag indicating
                                                ; that the Local-to-Global
                                                ; Transfer (either from the
                                                ; previous or to the next
                                                ; DSP) is INcomplete

LG_Xfr_Complete_Flag         .set 07f7fh        ; This is the flag indicating
                                                ; that the Local-to-Global
                                                ; Transfer (either from the
                                                ; previous or to the next
                                                ; DSP) is complete

LG_fr_Prev_Half_Done_Flag    .set 0007fh        ; This is the flag indicating
                                                ; that the LG Transfer from
                                                ; either A1 or A3 to A2 or
                                                ; C1 or C3 to
                                                ; C2 is complete.  This
                                                ; is used only for LDSP A2 and
                                                ; C2 (the DSPs with TWO
                                                ; Previous DSPs sending data
                                                ; on TWO Comm Ports

;    ********************************************************************
;    *          Constants Related to Transfer to 214                    *
;    ********************************************************************

Two14_Xfr_Incomplete_Flag    .set  0000h        ; This is the flag indicating
                                                ; that the Transfer to the
                                                ; 214 is INcomplete

Two14_Xfr_Complete_Flag      .set 0ffffh        ; This is the flag indicating
                                                ; that the Transfer to the
                                                ; 214 is   complete

;    ********************************************************************
;    *          Constants Related to Debug Section Entries              *
;    ********************************************************************

MFP_Trig_Tower_Et_Entry_Type .set    001h       ; This is the flag indicating
                                                ; that this Entry in the
                                                ; DeBug Section
                                                ; of the Data Block
                                                ; is Trigger Tower Et data

MFP_Ref_Set_Entry_Type       .set    002h       ; This is the flag indicating
                                                ; that this Entry in the
                                                ; DeBug Section
                                                ; of the Data Block
                                                ; is Reference Set data

MFP_Derived_Const_Entry_Type .set    003h       ; This is the flag indicating
                                                ; that this Entry in the
                                                ; DeBug Section
                                                ; of the Data Block
                                                ; is Derived Constant data

Synchronization_Entry_Type   .set    004h       ; This is the flag indicating
                                                ; that this Entry in the
                                                ; DeBug Section of the
                                                ; Data Block is Synchronization
                                                ; data.

;    ********************************************************************
;    *          Constants Related to Parameter Block Data               *
;    ********************************************************************

Frame_Parameter_Block_Type   .set    000h       ; Frame Parameter Block Type
                                                ; identifier

Local_Parameter_Block_Type   .set    001h       ; Local Parameter Block Type
                                                ; Identifier

Global_Parameter_Block_Type  .set    002h       ; Global Parameter Block
                                                ; Type Identifier

EM_Reference_Set_Type   .set         000h       ; This is the flag indicating
                                                ; EM Reference Set Type

Tot_Reference_Set_Type  .set         0ffh       ; This is the flag indicating
                                                ; Total Reference Set Type

;    ********************************************************************
;    *          Constants Related to the Wake Up Word                   *
;    ********************************************************************

Invalid_Wakeup_Word_Flag     .set    0ffh       ; This is the flag indicating
                                                ; that the Wake Up Word is
                                                ; invalid.

Normal_Wakeup_Word_Flag      .set    000h       ; This is the flag indicating
                                                ; that the Wake Up Word is
                                                ; valid and indicates that
                                                ; this cycle should be a
                                                ; "normal" (non-Mark and
                                                ; Force Pass) L1.5 CTCycle

MFP_Wakeup_Word_Flag         .set    001h       ; This is the flag indicating
                                                ; that the Wake Up Word is
                                                ; valid and indicates that
                                                ; this cycle should be a
                                                ; Mark and Force Pass
                                                ; L1.5 CTCycle

Largest_Valid_Wakeup_Word_Flag  .set 001h       ; This is the largest Valid
                                                ; Wakeup Word.

;    ********************************************************************
;    *          Constants Related to the Transfer Word                  *
;    ********************************************************************

Invalid_Transfer_Word_Flag   .set    0ffh       ; This is the flag indicating
                                                ; that the Transfer Word is
                                                ; invalid.

No_Transfer_Required_Flag    .set    000h       ; This is the flag indicating
                                                ; that the Transfer Word is
                                                ; valid and indicates that
                                                ; the data should NOT be
                                                ; transferred to the MVME214

Transfer_is_Required_Flag    .set    001h       ; This is the flag indicating
                                                ; that the Transfer Word is
                                                ; valid and indicates that
                                                ; the data should be
                                                ; transferred to the MVME214

Largest_Valid_Transfer_Word  .set    001h       ; This is the largest Valid
                                                ; Transfer Word.


;    ********************************************************************
;    *          Constants Related to the Term Answer Word               *
;    ********************************************************************

Fail_All_Term_Mask              .set  000h      ; This is the flag indicating
                                                ; that all Terms have failed

Pass_Term_0_Mask                .set  001h      ; This is the flag indicating
                                                ; that Term 0 has passed

Pass_Term_01_Mask               .set  003h      ; This is the flag indicating
                                                ; that Terms 0,1 have passed

Pass_Term_012_Mask              .set  007h      ; This is the flag indicating
                                                ; that Terms 0,1,2 have passed

******************************************************************************
*           Constants defined with assembler .set directives                 *
*                                                                            *
*   Per-Trigger Tower Zero Response Data                                     *
*                                                                            *
*   Note that the .set directives are of the format                          *
*                                                                            *
*       Zero_Response  .set     EM_PROM_Zeresp + HD_PROM_Zeresp              *
*                                                                            *
******************************************************************************

Eta_Fake_EM_Et_Zero_Response    .set    8 + 8           ; Zero Response for
                                                        ; the EM Et at eta
                                                        ; outside the range
                                                        ; {-20..-1,+1..+20}

Eta_1_EM_Et_Zero_Response       .set    8 + 8           ; Zero Response for
                                                        ; the EM Et at eta
                                                        ; +/-  1

Eta_2_EM_Et_Zero_Response       .set    8 + 8           ; Zero Response for
                                                        ; the EM Et at eta
                                                        ; +/-  2

Eta_3_EM_Et_Zero_Response       .set    8 + 9           ; Zero Response for
                                                        ; the EM Et at eta
                                                        ; +/-  3

Eta_4_EM_Et_Zero_Response       .set    8 + 10          ; Zero Response for
                                                        ; the EM Et at eta
                                                        ; +/-  4

Eta_5_EM_Et_Zero_Response       .set    8 + 11          ; Zero Response for
                                                        ; the EM Et at eta
                                                        ; +/-  5

Eta_6_EM_Et_Zero_Response       .set    8 + 10          ; Zero Response for
                                                        ; the EM Et at eta
                                                        ; +/-  6

Eta_7_EM_Et_Zero_Response       .set    8 + 12          ; Zero Response for
                                                        ; the EM Et at eta
                                                        ; +/-  7

Eta_8_EM_Et_Zero_Response       .set    8 + 12          ; Zero Response for
                                                        ; the EM Et at eta
                                                        ; +/-  8

Eta_9_EM_Et_Zero_Response       .set    8 + 13          ; Zero Response for
                                                        ; the EM Et at eta
                                                        ; +/-  9

Eta_10_EM_Et_Zero_Response      .set    8 + 13          ; Zero Response for
                                                        ; the EM Et at eta
                                                        ; +/- 10

Eta_11_EM_Et_Zero_Response      .set    8 + 13          ; Zero Response for
                                                        ; the EM Et at eta
                                                        ; +/- 11

Eta_12_EM_Et_Zero_Response      .set    8 + 12          ; Zero Response for
                                                        ; the EM Et at eta
                                                        ; +/- 12

Eta_13_EM_Et_Zero_Response      .set    8 + 12          ; Zero Response for
                                                        ; the EM Et at eta
                                                        ; +/- 13

Eta_14_EM_Et_Zero_Response      .set    8 + 14          ; Zero Response for
                                                        ; the EM Et at eta
                                                        ; +/- 14

Eta_15_EM_Et_Zero_Response      .set    8 + 14          ; Zero Response for
                                                        ; the EM Et at eta
                                                        ; +/- 15

Eta_16_EM_Et_Zero_Response      .set    8 + 14          ; Zero Response for
                                                        ; the EM Et at eta
                                                        ; +/- 16

Eta_17_EM_Et_Zero_Response      .set    8 + 14          ; Zero Response for
                                                        ; the EM Et at eta
                                                        ; +/- 17

Eta_18_EM_Et_Zero_Response      .set    8 + 14          ; Zero Response for
                                                        ; the EM Et at eta
                                                        ; +/- 18

Eta_19_EM_Et_Zero_Response      .set    8 + 14          ; Zero Response for
                                                        ; the EM Et at eta
                                                        ; +/- 19

Eta_20_EM_Et_Zero_Response      .set    8 + 14          ; Zero Response for
                                                        ; the EM Et at eta
                                                        ; +/- 20

Eta_Fake_Tot_Et_Zero_Response   .set    8 + 8           ; Zero Response for
                                                        ; the Tot Et at eta
                                                        ; outside the range
                                                        ; {-20..-1,+1..+20}

Eta_1_Tot_Et_Zero_Response      .set    8 + 8           ; Zero Response for
                                                        ; the Tot Et at eta
                                                        ; +/-  1

Eta_2_Tot_Et_Zero_Response      .set    8 + 8           ; Zero Response for
                                                        ; the Tot Et at eta
                                                        ; +/-  2

Eta_3_Tot_Et_Zero_Response      .set    9 + 9           ; Zero Response for
                                                        ; the Tot Et at eta
                                                        ; +/-  3

Eta_4_Tot_Et_Zero_Response      .set    10 + 10         ; Zero Response for
                                                        ; the Tot Et at eta
                                                        ; +/-  4

Eta_5_Tot_Et_Zero_Response      .set    11 + 11         ; Zero Response for
                                                        ; the Tot Et at eta
                                                        ; +/-  5

Eta_6_Tot_Et_Zero_Response      .set    12 + 10         ; Zero Response for
                                                        ; the Tot Et at eta
                                                        ; +/-  6

Eta_7_Tot_Et_Zero_Response      .set    12 + 12         ; Zero Response for
                                                        ; the Tot Et at eta
                                                        ; +/-  7

Eta_8_Tot_Et_Zero_Response      .set    12 + 12         ; Zero Response for
                                                        ; the Tot Et at eta
                                                        ; +/-  8

Eta_9_Tot_Et_Zero_Response      .set    13 + 13         ; Zero Response for
                                                        ; the Tot Et at eta
                                                        ; +/-  9

Eta_10_Tot_Et_Zero_Response     .set    13 + 13         ; Zero Response for
                                                        ; the Tot Et at eta
                                                        ; +/- 10

Eta_11_Tot_Et_Zero_Response     .set    13 + 13         ; Zero Response for
                                                        ; the Tot Et at eta
                                                        ; +/- 11

Eta_12_Tot_Et_Zero_Response     .set    14 + 12         ; Zero Response for
                                                        ; the Tot Et at eta
                                                        ; +/- 12

Eta_13_Tot_Et_Zero_Response     .set    14 + 12         ; Zero Response for
                                                        ; the Tot Et at eta
                                                        ; +/- 13

Eta_14_Tot_Et_Zero_Response     .set    14 + 14         ; Zero Response for
                                                        ; the Tot Et at eta
                                                        ; +/- 14

Eta_15_Tot_Et_Zero_Response     .set    14 + 14         ; Zero Response for
                                                        ; the Tot Et at eta
                                                        ; +/- 15

Eta_16_Tot_Et_Zero_Response     .set    14 + 14         ; Zero Response for
                                                        ; the Tot Et at eta
                                                        ; +/- 16

Eta_17_Tot_Et_Zero_Response     .set    14 + 14         ; Zero Response for
                                                        ; the Tot Et at eta
                                                        ; +/- 17

Eta_18_Tot_Et_Zero_Response     .set    14 + 14         ; Zero Response for
                                                        ; the Tot Et at eta
                                                        ; +/- 18

Eta_19_Tot_Et_Zero_Response     .set    14 + 14         ; Zero Response for
                                                        ; the Tot Et at eta
                                                        ; +/- 19

Eta_20_Tot_Et_Zero_Response     .set    8 + 14          ; Zero Response for
                                                        ; the Tot Et at eta
                                                        ; +/- 20

