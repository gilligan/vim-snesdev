" Vim syntax file
" Language:	ca65 snes assembler syntax
" By: Tobias Pflug <tobias.pflug@gmail.com>

if exists("b:current_syntax")
    finish
endif

syn clear
syn case ignore
syn keyword asm65Reg x y a


syn keyword asm65Op BRK ORA COP ORA TSB ORA ASL ORA PHP ORA ASL PHD TSB ORA ASL ORA BPL ORA ORA ORA TRB ORA ASL ORA CLC ORA INC TCS TRB ORA ASL ORA JSR AND JSR AND BIT AND ROL AND PLP AND ROL PLD BIT AND ROL AND BMI AND AND AND BIT AND ROL AND SEC AND DEC TSC BIT AND ROL AND RTI EOR WDM EOR MVP EOR LSR EOR PHA EOR LSR PHK JMP EOR LSR EOR BVC EOR EOR EOR MVN EOR LSR EOR CLI EOR PHY TCD JMP EOR LSR EOR RTS ADC PER ADC STZ ADC ROR ADC PLA ADC ROR RTL JMP ADC ROR ADC BVS ADC ADC ADC STZ ADC ROR ADC SEI ADC PLY TDC JMP ADC ROR ADC BRA STA BRL STA STY STA STX STA DEY BIT TXA PHB STY STA STX STA BCC STA STA STA STY STA STX STA TYA STA TXS TXY STZ STA STZ STA LDY LDA LDX LDA LDY LDA LDX LDA TAY LDA TAX PLB LDY LDA LDX LDA BCS LDA LDA LDA LDY LDA LDX LDA CLV LDA TSX TYX LDY LDA LDX LDA CPY CMP REP CMP CPY CMP DEC CMP INY CMP DEX WAI CPY CMP DEC CMP BNE CMP CMP CMP PEI CMP DEC CMP CLD CMP PHX STP JMP CMP DEC CMP CPX SBC SEP SBC CPX SBC INC SBC INX SBC NOP XBA CPX SBC INC SBC BEQ SBC SBC SBC PEA SBC INC SBC SED SBC PLX XCE JSR SBC INC SBC brk ora cop ora tsb ora asl ora php ora asl phd tsb ora asl ora bpl ora ora ora trb ora asl ora clc ora inc tcs trb ora asl ora jsr and jsr and bit and rol and plp and rol pld bit and rol and bmi and and and bit and rol and sec and dec tsc bit and rol and rti eor wdm eor mvp eor lsr eor pha eor lsr phk jmp eor lsr eor bvc eor eor eor mvn eor lsr eor cli eor phy tcd jmp eor lsr eor rts adc per adc stz adc ror adc pla adc ror rtl jmp adc ror adc bvs adc adc adc stz adc ror adc sei adc ply tdc jmp adc ror adc bra sta brl sta sty sta stx sta dey bit txa phb sty sta stx sta bcc sta sta sta sty sta stx sta tya sta txs txy stz sta stz sta ldy lda ldx lda ldy lda ldx lda tay lda tax plb ldy lda ldx lda bcs lda lda lda ldy lda ldx lda clv lda tsx tyx ldy lda ldx lda cpy cmp rep cmp cpy cmp dec cmp iny cmp dex wai cpy cmp dec cmp bne cmp cmp cmp pei cmp dec cmp cld cmp phx stp jmp cmp dec cmp cpx sbc sep sbc cpx sbc inc sbc inx sbc nop xba cpx sbc inc sbc beq sbc sbc sbc pea sbc inc sbc sed sbc plx xce jsr sbc inc sbc syn keyword asm65Branch bcc bcs beq bmi bne bpl bvc bvs jmp jsr jml

syn keyword snesregs INIDISP OBSEL OAMADDL OAMADDH OAMDATA BGMODE MOSAIC BG1SC BG2SC BG3SC BG4SC BG12NBA BG34NBA BG1HOFS BG1VOFS BG2HOFS BG2VOFS BG3HOFS BG3VOFS BG4HOFS BG4VOFS VMAIN VMADDL VMADDH VMDATAL VMDATAH M7SEL M7A M7B M7C M7D M7X M7Y CGADD CGDATA W12SEL W34SEL WOBJSEL WH0 WH1 WH2 WH3 WBGLOG WOBJLOG TM TS TMW TSW CGWSEL CGADSUB COLDATA SETINI re Proces MPYL MPYM MPYH SLHV RDOAM RDVRAML RDVRAMH RDCGRAM OPHCT OPVCT STAT77 STAT78 Processi APUI00 APUI01 APUI02 APUI03 WMDATA WMADDL WMADDM WMADDH NMITIMEN WRIO WRMPYA WRMPYB WRDIVL WRDIVH WRDIVB HTIMEL HTIMEH VTIMEL VTIMEH MDMAEN HDMAEN MEMSEL RDNMI TIMEUP HVBJOY RDIO RDDIVL RDDIVH RDMPYL RDMPYH JOY1L JOY1H JOY2L JOY2H JOY3L JOY3H JOY4L JOY4H DMAPx BBADx A1TxL A1TxH A1Bx DASxL DASxH DASBx A2AxL A2AxH NTRLx MIRRx

syn match asmLabel		"^[@]*[a-z_][a-z0-9_]*:"
syn match asmComment		";.*"hs=s+1 contains=asmTodo
syn keyword asmTodo	contained todo fixme xxx warning danger note notice bug
syn region asmString		start=+"+ skip=+\\"+ end=+"+
syn keyword asmSettings		opt org  segment global include byte word a8 i16 smart res import proc endproc macro endmacro if ifdef elseif match endif ifnblank define asciiz local

syn match decNumber	"\<\d\+\>"
syn match hexNumber	"\$\x\+\>" " 'bug', but adding \< doesn't behave!
syn match binNumber	"%[01]\+\>"
syn match asmImmediate	"#\$\x\+\>"
syn match asmImmediate	"#\d\+\>"
syn match asmImmediate	"<\$\x\+\>"
syn match asmImmediate	"<\d\+\>"
syn match asmImmediate	">\$\x\+\>"
syn match asmImmediate	">\d\+\>"
syn match asmImmediate	"#<\$\x\+\>"
syn match asmImmediate	"#<\d\+\>"
syn match asmImmediate	"#>\$\x\+\>"
syn match asmImmediate	"#\<[a-z0-9_]\+\>"

hi link asmLabel	Label
hi link asmString	String
hi link asmComment	Comment
hi link asmSettings	Statement
hi link asm65Op Statement
hi link asmSallyUndoc Special
hi link asm65Reg Identifier
hi link asm65Branch Conditional
hi link asmTodo Debug
hi link asmImmediate Special
hi link hexNumber	Number
hi link binNumber	Number
hi link decNumber	Number

let b:current_syntax = "snesasm"
