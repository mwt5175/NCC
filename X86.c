/************************************************************************
*
*	X86.c - Code generator
*
*   Copyright (c) BrokenThorn Entertainment Co. All Rights Reserved.
*
************************************************************************/

#include "ncc.h"
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>

// https://cmd.inp.nsk.su/old/cmd2/manuals/programming/GNU/gdb/stabs_4.html

/*
	This TEMPLATE table and matching algorithm was adopted from the
	Neptune Assembler at /SDK/NASM/TABLE.C. As the Neptune C Compiler
	only needs a subset, this is a reduced version and slightly modified.

	If TABLE.C is modified in the future, this should be double checked
	in case we want to update this as well.
*/

#define o16 ModSizeWord
#define o32 ModSizeDword
#define o64 ModSizeQword
#define a16 ModAddrSizeWord
#define a32 ModAddrSizeDword

#define PS ModPsuedo
#define OC ModEmitOpcodeOnly
#define X OpcodeExtNone
#define R ModMODRM
#define rcode ModRegOpcode

#define imm OperandImm
#define mem OperandClassMem
#define rm8 OperandRm8
#define rm16 OperandRm16
#define rm32 OperandRm32
#define rm64 OperandRm64
#define r8 OperandReg8
#define r16 OperandReg16
#define r32 OperandReg32
#define r64 OperandReg64
#define imm8 OperandImm8
#define imm16 OperandImm16
#define imm32 OperandImm32
#define imm64 OperandImm64
#define mem128 OperandMem128
#define mem80 OperandMem80
#define mem64 OperandMem64
#define mem32 OperandMem32
#define mem16 OperandMem16
#define mem8 OperandMem8
#define es OperandEs
#define ss OperandSs
#define fs OperandFs
#define gs OperandGs
#define cs OperandCs
#define ds OperandDs
#define al OperandAl
#define ah OperandAh
#define ax OperandAx
#define eax OperandEax
#define rax OperandRax
#define dx OperandDx
#define edx OperandEdx
#define rdx OperandRdx
#define cl OperandCl
#define cx OperandCx
#define ecx OperandEcx
#define rcx OperandRcx
#define seg OperandSeg
#define rel8 OperandRel8
#define rel16 OperandRel16
#define rel32 OperandRel32
#define ptr16_16 OperandPtr16_16
#define ptr16_32 OperandPtr16_32
#define sb8 OperandSByte8
#define sb16 OperandSByte16
#define sb32 OperandSByte32
#define cr0 OperandCr0
#define cr2 OperandCr2
#define cr3 OperandCr3
#define cr4 OperandCr4
#define st0 OperandST0
#define sti OperandClassReg|OperandRegFpu
#define mm OperandClassReg|OperandRegMmx
#define xmm OperandClassReg|OperandRegXmm

typedef enum INS {
	ADC,
	ADD,
	AND,
	CALL,
	CBW, CWDE, CWD, CDQ,
	CMP,
	CMPSB, CMPSW, CMPSD,
	DIV,
	IDIV,
	IMUL,
	INT3,
	JO, JNO,
	JB,
	JAE,
	JE,
	JNE,
	JBE,
	JA,
	JS,
	JNS,
	JP, JPE,
	JNP,
	JL,
	JGE,
	JLE,
	JG,
	JCXZ, JECXZ,
	JMP,
	LEA,
	MOV,
	MOVSB, MOVSW, MOVSD,
	MOVSX, MOVZX,
	MUL,
	NEG,
	NOT,
	NOP,
	OR,
	POP, PUSH,
	RET,
	SBB,
	SHL, SHLD, SHR, SHRD,
	STOSB, STOSW, STOSD,
	SUB,
	TEST,
	XOR,
	XCOORD, // psuedo instr
	XLABEL,
	INS_MAX
}INS;

PUBLIC bool_t IsXCoord(IN PINSTR ins) {

	return ins->op == XCOORD;
}

PUBLIC bool_t IsXLabel(IN PINSTR ins) {

	return ins->op == XLABEL;
}

// must match INS such that _inststr[XOR]="xor"
char* _inststr[] = {
	"ADC",
	"ADD",
	"AND",
	"CALL",
	"CBW", "CWDE", "CWD", "CDQ",
	"CMP",
	"CMPSB", "CMPSW", "CMPSD",
	"DIV",
	"IDIV",
	"IMUL",
	"INT3",
	"JO", "JNO",
	"JB",
	"JAE",
	"JE",
	"JNE",
	"JBE",
	"JA",
	"JS",
	"JNS",
	"JP", "JPE",
	"JNP",
	"JL",
	"JGE",
	"JLE",
	"JG",
	"JCXZ", "JECXZ",
	"JMP",
	"LEA",
	"MOV",
	"MOVSB", "MOVSW", "MOVSD",
	"MOVSX", "MOVZX",
	"MUL",
	"NEG",
	"NOT",
	"NOP",
	"OR",
	"POP", "PUSH",
	"RET",
	"SBB",
	"SHL", "SHLD", "SHR", "SHRD",
	"STOSB", "STOSW", "STOSD",
	"SUB",
	"TEST",
	"XOR",
	"XCOORD" // psuedo instr
};

REG _registers[] = {

	{ "al", RegAl, 0, OperandAl },
	{ "ax", RegAx, 0, OperandAx },
	{ "eax", RegEax, 0, OperandEax },
	{ "rax", RegRax, 0, OperandRax },
	{ "cl", RegCl, 1, OperandCl },
	{ "cx", RegCx, 1, OperandCx },
	{ "ecx", RegEcx, 1, OperandEcx },
	{ "rcx", RegRcx, 1, OperandRcx },
	{ "dl", RegDl, 2, OperandDl },
	{ "dx", RegDx, 2, OperandDx },
	{ "edx", RegEdx, 2, OperandEdx },
	{ "rdx", RegRdx, 2, OperandRdx },
	{ "bl", RegBl, 3, OperandBl },
	{ "bx", RegBx, 3, OperandBx },
	{ "ebx", RegEbx, 3, OperandEbx },
	{ "rbx", RegRbx, 3, OperandRbx },
	{ "ah", RegAh, 4, OperandAh },
	{ "sp", RegSp, 4, OperandSp },
	{ "esp", RegEsp, 4, OperandEsp },
	{ "rsp", RegRsp, 4, OperandRsp },
	{ "ch", RegCh, 5, OperandCh },
	{ "bp", RegBp, 5, OperandBp },
	{ "ebp", RegEbp, 5, OperandEbp },
	{ "rbp", RegRbp, 5, OperandRbp },
	{ "dh", RegDh, 6, OperandDh },
	{ "si", RegSi, 6, OperandSi },
	{ "esi", RegEsi, 6, OperandEsi },
	{ "rsi", RegRsi, 6, OperandRsi },
	{ "bh", RegBh, 7, OperandBh },
	{ "di", RegDi, 7, OperandDi },
	{ "edi", RegEdi, 7, OperandEdi },
	{ "rdi", RegRdi, 7, OperandRdi },
	{ "es", RegEs, 0, OperandEs },
	{ "cs", RegCs, 1, OperandCs },
	{ "ss", RegSs, 2, OperandSs },
	{ "ds", RegDs, 3, OperandDs },
	{ "fs", RegFs, 4, OperandFs },
	{ "gs", RegGs, 5, OperandGs },
	{ "cr0", RegCr0, 0, OperandCr0 },
	{ "cr1", RegCr1, 1, OperandCr1 },
	{ "cr2", RegCr2, 2, OperandCr2 },
	{ "cr3", RegCr3, 3, OperandCr3 },
	{ "cr4", RegCr4, 4, OperandCr4 },
	{ "cr5", RegCr5, 5, OperandCr5 },
	{ "cr6", RegCr6, 6, OperandCr6 },
	{ "cr7", RegCr7, 7, OperandCr7 },
	{ "cr8", RegCr8, 8, OperandCr8 },
	{ "cr9", RegCr9, 9, OperandCr9 },
	{ "cr10", RegCr10, 10, OperandCr10 },
	{ "cr11", RegCr11, 11, OperandCr11 },
	{ "cr12", RegCr12, 12, OperandCr12 },
	{ "cr13", RegCr13, 13, OperandCr13 },
	{ "cr14", RegCr14, 14, OperandCr14 },
	{ "cr15", RegCr15, 15, OperandCr15 },
	{ "r8l", RegR8l, 8, OperandR8l },
	{ "r9l", RegR9l, 9, OperandR9l },
	{ "r10l", RegR10l, 10, OperandR10l },
	{ "r11l", RegR11l, 11, OperandR11l },
	{ "r12l", RegR12l, 12, OperandR12l },
	{ "r13l", RegR13l, 13, OperandR13l },
	{ "r14l", RegR14l, 14, OperandR14l },
	{ "r15l", RegR15l, 15, OperandR15l },
	{ "r8w", RegR8w, 8, OperandR8w },
	{ "r9w", RegR9w, 9, OperandR9w },
	{ "r10w", RegR10w, 10, OperandR10w },
	{ "r11w", RegR11w, 11, OperandR11w },
	{ "r12w", RegR12w, 12, OperandR12w },
	{ "r13w", RegR13w, 13, OperandR13w },
	{ "r14w", RegR14w, 14, OperandR14w },
	{ "r15w", RegR15w, 15, OperandR15w },
	{ "r8d", RegR8d, 8, OperandR8d },
	{ "r9d", RegR9d, 9, OperandR9d },
	{ "r10d", RegR10d, 10, OperandR10d },
	{ "r11d", RegR11d, 11, OperandR11d },
	{ "r12d", RegR12d, 12, OperandR12d },
	{ "r13d", RegR13d, 13, OperandR13d },
	{ "r14d", RegR14d, 14, OperandR14d },
	{ "r15d", RegR15d, 15, OperandR15d },
	{ "r8", RegR8, 8, OperandR8 },
	{ "r9", RegR9, 9, OperandR9 },
	{ "r10", RegR10, 10, OperandR10 },
	{ "r11", RegR11, 11, OperandR11 },
	{ "r12", RegR12, 12, OperandR12 },
	{ "r13", RegR13, 13, OperandR13 },
	{ "r14", RegR14, 14, OperandR14 },
	{ "r15", RegR15, 15, OperandR15 },
	{ "st0", RegSt0, 0, OperandST0 },
	{ "st1", RegSt1, 1, OperandST1 },
	{ "st2", RegSt2, 2, OperandST2 },
	{ "st3", RegSt3, 3, OperandST3 },
	{ "st4", RegSt4, 4, OperandST4 },
	{ "st5", RegSt5, 5, OperandST5 },
	{ "st6", RegSt6, 6, OperandST6 },
	{ "st7", RegSt7, 7, OperandST7 },
	{ "mm0", RegMmx0, 0, OperandMmx0 },
	{ "mm1", RegMmx1, 1, OperandMmx1 },
	{ "mm2", RegMmx2, 2, OperandMmx2 },
	{ "mm3", RegMmx3, 3, OperandMmx3 },
	{ "mm4", RegMmx4, 4, OperandMmx4 },
	{ "mm5", RegMmx5, 5, OperandMmx5 },
	{ "mm6", RegMmx6, 6, OperandMmx6 },
	{ "mm7", RegMmx7, 7, OperandMmx7 },
	{ "xmm0", RegXmm0, 0, OperandXmm0 },
	{ "xmm1", RegXmm1, 1, OperandXmm1 },
	{ "xmm2", RegXmm2, 2, OperandXmm2 },
	{ "xmm3", RegXmm3, 3, OperandXmm3 },
	{ "xmm4", RegXmm4, 4, OperandXmm4 },
	{ "xmm5", RegXmm5, 5, OperandXmm5 },
	{ "xmm6", RegXmm6, 6, OperandXmm6 },
	{ "xmm7", RegXmm7, 7, OperandXmm7 },
	{ "xmm8", RegXmm8, 8, OperandXmm8 },
	{ "xmm9", RegXmm9, 9, OperandXmm9 },
	{ "xmm10", RegXmm10, 10, OperandXmm10 },
	{ "xmm11", RegXmm11, 11, OperandXmm11 },
	{ "xmm12", RegXmm12, 12, OperandXmm12 },
	{ "xmm13", RegXmm13, 13, OperandXmm13 },
	{ "xmm14", RegXmm14, 14, OperandXmm14 },
	{ "xmm15", RegXmm15, 15, OperandXmm15 },
};

// note this must match same order as REGTYPE such that
// _register[RegAx] = "ax"
char* _register[] = {
	"RegInvalid",
	"ss", "cs", "ds", "es", "fs", "gs",
	"ah", "al", "bh", "bl", "ch", "cl", "dh", "dl",
	"si", "di", "bp", "sp", "ax", "bx", "cx", "dx",
	"eax", "ebx", "ecx", "edx",
	"esi", "edi", "ebp", "esp",
	"rax", "rbx", "rcx", "rdx",
	"rsi", "rdi", "rbp", "rsp",
	"cr0", "cr1", "cr2", "cr3", "cr4", "cr5", "cr6", "cr7",
	"cr8", "cr9", "cr10", "cr11", "cr12", "cr13", "cr14", "cr15",
	"dr0", "dr1", "dr2", "dr3", "dr4", "dr5", "dr6", "dr7",
	"dr8", "dr9", "dr10", "dr11", "dr12", "dr13", "dr14", "dr15",
	"r8l", "r9l", "r10l", "r11l", "r12l", "r13l", "r14l", "r15l",
	"r8w", "r9w", "r10w", "r11w", "r12w", "r13w", "r14w", "r15w",
	"r8d", "r9d", "r10d", "r11d", "r12d", "r13d", "r14d", "r15d",
	"r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15",
	"mm0", "mm1", "mm2", "mm3", "mm4", "mm5", "mm6", "mm7",
	"xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7",
	"xmm8", "xmm9", "xmm10", "xmm11", "xmm12", "xmm13", "xmm14", "xmm15",
	"ymm0", "ymm1", "ymm2", "ymm3", "ymm4", "ymm5", "ymm6", "ymm7",
	"ymm8", "ymm9", "ymm10", "ymm11", "ymm12", "ymm13", "ymm14", "ymm15",
	"st0", "st1", "st2", "st3", "st4", "st5", "st6", "st7"
};

PRIVATE PREG GetRegister(IN REGTYPE type) {

	int k;
	int max;

	if (type == RegInvalid) return NULL;
	max = sizeof(_registers) / sizeof(REG);
	for (k = 0; k < max; k++) {
		if (_registers[k].type == type)
			return &_registers[k];
	}
	return NULL;
}

PRIVATE REGTYPE IsRegister(IN char* str) {

	int max;
	int i;

	max = sizeof(_register) / sizeof(char*);
	for (i = 0; i < max; i++) {
		if (strlen(str) != strlen(_register[i]))
			continue;
		if (_strnicmp(str, _register[i], strlen(str)) == 0)
			return (REGTYPE) i;
	}
	return RegInvalid;
}

PRIVATE PSYMBOL GetMemoryOp(IN PINSTR ins) {

	if (GetOperandClass(ins->op1) == OperandClassMem)
		return ins->src1;
	else if (GetOperandClass(ins->op2) == OperandClassMem)
		return ins->src2;
	return NULL;
}

// ----

PRIVATE void XVFNPrintInsOpMem(IN ADDCHAR addchar, IN PINSTR ins, IN PSYMBOL memsym, IN void* context) {

	char     val[32];

	VFNString(addchar, "dword ", context);

	if (memsym && memsym->symbolic && memsym->level > GLOBALS)
		VFNString(addchar, memsym->symbolic->val, context);

	VFNString(addchar, "[", context);

	if (memsym && memsym->symbolic && memsym->level <= GLOBALS)
		VFNString(addchar, memsym->symbolic->val, context);
	if (ins->addr.base)
		VFNString(addchar, ins->addr.base->name, context);
	if (ins->addr.index) {
		addchar(context, '+');
		VFNString(addchar, ins->addr.index->name, context);
	}
	if (ins->addr.scale > 1) {
		sprintf(val, "*%i", ins->addr.scale);
		VFNString(addchar, val, context);
	}
	if (ins->addr.disp) {
		sprintf(val, "%+i", ins->addr.disp);
		VFNString(addchar, val, context);
	}

	VFNString(addchar, "]", context);
}

PRIVATE void XVFNPrintInsOp(IN ADDCHAR addchar, IN PINSTR ins, IN int optype, IN PSYMBOL opsrc, IN void* context) {

	int      cls;
	REGTYPE  regty;
	PREG     reg;
	PSYMBOL  memsym;
	char     val[32];
	
	cls = GetOperandClass(optype);

	if (cls == OperandClassMem) {

		memsym = ins->addr.sym;
		XVFNPrintInsOpMem(addchar, ins, memsym, context);
	}
	else if (cls == OperandClassImm) {

		// symbolic reference
		if (opsrc && opsrc->symbolic) {
			VFNString(addchar, opsrc->symbolic->val, context);

			// NO!!! this will cause us to get MOV REG, IMM32 instead
			// of needed MOV REG, MEM32!!!

//			if (opsrc->level >= PARMS)
//				VFNString(addchar, "[ebp]", context);
		}

		// jmp <target>
		else if (ins->label)
			VFNString(addchar, ins->label->val, context);

		// immediate value:
		else{
			sprintf(val, "%i", ins->value);
			VFNString(addchar, val, context);
		}
	}
	else if (cls == OperandClassReg) {

		regty = GetOperandSubstruct(optype);
		reg = GetRegister(regty);
		VFNString(addchar, reg->name, context);

		sprintf(val, "(T%i)", opsrc->tempname);
		VFNString(addchar, val, context);
	}
}

PUBLIC void XVFNPrintIns(IN ADDCHAR addchar, IN PINSTR ins, IN void* context) {

	PSYMBOL sym;
	char*   str;
	char     val[32];


	if (IsXLabel(ins)) {

		sprintf(val, "%s:", ins->label->val);
		VFNString(addchar, val, context);
		return;
	}

	VFNString(addchar, _inststr[ins->op], context);

	if (ins->op1 != OperandInvalid) {
		VFNString(addchar, " ", context);
		XVFNPrintInsOp(addchar, ins, ins->op1, ins->src1, context);
	}

	if (ins->op2 != OperandInvalid) {
		VFNString(addchar, ", ", context);
		XVFNPrintInsOp(addchar, ins, ins->op2, ins->src2, context);
	}
}

PRIVATE TEMPLATE _instructions[] = {

	{ ADC, 0x10, X, 2, { rm8, r8 }, R },
	{ ADC, 0x11, X, 2, { rm16, r16 }, R | o16 },
	{ ADC, 0x11, X, 2, { rm32, r32 }, R | o32 },
	{ ADC, 0x12, X, 2, { r8, rm8 }, R },
	{ ADC, 0x13, X, 2, { r16, rm16 }, R | o16 },
	{ ADC, 0x13, X, 2, { r32, rm32 }, R | o32 },
	{ ADC, 0x83, 2, 2, { rm16, imm8 }, R | o16 },
	{ ADC, 0x83, 2, 2, { rm32, imm8 }, R | o32 },
	{ ADC, 0x81, 2, 2, { rm16, imm16 }, R | o16 },
	{ ADC, 0x81, 2, 2, { rm32, imm32 }, R | o32 },
	{ ADC, 0x14, X, 2, { al, imm8 }, 0 },
	{ ADC, 0x15, X, 2, { ax, imm16 }, o16 },
	{ ADC, 0x15, X, 2, { eax, imm32 }, o32 },

	{ ADD, 0, X, 2, { rm8, r8 }, R },
	{ ADD, 1, X, 2, { rm16, r16 }, R | o16 },
	{ ADD, 1, X, 2, { rm32, r32 }, R | o32 },
	{ ADD, 2, X, 2, { r8, rm8 }, R },
	{ ADD, 3, X, 2, { r16, rm16 }, R | o16 },
	{ ADD, 3, X, 2, { r32, rm32 }, R | o32 },
	{ ADD, 4, X, 2, { al, imm8 }, 0 },
	{ ADD, 5, X, 2, { ax, imm16 }, o16 },
	{ ADD, 5, X, 2, { eax, imm32 }, o32 },
	{ ADD, 0x80, 0, 2, { rm8, imm8 }, R },
	{ ADD, 0x81, 0, 2, { rm16, imm16 }, R | o16 },
	{ ADD, 0x81, 0, 2, { rm32, imm32 }, R | o32 },
	{ ADD, 0x83, 0, 2, { rm16, imm8 }, R | o16 },
	{ ADD, 0x83, 0, 2, { rm32, imm8 }, R | o32 },

	{ AND, 0x20, X, 2, { rm8, r8 }, R },
	{ AND, 0x21, X, 2, { rm16, r16 }, R | o16 },
	{ AND, 0x21, X, 2, { rm32, r32 }, R | o32 },
	{ AND, 0x22, X, 2, { r8, rm8 }, R },
	{ AND, 0x23, X, 2, { r16, rm16 }, R | o16 },
	{ AND, 0x23, X, 2, { r32, rm32 }, R | o32 },
	{ AND, 0x83, 4, 2, { rm16, imm8 }, R | o16 },
	{ AND, 0x83, 4, 2, { rm32, imm8 }, R | o32 },
	{ AND, 0x24, X, 2, { al, imm8 }, 0 },
	{ AND, 0x25, X, 2, { ax, imm16 }, o16 },
	{ AND, 0x25, X, 2, { eax, imm32 }, o32 },

	{ CALL, 0xe8, X, 1, { rel16 }, o16 },
	{ CALL, 0xe8, X, 1, { rel32 }, o32 },
	{ CALL, 0x9a, X, 1, { ptr16_16 }, o16 },
	{ CALL, 0x9a, X, 1, { ptr16_32 }, o32 },
	{ CALL, 0xff, 2, 1, { rm16 }, R | o16 },
	{ CALL, 0xff, 2, 1, { rm32 }, R | o32 },

	{ CBW, 0x98, X, 0, { 0 }, o16 },
	{ CWDE, 0x98, X, 0, { 0 }, o32 },
	{ CWD, 0x99, X, 0, { 0 }, o16 },
	{ CDQ, 0x99, X, 0, { 0 }, o32 },

	{ CMP, 0x38, X, 2, { rm8, r8 }, R },
	{ CMP, 0x39, X, 2, { rm16, r16 }, R | o16 },
	{ CMP, 0x39, X, 2, { rm32, r32 }, R | o32 },
	{ CMP, 0x3a, X, 2, { r8, rm8 }, R },
	{ CMP, 0x3b, X, 2, { r16, rm16 }, R | o16 },
	{ CMP, 0x3b, X, 2, { r32, rm32 }, R | o32 },
	{ CMP, 0x80, 7, 2, { rm8, imm8 }, R },
	{ CMP, 0x81, 7, 2, { rm16, imm16 }, R | o16 },
	{ CMP, 0x81, 7, 2, { rm32, imm32 }, R | o32 },
	{ CMP, 0x83, 7, 2, { rm16, imm8 }, R | o16 },
	{ CMP, 0x83, 7, 2, { rm32, imm8 }, R | o32 },
	{ CMP, 0x3c, X, 2, { al, imm8 }, 0 },
	{ CMP, 0x3d, X, 2, { ax, imm16 }, o16 },
	{ CMP, 0x3d, X, 2, { eax, imm32 }, o32 },

	{ CMPSB, 0xa6, X, 0, { 0 }, 0 },
	{ CMPSW, 0xa6, X, 0, { 0 }, 0 },
	{ CMPSD, 0xa6, X, 0, { 0 }, 0 },
	 
	{ DIV, 0xf6, 6, 1, { rm8 }, R },
	{ DIV, 0xf7, 6, 1, { rm16 }, R | o16 },
	{ DIV, 0xf7, 6, 1, { rm32 }, R | o32 },

	{ IDIV, 0xf6, 7, 1, { rm8 }, R },
	{ IDIV, 0xf7, 7, 1, { rm16 }, R | o16 },
	{ IDIV, 0xf7, 7, 1, { rm32 }, R | o32 },

	{ IMUL, 0xf6, 5, 1, { rm8 }, R },
	{ IMUL, 0xf7, 5, 1, { rm16 }, R | o16 },
	{ IMUL, 0xf7, 5, 1, { rm32 }, R | o32 },
	{ IMUL, 0x0faf, X, 2, { r16, rm16 }, R | o16 },
	{ IMUL, 0x0faf, X, 2, { r32, rm32 }, R | o32 },
	{ IMUL, 0x6b, X, 2, { r16, imm8 }, R | o16 },
	{ IMUL, 0x69, X, 2, { r16, imm16 }, R | o16 },
	{ IMUL, 0x6b, X, 2, { r32, imm8 }, R | o32 },
	{ IMUL, 0x69, X, 2, { r32, imm32 }, R | o32 },
	{ IMUL, 0x6b, X, 3, { r16, rm16, imm8 }, R | o16 },
	{ IMUL, 0x69, X, 3, { r16, rm16, imm16 }, R | o16 },
	{ IMUL, 0x6b, X, 3, { r32, rm32, imm8 }, R | o32 },
	{ IMUL, 0x69, X, 3, { r32, rm32, imm32 }, R | o32 },

	{ INT3, 0xcc, X, 0, { 0 }, 0 },

	{ JO, 0x70, X, 1, { rel8 }, 0 },
	{ JO, 0x0f80, X, 1, { rel16 }, o16 },
	{ JO, 0x0f80, X, 1, { rel32 }, o32 },

	{ JNO, 0x71, X, 1, { rel8 }, 0 },
	{ JNO, 0x0f81, X, 1, { rel16 }, o16 },
	{ JNO, 0x0f81, X, 1, { rel32 }, o32 },

	{ JB, 0x72, X, 1, { rel8 }, 0 },
	{ JB, 0x0f82, X, 1, { rel16 }, o16 },
	{ JB, 0x0f82, X, 1, { rel32 }, o32 },

	{ JAE, 0x73, X, 1, { rel8 }, 0 },
	{ JAE, 0x0f83, X, 1, { rel16 }, o16 },
	{ JAE, 0x0f83, X, 1, { rel32 }, o32 },

	{ JE, 0x74, X, 1, { rel8 }, 0 },
	{ JE, 0x0f84, X, 1, { rel16 }, o16 },
	{ JE, 0x0f84, X, 1, { rel32 }, o32 },

	{ JNE, 0x75, X, 1, { rel8 }, 0 },
	{ JNE, 0x0f85, X, 1, { rel16 }, o16 },
	{ JNE, 0x0f85, X, 1, { rel32 }, o32 },

	{ JBE, 0x76, X, 1, { rel8 }, 0 },
	{ JBE, 0x0f86, X, 1, { rel16 }, o16 },
	{ JBE, 0x0f86, X, 1, { rel32 }, o32 },

	{ JA, 0x77, X, 1, { rel8 }, 0 },
	{ JA, 0x0f87, X, 1, { rel16 }, o16 },
	{ JA, 0x0f87, X, 1, { rel32 }, o32 },

	{ JS, 0x78, X, 1, { rel8 }, 0 },
	{ JS, 0x0f88, X, 1, { rel16 }, o16 },
	{ JS, 0x0f88, X, 1, { rel32 }, o32 },

	{ JNS, 0x79, X, 1, { rel8 }, 0 },
	{ JNS, 0x0f89, X, 1, { rel16 }, o16 },
	{ JNS, 0x0f89, X, 1, { rel32 }, o32 },

	{ JP, 0x7a, X, 1, { rel8 }, 0 },
	{ JP, 0x0f8a, X, 1, { rel16 }, o16 },
	{ JP, 0x0f8a, X, 1, { rel32 }, o32 },

	{ JPE, 0x7a, X, 1, { rel8 }, 0 },
	{ JPE, 0x0f8a, X, 1, { rel16 }, o16 },
	{ JPE, 0x0f8a, X, 1, { rel32 }, o32 },

	{ JNP, 0x7b, X, 1, { rel8 }, 0 },
	{ JNP, 0x0f8b, X, 1, { rel16 }, o16 },
	{ JNP, 0x0f8b, X, 1, { rel32 }, o32 },

	{ JL, 0x7c, X, 1, { rel8 }, 0 },
	{ JL, 0x0f8c, X, 1, { rel16 }, o16 },
	{ JL, 0x0f8c, X, 1, { rel32 }, o32 },

	{ JGE, 0x7d, X, 1, { rel8 }, 0 },
	{ JGE, 0x0f8d, X, 1, { rel16 }, o16 },
	{ JGE, 0x0f8d, X, 1, { rel32 }, o32 },

	{ JLE, 0x7e, X, 1, { rel8 }, 0 },
	{ JLE, 0x0f8e, X, 1, { rel16 }, o16 },
	{ JLE, 0x0f8e, X, 1, { rel32 }, o32 },

	{ JG, 0x7f, X, 1, { rel8 }, 0 },
	{ JG, 0x0f8f, X, 1, { rel16 }, o16 },
	{ JG, 0x0f8f, X, 1, { rel32 }, o32 },

	{ JCXZ, 0xe3, X, 1, { rel8 }, a16 },
	{ JECXZ, 0xe3, X, 1, { rel8 }, a32 },

	{ JMP, 0xe9, X, 1, { rel16 }, o16 },
	{ JMP, 0xe9, X, 1, { rel32 }, o32 },
	{ JMP, 0xeb, X, 1, { rel8 }, 0 }, // JMP SHORT
	{ JMP, 0xea, X, 1, { ptr16_16 }, o16 },
	{ JMP, 0xea, X, 1, { ptr16_32 }, o32 },
	{ JMP, 0xff, 5, 1, { mem }, R | o16 },
	{ JMP, 0xff, 5, 1, { mem }, R | o32 },
	{ JMP, 0xff, 4, 1, { rm16 }, R | o16 },
	{ JMP, 0xff, 4, 1, { rm32 }, R | o32 },

	{ LEA, 0x8d, X, 2, { r16, mem }, R | o16 },
	{ LEA, 0x8d, X, 2, { r32, mem }, R | o32 },

	{ MOV, 0x89, X, 2, { rm32, r32 }, R | o32 },
	{ MOV, 0x89, X, 2, { rm64, r64 }, R | o64 },
	{ MOV, 0x8b, X, 2, { r32, rm32 }, R | o32 },
	{ MOV, 0xb8, X, 2, { r32, imm32 }, rcode | o32 },
	{ MOV, 0xb8, X, 2, { r64, imm64 }, rcode | o64 },
	{ MOV, 0xc7, 0, 2, { rm32, imm32 }, R | o32 },

	{ MOVSB, 0xa4, X, 0, { 0 }, 0 },
	{ MOVSW, 0xa5, X, 0, { 0 }, o16 },
	{ MOVSD, 0xa5, X, 0, { 0 }, o32 },

	{ MOVSX, 0x0fb3, X, 2, { r16, rm8 }, R | o16 },
	{ MOVSX, 0x0fbe, X, 2, { r32, rm8 }, R | o32 },
	{ MOVSX, 0x0fbf, X, 2, { r32, rm16 }, R | o32 },

	{ MOVZX, 0x0fb6, X, 2, { r16, rm8 }, R | o16 },
	{ MOVZX, 0x0fb6, X, 2, { r32, rm8 }, R | o32 },
	{ MOVZX, 0x0fb7, X, 2, { r32, rm16 }, R | o32 },

	{ MUL, 0xf6, 4, 1, { rm8 }, R },
	{ MUL, 0xf7, 4, 1, { rm16 }, R | o16 },
	{ MUL, 0xf7, 4, 1, { rm32 }, R | o32 },

	{ NEG, 0xf6, 3, 1, { rm8 }, R },
	{ NEG, 0xf7, 3, 1, { rm16 }, R | o16 },
	{ NEG, 0xf7, 3, 1, { rm32 }, R | o32 },

	{ NOT, 0xf6, 2, 1, { rm8 }, R },
	{ NOT, 0xf7, 2, 1, { rm16 }, R | o16 },
	{ NOT, 0xf7, 2, 1, { rm32 }, R | o32 },

	{ NOP, 0x90, X, 0, { 0 }, 0 },

	{ OR, 0x08, X, 2, { rm8, r8 }, R },
	{ OR, 0x09, X, 2, { rm16, r16 }, R | o16 },
	{ OR, 0x09, X, 2, { rm32, r32 }, R | o32 },
	{ OR, 0x0a, X, 2, { r8, rm8 }, R },
	{ OR, 0x0b, X, 2, { r16, rm16 }, R | o16 },
	{ OR, 0x0b, X, 2, { r32, rm32 }, R | o32 },
	{ OR, 0x80, 1, 2, { rm8, imm8 }, R },
	{ OR, 0x81, 1, 2, { rm16, imm16 }, R | o16 },
	{ OR, 0x81, 1, 2, { rm32, imm32 }, R | o32 },
	{ OR, 0x83, 1, 2, { rm16, imm8 }, R | o16 },
	{ OR, 0x83, 1, 2, { rm32, imm8 }, R | o32 },
	{ OR, 0x0c, X, 2, { al, imm8 }, 0 },
	{ OR, 0x0d, X, 2, { ax, imm16 }, 0 },
	{ OR, 0x0d, X, 2, { eax, imm32 }, 0 },

	{ POP, 0x58, X, 1, { r16 }, rcode | o16 },
	{ POP, 0x58, X, 1, { r32 }, rcode | o32 },
	{ POP, 0x8f, 0, 1, { rm16 }, R | o16 },
	{ POP, 0x8f, 0, 1, { rm32 }, R | o32 },

	{ PUSH, 0x50, X, 1, { r16 }, rcode | o16 },
	{ PUSH, 0x50, X, 1, { r32 }, rcode | o32 },
	{ PUSH, 0xff, 6, 1, { rm16 }, R | o16 },
	{ PUSH, 0xff, 6, 1, { rm32 }, R | o32 },
	{ PUSH, 0x6a, X, 1, { imm8 }, 0 },
	{ PUSH, 0x68, X, 1, { imm16 }, o16 },
	{ PUSH, 0x68, X, 1, { imm32 }, o32 },

	{ RET, 0xc3, X, 0, { 0 }, 0 },
	{ RET, 0xc2, X, 1, { imm16 }, 0 },

	{ SBB, 0x18, X, 2, { rm8, r8 }, R },
	{ SBB, 0x19, X, 2, { rm16, r16 }, R | o16 },
	{ SBB, 0x19, X, 2, { rm32, r32 }, R | o32 },

	{ SBB, 0x1a, X, 2, { r8, rm8 }, R },
	{ SBB, 0x1b, X, 2, { r16, rm16 }, R | o16 },
	{ SBB, 0x1b, X, 2, { r32, rm32 }, R | o32 },

	{ SBB, 0x80, 3, 2, { rm8, imm8 }, R },
	{ SBB, 0x81, 3, 2, { rm16, imm16 }, R | o16 },
	{ SBB, 0x81, 3, 2, { rm32, imm32 }, R | o32 },

	{ SBB, 0x83, 3, 2, { rm16, imm8 }, R | o16 },
	{ SBB, 0x83, 3, 2, { rm32, imm8 }, R | o32 },

	{ SBB, 0x1c, X, 2, { al, imm8 }, 0 },
	{ SBB, 0x1d, X, 2, { ax, imm16 }, o16 },
	{ SBB, 0x1d, X, 2, { eax, imm32 }, o32 },

	{ SHL, 0xd0, 4, 2, { rm8, 1 }, R }, // need to work on. Second operand is literal 1.
	{ SHL, 0xd2, 4, 2, { rm8, cl }, R },
	{ SHL, 0xc0, 4, 2, { rm8, imm8 }, R },
	{ SHL, 0xd1, 4, 2, { rm16, 1 }, R }, // see above. o16
	{ SHL, 0xd3, 4, 2, { rm16, cl }, R | o16 },
	{ SHL, 0xc1, 4, 2, { rm16, imm8 }, R | o16 },
	{ SHL, 0xd1, 4, 2, { rm32, 1 }, 0 }, // see above. o32
	{ SHL, 0xd3, 4, 2, { rm32, cl }, R | o32 },
	{ SHL, 0xc1, 4, 2, { rm32, imm8 }, R | o32 },

	{ SHR, 0xd0, 5, 2, { rm8, 1 }, R }, // need to work on. Second operand is literal 1.
	{ SHR, 0xd2, 5, 2, { rm8, cl }, R },
	{ SHR, 0xc0, 5, 2, { rm8, imm8 }, R },
	{ SHR, 0xd1, 5, 2, { rm16, 1 }, R | o16 }, // see above. o16
	{ SHR, 0xd3, 5, 2, { rm16, cl }, R | o16 },
	{ SHR, 0xc1, 5, 2, { rm16, imm8 }, R | o16 },
	{ SHR, 0xd1, 5, 2, { rm32, 1 }, R | o32 }, // see above. o32
	{ SHR, 0xd3, 5, 2, { rm32, cl }, R | o32 },
	{ SHR, 0xc1, 5, 2, { rm32, imm8 }, R | o32 },

	{ SHLD, 0x0fa4, X, 3, { rm16, r16, imm8 }, R | o16 },
	{ SHLD, 0x0fa4, X, 3, { rm16, r32, imm8 }, R | o32 },
	{ SHLD, 0x0fa5, X, 3, { rm16, r16, cl }, R | o16 },
	{ SHLD, 0x0fa5, X, 3, { rm16, r32, cl }, R | o32 },

	{ SHRD, 0x0fac, X, 3, { rm16, r16, imm8 }, R | o16 },
	{ SHRD, 0x0fac, X, 3, { rm16, r32, imm8 }, R | o32 },
	{ SHRD, 0x0fad, X, 3, { rm16, r16, cl }, R | o16 },
	{ SHRD, 0x0fad, X, 3, { rm16, r32, cl }, R | o32 },

	{ STOSB, 0xaa, X, 0, { 0 }, 0 },
	{ STOSW, 0xab, X, 0, { 0 }, o16 },
	{ STOSD, 0xab, X, 0, { 0 }, o32 },

	{ SUB, 0x28, X, 2, { rm8, r8 }, R },
	{ SUB, 0x29, X, 2, { rm16, r16 }, R | o16 },
	{ SUB, 0x29, X, 2, { rm32, r32 }, R | o32 },
	{ SUB, 0x2a, X, 2, { r8, rm8 }, R },
	{ SUB, 0x2a, X, 2, { r16, rm16 }, R | o16 },
	{ SUB, 0x2a, X, 2, { r32, rm32 }, R | o32 },
	{ SUB, 0x80, 5, 2, { rm8, imm8 }, R },
	{ SUB, 0x81, 5, 2, { rm16, imm16 }, R | o16 },
	{ SUB, 0x81, 5, 2, { rm32, imm32 }, R | o32 },
	{ SUB, 0x83, 5, 2, { rm16, imm8 }, R | o16 },
	{ SUB, 0x83, 5, 2, { rm32, imm8 }, R | o32 },
	{ SUB, 0x2c, X, 2, { al, imm8 }, 0 },
	{ SUB, 0x2d, X, 2, { ax, imm16 }, o16 },
	{ SUB, 0x2d, X, 2, { eax, imm32 }, o32 },

	{ TEST, 0x84, X, 2, { rm8, r8 }, R },
	{ TEST, 0x85, X, 2, { rm16, r16 }, R | o16 },
	{ TEST, 0x85, X, 2, { rm32, r32 }, R | o32 },
	{ TEST, 0xf6, 0, 2, { rm8, imm8 }, R },
	{ TEST, 0xf7, 0, 2, { rm16, imm16 }, R | o16 },
	{ TEST, 0xf7, 0, 2, { rm32, imm32 }, R | o32 },
	{ TEST, 0xa8, X, 2, { al, imm8 }, 0 },
	{ TEST, 0xa9, X, 2, { ax, imm16 }, o16 },
	{ TEST, 0xa9, X, 2, { eax, imm32 }, o32 },

	{ XOR, 0x30, X, 2, { rm8, r8 }, R },
	{ XOR, 0x31, X, 2, { rm16, r16 }, R | o16 },
	{ XOR, 0x31, X, 2, { rm32, r32 }, R | o32 },
	{ XOR, 0x32, X, 2, { r8, rm8 }, R },
	{ XOR, 0x33, X, 2, { r16, rm16 }, R | o16 },
	{ XOR, 0x33, X, 2, { r32, rm32 }, R | o32 },
	{ XOR, 0x80, 6, 2, { rm8, imm8 }, R },
	{ XOR, 0x81, 6, 2, { rm16, imm16 }, R | o16 },
	{ XOR, 0x81, 6, 2, { rm32, imm32 }, R | o32 },
	{ XOR, 0x83, 6, 2, { rm16, imm8 }, R | o16 },
	{ XOR, 0x83, 6, 2, { rm32, imm8 }, R | o32 },
	{ XOR, 0x34, X, 2, { al, imm8 }, 0 },
	{ XOR, 0x35, X, 2, { ax, imm16 }, o16 },
	{ XOR, 0x35, X, 2, { eax, imm32 }, o32 },
};

/**
*	MatchOperand
*
*	Description : Compares operand types (one from table, one given from user)
*	and compares them to see if they are compatible.
*/
PRIVATE bool_t MatchOperand(IN OPTYPE a, IN OPTYPE b) {

	OperandClass ca = GetOperandClass(a);
	OperandClass cb = GetOperandClass(b);

	OPSIZE      sa = GetOperandSize(a);
	OPSIZE      sb = GetOperandSize(b);

	if ((ca == OperandClassImm && (GetOperandSubclass(a) & OperandImmPtr) == OperandImmPtr)
		&& (cb == OperandClassImm && (GetOperandSubclass(b) & OperandImmPtr) != OperandImmPtr)) {

		return FALSE;
	}

	/* if the user requested a pointer operand, we must only match ptr16:16 or ptr16:32. */
	if (cb == OperandClassImm && (GetOperandSubclass(b) & OperandImmPtr) == OperandImmPtr) {
		if (ca == OperandClassImm && (GetOperandSubclass(a) & OperandImmPtr) == OperandImmPtr) {

			if (sa == sb)
				return TRUE;
		}
		return FALSE;
	}

	/* if both operand types are the same, its a match. */
	if (ca == OperandClassMem && cb == OperandClassMem)
		return TRUE;

	else if (ca == OperandClassImm && cb == OperandClassImm) {
		return TRUE;
	}

	/* Register operands. */
	else if (ca == OperandClassReg && cb == OperandClassReg) {

		/* Make sure the type of register matches. */
		if ((GetOperandSubclass(a) != GetOperandSubclass(b)))
			return FALSE;

		/* If the table entry field also has a specific register... */
		if (GetOperandSubstruct(a) != 0) {

			/* ...Check to make sure the register id's match. */
			if (GetOperandSubstruct(a) != GetOperandSubstruct(b))
				return FALSE;

			return TRUE;
		}

		/* If this instruction accepts a generic register, the sizes must match. */
		if (GetOperandSize(a) == GetOperandSize(b))
			return TRUE;

		/* Does not match. */
		return FALSE;
	}

	/* memory or memory/reg operand. */
	else if (ca == OperandClassMem) {

		/* memory/reg operand. */
		if ((a == OperandRm8 || a == OperandRm16 || a == OperandRm32 || a == OperandRm64)) {

			if (cb == OperandClassReg) {
				if (GetOperandSubclass(a) == GetOperandSubclass(b)) {
					if (GetOperandSize(a) == GetOperandSize(b))
						return TRUE;
				}
				return FALSE;
			}

			else if (cb == OperandClassMem)
				return TRUE;

			return FALSE;
		}

		/* memory only operand. */
		else if (cb == OperandClassMem) {
			return TRUE;
		}

		return FALSE;
	}

	/* no match. */
	else return FALSE;
}

/**
*	IsBetterMatch
*
*	Description : test if "compareWith" is a better match then "currentBest"
*	given the requested OPTYPE's. "better" is the smallest instruction
*	(in number of bytes of the resulting byte code) that still satisfies the
*	requirements of "op1", "op2", and "op3".
*/
PRIVATE bool_t IsBetterMatch(IN PTEMPLATE currentBest, IN PTEMPLATE compareWith,
	IN OPTIONAL OPTYPE op1, IN OPTIONAL OPTYPE op2, IN OPTIONAL OPTYPE op3) {

	if (!currentBest)
		return TRUE;

	/* only look at table enteries that are greater then or equal to what we want to match: */
	if (compareWith) {
		if (compareWith->numParms > 0 && GetOperandSize(compareWith->parms[0]) < GetOperandSize(op1)) return FALSE;
		if (compareWith->numParms > 1 && GetOperandSize(compareWith->parms[1]) < GetOperandSize(op2)) return FALSE;
		if (compareWith->numParms > 2 && GetOperandSize(compareWith->parms[2]) < GetOperandSize(op3)) return FALSE;
	}

	/* Use Pigeonhole Principle to find the best match. */
	if (compareWith->numParms > 0 && GetOperandSize(currentBest->parms[0]) > GetOperandSize(op1)
		&& GetOperandSize(compareWith->parms[0]) <= GetOperandSize(currentBest->parms[0])) {
		return TRUE;
	}
	if (compareWith->numParms > 0 && GetOperandSize(currentBest->parms[0]) < GetOperandSize(op1)
		&& GetOperandSize(compareWith->parms[0]) >= GetOperandSize(currentBest->parms[0])) {
		return TRUE;
	}
	if (compareWith->numParms > 1 && GetOperandSize(currentBest->parms[1]) > GetOperandSize(op2)
		&& GetOperandSize(compareWith->parms[1]) <= GetOperandSize(currentBest->parms[1])) {
		return TRUE;
	}
	if (compareWith->numParms > 1 && GetOperandSize(currentBest->parms[1]) < GetOperandSize(op2)
		&& GetOperandSize(compareWith->parms[1]) >= GetOperandSize(currentBest->parms[1])) {
		return TRUE;
	}
	if (compareWith->numParms > 2 && GetOperandSize(currentBest->parms[1]) > GetOperandSize(op2)
		&& GetOperandSize(compareWith->parms[1]) <= GetOperandSize(currentBest->parms[1])) {
		return TRUE;
	}
	if (compareWith->numParms > 2 && GetOperandSize(currentBest->parms[2]) < GetOperandSize(op3)
		&& GetOperandSize(compareWith->parms[2]) >= GetOperandSize(currentBest->parms[2])) {
		return TRUE;
	}

	/* if "compareWith" specifies register codes and "currentBest" does not. */
	if (compareWith->numParms > 0 && GetOperandClass(compareWith->parms[0]) == OperandClassReg
		&& GetOperandSubstruct(compareWith->parms[0]) != 0) {
		if (GetOperandSubstruct(compareWith->parms[0]) != GetOperandSubstruct(currentBest->parms[0]))
			return TRUE;
	}
	if (compareWith->numParms > 1 && GetOperandClass(compareWith->parms[1]) == OperandClassReg
		&& GetOperandSubstruct(compareWith->parms[1]) != 0) {
		if (GetOperandSubstruct(compareWith->parms[1]) != GetOperandSubstruct(currentBest->parms[1]))
			return TRUE;
	}

	/* keep the current best match. */
	return FALSE;
}

/**
*	MatchInstruction
*
*	Description : matches instruction that satisfies the specified operand types and operand count.
*	Example of use: MatchInstruction(ADD, 2, OperandClassMem, OperandImm32, OperandNone); 
*/
PRIVATE PTEMPLATE MatchInstruction(IN int mnemonic, IN int operandCount,
	IN OPTIONAL OPTYPE op1, IN OPTIONAL OPTYPE op2, IN OPTIONAL OPTYPE op3) {

	int       c;
	PTEMPLATE best;
	int       instructionTableSize;

	instructionTableSize = sizeof(_instructions) / sizeof(TEMPLATE);
	best = NULL;
	for (c = 0; c < instructionTableSize; c++) {

		PTEMPLATE k = &_instructions[c];

		if (best && (k->mnemonic != mnemonic))
			break;

		/* if we match a psuedo instruction, we ignore the operands. */
		if (k->mnemonic == mnemonic && k->modifier & ModPsuedo)
			return k;

		/* match real instructions: */
		if (k->mnemonic == mnemonic && k->numParms == operandCount) {

			OperandClass parm0 = k->parms[0];
			OperandClass parm1 = k->parms[1];
			OperandClass parm2 = k->parms[2];

			if (operandCount > 0 && !MatchOperand(parm0, op1)) continue;
			if (operandCount > 1 && !MatchOperand(parm1, op2)) continue;
			if (operandCount > 2 && !MatchOperand(parm2, op3)) continue;

			if (IsBetterMatch(best, k, op1, op2, op3))
				best = k;
		}
	}

	if (!best)
		Fatal("*** BUGCHECK: x86: MatchInstruction failed.");

	return best;
}

PRIVATE PINSTR InsJmp(IN int op, IN int opflags) {

	PINSTR ins;

	ins = calloc(1, sizeof(INSTR));
	ins->op = op;
	ins->opcount = 1;
	ins->op1 = opflags;
	ins->templ = MatchInstruction(op, 1, opflags, OperandInvalid, OperandInvalid);
	return ins;
}

PRIVATE PSYMBOL InsGetMemOperand(IN PINSTR ins) {

	int opflags1;
	int opflags2;

	opflags1 = 0;
	opflags2 = 0;

	if (ins->src1)
		opflags1 = GetOperandClass(ins->src1->opflags);

	if (ins->src2)
		opflags2 = GetOperandClass(ins->src2->opflags);

	if (opflags1 == OperandClassMem && opflags2 == OperandClassMem)
		Fatal("*** bugcheck: x86.c: InsGetMemOperand");

	if (opflags1 == OperandClassMem) return ins->src1;
	if (opflags2 == OperandClassMem) return ins->src2;
	return NULL;
}

PRIVATE PINSTR InsAddrMode(IN PINSTR ins) {

	PSYMBOL sym;

	sym = InsGetMemOperand(ins);
	if (!sym) return ins;

	ins->addr.sym = sym;
	if (sym->baseReg)
		ins->addr.base = GetRegister(sym->baseReg);
}

PRIVATE PINSTR Ins(IN int op, IN int opcount, IN PSYMBOL src1, IN PSYMBOL src2) {

	PINSTR ins;

	ins = calloc(1, sizeof(INSTR));
	ins->op = op;
	ins->opcount = opcount;
	ins->src1 = src1;
	ins->src2 = src2;
	InsAddrMode(ins);
	if (opcount == 2) {
		ins->templ = MatchInstruction(op, opcount, src1->opflags, src2->opflags, OperandInvalid);
		ins->op1 = src1->opflags;
		ins->op2 = src2->opflags;
	}
	else if (opcount == 1) {
		ins->templ = MatchInstruction(op, opcount, src1->opflags, OperandInvalid, OperandInvalid);
		ins->op1 = src1->opflags;
		ins->op2 = OperandInvalid;
	}
	else {
		ins->templ = MatchInstruction(op, opcount, OperandInvalid, OperandInvalid, OperandInvalid);
		ins->op1 = OperandInvalid;
		ins->op2 = OperandInvalid;
	}
	return ins;
}

PRIVATE PINSTR Ins2(IN int op, IN PSYMBOL src1, IN PSYMBOL src2) {

	return Ins(op, 2, src1, src2);
}

PRIVATE PINSTR Ins1(IN int op, IN PSYMBOL src1) {

	return Ins(op, 1, src1, NULL);
}

PRIVATE PINSTR Ins0(IN int op) {

	return Ins(op, 0, NULL, NULL);
}

PUBLIC PINSTR XNop(void) {

	return Ins0(NOP);
}

PUBLIC PINSTR XAdd(IN PSYMBOL src1, IN PSYMBOL src2) {

	return Ins2(ADD, src1, src2);
}

PUBLIC PINSTR XSub(IN PSYMBOL src1, IN PSYMBOL src2) {

	return Ins2(SUB, src1, src2);
}

// unsigned multiply:

PUBLIC PINSTR XMul(IN PSYMBOL src1) {

	return Ins1(MUL, src1);
}

// signed multiply:

PUBLIC PINSTR XIMul1(IN PSYMBOL src1) {

	return Ins1(IMUL, src1);
}

PUBLIC PINSTR XIMul2(IN PSYMBOL src1, IN PSYMBOL src2) {

	return Ins2(IMUL, src1, src2);
}

// unsigned div:

PUBLIC PINSTR XDiv(IN PSYMBOL src1) {

	return Ins1(DIV, src1);
}

// signed div:

PUBLIC PINSTR XIDiv(IN PSYMBOL src1) {

	return Ins1(IDIV, src1);
}

PUBLIC PINSTR XXor(IN PSYMBOL src1, IN PSYMBOL src2) {

	return Ins2(XOR, src1, src2);
}

PUBLIC PINSTR XAnd(IN PSYMBOL src1, IN PSYMBOL src2) {

	return Ins2(AND, src1, src2);
}

PUBLIC PINSTR XOr(IN PSYMBOL src1, IN PSYMBOL src2) {

	return Ins2(OR, src1, src2);
}

PUBLIC PINSTR XMov(IN PSYMBOL src1, IN PSYMBOL src2) {

	if (GetOperandClass(src2->opflags) == OperandMem32)
		__debugbreak();

	return Ins2(MOV, src1, src2);
}

PUBLIC PINSTR XMovInt(IN PSYMBOL src1, IN int val) {

	PINSTR ins;

	ins = calloc(1, sizeof(INSTR));
	ins->op = MOV;
	ins->opcount = 2;
	ins->src1 = src1;
	ins->op1 = src1->opflags;
	ins->op2 = OperandImm32;
	ins->templ = MatchInstruction(MOV, 2, src1->opflags, OperandImm32, OperandInvalid);
	ins->value = val;
	return ins;
}

PUBLIC PINSTR XJmpc(IN int condcode, IN PSTRING label) {

	PINSTR ins;
	int    op;

	switch (condcode) {
	case QEQ: op = JE; break;
	case QNE: op = JNE; break;
	case QLT: op = JL; break;
	case QLE: op = JLE; break;
	case QGT: op = JG; break;
	case QGE: op = JGE; break;
	default:
		Fatal("*** x86.c XJmpc invalid condition code");
		return XNop();
	};

	ins = InsJmp(op, rel16);
	ins->label = label;
//	ins->target = xblock;
	return ins;
}

PUBLIC PINSTR XJmp(IN PSTRING label) {

	PINSTR ins;

	ins = InsJmp(JMP, rel16);
	ins->label = label;
//	ins->target = xblock;
	return ins;
}

PUBLIC PINSTR XLea(IN PSYMBOL src1, IN PSYMBOL src2) {

	PINSTR ins;

	ins = Ins2(LEA, src1, src2);

	// need to check this for GLOBALS...
	// for now, assume local relative to EBP

	ins->addr.sym = src2;
	ins->addr.disp = 0;
	ins->addr.base = GetRegister(RegEbp);
	ins->addr.scale = 1;
	return ins;
}

PUBLIC PINSTR XCmp(IN PSYMBOL src1, IN PSYMBOL src2) {

	return Ins2(CMP, src1, src2);
}

PUBLIC PINSTR XCmpInt(IN PSYMBOL src1, IN int val) {

	PINSTR ins;

	ins = calloc(1, sizeof(INSTR));
	ins->op = CMP;
	ins->opcount = 2;
	ins->src1 = src1;
	ins->op1 = src1->opflags;
	ins->op2 = OperandImm32;
	ins->templ = MatchInstruction(CMP, 2, src1->opflags, OperandImm32, OperandInvalid);
	ins->value = val;
	return ins;
}

PUBLIC PINSTR XPush(IN PSYMBOL src1) {

	return Ins1(PUSH, src1);
}

PUBLIC PINSTR XPop(IN PSYMBOL src1) {

	return Ins1(POP, src1);
}

PUBLIC PINSTR XRet(void) {

	return Ins0(RET);
}

PUBLIC PINSTR XCall(IN PSYMBOL src1) {

	PINSTR ins;

	if (!src1->opflags)
		src1->opflags = OperandRel32;

	ins = Ins1(CALL, src1);

	return ins;
}

PUBLIC PINSTR XLoad(IN PSYMBOL target, IN PRVAL val, IN PRVAL index, IN int disp) {

	PINSTR  ins;

	fprintf(stderr, "\n\nXLOAD: OPFLAGS = %x\n\n", val->var->opflags);

	ins = calloc(1, sizeof(INSTR));
	ins->op = MOV;
	ins->opcount = 2;
	ins->op2 = OperandMem32;
	ins->op1 = target->opflags;
	ins->src1 = target;
	ins->addr.sym = val->var;

	if (val->var) {
		if (val->var->baseReg)
			ins->addr.base = GetRegister(val->var->baseReg);
		else if (GetOperandClass(val->var->opflags) == OperandClassReg)
			ins->addr.base = GetRegister(GetOperandSubstruct(val->var->opflags));
	}

	// MOV src1, [mySymbol + displacement]
	ins->addr.disp = disp;

	// MOV src1, [mySymbol + index]
	if (index && index->var)
		ins->addr.index = GetRegister(GetOperandSubstruct(index->var->opflags));

	// following not needed. keeping just in case for the time being.

	// MOV src1, [register] OR MOV src1, [myVar]
//	if (val->var) {
		// this isnt right. OperandClassReg cannot be set with OperandClassMem.
//		if (GetOperandClass(val->var->opflags) & OperandClassReg)
//			ins->addr.base = GetRegister(GetOperandSubstruct(val->var->opflags));
//		else {
//			ins->addr.sym = val->var;
//			if (val->var->level >= PARMS)
//				ins->addr.base = GetRegister(RegEbp); // already set
//		}
//	}

	ins->templ = MatchInstruction(MOV, 2, target->opflags, OperandMem32, OperandInvalid);

	return ins;
}

PUBLIC PINSTR XStore(IN PSYMBOL target, IN PRVAL val, IN PRVAL index, IN int disp) {

	PINSTR  ins;
	REGTYPE reg;
	REGTYPE idxReg;

	reg = GetOperandSubstruct(target->opflags);

	ins = calloc(1, sizeof(INSTR));
	ins->op = MOV;
	ins->opcount = 2;
	ins->op1 = OperandMem32;

	if (target->baseReg)
		ins->addr.base = GetRegister(target->baseReg);

	// MOV [mySymbol + displacement], src2
	ins->addr.disp = disp;

	// MOV [mySymbol + index], src2
	if (index && index->var) {
		idxReg = GetOperandSubstruct(index->var->opflags);
		ins->addr.index = GetRegister(idxReg);
	}

	// MOV [mySymbol], src2
	if (target->symbolic) {
		ins->addr.sym = target;
	}

	// MOV [register], src2
	else if (GetOperandClass(target->opflags) == OperandClassReg)
		ins->addr.base = GetRegister(reg);

	// MOV [src1], register
	if (val->var) {
		ins->src2 = val->var;
		ins->op2 = val->var->opflags;
	}

	// MOV [src1], number
	else if (IsInt(val->type)) {
		ins->value = val->intLit;
		ins->op2 = OperandImm32;
	}

	ins->templ = MatchInstruction(MOV, 2, OperandMem32, ins->op2, OperandInvalid);
	return ins;
}

PUBLIC ADDRMODE XAddr(IN REGTYPE base, IN int disp) {

	ADDRMODE addr;

	addr.base = GetRegister(base);
	addr.index = NULL;
	addr.disp = disp;
	addr.scale = 1;
	return addr;
}

PUBLIC PINSTR XCoord(IN COORD coord) {

	PINSTR ins;

	ins = calloc(1, sizeof(INSTR));
	ins->op = XCOORD;
	ins->opcount = 0;
	ins->modifier = ModPsuedo;
	ins->coord = coord;

	return ins;
}

extern PUBLIC PINSTR   XLabel(IN PSTRING label) {

	PINSTR ins;

	ins = calloc(1, sizeof(INSTR));
	ins->op = XLABEL;
	ins->opcount = 0;
	ins->modifier = ModPsuedo;
	ins->label = label;

	return ins;
}
