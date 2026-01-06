/************************************************************************
*
*	X86gen.c - Code generator
*
*   Copyright (c) BrokenThorn Entertainment Co. All Rights Reserved.
*
************************************************************************/

#include "ncc.h"
#include <stdlib.h>

#define MAX_BUCKETS     32
#define STACK_ITEM_SIZE 4

PRIVATE PMAP         _labelToXBlock;
PRIVATE unsigned     _blockCount;
PRIVATE PINSTR       _curxcode;
PRIVATE PSYMBOL      _symbol;

PRIVATE void Add(IN PINSTR ins) {

	if (!_curxcode) {
		_symbol->xcode = ins;
		_curxcode = ins;
		return;
	}
	_curxcode->next = ins;
	ins->prev = _curxcode;
	_curxcode = ins;
}

PRIVATE int Align(IN int n, IN int align) {

	return (n + align - 1) / align * align;
}

PRIVATE void AssignStackOffsets(IN PSYMBOL sym) {

	PSYMBOL item;
	int     top;
	int     bottom;

	//
	// +-------------+
	// | parameters  | EBP+8
	// | return addr |
	// | EBP         |
	// |-------------| EBP
	// | locals      |
	// +-------------+ EBP-k
	//
	top = 8;
	bottom = 0;

	for (item = sym->localsListHead; item && item->level == PARMS; item = item->nextLocal) {
		top = Align(top, STACK_ITEM_SIZE);
		item->offset = top;
		top += item->type->size;
		item->memory = TRUE;
		item->opflags = OperandMem32; //32 bit ptr
		item->baseReg = RegEbp;
	}

	for (; item; item = item->nextLocal) {
		if (item->sclass == SC_STATIC)
			continue;
		bottom += item->type->size;
		bottom = Align(bottom, STACK_ITEM_SIZE);
		item->offset = -bottom;
		item->memory = TRUE;
		item->opflags = OperandMem32; //32 bit ptr
		item->baseReg = RegEbp;
	}

	// this can be extended to push all TEMP's that spill.
	// also STRUCT and UNION types need to be here as well.

	sym->stacksize = Align(bottom, STACK_ITEM_SIZE);
}

PRIVATE void DoQuad(IN PQUAD quad) {

	PSYMBOL target;
	PINSTR  ins;
	PRVAL   src1;
	PRVAL   src2;
	PSTRING label;

	target = quad->target;
	src1 = quad->src1;
	src2 = quad->src2;

	switch (OPCODE(quad->op)) {

	// coord psuedo-instruction:

	case QCOORD:

		Add(XCoord(quad->coord));
		break;

	// store / load:

	case QADDRG:
		break;
	case QADDRL:
	case QADDRF:
		Add(XLea(target, src1->var));
		break;

//		mov    ax, word[ebx]

		// MOV   OperandReg16, OperandMem

//		{ MOV, 0x89, X, 2, { rm32, r32 }, R | o32 },
//		{ MOV, 0x89, X, 2, { rm64, r64 }, R | o64 },
//		{ MOV, 0x8b, X, 2, { r32, rm32 }, R | o32 },
//		{ MOV, 0xb8, X, 2, { r32, imm32 }, rcode | o32 },
//		{ MOV, 0xb8, X, 2, { r64, imm64 }, rcode | o64 },
//		{ MOV, 0xc7, 0, 2, { rm32, imm32 }, R | o32 },


	case QASGN:

		Add(XStore(target, src1, quad->index, 0));
		break;

	case QINDIR:

		Add(XLoad(target, src1, quad->index, 0));
		break;

	case QMOV:

		if (src1->var == NULL && IsInt(src1->type))
			Add(XMovInt(target, src1->intLit));
		else
			Add(XMov(target, src1->var));
		break;

	// expressions:

	// target = a + b
//		MOV eax, 1
//		MOV eax, ebx
//		ADD eax, eax
	case QADD:
//		if (target->baseReg != src1->var->baseReg)
			Add(XMov(target, src1->var));
		Add(XAdd(target, src2->var));
		break;
	case QSUB:
		Add(XMov(target, src1->var));
		Add(XSub(target, src2->var));
		break;
	case QNOT:
		// target = -1 * src1
		break;
	case QMUL:
		Add(XMov(target, src1->var));
		Add(XMul(src2->var));
		break;
	case QDIV:
		break;
	case QMOD:
		break;
	case QNEG:
		break;
	case QBSHL:
		break;
	case QBSHR:
		break;
	case QBAND:
		Add(XAnd(target, src1->var));
		break;
	case QBOR:
		Add(XOr(target, src1->var));
		break;
	case QBXOR:
		Add(XXor(target, src1->var));
		break;
	case QBCMPL:
//		Add(XNot(target, src1));
		break;
	case QCALL:
		Add(XCall(target));
		break;
	case QARG:
		Add(XPush(target));
		break;

	// convert:

	case QCVTI:
		break;
	case QCVTF:
		break;
	case QCVTU:
		break;
	case QCVTP:
		break;

	// control flow:

	// perhaps instead of using PSYMBOL in X-Code functions,
	// use PRVAL instead? This way we can just pass "src1"
	// instead of "src1->var" and X-Code to detect if INT
	// or VAR.

	case QJMP:

		label = quad->label->src1->stringlit;

		if (quad->condCode > 0) {
			if (src2->var)
				Add(XCmpInt(src1->var, src2->var));
			else
				Add(XCmpInt(src1->var, src2->intLit));
			ins = XJmpc(quad->condCode, label);
			Add(ins);
		}
		else{
			ins = XJmp(label);
			Add(ins);
		}
		break;

	case QLABEL:

		Add(XLabel(quad->src1->stringlit));
		break;

	case QRET:
//		Add(XJmp(NewStringA("<label>")));  // JMP to eprologue
		break;
	case QLBRACE:
	case QRBRACE:
		break;
	default:
		__debugbreak();
	};
}

PRIVATE void DoBlock(IN PBLOCK block) {

	PQUAD_LIST quadEntry;

	for (quadEntry = block->quads; quadEntry; quadEntry = quadEntry->next)
		DoQuad(quadEntry->me);
}

PRIVATE PSYMBOL _EBP;
PRIVATE PSYMBOL _ESP;
PRIVATE PSYMBOL _EAX;
PRIVATE PSYMBOL _EBX;

PRIVATE PSYMBOL ImmSym(IN int opflag, IN int val) {

	PSYMBOL reg;

	reg = calloc(1, sizeof(SYMBOL));
	reg->type = IntType;
	reg->opflags = opflag;
	reg->val.i = val;
	return reg;
}

PRIVATE PSYMBOL InitRegSymbol(IN int opflag) {

	return ImmSym(opflag, 0);
}

PRIVATE void InitRegSymbols(void) {

	_EBP = InitRegSymbol(OperandEbp);
	_ESP = InitRegSymbol(OperandEsp);
	_EAX = InitRegSymbol(OperandEax);
	_EBX = InitRegSymbol(OperandEbx);
}

PRIVATE PBLOCK_LIST _blocks;
PRIVATE PBLOCK_LIST _visited;

PRIVATE PBLOCK_LIST GetLastBlockDfs(void) {

	PBLOCK_LIST item;

	for (item = _blocks; item->next; item = item->next)
		;
	return item;
}

PRIVATE bool_t Visited(IN PBLOCK block) {

	PBLOCK_LIST item;

	item = BlockListFind(_visited, block);
	if (item) return TRUE;
	return FALSE;
}

// reverse post ordering of blocks that prioritize "else" branch

PRIVATE void BlockDfs(IN PBLOCK block) {

	BlockListAdd(&_visited, block);
	if (block->outCond && !Visited(block->outCond))
		BlockDfs(block->outCond);
	if (block->outDirect && !Visited(block->outDirect))
		BlockDfs(block->outDirect);
	BlockListAdd(&_blocks, block);
}

PRIVATE void DoFunct(IN PSYMBOL sym) {

	PBLOCK      block;
	PBLOCK_LIST item;

	_blockCount = 0;

//	BlockDfs(sym->code);

	Add(XPush(_EBP));
	Add(XMov(_EBP, _ESP));
	Add(XSub(_ESP, ImmSym(OperandImm32, 4)));

//	for (item = GetLastBlockDfs(); item; item = item->prev) {
//		block = item->me;
//		DoBlock(block);
//	}

	for (block = sym->code; block; block = block->next)
 		DoBlock(block);


//	for (block = sym->codePreorder; block; block = block->preordnext)
// 		DoBlock(block);

//	AddBlock(eprologue);
	Add(XMov(_ESP, _EBP));
	Add(XPop(_EBP));
	Add(XMov(_EAX, ImmSym(OperandImm32, 0))); // RETURN Value
	Add(XRet());
}

// pg 7 https://citeseerx.ist.psu.edu/document?repid=rep1&type=pdf&doi=347e83b8e0b895f463e6d64d3e258f67aae77527

// FIRST STEP.... try to remove XBLOCK so its just INSTR. QBLOCK labels
// should be corrected in BuildPreOrderList

// SECOND STEP... double linked INSTR list is perfect. Use LABEL
// and other psuedo instructions as needed. Don't worry about JMP's.

// PEEPHOLE OPTIMIZE... Every time we "Add" just compare with a list of
// rules, looking at each instruction going backward. See above link


extern PUBLIC void EmitListing(IN PSYMBOL sym);


PRIVATE void Emit(IN PSYMBOL sym) {

	// AddSymbolCOFF

	_symbol = sym;
	_curxcode = sym->xcode;

	if (IsFunction(sym->type) && sym->code) {

		AssignStackOffsets(sym);
		DoFunct(sym);
//		RenameXBlocks();
//		sym->xcode = _listXBlock;
	}
	else {

		GenerateData(sym);
	}

	EmitListing(sym);
}

PRIVATE void Init(void) {

	InitRegSymbols();


#if 0
	Out1("bits 32");
	Out1("section .text");
	OutType(IntType);
	OutType(CharType);
	OutType(WideCharType);
	OutType(SignedCharType);
	OutType(LongType);
	OutType(LongLongType);
	OutType(ShortType);
	OutType(UnsignedCharType);
	OutType(UnsignedLongType);
	OutType(UnsignedLongLongType);
	OutType(UnsignedShortType);
	OutType(UnsignedIntType);
	OutType(FloatType);
	OutType(DoubleType);
	OutType(LongDoubleType);
	OutType(VoidType);
#endif
}

TARGET x86IR = {
	1, 1, 0,  /* char */
	2, 2, 0,  /* short */
	4, 4, 0,  /* int */
	4, 4, 0,  /* long */
	4, 4, 0,  /* long long */
	4, 4, 1,  /* float */
	8, 4, 1,  /* double */
	8, 4, 1,  /* long double */
	4, 4, 0,  /* T * */
	0, 1, 0,   /* struct */
	Init,
	NULL, // close
	Emit,
	NULL, // local
	NULL, // import
	NULL // export
};
