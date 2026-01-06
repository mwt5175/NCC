/************************************************************************
*
*	colorPass.c - Symbol coloring
*
*   Copyright (c) BrokenThorn Entertainment Co. All Rights Reserved.
*
************************************************************************/

#include "ncc.h"
#include <stdlib.h>

PRIVATE void Precolor(IN PSYMBOL func) {

	PBLOCK       block;
	PQUAD_LIST   quadEntry;
	PSYMBOL_LIST def;
	PSYMBOL_LIST use;

	for (block = func->codePreorder; block; block = block->preordnext) {
		for (quadEntry = block->quads; quadEntry; quadEntry = quadEntry->next) {

			// XOR EDX, EDX
			// MOV TARGET, EAX;
			// EDX:EAX = EAX * SRC2
			if (OPCODE(quadEntry->me->op) == QMUL) {

				quadEntry->me->target->opflags = OperandEax;
				quadEntry->me->target->opflags2 = OperandEdx;
				quadEntry->me->src1->var->opflags = OperandEax;

				FPrintf(stderr, "Precolor ... T%i\n", quadEntry->me->target->tempname);
				FPrintf(stderr, "Precolor ... T%i\n", quadEntry->me->src1->var->tempname);
			};

			// XOR EDX, EDX; MOV TARGET, EAX; EDX:EAX = EAX / SRC2
			if (OPCODE(quadEntry->me->op) == QDIV) {

				quadEntry->me->target->opflags = OperandEax;
				quadEntry->me->target->opflags2 = OperandEdx;
				quadEntry->me->src1->var->opflags = OperandEax;

				FPrintf(stderr, "Precolor ... T%i\n", quadEntry->me->target->tempname);
				FPrintf(stderr, "Precolor ... T%i\n", quadEntry->me->src1->var->tempname);
			}

			// MOV TARGET, SRC1; TARGET << ECX
			if (OPCODE(quadEntry->me->op) == QBSHL) {
				quadEntry->me->src2->var->opflags = OperandEcx;
				FPrintf(stderr, "Precolor ... T%i\n", quadEntry->me->src2->var->tempname);
			}
			if (OPCODE(quadEntry->me->op) == QBSHR) {
				quadEntry->me->src2->var->opflags = OperandEcx;
				FPrintf(stderr, "Precolor ... T%i\n", quadEntry->me->src2->var->tempname);
			}
		}
	}
}

PRIVATE PSYMBOL_LIST BuildInterfGraph(IN PSYMBOL func) {

	PBLOCK       block;
	PQUAD_LIST   quadEntry;
	PSYMBOL_LIST def;
	PSYMBOL_LIST use;
	PSYMBOL_LIST nodes;

	nodes = NULL;

//	for (block = func->codePreorder; block; block = block->preordnext) {
	for (block = func->code; block; block = block->next) {
		for (quadEntry = block->quads; quadEntry; quadEntry = quadEntry->next) {

			// "def" interferes with "use" if "use" is in "outlist".
			for (def = quadEntry->me->deflist; def; def = def->next) {

				if (def->me->memory) continue;

				SymbolListAdd(&nodes, def->me);

				for (use = quadEntry->me->uselist; use; use = use->next) {

					// "memory" symbols are excluded as they aren't stored in regs:
					if (use->me->memory) continue;

					// add bidirectional edges:
					SymbolListAdd(&use->me->interf, def->me);
					SymbolListAdd(&def->me->interf, use->me);
				}

				for (use = quadEntry->me->outlist; use; use = use->next) {

					// "def" is always in its own "out" list so avoid
					// adding ourself:
					if (def->me == use->me) continue;

					// "memory" symbols are excluded as they aren't stored in regs:
					if (use->me->memory)
						continue;

					// add bidirectional edges:
					SymbolListAdd(&use->me->interf, def->me);
					SymbolListAdd(&def->me->interf, use->me);
				}
			}
		}
	}
	return nodes;
}

PRIVATE unsigned SymbolListSize(IN PSYMBOL_LIST list) {

	PSYMBOL_LIST entry;
	unsigned     count;

	count = 0;
	for (entry = list; entry; entry = entry->next)
		count++;
	return count;
}

PRIVATE int CompareSymbol(IN PSYMBOL* a, IN PSYMBOL* b) {

	return SymbolListSize((*a)->interf) - SymbolListSize((*b)->interf);
}

// add t2, t1  ; t2 spilled to rbp-16

// spilled code:

// mov t99, [rbp-16]
// add t99, t1
// mov [rbp-16], t99

// mark spilled variables as "spilled" so they will not be selected
// again for spilling.

// compute liveness again but can be done incrementally.

// symbols that cant be colored are added to "spill list".

// live range splitting


// https://courses.cs.cornell.edu/cs4120/2022sp/notes.html?id=regalloc


PRIVATE OPTYPE _REG[] = {
	OperandEax, OperandEbx, OperandEcx,
	OperandEdx, OperandEsi, OperandEdi
};
#define R_INT_MAX 6

PUBLIC bool_t Interferes(IN PSYMBOL var, IN OPTYPE regty) {

	PSYMBOL_LIST entry;

	for (entry = var->interf; entry; entry = entry->next) {
		if (entry->me->opflags == regty)
			return TRUE;
		else if (entry->me->opflags2 == regty)
			return TRUE;
	}
	return FALSE;
}

PUBLIC void ColorSymbol(IN PSYMBOL var) {

	unsigned     idx;
	OPTYPE       regty;

	FPrintf(stderr, "ColorSymbol ... T%i\n", var->tempname);

	for (idx = 0; idx < R_INT_MAX; idx++) {
		regty = _REG[idx];
		if (Interferes(var, regty))
			continue;
		var->opflags = regty;
		return;
	}

	// need to spill

	fprintf(stderr, "*** ColorSymbol: warn: out of regs");
	__debugbreak();
}

PUBLIC void ColorPass(IN PSYMBOL func) {

	PSYMBOL_LIST nodes;
	PSYMBOL_LIST entry;
	PSYMBOL*     stack;
	PSYMBOL      top;
	unsigned     count;
	unsigned     idx;

	count = 0;
	idx = 0;

	// Precolor and build Register Interference Graph

	nodes = BuildInterfGraph(func);
	Precolor(func);

	// Build stack of nodes arranged by # of nodes they interfere with:

	for (entry = nodes; entry; entry = entry->next)
		count++;
	stack = malloc(sizeof (PSYMBOL)* count);
	for (entry = nodes; entry; entry = entry->next)
		stack[idx++] = entry->me;
	qsort(stack, count, sizeof(PSYMBOL), CompareSymbol);

	if (idx == 0)
		return;

	// Walk the stack and color the nodes:

	for (idx=idx-1; idx; idx--) {
		top = stack[idx];
		if (top->opflags == OperandInvalid)
			ColorSymbol(top);
	}
	top = stack[0];
	ColorSymbol(top);

	func->interf = nodes;
}

PUBLIC void DumpInterfGraph(IN FILE* out, IN PSYMBOL func) {

	PSYMBOL_LIST entry;
	PSYMBOL_LIST interf;
	PSYMBOL_LIST nodes;

	nodes = func->interf;

	FPrintf(stderr, "\n");
	for (entry = nodes; entry; entry = entry->next) {

		if (!entry->me->tempname)
			__debugbreak();

		FPrintf(stderr, "\nT%i . . . ", entry->me->tempname);
		for (interf = entry->me->interf; interf; interf = interf->next) {
			if (interf->me->symbolic)
				FPrintf(stderr, "%A, ", interf->me->symbolic);
			else
				FPrintf(stderr, "T%i, ", interf->me->tempname);
		}
	}
	FPrintf(stderr, "\n\n");
	for (entry = nodes; entry; entry = entry->next) {
		FPrintf(stderr, "\nT%i . . . ", entry->me->tempname);
		if (entry->me->opflags == OperandEax)
			FPrintf(stderr, "EAX");
		if (entry->me->opflags == OperandEbx)
			FPrintf(stderr, "EBX");
		if (entry->me->opflags == OperandEcx)
			FPrintf(stderr, "ECX");
		if (entry->me->opflags == OperandEdx)
			FPrintf(stderr, "EDX");
		if (entry->me->opflags == OperandEsi)
			FPrintf(stderr, "ESI");
		if (entry->me->opflags == OperandEdi)
			FPrintf(stderr, "EDI");
	}
}
