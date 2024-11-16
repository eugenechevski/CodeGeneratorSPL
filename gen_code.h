#ifndef _GEN_code_seq_H
#define _GEN_code_seq_H
#include <stdio.h>
#include "ast.h"
#include "bof.h"
#include "instruction.h"
#include "code_seq.h"

// Initialize the code_seq generator
extern void gen_code_seq_initialize();

// Requires: bf if open for writing in binary
// Generate code_seq for the given AST
extern void gen_code_seq_program(BOFFILE bf, block_t prog);

// Requires: bf if open for writing in binary
// Generate code_seq for prog into bf
extern void gen_code_seq_program(BOFFILE bf, block_t prog);

// Generate code_seq for the var_decls_t vds to out
// There are 2 instructions generated for each identifier declared
// (one to allocate space and another to initialize that space)
extern code_seq gen_code_seq_var_decls(var_decls_t vds);

// Generate code_seq for a single <var-decl>, vd,
// There are 2 instructions generated for each identifier declared
// (one to allocate space and another to initialize that space)
extern code_seq gen_code_seq_var_decl(var_decl_t vd);

// Generate code_seq for the identifiers in idents with type t
// in reverse order (so the first declared are allocated last).
// There are 2 instructions generated for each identifier declared
// (one to allocate space and another to initialize that space)
extern code_seq gen_code_seq_idents(ident_list_t idents);

// Generate code_seq for stmt
extern code_seq gen_code_seq_stmt(stmt_t stmt);

// Generate code_seq for stmt
extern code_seq gen_code_seq_assign_stmt(assign_stmt_t stmt);

// Generate code_seq for the list of statements given by stmts to out
extern code_seq gen_code_seq_stmts(stmts_t stmts);

// Generate code_seq for the if-statement given by stmt
extern code_seq gen_code_seq_if_stmt(if_stmt_t stmt);

// Generate code_seq for the read statement given by stmt
extern code_seq gen_code_seq_read_stmt(read_stmt_t stmt);

// Generate code_seq for the write statement given by stmt.
extern code_seq gen_code_seq_print_stmt(print_stmt_t stmt);

// Generate code_seq for the expression exp
// putting the result on top of the stack,
// and using V0 and AT as temporary registers
// May also modify SP, HI,LO when executed
extern code_seq gen_code_seq_expr(expr_t exp);

// Generate code_seq for the expression exp
// putting the result on top of the stack,
// and using V0 and AT as temporary registers
// May also modify SP, HI,LO when executed
extern code_seq gen_code_seq_binary_op_expr(binary_op_expr_t exp);

// Generate code_seq to apply the floating-point arith_op to the
// 2nd from top and top of the stack,
// putting the result on top of the stack in their place,
// and using V0 and AT as temporary registers
// Also modifies SP when executed
extern code_seq gen_code_seq_arith_op(token_t arith_op);

// Generate code_seq for the rel_op
// applied to 2nd from top and top of the stack,
// putting the result on top of the stack in their place,
// and using V0 and AT as temporary registers
// Also modifies SP when executed
extern code_seq gen_code_seq_rel_op(token_t rel_op);

// Generate code_seq to put the value of the given identifier
// on top of the stack
// Modifies T9, V0, and SP when executed
extern code_seq gen_code_seq_ident(ident_t id);

// Generate code_seq to put the given number on top of the stack
// Modifies V0 when executed
extern code_seq gen_code_seq_number(number_t num);

// Generate code_seq for the expression exp
// putting the result on top of the stack,
// and using V0 and AT as temporary registers
// May also modify SP, HI,LO when executed
extern code_seq gen_code_seq_logical_not_expr(expr_t exp);

#endif