#ifndef _GEN_code_seq_H
#define _GEN_code_seq_H
#include <stdio.h>
#include <limits.h>
#include <string.h>
#include "spl.tab.h"
#include "ast.h"
#include "code.h"
#include "code_seq.h"
#include "code_utils.h"
#include "id_use.h"
#include "literal_table.h"
#include "gen_code.h"
#include "utilities.h"
#include "regname.h"
#include "ast.h"
#include "bof.h"
#include "instruction.h"

// Initialize the code_seq generator
extern void gen_code_seq_initialize();

// Requires: bf if open for writing in binary
// Generate code_seq for the given AST
extern void gen_code_output_seq(BOFFILE bf, code_seq cs);

// Requires: bf if open for writing in binary
// Write the program's BOFFILE to bf
extern BOFHeader gen_code_program_header(code_seq main_cs);

// Requires: bf is open for writing in binary
// Write literals to the BOF file
extern void gen_code_output_literals(BOFFILE bf);

// Requires: bf if open for writing in binary
// Generate code_seq for the given AST
extern void gen_code_output_program(BOFFILE bf, code_seq main_cs);

// Requires: bf is open for writing in binary
// Generate code for the entire program
extern void gen_code_program(BOFFILE bf, block_t prog);

// Generate code_seq for the var_decls_t vds to out
extern code_seq gen_code_var_decls(var_decls_t vds);

// Generate code_seq for a single <var-decl>, vd,
extern code_seq gen_code_var_decl(var_decl_t vd);

// Generate code_seq for the identifiers in idents with type t
extern code_seq gen_code_var_idents(ident_list_t idents);

// Generate code for constant declarations
extern code_seq gen_code_const_decls(const_decls_t cds);

// Generate code for a single constant declaration
extern code_seq gen_code_const_decl(const_decl_t cd);

// Generate code for a list of constant definitions
extern code_seq gen_code_const_def_list(const_def_list_t cdl);

// Generate code for a single constant definition
extern code_seq gen_code_const_def(const_def_t cd);

// Generate code for the list of statments given by stmts
extern code_seq gen_code_stmts(stmts_t stmts);

// Generate code for stmt
extern code_seq gen_code_stmt(stmt_t stmt); // Generate code for stmt

// Generate code for an assignment statement
extern code_seq gen_code_assign_stmt(assign_stmt_t stmt);

// Generate code for the if-statement
extern code_seq gen_code_if_stmt(if_stmt_t stmt);

// Generate code for the condition
extern code_seq gen_code_condition(condition_t cond);

// Generate code for a relational condition
extern code_seq gen_code_rel_op_condition(rel_op_condition_t cond);

// Generate code for a divisible condition
extern code_seq gen_code_db_condition(db_condition_t cond);

// Generate code for a while statement
extern code_seq gen_code_while_stmt(while_stmt_t stmt);

// Generate code for the read statement
extern code_seq gen_code_read_stmt(read_stmt_t stmt);

// Generate code for a print statement
extern code_seq gen_code_print_stmt(print_stmt_t stmt);

// Generate code for the block statement given by stmt
extern code_seq gen_code_block_stmt(block_stmt_t stmt);

// Generate code for the expression exp
// putting the result on top of the stack,
extern code_seq gen_code_expr(expr_t exp);

// Generate code for the expression exp
// putting the result on top of the stack,
extern code_seq gen_code_binary_op_expr(binary_op_expr_t exp);

// Generate code to apply op to the
// 2nd from top and top of the stack,
// putting the result on top of the stack in their place,
extern code_seq gen_code_op(token_t op);

// Generate code to apply the arithmetic operation to the top two stack elements
extern code_seq gen_code_arith_op(token_t arith_op);

// Generate code for a relational operator
extern code_seq gen_code_rel_op(token_t rel_op);

// Generate code to put the value of the given identifier
// on top of the stack
extern code_seq gen_code_ident(ident_t id);

// Generate code to put the given number on top of the stack
extern code_seq gen_code_number(number_t num);

// Generate code for the expression exp
// putting the result on top of the stack,
extern code_seq gen_code_negated(negated_expr_t exp);

#endif