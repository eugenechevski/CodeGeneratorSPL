#include "gen_code.h"

#define STACK_SPACE 4096

// Initialize the code generator
void gen_code_initialize()
{
    literal_table_initialize();
}

// Write a sequence of instructions to the BOF file
void gen_code_output_seq(BOFFILE bf, code_seq cs) {
    while (!code_seq_is_empty(cs)) {
        bin_instr_t inst = code_seq_first(cs)->instr;
        instruction_write_bin_instr(bf, inst);
        cs = code_seq_rest(cs);
    }
}

// Generate the BOF header
BOFHeader gen_code_program_header(code_seq main_cs) {
    BOFHeader ret;

    strncpy(ret.magic, "BO32", 4); // SPL magic number
    ret.text_start_address = 0;
    ret.text_length = code_seq_size(main_cs) * BYTES_PER_WORD;
    ret.data_start_address = MAX(ret.text_length, 1024) + BYTES_PER_WORD;
    ret.data_length = literal_table_size() * BYTES_PER_WORD;
    ret.stack_bottom_addr = ret.data_start_address + ret.data_length + STACK_SPACE;

    return ret;
}

// Write literals to the BOF file
void gen_code_output_literals(BOFFILE bf) {
    literal_table_start_iteration();
    while (literal_table_iteration_has_next()) {
        word_type w = literal_table_iteration_next();
        bof_write_word(bf, w);
    }
    literal_table_end_iteration();
}

// Requires: bf is open for writing in binary
// Write the program's BOFFILE to bf
void gen_code_output_program(BOFFILE bf, code_seq main_cs)
{
    BOFHeader bfh = gen_code_program_header(main_cs);
    bof_write_header(bf, bfh);
    gen_code_output_seq(bf, main_cs);
    gen_code_output_literals(bf);
    bof_close(bf);
}

// Generate code for the entire program
void gen_code_program(BOFFILE bf, block_t prog) {
    code_seq main_cs = code_utils_set_up_program();

    // Handle variable declarations
    code_seq var_decls_cs = gen_code_var_decls(prog.var_decls);
    code_seq_concat(&main_cs, var_decls_cs);

    // Handle constant declarations
    code_seq const_decls_cs = gen_code_const_decls(prog.const_decls);
    code_seq_concat(&main_cs, const_decls_cs);

    // Generate code for statements
    code_seq stmts_cs = gen_code_stmts(prog.stmts);
    code_seq_concat(&main_cs, stmts_cs);

   /*  // Deallocate stack space for variables
    int total_stack_space = calculate_total_stack_space(prog.var_decls);
    code_seq dealloc_cs = code_utils_deallocate_stack_space(total_stack_space);
    code_seq_concat(&main_cs, dealloc_cs); */

    // Tear down activation record and add exit instruction
    code_seq tear_down_cs = code_utils_tear_down_program();
    code_seq_concat(&main_cs, tear_down_cs);
    code_seq_add_to_end(&main_cs, code_exit(0));

    gen_code_output_program(bf, main_cs);
}
/* 
int calculate_total_stack_space(var_decls_t var_decls)
{
    int total_space = 0;
    var_decl_t *vdp = var_decls.var_decls;
    while (vdp != NULL)
    {
        // Assuming each identifier takes up one word
        int num_idents = count_idents(vdp->ident_list);
        total_space += num_idents * BYTES_PER_WORD;
        vdp = vdp->next;
    }
    return total_space;
}

int count_idents(ident_list_t idents)
{
    int count = 0;
    ident_t *idp = idents.start;
    while (idp != NULL)
    {
        count++;
        idp = idp->next;
    }
    return count;
} */

// Generate code for variable declarations
code_seq gen_code_var_decls(var_decls_t vds) {
    code_seq ret = code_seq_empty();
    var_decl_t *var_decl = vds.var_decls;
    while (var_decl != NULL) {
        code_seq var_decl_cs = gen_code_var_decl(*var_decl);
        code_seq_concat(&ret, var_decl_cs);
        var_decl = var_decl->next;
    }
    return ret;
}

// Generate code for a single variable declaration
code_seq gen_code_var_decl(var_decl_t vd) {
    return gen_code_var_idents(vd.ident_list);
}

// Generate code for variable identifiers
code_seq gen_code_var_idents(ident_list_t idents) {
    /*
        Layout 2:
                                   offset
         [      ...               ]
         [ local variables        ]
         [      ...               ]
   FP -->[ local constants        ] 0
         [  saved     SP          ]-1
         [  registers FP          ]-2
         [            link ]-3
         [            RA          ]-4
         [ temporary storage      ]
   SP -->[      ...               ]

        How to initialize variables?
        [allocate a word on the stack]
        [zero out that word on the stack]
            code_lit(SP, 0, 0)
        SRI $sp, 1
        LIT $sp, 0, 0


         TRANSLATION SCHEME FOR VARIABLE NAMES
                  (AND CONSTANTS)

         want to use CPW instruction to get the value
            and put it on the stack

         need the lexical address for finding the value
            instructions in the SSM all use a base register
              and an offset

         I used $r3 for this

         suppose the lexical address of the variable, x, is
            (levelsOut, ofst)

           # The following is done by code_utils_compute_fp
           # load the FP for x's stack frame into $r3
           [load FP into $r3]  # this is for current AR
           [load the next link into $r3]  }
              ...                                } levelsOut times
           [load the next link into $r3]  }
           # $r3 is the base of x's AR
           [push x's value using $r3 as base onto stack]
                CPW $sp, 0, $r3, ofst
    */
    code_seq ret = code_seq_empty();
    ident_t *ident = idents.start;

    // Instructions are appended in reverse order
    // so that the first declared identifier is allocated last
    while (ident != NULL) {
        // Allocate space for the identifier on the stack
        code_seq alloc = code_utils_allocate_stack_space(1);

        // Find the value of the variable by traversing the link
        // and copying the value into the allocated space from the offset
        // Using $r3 as the base register
        lexical_address *ld = id_use_2_lexical_address(ident->idu);
        code_seq base = code_utils_compute_fp(R3, ld->levelsOutward);
        code_seq_add_to_end(&alloc, &base);
        code_seq_add_to_end(&alloc, code_cpw(SP, 0, R3, ld->offsetInAR)); // Copy value from R3 + offset to SP
        
        // Concatenate this identifier's code into the result sequence
        code_seq_add_to_end(&alloc, code_lit(SP, 0, 0)); // Zero out the allocated space
        code_seq_add_to_end(&alloc, code_sri(SP, 1)); // Increment SP
        break;
        
        // Concatenate this identifier's code into the result sequence
        code_seq_concat(&ret, alloc);

        // Move to the next identifier in the list
        ident = ident->next;
    }

    return ret;
}

// Generate code for constant declarations
code_seq gen_code_const_decls(const_decls_t cds) {
    code_seq ret = code_seq_empty();
    const_decl_t *const_decl = cds.start;
    while (const_decl != NULL) {
        code_seq const_decl_cs = gen_code_const_decl(*const_decl);
        code_seq_concat(&ret, const_decl_cs);
        const_decl = const_decl->next;
    }
    return ret;
}

// Generate code for a single constant declaration
code_seq gen_code_const_decl(const_decl_t cd) {
    return gen_code_const_def_list(cd.const_def_list);
}

// Generate code for a list of constant definitions
code_seq gen_code_const_def_list(const_def_list_t cdl) {
    code_seq ret = code_seq_empty();
    const_def_t *const_def = cdl.start;
    while (const_def != NULL) {
        code_seq const_def_cs = gen_code_const_def(*const_def);
        code_seq_concat(&ret, const_def_cs);
        const_def = const_def->next;
    }
    return ret;
}

// Generate code for a single constant definition
code_seq gen_code_const_def(const_def_t cd) {
    /*
        How to initialize constants?
            use literal table to find each constant's offset
            then copy from $gp+offset to the storage allocated
    */

    ident_t ident = cd.ident;

    // Allocate space for the identifier on the stack
    code_seq alloc = code_utils_allocate_stack_space(1);
    // Initialize constant using literal table
    // Find the offset of the constant
    unsigned int offset = id_use_get_attrs(ident.idu)->offset_count;
    code_seq_add_to_end(&alloc, code_cpw(SP, 0, GP, offset)); // Load constant from GP + offset
}

// Generate code for the list of statments given by stmts
code_seq gen_code_stmts(stmts_t stmts)
{
    code_seq ret = code_seq_empty();
    stmt_t *sp = stmts.stmt_list.start;
    while (sp != NULL)
    {
        code_seq_concat(&ret, gen_code_stmt(*sp));
        sp = sp->next;
    }
    return ret;
}

// Generate code for stmt
code_seq gen_code_stmt(stmt_t stmt)
{
    switch (stmt.stmt_kind)
    {
    case assign_stmt:
        return gen_code_assign_stmt(stmt.data.assign_stmt);
        break;
    case if_stmt:
        return gen_code_if_stmt(stmt.data.if_stmt);
        break;
    case while_stmt:
        return gen_code_while_stmt(stmt.data.while_stmt);
        break;
    case read_stmt:
        return gen_code_read_stmt(stmt.data.read_stmt);
        break;
    case print_stmt:
        return gen_code_print_stmt(stmt.data.print_stmt);
        break;
    case block_stmt:
        return gen_code_block_stmt(stmt.data.block_stmt);
        break;
    default:
        bail_with_error("Call to gen_code_stmt with an AST that is not a statement!");
        break;
    }
    // The following can never execute, but this quiets gcc's warning
    return code_seq_empty();
}


// Generate code for an assignment statement
code_seq gen_code_assign_stmt(assign_stmt_t stmt) {
    code_seq ret = code_seq_empty();

    // Generate code for the RHS expression
    code_seq rhs_code = gen_code_expr(*stmt.expr); // Evaluate stmt.expr and push result onto stack
    code_seq_concat(&ret, rhs_code);

    // Get lexical address information for the LHS variable
    if (stmt.idu == NULL || id_use_get_attrs(stmt.idu) == NULL) {
        bail_with_error("Invalid id_use or attributes in gen_code_assign_stmt!");
    }

    id_attrs *attrs = id_use_get_attrs(stmt.idu);
    unsigned int levelsOut = stmt.idu->levelsOutward;
    unsigned int offsetInAR = attrs->offset_count;

    // Compute the frame pointer for the LHS variable
    code_seq fp_code = code_utils_compute_fp(R3, levelsOut); // Load correct FP into $R3
    code_seq_concat(&ret, fp_code);

    // Store the RHS value into the memory location of the LHS variable
    if (attrs->kind == variable_idk) {
        code_seq_add_to_end(&ret, code_cpw(R3, offsetInAR, SP, 0)); // Store value in SP+0 to R3+offsetInAR
    } else {
        bail_with_error("Unsupported kind of identifier in gen_code_assign_stmt!");
    }

    return ret;
}


// Generate code for the if-statement
code_seq gen_code_if_stmt(if_stmt_t stmt) {
    code_seq ret = code_seq_empty();

    // Generate code for the condition
    code_seq condition_code = gen_code_condition(stmt.condition); // Evaluate condition
    code_seq_concat(&ret, condition_code);

    // Conditional branching
    int else_jump_offset = 0;
    int end_jump_offset = 0;

    if (stmt.else_stmts != NULL) {
        else_jump_offset = code_seq_size(gen_code_stmts(*stmt.then_stmts)) + 2; // Jump over "then"
        end_jump_offset = code_seq_size(gen_code_stmts(*stmt.else_stmts)) + 1;  // Jump over "else"
    } else {
        else_jump_offset = code_seq_size(gen_code_stmts(*stmt.then_stmts)) + 1; // Jump to end
    }

    // Add branch instruction to skip "then" block if condition is false
    code_seq_add_to_end(&ret, code_beq(SP, 0, else_jump_offset));

    // Generate code for the "then" block
    code_seq then_code = gen_code_stmts(*stmt.then_stmts); // Generate "then" stmts
    code_seq_concat(&ret, then_code);

    // If there’s an "else" block, jump past it
    if (stmt.else_stmts != NULL) {
        code_seq_add_to_end(&ret, code_jrel(end_jump_offset));
    }

    // Generate code for the "else" block (if present)
    if (stmt.else_stmts != NULL) {
        code_seq else_code = gen_code_stmts(*stmt.else_stmts); // Generate "else" stmts
        code_seq_concat(&ret, else_code);
    }

    return ret;
}

code_seq gen_code_condition(condition_t cond) {
    switch (cond.cond_kind) {
        case ck_db:
            return gen_code_db_condition(cond.data.db_cond);
        case ck_rel:
            return gen_code_rel_op_condition(cond.data.rel_op_cond);
        default:
            bail_with_error("Unsupported condition kind in gen_code_condition!");
    }
}

// Generate code for a relational condition
code_seq gen_code_rel_op_condition(rel_op_condition_t cond) {
    code_seq ret = code_seq_empty();

    // Generate code for the two expressions
    // Evaluate the two expressions and push the results onto the stack
    code_seq expr1_code = gen_code_expr(cond.expr1);
    code_seq expr2_code = gen_code_expr(cond.expr2);
    
    // Apply the relational operator to the two expressions
    code_seq rel_op_code = gen_code_rel_op(cond.rel_op);

    // Concatenate the code sequences
    code_seq_concat(&ret, rel_op_code);

    return ret;
}

// Generate code for a divisible condition
code_seq gen_code_db_condition(db_condition_t cond) {
    code_seq ret = code_seq_empty();

    // Generate code for the two expressions
    // Evaluate the two expressions and push the results onto the stack
    code_seq divisor_code = gen_code_expr(cond.divisor); // SP+1
    code_seq dividend_code = gen_code_expr(cond.dividend); // SP+0

    // Apply the division operation to the two expressions
    code_seq* div_op_code = code_div(SP, 1);
    
    // Concatenate the code sequences
    code_seq_concat(&ret, *div_op_code);

    return ret;
}


// Generate code for a while statement
code_seq gen_code_while_stmt(while_stmt_t stmt) {
    code_seq ret = code_seq_empty();

    // Mark the start of the loop (loop entry)
    int loop_entry_offset = code_seq_size(ret);

    // Generate code for the condition
    code_seq condition_code = gen_code_condition(stmt.condition);
    code_seq_concat(&ret, condition_code);

    // Add a conditional jump to skip the loop body if condition is false
    int exit_offset = code_seq_size(gen_code_stmts(*stmt.body)) + 2; // Skip body and jump back
    code_seq_add_to_end(&ret, code_beq(SP, 0, exit_offset));       // If false, jump out of loop

    // Generate code for the loop body
    code_seq body_code = gen_code_stmts(*stmt.body);
    code_seq_concat(&ret, body_code);

    // Add a jump back to the loop entry
    int back_jump_offset = -(code_seq_size(condition_code) + code_seq_size(body_code) + 1);
    code_seq_add_to_end(&ret, code_jrel(back_jump_offset));         // Jump to loop entry

    // Mark the end of the loop (exit point)
    return ret;
}

// Generate code for a read statement
code_seq gen_code_read_stmt(read_stmt_t stmt) {
    code_seq ret = code_seq_empty();

    // Use READ instruction to read input and push it onto the stack
    code_seq_add_to_end(&ret, code_rch(SP, 0)); // Read input into stack top

    // Get lexical address information for the variable
    if (stmt.idu == NULL || id_use_get_attrs(stmt.idu) == NULL) {
        bail_with_error("Invalid id_use or attributes in gen_code_read_stmt!");
    }

    id_attrs *attrs = id_use_get_attrs(stmt.idu);
    unsigned int levelsOut = stmt.idu->levelsOutward;
    unsigned int offsetInAR = attrs->offset_count;

    // Compute the frame pointer for the variable
    code_seq fp_code = code_utils_compute_fp(R3, levelsOut); // Load correct FP into $R3
    code_seq_concat(&ret, fp_code);

    // Store the read value from the stack into the variable's memory location
    code_seq_add_to_end(&ret, code_cpw(R3, offsetInAR, SP, 0)); // Store value in SP+0 to R3+offsetInAR

    return ret;
}

// Generate code for a print statement
code_seq gen_code_print_stmt(print_stmt_t stmt) {
    code_seq ret = code_seq_empty();

    // Generate code to evaluate the expression
    code_seq expr_code = gen_code_expr(stmt.expr);
    code_seq_concat(&ret, expr_code);

    // Print the value at the top of the stack using
    // PSTR instruction
    code_seq_add_to_end(&ret, code_pstr(SP, 0)); // Print the value at SP+0

    return ret;
}

// Generate code for the block statement given by stmt
code_seq gen_code_block_stmt(block_stmt_t stmt)
{
    bail_with_error("gen_code_block_stmt not implemented!");
    return;
}

// Generate code for the expression exp
// putting the result on top of the stack,
code_seq gen_code_expr(expr_t exp)
{
    switch (exp.expr_kind)
    {
    case expr_bin:
        return gen_code_binary_op_expr(exp.data.binary);
        break;
    case expr_ident:
        return gen_code_ident(exp.data.ident);
        break;
    case expr_number:
        return gen_code_number(exp.data.number);
        break;
    case expr_negated:
        return gen_code_negated(exp.data.negated);
        break;
    default:
        bail_with_error("Unexpected expr_kind_e (%d) in gen_code_expr",
                        exp.expr_kind);
        break;
    }
    // never happens, but suppresses a warning from gcc
    return code_seq_empty();
}

// Generate code for the expression exp
// putting the result on top of the stack,
code_seq gen_code_binary_op_expr(binary_op_expr_t exp)
{
    // put the values of the two subexpressions on the stack
    code_seq ret = gen_code_expr(*(exp.expr1));
    code_seq_concat(&ret, gen_code_expr(*(exp.expr2)));
    
    // do the operation, putting the result on the stack
    code_seq_concat(&ret, gen_code_op(exp.arith_op));
    return ret;
}

// Generate code to apply op to the
// 2nd from top and top of the stack,
// putting the result on top of the stack in their place,
code_seq gen_code_op(token_t op)
{
    switch (op.code)
    {
    case eqsym:
    case neqsym:
    case ltsym:
    case leqsym:
    case gtsym:
    case geqsym:
        return gen_code_rel_op(op);
        break;
    case plussym:
    case minussym:
    case multsym:
    case divsym:
        return gen_code_arith_op(op);
        break;
    default:
        bail_with_error("Unknown token code (%d) in gen_code_op",
                        op.code);
        break;
    }
    return code_seq_empty();
}

// Generate code to apply the arithmetic operation to the top two stack elements
code_seq gen_code_arith_op(token_t arith_op) {
    code_seq ret = code_seq_empty();

    // Select the instruction based on the arithmetic operator
    switch (arith_op.code) {
        case plussym: {
            // Add the second-to-top (SP+1) to the top of the stack (SP+0)
            code_seq_add_to_end(&ret, code_add(SP, 1, SP, 0)); // v1 + v2
            break;
        }
        case minussym: {
            // Subtract the top of the stack (SP+0) from the second-to-top (SP+1)
            code_seq_add_to_end(&ret, code_sub(SP, 1, SP, 0)); // v1 - v2
            break;
        }
        case multsym: {
            // Multiply the second-to-top (SP+1) by the top of the stack (SP+0)
            code_seq_add_to_end(&ret, code_mul(SP, 1));    // Multiply SP+1 by SP
            break;
        }
        case divsym: {
            // Divide the second-to-top (SP+1) by the top of the stack (SP+0)
            code_seq_add_to_end(&ret, code_div(SP, 1));    // Divide SP+1 by SP
            break;
        }
        default:
            bail_with_error("Unsupported arithmetic operator in gen_code_arith_op!");
    }

    // Adjust the stack pointer (pop one operand, leaving the result)
    code_seq_add_to_end(&ret, code_ari(SP, 1)); // Deallocate 1 word from the stack

    return ret;
}

// Generate code for a relational operator
code_seq gen_code_rel_op(token_t rel_op) {
    code_seq ret = code_seq_empty();

    // Subtract the top two stack elements (SP+1 - SP+0)
    code_seq_add_to_end(&ret, code_sub(SP, 1, SP, 0)); // SP+1 - SP+0

    // Generate conditional branching
    switch (rel_op.code) {
        case eqsym: { // ==
            code_seq_add_to_end(&ret, code_beq(SP, 0, 3)); // If SP == 0, jump ahead 3 instructions
            break;
        }
        case neqsym: { // !=
            code_seq_add_to_end(&ret, code_bne(SP, 0, 3)); // If SP != 0, jump ahead 3 instructions
            break;
        }
        case ltsym: { // <
            code_seq_add_to_end(&ret, code_bltz(SP, 0, 3)); // If SP < 0, jump ahead 3 instructions
            break;
        }
        case leqsym: { // <=
            code_seq_add_to_end(&ret, code_blez(SP, 0, 3)); // If SP <= 0, jump ahead 3 instructions
            break;
        }
        case gtsym: { // >
            code_seq_add_to_end(&ret, code_bgtz(SP, 0, 3)); // If SP > 0, jump ahead 3 instructions
            break;
        }
        case geqsym: { // >=
            code_seq_add_to_end(&ret, code_bgez(SP, 0, 3)); // If SP >= 0, jump ahead 3 instructions
            break;
        }
        default:
            bail_with_error("Unsupported relational operator in gen_code_rel_op!");
    }

    // Push false (0) onto the stack if the condition is false
    code_seq_add_to_end(&ret, code_lit(SP, 0, 0)); // Push 0 (false)
    code_seq_add_to_end(&ret, code_jrel(2));       // Jump over the true case

    // Push true (1) onto the stack if the condition is true
    code_seq_add_to_end(&ret, code_lit(SP, 0, 1)); // Push 1 (true)

    // Adjust the stack pointer
    code_seq_add_to_end(&ret, code_ari(SP, 1)); // Pop one operand from the stack

    return ret;
}


// Generate code to put the value of the given identifier
// on top of the stack
code_seq gen_code_ident(ident_t id)
{    

    // Find the value of the variable by traversing the link
    // and copying the value into the allocated space from the offset
    // Using $r3 as the base register
    lexical_address *ld = id_use_2_lexical_address(id.idu);
    code_seq ret = code_utils_compute_fp(R3, ld->levelsOutward);
    code_seq_add_to_end(&ret, code_cpw(SP, 0, R3, ld->offsetInAR)); // Copy value from R3 + offset to SP

    return ret;
}

// Generate code to put the given number on top of the stack
code_seq gen_code_number(number_t num)
{
    unsigned int global_offset = literal_table_lookup(num.text, num.value);
    code_seq ret = code_utils_allocate_stack_space(1);
	code_seq_add_to_end(&ret, code_cpw(SP, 0, GP, global_offset));

    return ret;
}

// Generate code for the expression exp
// putting the result on top of the stack,
code_seq gen_code_negated(negated_expr_t exp)
{
    // evaluate the subexpression
    code_seq ret = gen_code_expr(*exp.expr->data.negated.expr);

    // negate the result
    code_seq neg_res = code_seq_singleton(code_neg(SP, 0, SP, 0));
    code_seq_add_to_end(&ret, &neg_res);

    return ret;    
}