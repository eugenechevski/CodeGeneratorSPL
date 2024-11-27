#include "gen_code.h"

#define STACK_SPACE 4096

// Initialize the code generator
void gen_code_initialize()
{
    literal_table_initialize();
}

// Write a sequence of instructions to the BOF file
static void gen_code_output_seq(BOFFILE bf, code_seq cs) {
    while (!code_seq_is_empty(cs)) {
        bin_instr_t inst = code_seq_first(cs)->instr;
        instruction_write_bin_instr(bf, inst);
        cs = code_seq_rest(cs);
    }
}

// Generate the BOF header
static BOFHeader gen_code_program_header(code_seq main_cs) {
    BOFHeader ret;

    strncpy(ret.magic, "MAGIC", 4); // SPL magic number
    ret.text_start_address = 0;
    ret.text_length = code_seq_size(main_cs) * BYTES_PER_WORD;
    ret.data_start_address = MAX(ret.text_length, 1024) + BYTES_PER_WORD;
    ret.data_length = literal_table_size() * BYTES_PER_WORD;
    ret.stack_bottom_addr = ret.data_start_address + ret.data_length + STACK_SPACE;

    return ret;
}

// Write literals to the BOF file
static void gen_code_output_literals(BOFFILE bf) {
    literal_table_start_iteration();
    while (literal_table_iteration_has_next()) {
        word_type w = literal_table_iteration_next();
        bof_write_word(bf, w);
    }
    literal_table_end_iteration();
}

// Requires: bf is open for writing in binary
// Write the program's BOFFILE to bf
static void gen_code_output_program(BOFFILE bf, code_seq main_cs)
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

    // Generate code for statements
    code_seq stmts_cs = gen_code_stmts(prog.stmts);
    code_seq_concat(&main_cs, stmts_cs);

    // Deallocate stack space for variables
    int total_stack_space = calculate_total_stack_space(prog.var_decls);
    code_seq dealloc_cs = code_utils_deallocate_stack_space(total_stack_space);
    code_seq_concat(&main_cs, dealloc_cs);

    // Tear down activation record and add exit instruction
    code_seq tear_down_cs = code_utils_tear_down_program();
    code_seq_concat(&main_cs, tear_down_cs);
    code_seq_add_to_end(&main_cs, code_exit(0));

    gen_code_output_program(bf, main_cs);
}

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
}

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
         [            static link ]-3
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
           [load the next static link into $r3]  }
              ...                                } levelsOut times
           [load the next static link into $r3]  }
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

        // Find the value of the variable by traversing the static link
        // and copying the value into the allocated space from the offset
        // Using $r3 as the base register
        int levelsOut = ident->idu->levelsOutward;
        int offset = id_use_get_attrs(ident->idu)->offset_count;
        code_seq base = code_utils_compute_fp(R3, levelsOut);
        code_seq_add_to_end(&alloc, &base);
        code_seq_add_to_end(&alloc, code_cpw(SP, 0, R3, offset)); // Copy value from R3 + offset to SP
        
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

// Generate code for stmt
code_seq gen_code_assign_stmt(assign_stmt_t stmt)
{
    // can't call gen_code_ident,
    // since stmt.name is not an ident_t
    code_seq ret;

    // put value of expression in $v0
    ret = gen_code_expr(*(stmt.expr));
    assert(stmt.idu != NULL);
    assert(id_use_get_attrs(stmt.idu) != NULL);

    id_kind typ = id_use_get_attrs(stmt.idu)->kind;

    code_seq_concat(&ret, code_utils_pop_stack_into_reg(V0, typ));
    // put frame pointer from the lexical address of the name
    // (using stmt.idu) into $t9

    code_seq_concat(&ret,
                    code_utils_compute_fp(T9, stmt.idu->levelsOutward));
    unsigned int offset_count = id_use_get_attrs(stmt.idu)->offset_count;
    assert(offset_count <= USHRT_MAX); // it has to fit!

    switch (id_use_get_attrs(stmt.idu)->kind)
    {
    case constant_idk:
        code_seq_add_to_end(&ret,
                            code_fsw(T9, V0, offset_count));
        break;
    case variable_idk:
        code_seq_add_to_end(&ret,
                            code_sw(T9, V0, offset_count));
        break;
    default:
        bail_with_error("Bad kind (%d) for ident in assignment stmt!",
                        id_use_get_attrs(stmt.idu)->kind);
        break;
    }
    return ret;
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

// Generate code for the if-statment given by stmt
code_seq gen_code_if_stmt(if_stmt_t stmt)
{
    // put truth value of stmt.expr in $v0
    code_seq ret = gen_code_expr(stmt.expr);
    code_seq_concat(&ret, code_pop_stack_into_reg(V0, bool_te));
    code_seq cbody = gen_code_stmt(*(stmt.body));
    int cbody_len = code_seq_size(cbody);
    // skip over body if $v0 contains false
    code_seq_add_to_end(&ret,
                              code_beq(V0, 0, cbody_len));
    return code_seq_concat(ret, cbody);
}

// Generate code for the while-statment given by stmt
code_seq gen_code_while_stmt(while_stmt_t stmt)
{
    code_seq ret = code_seq_empty();
    code_seq cbody = gen_code_stmt(*(stmt.body));
    int cbody_len = code_seq_size(cbody);
    // put truth value of stmt.expr in $v0
    ret = gen_code_expr(stmt.expr);
    code_seq_concat(&ret, code_pop_stack_into_reg(V0, bool_te));
    // skip over body if $v0 contains false
    code_seq_add_to_end(&ret,
                              code_beq(V0, 0, cbody_len));
    // add the body
    code_seq_concat(&ret, cbody);
    // add the loop back
    code_seq_add_to_end(&ret,
                              code_b(0, -cbody_len - code_seq_size(ret)));
    return ret;
}

// Generate code for the read statment given by stmt
code_seq gen_code_read_stmt(read_stmt_t stmt)
{
    // put number read into $v0
    code_seq ret = code_seq_singleton(code_rch());
    // put frame pointer from the lexical address of the name
    // (using stmt.idu) into $t9
    assert(stmt.idu != NULL);
    code_seq_concat(&ret,
                          code_compute_fp(T9, stmt.idu->levelsOutward));
    assert(id_use_get_attrs(stmt.idu) != NULL);
    unsigned int offset_count = id_use_get_attrs(stmt.idu)->offset_count;
    assert(offset_count <= USHRT_MAX); // it has to fit!
    code_seq_add_to_end(&ret,
                              code_seq_singleton(code_fsw(T9, V0, offset_count)));
    return ret;
}

// Generate code for the print statment given by stmt
code_seq gen_code_print_stmt(print_stmt_t stmt)
{
    // put value of expression in $v0
    code_seq ret = gen_code_expr(stmt.expr);
    code_seq_concat(&ret, code_pop_stack_into_reg(V0, float_te));
    // print the value
    code_seq_add_to_end(&ret, code_fpr(V0));
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
// and using V0 and AT as temporary registers
// May also modify SP, HI,LO when executed
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
// and using V0 and AT as temporary registers
// May also modify SP, HI,LO when executed
code_seq gen_code_binary_op_expr(binary_op_expr_t exp)
{
    // put the values of the two subexpressions on the stack
    code_seq ret = gen_code_expr(*(exp.expr1));
    code_seq_concat(&ret, gen_code_expr(*(exp.expr2)));
    // check the types match
    type_exp_e t1 = ast_expr_type(*(exp.expr1));
    assert(ast_expr_type(*(exp.expr2)) == t1);
    // do the operation, putting the result on the stack
    code_seq_concat(&ret, gen_code_op(exp.op, t1));
    return ret;
}

// Generate code to apply op to the
// 2nd from top and top of the stack,
// putting the result on top of the stack in their place,
// and using V0 and AT as temporary registers
// Modifies SP when executed
code_seq gen_code_op(token_t op, type_exp_e typ)
{
    switch (op.code)
    {
    case eqsym:
    case neqsym:
    case ltsym:
    case leqsym:
    case gtsym:
    case geqsym:
        return gen_code_rel_op(op, typ);
        break;
    case plussym:
    case minussym:
    case multsym:
    case divsym:
        assert(typ == float_te);
        return gen_code_arith_op(op);
        break;
    default:
        bail_with_error("Unknown token code (%d) in gen_code_op",
                        op.code);
        break;
    }
    return code_seq_empty();
}

// Generate code to apply the floating-point arith_op to the
// 2nd from top and top of the stack,
// putting the result on top of the stack in their place,
// and using V0 and AT as temporary registers
// Also modifies SP when executed
code_seq gen_code_arith_op(token_t arith_op)
{
    // load top of the stack (the second operand) into AT
    code_seq ret = code_pop_stack_into_reg(AT, float_te);
    // load next element of the stack into V0
    code_seq_concat(&ret, code_pop_stack_into_reg(V0, float_te));

    code_seq do_op = code_seq_empty();
    switch (arith_op.code)
    {
    case plussym:
        do_op = code_seq_add_to_end(do_op, code_fadd(V0, AT, V0));
        break;
    case minussym:
        do_op = code_seq_add_to_end(do_op, code_fsub(V0, AT, V0));
        break;
    case multsym:
        do_op = code_seq_add_to_end(do_op, code_fmul(V0, AT, V0));
        break;
    case divsym:
        do_op = code_seq_add_to_end(do_op, code_fdiv(V0, AT, V0));
        break;
    default:
        bail_with_error("Unexpected arithOp (%d) in gen_code_arith_op",
                        arith_op.code);
        break;
    }
    do_op = code_seq_concat(do_op, code_push_reg_on_stack(V0, float_te));
    return code_seq_concat(ret, do_op);
}

// Generate code for the rel_op
// applied to 2nd from top and top of the stack,
// putting the result on top of the stack in their place,
// and using V0 and AT as temporary registers
// Also modifies SP when executed
code_seq gen_code_rel_op(token_t rel_op, type_exp_e typ)
{
    // load top of the stack (the second operand) into AT
    code_seq ret = code_pop_stack_into_reg(AT, typ);
    // load next element of the stack into V0
    code_seq_concat(&ret, code_pop_stack_into_reg(V0, typ));

    // start out by doing the comparison
    // and skipping the next 2 instructions if it's true
    code_seq do_op = code_seq_empty();
    switch (rel_op.code)
    {
    case eqsym:
        if (typ == float_te)
        {
            do_op = code_seq_singleton(code_bfeq(V0, AT, 2));
        }
        else
        {
            do_op = code_seq_singleton(code_beq(V0, AT, 2));
        }
        break;
    case neqsym:
        if (typ == float_te)
        {
            do_op = code_seq_singleton(code_bfne(V0, AT, 2));
        }
        else
        {
            do_op = code_seq_singleton(code_bne(V0, AT, 2));
        }
        break;
    case ltsym:
        if (typ == float_te)
        {
            do_op = code_seq_singleton(code_fsub(V0, AT, V0));
            do_op = code_seq_add_to_end(do_op, code_bfltz(V0, 2));
        }
        else
        {
            do_op = code_seq_singleton(code_sub(V0, AT, V0));
            do_op = code_seq_add_to_end(do_op, code_bltz(V0, 2));
        }
        break;
    case leqsym:
        if (typ == float_te)
        {
            do_op = code_seq_singleton(code_fsub(V0, AT, V0));
            do_op = code_seq_add_to_end(do_op, code_bflez(V0, 2));
        }
        else
        {
            do_op = code_seq_singleton(code_sub(V0, AT, V0));
            do_op = code_seq_add_to_end(do_op, code_blez(V0, 2));
        }
        break;
    case gtsym:
        if (typ == float_te)
        {
            do_op = code_seq_singleton(code_fsub(V0, AT, V0));
            do_op = code_seq_add_to_end(do_op, code_bfgtz(V0, 2));
        }
        else
        {
            do_op = code_seq_singleton(code_sub(V0, AT, V0));
            do_op = code_seq_add_to_end(do_op, code_bgtz(V0, 2));
        }
        break;
    case geqsym:
        if (typ == float_te)
        {
            do_op = code_seq_singleton(code_fsub(V0, AT, V0));
            do_op = code_seq_add_to_end(do_op, code_bfgez(V0, 2));
        }
        else
        {
            do_op = code_seq_singleton(code_sub(V0, AT, V0));
            do_op = code_seq_add_to_end(do_op, code_bgez(V0, 2));
        }
        break;
    default:
        bail_with_error("Unknown token code (%d) in gen_code_rel_op",
                        rel_op.code);
        break;
    }
    code_seq_concat(&ret, do_op);
    // rest of the code for the comparisons
    code_seq_add_to_end(&ret, code_add(0, 0, AT));  // put false in AT
    code_seq_add_to_end(&ret, code_beq(0, 0, 1));   // skip next instr
    code_seq_add_to_end(&ret, code_addi(0, AT, 1)); // put true in AT
    code_seq_concat(&ret, code_push_reg_on_stack(AT, bool_te));
    return ret;
}

// Generate code to put the value of the given identifier
// on top of the stack
// Modifies T9, V0, and SP when executed
code_seq gen_code_ident(ident_t id)
{
    assert(id.idu != NULL);
    code_seq ret = code_compute_fp(T9, id.idu->levelsOutward);
    assert(id_use_get_attrs(id.idu) != NULL);
    unsigned int offset_count = id_use_get_attrs(id.idu)->offset_count;
    assert(offset_count <= USHRT_MAX); // it has to fit!
    type_exp_e typ = id_use_get_attrs(id.idu)->type;
    if (typ == float_te)
    {
        code_seq_add_to_end(&ret,
                                  code_flw(T9, V0, offset_count));
    }
    else
    {
        code_seq_add_to_end(&ret,
                                  code_lw(T9, V0, offset_count));
    }
    return code_seq_concat(ret, code_push_reg_on_stack(V0, typ));
}

// Generate code to put the given number on top of the stack
// Modifies V0 when executed
code_seq gen_code_number(number_t num)
{
    unsigned int global_offset = literal_table_lookup(num.text, num.value);
    return code_seq_concat(code_seq_singleton(code_flw(GP, V0, global_offset)),
                           code_push_reg_on_stack(V0, float_te));
}

// Generate code for the expression exp
// putting the result on top of the stack,
// and using V0 and AT as temporary registers
// May also modify SP, HI,LO when executed
code_seq gen_code_negated(negated_expr_t exp)
{
    code_seq ret = gen_code_expr(exp);

    code_seq_concat(&ret, code_pop_stack_into_reg(AT, bool_te));
    // if 0 skip next 2 instructions
    code_seq_add_to_end(&ret, code_beq(0, AT, 2));
    // it was 1, so put 0 in AT
    code_seq_add_to_end(&ret, code_add(0, 0, AT));
    // and skip the next instruction
    code_seq_add_to_end(&ret, code_beq(0, 0, 1));
    // put 1 in AT
    code_seq_add_to_end(&ret, code_addi(0, AT, 1));
    // push the result on the stack
    code_seq_concat(&ret, code_push_reg_on_stack(AT, bool_te));
    
    return ret;
}