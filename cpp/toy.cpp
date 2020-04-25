#include "KaleidoscopeJIT.h"
#include <llvm/ADT/APFloat.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/GVN.h>

#include <algorithm>
#include <cassert>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <memory>
#include <string>
#include <vector>

using namespace llvm;
using namespace llvm::orc;


//
// Lexer
//

// The lexer returns tokens [0-255] if it is an unknown character,
// otherwise one of these for known things.
enum Token {
    tok_eof = -1,

    // commands
    tok_def = -2,
    tok_extern = -3,

    // primary
    tok_identifier = -4,
    tok_number = -5,

    // control
    tok_if = -6,
    tok_then = -7,
    tok_else = -8,
    tok_for = -9,
    tok_in = -10
};

// Global variables
static std::string g_identifier_str;    // Filled in if tok_identifier
static double g_num_val;                // Filled in if tok_number

// gettok - returns the next token fron standard input
static int gettok() {
    static int last_char = ' ';

    // skip any whitespaces
    while (isspace(last_char) || last_char == '\n')
        last_char = getchar();

    // identifier: [a-zA-Z][a-zA-Z0-9]*
    if (isalpha(last_char)) {
        g_identifier_str = last_char;
        while (isalnum(last_char = getchar()))
            g_identifier_str += last_char;

        if (g_identifier_str == "def")
            return tok_def;
        if (g_identifier_str == "extern")
            return tok_extern;
        if (g_identifier_str == "if")
            return tok_if;
        if (g_identifier_str == "then")
            return tok_then;
        if (g_identifier_str == "else")
            return tok_else;
        if (g_identifier_str == "for")
            return tok_for;
        if (g_identifier_str == "in")
            return tok_in;

        return tok_identifier;
    }

    // number: [0-9.]+
    if (isdigit(last_char) or last_char == '.') {
        std::string num_str;
        do {
            num_str += last_char;
            last_char = getchar();
        } while (isdigit(last_char) or last_char == '.');

        g_num_val = strtod(num_str.c_str(), nullptr);
        return tok_number;
    }

    // comments
    if (last_char == '#') {
        // skip to the end of the line
        do
            last_char = getchar();
        while (last_char != EOF and last_char != '\n' and last_char != '\r');

        if (last_char != EOF)
            return gettok();
    }

    // end of file
    if (last_char == EOF)
        return tok_eof;

    // otherwise, just return the character as its ascii value
    int this_char = last_char;
    last_char = getchar();
    return this_char;
}


//
// Abstract Syntax Tree
//

namespace {

// Base class for all expression nodes
class ExprAST {
public:
    virtual ~ExprAST() = default;
    virtual Value* codegen() = 0;
};

// Expression class for numeric literals like "1.0"
class NumberExprAST : public ExprAST {
public:
    NumberExprAST(double val) : _val(val) {}
    Value* codegen() override;
private:
    double _val;
};

// Expression class for referencing variable, like "a"
class VariableExprAST : public ExprAST {
public:
    VariableExprAST(const std::string& name) : _name(name) {}
    Value* codegen() override;
private:
    std::string _name;
};

// Expression class for binary operator
class BinaryExprAST : public ExprAST {
public:
    BinaryExprAST(
        char op,
        std::unique_ptr<ExprAST> lhs,
        std::unique_ptr<ExprAST> rhs
    )
    : _op(op)
    , _lhs(std::move(lhs))
    , _rhs(std::move(rhs))
    {}
    Value* codegen() override;
private:
    char _op;
    std::unique_ptr<ExprAST> _lhs, _rhs;
};

// Expression class for function calls
class CallExprAST : public ExprAST {
public:
    CallExprAST(
        const std::string& callee,
        std::vector<std::unique_ptr<ExprAST>> args
    )
    : _callee(callee)
    , _args(std::move(args))
    {}
    Value* codegen() override;
private:
    std::string _callee;
    std::vector<std::unique_ptr<ExprAST>> _args;
};

// Expression class for if / then / else
class IfExprAST : public ExprAST {
public:
    IfExprAST(
        std::unique_ptr<ExprAST> cond,
        std::unique_ptr<ExprAST> then,
        std::unique_ptr<ExprAST> else_expr
    )
    : _cond(std::move(cond))
    , _then(std::move(then))
    , _else(std::move(else_expr))
    {}
    Value* codegen() override;
private:
    std::unique_ptr<ExprAST> _cond, _then, _else;
};

// Expression class for for / in
class ForExprAST : public ExprAST {
public:
    ForExprAST(
        const std::string& var_name,
        std::unique_ptr<ExprAST> start,
        std::unique_ptr<ExprAST> end,
        std::unique_ptr<ExprAST> step,
        std::unique_ptr<ExprAST> body
    )
    : _var_name(var_name)
    , _start(std::move(start))
    , _end(std::move(end))
    , _step(std::move(step))
    , _body(std::move(body))
    {}
    Value* codegen() override;
private:
    std::string _var_name;
    std::unique_ptr<ExprAST> _start, _end, _step, _body;
};

// Represents the "prototype" for a function, which captures its name,
// and its arguments names (thus implicitly the number of arguments
// the function takes)
class PrototypeAST {
public:
    PrototypeAST(
        const std::string& name,
        std::vector<std::string> args
    )
    : _name(name)
    , _args(std::move(args))
    {}
    Function* codegen();
    const std::string& getName() const { return _name; }

private:
    std::string _name;
    std::vector<std::string> _args;
};

// Represents a function definition itself
class FunctionAST {
public:
    FunctionAST(
        std::unique_ptr<PrototypeAST> prototype,
        std::unique_ptr<ExprAST> body
    )
    : _prototype(std::move(prototype))
    , _body(std::move(body))
    {}
    Function* codegen();
private:
    std::unique_ptr<PrototypeAST> _prototype;
    std::unique_ptr<ExprAST> _body;
};

} // end anonymous namespace


//
// Parser
//

// Provide a simple token buffer
// g_cur_tok is the current token the parser is looking at
// get_next_token reads another token fron the lexer and updates g_cur_tok
static int g_cur_tok;
static int get_next_token() { return g_cur_tok = gettok(); }

// Holds the precedence for each binary operator
static std::map<char, int> g_binop_precedence;

// Returns the precedence of the pending binary operator token
int get_tok_precedence() {
    if (!isascii(g_cur_tok))
        return -1;

    int tok_prec = g_binop_precedence[g_cur_tok];
    if (tok_prec <= 0)
        return -1;

    return tok_prec;
}

// Error handling helpers
std::unique_ptr<ExprAST> log_error(const char* str) {
    fprintf(stderr, "Error: %s\n", str);
    return nullptr;
}

std::unique_ptr<PrototypeAST> log_error_prototype(const char* str) {
    log_error(str);
    return nullptr;
}

// Forward declaration
std::unique_ptr<ExprAST> parse_expression();

// numberexpr ::= number
std::unique_ptr<ExprAST> parse_number_expr() {
    auto result = std::make_unique<NumberExprAST>(g_num_val);
    get_next_token(); // consume the number
    return result;
}

// parenthesexpr ::= '(' expression ')'
std::unique_ptr<ExprAST> parse_parenthesexpr() {
    get_next_token(); // consume '('

    auto v = parse_expression();
    if (!v)
        return nullptr;

    if (g_cur_tok != ')')
        return log_error("expected ')'");

    get_next_token(); // consume ')'
    return v;
}

// identifierexpr
//  ::= identifier
//  ::= identifier '(' expression* ')'
std::unique_ptr<ExprAST> parse_identifierexpr() {
    std::string id_name = g_identifier_str;
    get_next_token();

    // simple variable ref
    if (g_cur_tok != '(')
        return std::make_unique<VariableExprAST>(id_name);

    // function call
    get_next_token(); // consume '('

    std::vector<std::unique_ptr<ExprAST>> args;
    if (g_cur_tok != ')') {
        while (true) {
            if (auto arg = parse_expression())
                args.push_back(std::move(arg));
            else
                return nullptr;

            if (g_cur_tok == ')')
                break;

            if (g_cur_tok != ',')
                return log_error("Expected ')' or ',' in argument list");

            get_next_token();
        }
    }

    get_next_token(); // consume ')'

    return std::make_unique<CallExprAST>(id_name, std::move(args));
}

// ifexpr
//  ::= 'if' expression 'then' expression 'else' expression
std::unique_ptr<ExprAST> parse_ifexpr() {
    get_next_token(); // consume 'if'

    auto condition_expr = parse_expression();
    if (!condition_expr)
        return nullptr;

    if (g_cur_tok != tok_then)
        return log_error("Expected 'then'");
    get_next_token(); // consume 'then'

    auto then_expr = parse_expression();
    if (!then_expr)
        return nullptr;

    if (g_cur_tok != tok_else)
        return log_error("Expected 'else'");
    get_next_token(); // consume 'else'

    auto else_expr = parse_expression();
    if (!else_expr)
        return nullptr;

    return std::make_unique<IfExprAST>(
        std::move(condition_expr),
        std::move(then_expr),
        std::move(else_expr)
    );
}

std::unique_ptr<ExprAST> parse_forexpr() {
    get_next_token(); // consume 'for'

    if (g_cur_tok != tok_identifier)
        return log_error("Expected identifier after 'for'");

    std::string var_name = g_identifier_str;
    get_next_token(); // consume identifier

    if (g_cur_tok != '=')
        return log_error("Expected '=' after 'for'");
    get_next_token(); // consume '='

    auto start_expr = parse_expression();
    if (!start_expr)
        return nullptr;

    if (g_cur_tok != ',')
        return log_error("Expected ',' after 'for' start value");
    get_next_token(); // consume ','

    auto end_expr = parse_expression();
    if (!end_expr)
        return nullptr;

    // step is optional
    std::unique_ptr<ExprAST> step_expr;
    if (g_cur_tok == ',') {
        get_next_token(); // consume ','

        step_expr = parse_expression();
        if (!step_expr)
            return nullptr;
    }

    if (g_cur_tok != tok_in)
        return log_error("Expected 'in' after 'for'");
    get_next_token(); // consume 'in'

    auto body_expr = parse_expression();
    if (!body_expr)
        return nullptr;

    return std::make_unique<ForExprAST>(
        var_name,
        std::move(start_expr),
        std::move(end_expr),
        std::move(step_expr),
        std::move(body_expr)
    );
}

// primary
//  ::= identifierexpr
//  ::= numberexpr
//  ::= parenexpr
std::unique_ptr<ExprAST> parse_primary() {
    switch (g_cur_tok) {
        case tok_identifier:
            return parse_identifierexpr();
        case tok_number:
            return parse_number_expr();
        case '(':
            return parse_parenthesexpr();
        case tok_if:
            return parse_ifexpr();
        case tok_for:
            return parse_forexpr();
        default:
            return log_error("Unknown token when expecting expression");
    }
}

// binop rhs
//  ::= ('+' primary)*
std::unique_ptr<ExprAST> parse_binoprhs(
    int expr_prec, std::unique_ptr<ExprAST> lhs) {
    // if this is a binop, find its precedence
    while (true) {
        int tok_prec = get_tok_precedence();

        // if next op binds less tightly as the current op (starts at 0
        // for the first lhs), we are done
        if (tok_prec < expr_prec)
            return lhs;

        int binop = g_cur_tok;
        get_next_token(); // consume binop

        // parse the primary expression after the binop
        auto rhs = parse_primary();
        if (!rhs)
            return nullptr;

        // if the next binop binds more tightly, let it take the
        // current rhs as its lhs
        int next_prec = get_tok_precedence();
        if (tok_prec < next_prec) {
            rhs = parse_binoprhs(tok_prec + 1, std::move(rhs));
            if (!rhs)
                return nullptr;
        }

        // merge lhs / rhs
        lhs = std::make_unique<BinaryExprAST>(
            binop,
            std::move(lhs),
            std::move(rhs)
        );
    }
}

// expression
//  ::= primary binoprhs
std::unique_ptr<ExprAST> parse_expression() {
    auto lhs = parse_primary();
    if (!lhs)
        return nullptr;

    return parse_binoprhs(0, std::move(lhs));
}

// prototype
//  ::= id '(' id* ')'
std::unique_ptr<PrototypeAST> parse_prototype() {
    if (g_cur_tok != tok_identifier)
        return log_error_prototype("Expected function mae in prototype");

    std::string func_name = g_identifier_str;
    get_next_token();

    if (g_cur_tok != '(')
        return log_error_prototype("Expected '(' in prototype");

    std::vector<std::string> arg_names;
    while (get_next_token() == tok_identifier)
        arg_names.push_back(g_identifier_str);

    if (g_cur_tok != ')')
        return log_error_prototype("Expected ')' in prototype");

    get_next_token(); // consume ')'

    return std::make_unique<PrototypeAST>(
        func_name, std::move(arg_names)
    );
}

// definition
//  ::= 'def' prototype expression
std::unique_ptr<FunctionAST> parse_definition() {
    get_next_token(); // consume 'def'

    auto prototype = parse_prototype();
    if (!prototype)
        return nullptr;

    if (auto expr = parse_expression())
        return std::make_unique<FunctionAST>(
            std::move(prototype), std::move(expr)
        );

    return nullptr;
}

// toplevelexpr
//  ::= expression
std::unique_ptr<FunctionAST> parse_toplevelexpr() {
    // evaluate a top level expression into an anonymous function
    if (auto expr = parse_expression()) {
        // make an anonymous prototype
        auto prototype = std::make_unique<PrototypeAST>(
            "__anon_expr", std::vector<std::string>()
        );

        return std::make_unique<FunctionAST>(
            std::move(prototype), std::move(expr)
        );
    }

    return nullptr;
}

// external
//  ::= 'extern' prototype
std::unique_ptr<PrototypeAST> parse_extern() {
    get_next_token(); // consume 'extern'
    return parse_prototype();
}


//
// Code generation
//

static LLVMContext g_context;
static IRBuilder<> g_builder(g_context);
static std::unique_ptr<Module> g_module;
static std::unique_ptr<legacy::FunctionPassManager> g_fpm;
static std::unique_ptr<KaleidoscopeJIT> g_jit;

static std::map<std::string, Value*> g_named_values;
static std::map<std::string, std::unique_ptr<PrototypeAST>> g_function_prototypes;


Value* log_error_v(const char* str) {
    log_error(str);
    return nullptr;
}

Function* get_function(const std::string& name) {
    // function is already available in the current module
    if (auto func = g_module->getFunction(name))
        return func;

    // try to codegen the declaration from some existing prototype
    auto it = g_function_prototypes.find(name);
    if (it != g_function_prototypes.end())
        return it->second->codegen();

    return nullptr;
}

Value* NumberExprAST::codegen() {
    return ConstantFP::get(g_context, APFloat(_val));
}

Value* VariableExprAST::codegen() {
    Value* val = g_named_values[_name];
    if (!val)
        return log_error_v("Unknown variable name");
    return val;
}

Value* BinaryExprAST::codegen() {
    Value* lhs = _lhs->codegen();
    Value* rhs = _rhs->codegen();
    if (!lhs or !rhs)
        return nullptr;

    switch (_op) {
        case '+':
            return g_builder.CreateFAdd(lhs, rhs, "addtmp");
        case '-':
            return g_builder.CreateFSub(lhs, rhs, "subtmp");
        case '*':
            return g_builder.CreateFMul(lhs, rhs, "multmp");
        case '<':
            lhs = g_builder.CreateFCmpULT(lhs, rhs, "cmptmp");
            // convert bool 0 / 1 to double 0.0 / 1.0
            return g_builder.CreateUIToFP(lhs, Type::getDoubleTy(g_context), "booltmp");
        default:
            return log_error_v("Invalid Binary operator");
    }
}

Value* CallExprAST::codegen() {
    // look-up name in the global module table
    Function* callee_f = get_function(_callee);
    if (!callee_f)
        return log_error_v("Unknown function referenced");

    // argument mismatch error
    if (callee_f->arg_size() != _args.size())
        return log_error_v("Incorrect # arguments passed");

    std::vector<Value*> args_v;
    for (unsigned i = 0, e = _args.size(); i != e; ++i) {
        args_v.push_back(_args[i]->codegen());
        if (!args_v.back())
            return nullptr;
    }

    return g_builder.CreateCall(callee_f, args_v, "calltmp");
}

Value* IfExprAST::codegen() {
    Value* condition_value = _cond->codegen();
    if (!condition_value)
        return nullptr;

    // convert to boolean
    condition_value = g_builder.CreateFCmpONE(
        condition_value, ConstantFP::get(g_context, APFloat(0.0)), "ifcond");

    Function* function = g_builder.GetInsertBlock()->getParent();

    // control flow blocks, 'then' block is inserted at the end of
    // the current function
    BasicBlock* then_block = BasicBlock::Create(g_context, "then", function);
    BasicBlock* else_block = BasicBlock::Create(g_context, "else");
    BasicBlock* merge_block = BasicBlock::Create(g_context, "ifcont");

    g_builder.CreateCondBr(condition_value, then_block, else_block);

    // emit then block
    g_builder.SetInsertPoint(then_block);

    Value* then_value = _then->codegen();
    if (!then_value)
        return nullptr;

    g_builder.CreateBr(merge_block);
    // codegen for 'then' can change the current block, keep this
    // updated block for the phi node
    then_block = g_builder.GetInsertBlock();

    // emit else block
    function->getBasicBlockList().push_back(else_block);
    g_builder.SetInsertPoint(else_block);

    Value* else_value = _else->codegen();
    if (!else_value)
        return nullptr;

    g_builder.CreateBr(merge_block);
    // codegen for 'else' can change the current block, keep this
    // updated block for the phi node
    else_block = g_builder.GetInsertBlock();

    // emit merge block
    function->getBasicBlockList().push_back(merge_block);
    g_builder.SetInsertPoint(merge_block);

    // phi node for the return value
    PHINode* phi_node = g_builder.CreatePHI(Type::getDoubleTy(g_context), 2, "iftmp");
    phi_node->addIncoming(then_value, then_block);
    phi_node->addIncoming(else_value, else_block);
    return phi_node;
}

// output for loop as:
//   ...
//   start = startexpr
//   goto loop
// loop:
//   variable = phi [start, loopheader], [nextvariable, loopend]
//   ...
//   bodyexpr
//   ...
// loopend:
//   step = stepexpr
//   nextvariable = variable + step
//   endcond = endexpr
//   br endcond, loop, endloop
// outloop:
//   0
Value* ForExprAST::codegen() {
    // emit start
    Value* start_value = _start->codegen();
    if (!start_value)
        return nullptr;

    Function* function = g_builder.GetInsertBlock()->getParent();

    // control flow blocks, 'loop' block is inserted at the end of
    // the current function
    BasicBlock* header_block = g_builder.GetInsertBlock();
    BasicBlock* loop_block = BasicBlock::Create(g_context, "loop", function);

    g_builder.CreateBr(loop_block);
    g_builder.SetInsertPoint(loop_block);

    // phi node for loop variable
    PHINode* phi_node = g_builder.CreatePHI(Type::getDoubleTy(g_context), 2, _var_name);
    phi_node->addIncoming(start_value, header_block);

    // loop variable is defined as the phi node, handle possible
    // shadowing of existing variable
    Value* old_value = g_named_values[_var_name];
    g_named_values[_var_name] = phi_node;

    // emit loop body, ignoring the return value
    if (!_body->codegen())
        return nullptr;

    // emit step
    Value* step_value = nullptr;
    if (_step) {
        step_value = _step->codegen();
        if (!step_value)
            return nullptr;
    }
    else {
        step_value = ConstantFP::get(g_context, APFloat(1.0));
    }

    Value* next_value = g_builder.CreateFAdd(phi_node, step_value, "nextvar");

    // emit end
    Value* end_condition = _end->codegen();
    if (!end_condition)
        return nullptr;

    // convert to boolean
    end_condition = g_builder.CreateFCmpONE(
        end_condition, ConstantFP::get(g_context, APFloat(0.0)), "loopcond");

    // codegen for 'loop' can change the current block
    BasicBlock* loop_end_block = g_builder.GetInsertBlock();
    BasicBlock* after_block = BasicBlock::Create(g_context, "afterloop", function);

    g_builder.CreateCondBr(end_condition, loop_block, after_block);
    g_builder.SetInsertPoint(after_block);

    // we only know now what is the loop real end block,
    // because of the possibility to have nested control flow structures
    phi_node->addIncoming(next_value, loop_end_block);

    // unshadow loop variable
    if (old_value)
        g_named_values[_var_name] = old_value;
    else
        g_named_values.erase(_var_name);

    // for expr always returns 0.0
    return Constant::getNullValue(Type::getDoubleTy(g_context));
}

Function* PrototypeAST::codegen() {
    // function type: double(double, double) etc
    std::vector<Type*> doubles(_args.size(), Type::getDoubleTy(g_context));
    // last argument imply non var arg function
    FunctionType* ft = FunctionType::get(Type::getDoubleTy(g_context), doubles, false);
    Function* f = Function::Create(ft, Function::ExternalLinkage, _name, g_module.get());

    // set names for all arguments
    unsigned idx = 0;
    for (auto &arg : f->args())
        arg.setName(_args[idx++]);

    return f;
}

Function* FunctionAST::codegen() {
    // transfer ownership of prototype to the global map
    auto name = _prototype->getName();
    g_function_prototypes[name] = std::move(_prototype);
    Function *function = get_function(name);

    if (!function)
        return nullptr;

    // create a new basic block to start insertion into
    BasicBlock *bb = BasicBlock::Create(g_context, "entry", function);
    g_builder.SetInsertPoint(bb);

    // record the function arguments in the symbol map
    g_named_values.clear();
    for (auto& arg : function->args())
        g_named_values[std::string(arg.getName())] = &arg;

    if (Value* retval = _body->codegen()) {
        g_builder.CreateRet(retval);

        // consistency checks
        verifyFunction(*function);

        // optimizer
        g_fpm->run(*function);

        return function;
    }

    // error generating the function body
    function->eraseFromParent();
    return nullptr;
}


//
// Top level parser and JIT driver
//

void initialize_module_and_passmanager() {
    // open a new module
    g_module = std::make_unique<Module>("my cool jit", g_context);
    g_module->setDataLayout(g_jit->getTargetMachine().createDataLayout());

    // attach a pass manager
    g_fpm = std::make_unique<legacy::FunctionPassManager>(g_module.get());

    // simple peephole optimizations and bit-twiddling
    g_fpm->add(createInstructionCombiningPass());
    // reassociate expressions
    g_fpm->add(createReassociatePass());
    // eliminate common subexpressions
    g_fpm->add(createGVNPass());
    // simplify control flow graph (eg. deleting unreachable blocks)
    g_fpm->add(createCFGSimplificationPass());

    g_fpm->doInitialization();
}

void handle_definition() {
    if (auto ast = parse_definition()) {
        if (auto ir = ast->codegen()) {
            fprintf(stderr, "Parsed a function definition\n");
            ir->print(errs());
            fprintf(stderr, "\n");
            g_jit->addModule(std::move(g_module));
            initialize_module_and_passmanager();
        }
    }
    else
        // skip token for error recovery
        get_next_token();
}

void handle_extern() {
    if (auto ast = parse_extern()) {
        if (auto ir = ast->codegen()) {
            fprintf(stderr, "Parsed an extern\n");
            ir->print(errs());
            fprintf(stderr, "\n");
            g_function_prototypes[ast->getName()] = std::move(ast);
        }
    }
    else
        // skip token for error recovery
        get_next_token();
}

void handle_toplevel_expression() {
    if (auto ast = parse_toplevelexpr()) {
        if (auto ir = ast->codegen()) {

            fprintf(stderr, "Parsed a top-level expression\n");
            ir->print(errs());
            fprintf(stderr, "\n");

            // add current module to JIT
            auto module = g_jit->addModule(std::move(g_module));
            initialize_module_and_passmanager();

            // search for the anonymous function symbol
            auto expr_symbol = g_jit->findSymbol("__anon_expr");
            assert(expr_symbol && "Function not found");

            // get symbol address and cast to the anonymous function type
            typedef double (*FuncT)();
            FuncT func = (FuncT) (intptr_t) cantFail(expr_symbol.getAddress());
            fprintf(stderr, "Evaluated to %f\n", func());

            // delete module from JIT
            g_jit->removeModule(module);
        }
    }
    else
        // skip token for error recovery
        get_next_token();
}

// top
//  ::= definition | external | expression | ';'
void main_loop() {
    while (true) {
        fprintf(stderr, "ready> ");
        switch (g_cur_tok) {
            case tok_eof:
                return;
            // ignore top-level semicolons
            case ';':
                get_next_token();
                break;
            case tok_def:
                handle_definition();
                break;
            case tok_extern:
                handle_extern();
                break;
            default:
                handle_toplevel_expression();
                break;
        }
    }
}


//
// Library functions
//

extern "C" double putchard(double x) {
    fputc((char) x, stderr);
    return 0.0;
}

extern "C" double printd(double x) {
    fprintf(stderr, "%f\n", x);
    return 0;
}


//
// Driver code
//

int main() {
    InitializeNativeTarget();
    InitializeNativeTargetAsmPrinter();
    InitializeNativeTargetAsmParser();

    // 1 is the lowest precedence
    g_binop_precedence['<'] = 10;
    g_binop_precedence['+'] = 20;
    g_binop_precedence['-'] = 20;
    g_binop_precedence['*'] = 40;

    fprintf(stderr, "ready> ");
    get_next_token();

    g_jit = std::make_unique<KaleidoscopeJIT>();
    initialize_module_and_passmanager();

    main_loop();

    return 0;
}