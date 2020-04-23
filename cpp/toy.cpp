#include <llvm/ADT/APFloat.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>

#include <algorithm>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <memory>
#include <string>
#include <vector>

using namespace llvm;


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
    tok_number = -5
};

// Global variables
static std::string g_identifier_str;    // Filled in if tok_identifier
static double g_num_val;                // Filled in if tok_number

// gettok - returns the next token fron standard input
static int gettok() {
    static int last_char = ' ';

    // skip any whitespaces
    while (isspace(last_char))
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
    return std::move(result); // TODO: remove the move ?
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
static std::map<std::string, Value*> g_named_values;

Value* log_error_v(const char* str) {
    log_error(str);
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
    Function* callee_f = g_module->getFunction(_callee);
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
    // check for existing function from a previous `extern` declaration
    Function *function = g_module->getFunction(_prototype->getName());

    if (!function)
        function = _prototype->codegen();

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
        verifyFunction(*function);
        return function;
    }

    // error generating the function body
    function->eraseFromParent();
    return nullptr;
}


//
// Top level parser
//

void handle_definition() {
    if (auto ast = parse_definition()) {
        if (auto ir = ast->codegen()) {
            fprintf(stderr, "Parsed a function definition\n");
            ir->print(errs());
            fprintf(stderr, "\n");
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
// Driver code
//

int main() {
    // 1 is the lowest precedence
    g_binop_precedence['<'] = 10;
    g_binop_precedence['+'] = 20;
    g_binop_precedence['-'] = 20;
    g_binop_precedence['*'] = 40;

    fprintf(stderr, "ready> ");
    get_next_token();

    g_module = std::make_unique<Module>("my cool jit", g_context);

    main_loop();

    g_module->print(errs(), nullptr);

    return 0;
}