#include "KaleidoscopeJIT.h"
#include <llvm/ADT/APFloat.h>
#include <llvm/ADT/Optional.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/DIBuilder.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/Host.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/GVN.h>
#include <llvm/Transforms/Utils.h>
#include <llvm/Transforms/Utils/Cloning.h>

#include <algorithm>
#include <cassert>
#include <cctype>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <memory>
#include <string>
#include <utility>
#include <vector>

using namespace llvm;
using namespace llvm::orc;
using namespace llvm::sys;


static bool g_ir_mode = false;
static bool g_obj_mode = false;
static bool g_dbg_mode = false;

static LLVMContext g_context;
static IRBuilder<> g_builder(g_context);

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
    tok_in = -10,

    // operators
    tok_binary = -11,
    tok_unary = -12,

    // var definition
    tok_var = -13
};

std::string get_tok_name(int tok) {
    switch (tok) {
        case tok_eof:
            return "eof";
        case tok_def:
            return "def";
        case tok_extern:
            return "extern";
        case tok_identifier:
            return "identifier";
        case tok_number:
            return "number";
        case tok_if:
            return "if";
        case tok_then:
            return "then";
        case tok_else:
            return "else";
        case tok_for:
            return "for";
        case tok_in:
            return "in";
        case tok_binary:
            return "binary";
        case tok_unary:
            return "unary";
        case tok_var:
            return "var";
    }

    return std::string(1, (char) tok);
}

namespace {
    class PrototypeAST;
    class ExprAST;
}

struct DebugInfo {
    DICompileUnit* compile_unit;
    DIType* double_type;
    std::vector<DIScope*> lexical_blocks;

    void emit_location(ExprAST* ast);
    DIType* getDoubleTy();
} KSDbgInfo;

struct SourceLocation {
    int line;
    int col;
};

static SourceLocation g_cur_loc;
static SourceLocation g_lex_loc = {1, 0};

int advance() {
    int last_char = getchar();

    if (last_char == '\n' || last_char == '\r') {
        g_lex_loc.line++;
        g_lex_loc.col = 0;
    }
    else {
        g_lex_loc.col++;
    }

    return last_char;
}

static std::string g_identifier_str;    // Filled in if tok_identifier
static double g_num_val;                // Filled in if tok_number

// gettok - returns the next token fron standard input
int gettok() {
    static int last_char = ' ';

    // skip any whitespaces
    while (isspace(last_char) || last_char == '\n')
        last_char = advance();

    // identifier: [a-zA-Z][a-zA-Z0-9]*
    if (isalpha(last_char)) {
        g_identifier_str = last_char;
        while (isalnum(last_char = advance()))
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
        if (g_identifier_str == "binary")
            return tok_binary;
        if (g_identifier_str == "unary")
            return tok_unary;
        if (g_identifier_str == "var")
            return tok_var;

        return tok_identifier;
    }

    // number: [0-9.]+
    if (isdigit(last_char) or last_char == '.') {
        std::string num_str;
        do {
            num_str += last_char;
            last_char = advance();
        } while (isdigit(last_char) or last_char == '.');

        g_num_val = strtod(num_str.c_str(), nullptr);
        return tok_number;
    }

    // comments
    if (last_char == '#') {
        // skip to the end of the line
        do
            last_char = advance();
        while (last_char != EOF and last_char != '\n' and last_char != '\r');

        if (last_char != EOF)
            return gettok();
    }

    // end of file
    if (last_char == EOF)
        return tok_eof;

    // otherwise, just return the character as its ascii value
    int this_char = last_char;
    last_char = advance();
    return this_char;
}


//
// Abstract Syntax Tree
//

namespace {

raw_ostream& indent(raw_ostream& out, int size) {
    return out << std::string(size, ' ');
}

// Base class for all expression nodes
class ExprAST {
public:
    ExprAST(SourceLocation& loc = g_cur_loc) : _loc(loc) {}
    virtual ~ExprAST() = default;
    virtual Value* codegen() = 0;
    int get_line() const { return _loc.line; }
    int get_col() const { return _loc.col; }
    virtual raw_ostream& dump(raw_ostream& out, int ind) {
        return out << ':' << get_line() << ':' << get_col() << '\n';
    }
private:
    SourceLocation _loc;
};

// Expression class for numeric literals like "1.0"
class NumberExprAST : public ExprAST {
public:
    NumberExprAST(double val) : _val(val) {}
    Value* codegen() override;
    raw_ostream& dump(raw_ostream& out, int ind) override {
        return ExprAST::dump(out << _val, ind);
    }
private:
    double _val;
};

// Expression class for referencing variable, like "a"
class VariableExprAST : public ExprAST {
public:
    VariableExprAST(SourceLocation& loc, const std::string& name)
    : ExprAST(loc), _name(name)
    {}
    Value* codegen() override;
    raw_ostream& dump(raw_ostream& out, int ind) override {
        return ExprAST::dump(out << _name, ind);
    }
    const std::string& getName() const { return _name; }
private:
    std::string _name;
};

// Expression class for unary operator
class UnaryExprAST : public ExprAST {
public:
    UnaryExprAST(
        char op,
        std::unique_ptr<ExprAST> operand
    )
    : _op(op)
    , _operand(std::move(operand))
    {}
    Value* codegen() override;
    raw_ostream& dump(raw_ostream& out, int ind) override {
        ExprAST::dump(out << "unary" << _op, ind);
        _operand->dump(out, ind + 1);
        return out;
    }
private:
    char _op;
    std::unique_ptr<ExprAST> _operand;
};

// Expression class for binary operator
class BinaryExprAST : public ExprAST {
public:
    BinaryExprAST(
        SourceLocation& loc,
        char op,
        std::unique_ptr<ExprAST> lhs,
        std::unique_ptr<ExprAST> rhs
    )
    : ExprAST(loc)
    , _op(op)
    , _lhs(std::move(lhs))
    , _rhs(std::move(rhs))
    {}
    Value* codegen() override;
    raw_ostream& dump(raw_ostream& out, int ind) override {
        ExprAST::dump(out << "binary" << _op, ind);
        _lhs->dump(indent(out, ind) << "lhs:", ind + 1);
        _rhs->dump(indent(out, ind) << "rhs:", ind + 1);
        return out;
    }
private:
    char _op;
    std::unique_ptr<ExprAST> _lhs, _rhs;
};

// Expression class for function calls
class CallExprAST : public ExprAST {
public:
    CallExprAST(
        SourceLocation& loc,
        const std::string& callee,
        std::vector<std::unique_ptr<ExprAST>> args
    )
    : ExprAST(loc)
    , _callee(callee)
    , _args(std::move(args))
    {}
    Value* codegen() override;
    raw_ostream& dump(raw_ostream& out, int ind) override {
        ExprAST::dump(out << "call " << _callee, ind);
        for (const auto& arg : _args)
            arg->dump(indent(out, ind + 1), ind + 1);
        return out;
    }
private:
    std::string _callee;
    std::vector<std::unique_ptr<ExprAST>> _args;
};

// Expression class for if / then / else
class IfExprAST : public ExprAST {
public:
    IfExprAST(
        SourceLocation& loc,
        std::unique_ptr<ExprAST> cond,
        std::unique_ptr<ExprAST> then,
        std::unique_ptr<ExprAST> else_expr
    )
    : ExprAST(loc)
    , _cond(std::move(cond))
    , _then(std::move(then))
    , _else(std::move(else_expr))
    {}
    Value* codegen() override;
    raw_ostream& dump(raw_ostream& out, int ind) override {
        ExprAST::dump(out << "if", ind);
        _cond->dump(indent(out, ind) << "cond:", ind + 1);
        _then->dump(indent(out, ind) << "then:", ind + 1);
        _else->dump(indent(out, ind) << "else:", ind + 1);
        return out;
    }
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
    raw_ostream& dump(raw_ostream& out, int ind) override {
        ExprAST::dump(out << "for", ind);
        _start->dump(indent(out, ind) << "start:", ind + 1);
        _end->dump(indent(out, ind) << "end:", ind + 1);
        _step->dump(indent(out, ind) << "step:", ind + 1);
        _body->dump(indent(out, ind) << "body:", ind + 1);
        return out;
    }
private:
    std::string _var_name;
    std::unique_ptr<ExprAST> _start, _end, _step, _body;
};

// Expression class for for / in
class VarExprAST : public ExprAST {
public:
    VarExprAST(
        std::vector<std::pair<std::string, std::unique_ptr<ExprAST>>> var_names,
        std::unique_ptr<ExprAST> body
    )
    : _var_names(std::move(var_names))
    , _body(std::move(body))
    {}
    Value* codegen() override;
    raw_ostream& dump(raw_ostream& out, int ind) override {
        ExprAST::dump(out << "var", ind);
        for (const auto& named_var : _var_names)
            named_var.second->dump(indent(out, ind) << named_var.first << ':', ind + 1);
        _body->dump(indent(out, ind) << "body:", ind + 1);
        return out;
    }
private:
    std::vector<std::pair<std::string, std::unique_ptr<ExprAST>>> _var_names;
    std::unique_ptr<ExprAST> _body;
};

// Represents the "prototype" for a function, which captures its name,
// and its arguments names (thus implicitly the number of arguments
// the function takes)
class PrototypeAST {
public:
    PrototypeAST(
        SourceLocation& loc,
        const std::string& name,
        std::vector<std::string> args,
        bool is_operator = false,
        unsigned precedence = 0
    )
    : _line(loc.line)
    , _name(name)
    , _args(std::move(args))
    , _is_operator(is_operator)
    , _precedence(precedence)
    {}
    Function* codegen();

    const std::string& getName() const { return _name; }
    char getOperatorName() const {
        assert(isUnaryOp() || isBinaryOp());
        return _name[_name.size() - 1];
    }
    unsigned getBinaryPrecedence() const { return _precedence; }
    int getLine() const { return _line; }

    bool isUnaryOp() const { return _is_operator && _args.size() == 1; }
    bool isBinaryOp() const { return _is_operator && _args.size() == 2; }

private:
    std::string _name;
    std::vector<std::string> _args;
    bool _is_operator;
    unsigned _precedence;
    int _line;
};

// Represents a function definition itself
class FunctionAST {
public:
    FunctionAST(
        std::unique_ptr<PrototypeAST> prototype,
        std::unique_ptr<ExprAST> body
    )
    : _name(prototype->getName())
    , _prototype(std::move(prototype))
    , _body(std::move(body))
    {}
    Function* codegen();
    raw_ostream& dump(raw_ostream& out, int ind) {
        indent(out, ind++) << "FunctionAST\n";
        indent(out, ind) << "Body:";
        return _body ? _body->dump(out, ind) : out << "null\n";
    }
    const std::string& getName() const { return _name; }
private:
    std::string _name;
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

std::unique_ptr<FunctionAST> log_error_function(const char* str) {
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
    SourceLocation loc = g_cur_loc;
    get_next_token();

    // simple variable ref
    if (g_cur_tok != '(')
        return std::make_unique<VariableExprAST>(loc, id_name);

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

    return std::make_unique<CallExprAST>(loc, id_name, std::move(args));
}

// ifexpr
//  ::= 'if' expression 'then' expression 'else' expression
std::unique_ptr<ExprAST> parse_ifexpr() {
    SourceLocation loc = g_cur_loc;
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
        loc,
        std::move(condition_expr),
        std::move(then_expr),
        std::move(else_expr)
    );
}

// forexpr
//  ::= 'for' identifier '=' expr ',' expr (',' expr)? 'in' expression
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

// varexpr
//  ::= 'var' identifier ('=' expression)?
//             (',' identifier ('=' expression)?)* 'in' expression
std::unique_ptr<ExprAST> parse_varexpr() {
    get_next_token(); // consume 'var'

    std::vector<std::pair<std::string, std::unique_ptr<ExprAST>>> var_names;

    // at least one variable name is required
    if (g_cur_tok != tok_identifier)
        return log_error("Expected identifier after 'var'");

    while (true) {
        std::string name = g_identifier_str;
        get_next_token(); // consume identifier

        // optional initializer
        std::unique_ptr<ExprAST> init = nullptr;
        if (g_cur_tok == '=') {
            get_next_token(); // consume '='
            init = parse_expression();
            if (!init)
                return nullptr;
        }

        var_names.push_back(std::make_pair(name, std::move(init)));

        // end of var list
        if (g_cur_tok != ',')
            break;
        get_next_token(); // consume ','

        if (g_cur_tok != tok_identifier)
            return log_error("Expected identifier list after 'var'");
    }

    if (g_cur_tok != tok_in)
        return log_error("Expected 'in' keyword after 'var'");
    get_next_token(); // consume 'in'

    auto body = parse_expression();
    if (!body)
        return nullptr;

    return std::make_unique<VarExprAST>(std::move(var_names), std::move(body));
}


// primary
//  ::= identifierexpr
//  ::= numberexpr
//  ::= parenexpr
//  ::= ifexpr
//  ::= forexpr
//  ::= varexpr
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
        case tok_var:
            return parse_varexpr();
        default:
            return log_error("Unknown token when expecting expression");
    }
}

// unary
//  ::= primary
//  ::= '!' unary
std::unique_ptr<ExprAST> parse_unary() {
    // current token is not an operator, parse the primary expr
    if (!isascii(g_cur_tok) || g_cur_tok == '(' || g_cur_tok == ',')
        return parse_primary();

    int op = g_cur_tok;
    get_next_token(); // consume op

    // recursive call handle unary op chaining (eg. !!var)
    if (auto operand = parse_unary())
        return std::make_unique<UnaryExprAST>(op, std::move(operand));
    return nullptr;
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
        SourceLocation loc = g_cur_loc;
        get_next_token(); // consume binop

        // parse the unary expression after the binop
        auto rhs = parse_unary();
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
            loc,
            binop,
            std::move(lhs),
            std::move(rhs)
        );
    }
}

// expression
//  ::= primary binoprhs
std::unique_ptr<ExprAST> parse_expression() {
    auto lhs = parse_unary();
    if (!lhs)
        return nullptr;

    return parse_binoprhs(0, std::move(lhs));
}

// prototype
//  ::= id '(' id* ')'
//  ::= binary LETTER number? (id, id)
//  ::= unary LETTER (id)
std::unique_ptr<PrototypeAST> parse_prototype() {
    std::string func_name;
    SourceLocation loc = g_cur_loc;

    // 0 = identifier, 1 = unary, 2 = binary
    unsigned kind = 0;
    unsigned binary_precedence = 0;

    switch (g_cur_tok) {
        case tok_identifier:
            func_name = g_identifier_str;
            kind = 0;
            get_next_token(); // consume func_name
            break;
        case tok_unary:
            get_next_token(); // consume 'unary'
            if (!isascii(g_cur_tok))
                return log_error_prototype("Expected unary operator");
            func_name = "unary";
            func_name += (char) g_cur_tok;
            kind = 1;
            get_next_token(); // consume op
            break;
        case tok_binary:
            get_next_token(); // consume 'binary'
            if (!isascii(g_cur_tok))
                return log_error_prototype("Expected binary operator");
            func_name = "binary";
            func_name += (char) g_cur_tok;
            kind = 2;
            get_next_token(); // consume op

            // parse optional precedence
            if (g_cur_tok == tok_number) {
                if (g_num_val < 1 || g_num_val > 100)
                    return log_error_prototype("Invalid precedence: must be 1..100");
                binary_precedence = (unsigned) g_num_val;
                get_next_token(); // consume precedence
            }
            break;
        default:
            return log_error_prototype("Expected function name in prototype");
    }

    // parse arguments
    if (g_cur_tok != '(')
        return log_error_prototype("Expected '(' in prototype");

    std::vector<std::string> arg_names;
    while (get_next_token() == tok_identifier)
        arg_names.push_back(g_identifier_str);
    if (g_cur_tok != ')')
        return log_error_prototype("Expected ')' in prototype");

    get_next_token(); // consume ')'

    // operator arguments count checking
    if (kind && arg_names.size() != kind)
        return log_error_prototype("Invalid number of operands for operator");

    return std::make_unique<PrototypeAST>(
        loc,
        func_name,
        arg_names,
        kind != 0,
        binary_precedence
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
    SourceLocation loc = g_cur_loc;

    // unique identifier for anonymous expr
    static uint32_t s_anon_count = 0;

    // evaluate a top level expression into an anonymous function
    if (auto expr = parse_expression()) {
        std::string name;

        // in IR emiting mode, only allow for one 'main' top level expr
        if (g_ir_mode and s_anon_count++ == 0)
            name = "main";
        else if (g_ir_mode and s_anon_count > 0)
            return log_error_function("Only one top-level expr allowed");
        else
            name = "__anon_expr_" + std::to_string(s_anon_count++);

        // make an anonymous prototype
        auto prototype = std::make_unique<PrototypeAST>(
            loc,
            name,
            std::vector<std::string>()
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
// Debug info
//

std::unique_ptr<DIBuilder> g_dbuilder;

DIType* DebugInfo::getDoubleTy() {
    if (double_type)
        return double_type;

    double_type = g_dbuilder->createBasicType("double", 64, dwarf::DW_ATE_float);
    return double_type;
}

void DebugInfo::emit_location(ExprAST* ast) {
    if (!ast)
        return g_builder.SetCurrentDebugLocation(DebugLoc());

    DIScope* scope;
    if (lexical_blocks.empty())
        scope = compile_unit;
    else
        scope = lexical_blocks.back();

    g_builder.SetCurrentDebugLocation(
        DebugLoc::get(ast->get_line(), ast->get_col(), scope)
    );
}

DISubroutineType* create_function_type(unsigned num_args, DIFile* unit) {
    SmallVector<Metadata*, 8> elem_type;
    DIType* double_type = KSDbgInfo.getDoubleTy();

    // add result type
    elem_type.push_back(double_type);

    for (unsigned i = 0, e = num_args; i != e; ++i)
        elem_type.push_back(double_type);

    return g_dbuilder->createSubroutineType(g_dbuilder->getOrCreateTypeArray(elem_type));
}


//
// Code generation
//

static std::unique_ptr<Module> g_module;
static std::unique_ptr<legacy::FunctionPassManager> g_fpm;
static std::unique_ptr<KaleidoscopeJIT> g_jit;

static std::map<std::string, AllocaInst*> g_named_values;
static std::map<std::string, std::unique_ptr<PrototypeAST>> g_function_prototypes;


Value* log_error_value(const char* str) {
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

// create an alloca instruction in the entry block of the function
// this is used for mutable variables
AllocaInst* create_entry_block_alloca(Function* function, StringRef var_name) {
    IRBuilder<> tmp_block(
        &function->getEntryBlock(),
        function->getEntryBlock().begin()
    );
    return tmp_block.CreateAlloca(
        Type::getDoubleTy(g_context),
        nullptr,
        var_name
    );
}

Value* NumberExprAST::codegen() {
    KSDbgInfo.emit_location(this);
    return ConstantFP::get(g_context, APFloat(_val));
}

Value* VariableExprAST::codegen() {
    Value* val = g_named_values[_name];
    if (!val)
        return log_error_value("Unknown variable name");

    KSDbgInfo.emit_location(this);
    // load the value
    return g_builder.CreateLoad(val, _name.c_str());
}

Value* UnaryExprAST::codegen() {
    Value* operand_value = _operand->codegen();
    if (!operand_value)
        return nullptr;

    Function* func = get_function(std::string("unary") + _op);
    if (!func)
        return log_error_value("Unknown unary operator");

    KSDbgInfo.emit_location(this);
    return g_builder.CreateCall(func, operand_value, "unop");
}

Value* BinaryExprAST::codegen() {
    KSDbgInfo.emit_location(this);

    // for '=', we don't want to emit the LHS as an expression
    if (_op == '=') {
        // assignment require lhs to be an identifier
        // this assume building without RTTI because LLVM builds that way
        // by default, if building LLVM with RTTI, this can be changed to
        // a dynamic_cast for automatic error checking
        VariableExprAST* lhs = static_cast<VariableExprAST*>(_lhs.get());
        if (!lhs)
            return log_error_value("Destination of '=' must be a variable");

        // codegen rhs
        Value* rhs = _rhs->codegen();
        if (!rhs)
            return nullptr;

        // look up variable name
        Value* variable = g_named_values[lhs->getName()];
        if (!variable)
            return log_error_value("Unknown variable name");

        g_builder.CreateStore(rhs, variable);
        return rhs;
    }


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
        case '/':
            return g_builder.CreateFDiv(lhs, rhs, "divtmp");
        case '<':
            lhs = g_builder.CreateFCmpULT(lhs, rhs, "cmptmp");
            // convert bool 0 / 1 to double 0.0 / 1.0
            return g_builder.CreateUIToFP(lhs, Type::getDoubleTy(g_context), "booltmp");
        default:
            break;
    }

    // user defined operator
    Function* func = get_function(std::string("binary") + _op);
    if (!func)
        return log_error_value("Unknown binary operator");

    Value* args[] = {lhs, rhs};
    return g_builder.CreateCall(func, args, "binop");
}

Value* CallExprAST::codegen() {
    KSDbgInfo.emit_location(this);

    // look-up name in the global module table
    Function* callee_f = get_function(_callee);
    if (!callee_f)
        return log_error_value("Unknown function referenced");

    // argument mismatch error
    if (callee_f->arg_size() != _args.size())
        return log_error_value("Incorrect # arguments passed");

    std::vector<Value*> args_v;
    for (unsigned i = 0, e = _args.size(); i != e; ++i) {
        args_v.push_back(_args[i]->codegen());
        if (!args_v.back())
            return nullptr;
    }

    return g_builder.CreateCall(callee_f, args_v, "calltmp");
}

Value* IfExprAST::codegen() {
    KSDbgInfo.emit_location(this);

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
    Function* function = g_builder.GetInsertBlock()->getParent();

    // alloca for the loop variable in the entry block
    AllocaInst* alloca = create_entry_block_alloca(function, _var_name);

    KSDbgInfo.emit_location(this);

    // emit start codem without 'variable' in scope
    Value* start_value = _start->codegen();
    if (!start_value)
        return nullptr;

    // store loop variable
    g_builder.CreateStore(start_value, alloca);

    // control flow blocks, 'loop' block is inserted at the end of
    // the current function
    BasicBlock* header_block = g_builder.GetInsertBlock();
    BasicBlock* loop_block = BasicBlock::Create(g_context, "loop", function);

    g_builder.CreateBr(loop_block);
    g_builder.SetInsertPoint(loop_block);

    // loop variable is defined as the phi node, handle possible
    // shadowing of existing variable
    AllocaInst* old_value = g_named_values[_var_name];
    g_named_values[_var_name] = alloca;

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
    // emit end
    Value* end_condition = _end->codegen();
    if (!end_condition)
        return nullptr;

    // reload, increment and restore the alloca
    // this handle case where the loop body mutates the variable
    Value* cur_value = g_builder.CreateLoad(alloca, _var_name.c_str());
    Value* next_value = g_builder.CreateFAdd(cur_value, step_value, "nextvar");
    g_builder.CreateStore(next_value, alloca);

    // convert to boolean
    end_condition = g_builder.CreateFCmpONE(
        end_condition, ConstantFP::get(g_context, APFloat(0.0)), "loopcond");

    BasicBlock* after_block = BasicBlock::Create(g_context, "afterloop", function);
    g_builder.CreateCondBr(end_condition, loop_block, after_block);
    g_builder.SetInsertPoint(after_block);

    // unshadow loop variable
    if (old_value)
        g_named_values[_var_name] = old_value;
    else
        g_named_values.erase(_var_name);

    // for expr always returns 0.0
    return Constant::getNullValue(Type::getDoubleTy(g_context));
}

Value* VarExprAST::codegen() {
    std::vector<AllocaInst*> old_bindings;

    Function* function = g_builder.GetInsertBlock()->getParent();

    // register all variables and emit their initializer
    for (unsigned i = 0, e = _var_names.size(); i != e; ++i) {
        const std::string& var_name = _var_names[i].first;
        ExprAST* init = _var_names[i].second.get();

        // emit initializer before adding the variable to scope,
        // this prevents the initializer fron referencing the variable
        // itselfm and permits stuff like this:
        //  var a = 1 in
        //    var a = a in ... # refers to outer 'a'
        Value* init_val;
        if (init) {
            init_val = init->codegen();
            if (!init_val)
                return nullptr;
        }
        // use 0.0 by default
        else {
            init_val = ConstantFP::get(g_context, APFloat(0.0));
        }

        AllocaInst* alloca = create_entry_block_alloca(function, var_name);
        g_builder.CreateStore(init_val, alloca);

        // remember the old variable binding
        old_bindings.push_back(g_named_values[var_name]);
        g_named_values[var_name] = alloca;
    }

    KSDbgInfo.emit_location(this);

    // now that all vars are in scope, codegen the body
    Value* body_val = _body->codegen();
    if (!body_val)
        return nullptr;

    // restore variables
    for (unsigned i = 0, e = _var_names.size(); i != e; ++i)
        g_named_values[_var_names[i].first] = old_bindings[i];

    return body_val;
}

Function* PrototypeAST::codegen() {
    // function type: double(double, double) etc
    std::vector<Type*> doubles(_args.size(), Type::getDoubleTy(g_context));
    // main function has a int32 returns type to provide c compatibility
    auto ret_type = _name == "main" ?
        Type::getInt32Ty(g_context) : Type::getDoubleTy(g_context);
    // last argument imply non var arg function
    FunctionType* ft = FunctionType::get(ret_type, doubles, false);
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
    auto& prototype = g_function_prototypes[name];

    Function *function = get_function(prototype->getName());
    if (!function)
        return nullptr;

    if (prototype->isBinaryOp())
        g_binop_precedence[prototype->getOperatorName()] = prototype->getBinaryPrecedence();

    // create a new basic block to start insertion into
    BasicBlock *bb = BasicBlock::Create(g_context, "entry", function);
    g_builder.SetInsertPoint(bb);

    // create subprogram DIE for this function
    DIFile* unit = g_dbuilder->createFile(
        KSDbgInfo.compile_unit->getFilename(),
        KSDbgInfo.compile_unit->getDirectory()
    );

    DIScope* func_context = unit;
    unsigned line_no = prototype->getLine();
    unsigned scope_line = line_no;
    DISubprogram* sub_prog = g_dbuilder->createFunction(
        func_context,
        name,
        StringRef(),
        unit,
        line_no,
        create_function_type(function->arg_size(), unit),
        scope_line,
        DINode::FlagPrototyped,
        DISubprogram::SPFlagDefinition
    );
    function->setSubprogram(sub_prog);

    // push the current scope
    KSDbgInfo.lexical_blocks.push_back(sub_prog);

    // unset the location for the prologue emission (leading instructions with
    // no location in a function are considered part of the prologue and the
    // debugger will run past them when breaking on a function)
    KSDbgInfo.emit_location(nullptr);

    // record the function arguments in the symbol map
    g_named_values.clear();
    unsigned arg_idx = 0;
    for (auto& arg : function->args()) {
        AllocaInst* alloca = create_entry_block_alloca(function, arg.getName());

        // create debug descriptor for the variable
        DILocalVariable* desc = g_dbuilder->createParameterVariable(
            sub_prog,
            arg.getName(),
            ++arg_idx,
            unit,
            line_no,
            KSDbgInfo.getDoubleTy(),
            true
        );

        g_dbuilder->insertDeclare(
            alloca,
            desc,
            g_dbuilder->createExpression(),
            DebugLoc::get(line_no, 0, sub_prog),
            g_builder.GetInsertBlock()
        );

        g_builder.CreateStore(&arg, alloca);
        g_named_values[std::string(arg.getName())] = alloca;
    }

    KSDbgInfo.emit_location(_body.get());

    if (Value* retval = _body->codegen()) {
        // add a double to int32 conversion in place for the main
        if (_name == "main")
            retval = g_builder.CreateFPToUI(
                retval, Type::getInt32Ty(g_context), "rettmp");

        g_builder.CreateRet(retval);

        // pop lexical block for the function
        KSDbgInfo.lexical_blocks.pop_back();

        // consistency checks
        verifyFunction(*function);

        // optimizer
        if (not g_dbg_mode)
            g_fpm->run(*function);

        return function;
    }

    // error generating the function body
    function->eraseFromParent();
    if (prototype->isBinaryOp())
        g_binop_precedence.erase(prototype->getOperatorName());

    // pop lexical block for the function
    KSDbgInfo.lexical_blocks.pop_back();

    return nullptr;
}

//
// Console print utilities
//

void print_prompt() {
    if (g_ir_mode)
        return;

    fprintf(stderr, "ready> ");
}

template <typename IR>
void print_ir(IR ir, const std::string& header) {
    if (g_ir_mode)
        return;

    fprintf(stderr, "%s", header.c_str());
    ir->print(errs());
    fprintf(stderr, "\n");
}

typedef double (*FuncT)();

void print_eval(FuncT& func) {
    if (g_ir_mode)
        return;

    fprintf(stderr, "Evaluated to %f\n", func());
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

    // promote alloca to registers
    g_fpm->add(createPromoteMemoryToRegisterPass());
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
            print_ir(ir, "Parsed a function definition\n");
            g_jit->addModule(CloneModule(*g_module));
        }
    }
    else
        // skip token for error recovery
        get_next_token();
}

void handle_extern() {
    if (auto ast = parse_extern()) {
        if (auto ir = ast->codegen()) {
            print_ir(ir, "Parsed an extern\n");
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

            print_ir(ir, "Parsed a top-level expression\n");

            // add current module to JIT
            auto module = g_jit->addModule(CloneModule(*g_module));

            // search for the anonymous function symbol
            auto expr_symbol = g_jit->findSymbol(ast->getName());
            assert(expr_symbol && "Function not found");

            // get symbol address and cast to the anonymous function type
            FuncT func = (FuncT) (intptr_t) cantFail(expr_symbol.getAddress());
            print_eval(func);

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
        switch (g_cur_tok) {
            case tok_eof:
                return;
            // ignore top-level semicolons
            case ';':
                get_next_token();
                continue;
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
        print_prompt();
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

int emit_ir() {
    g_module->print(errs(), nullptr);
    return 0;
}

int emit_obj() {
    // setup compilation target architecture
    auto target_triple = sys::getDefaultTargetTriple();
    g_module->setTargetTriple(target_triple);

    std::string error;
    auto target = TargetRegistry::lookupTarget(target_triple, error);
    if (!target) {
        errs() << error;
        return 1;
    }

    auto cpu = "generic";
    auto features = "";

    TargetOptions opt;
    auto reloc = Optional<Reloc::Model>();
    auto target_machine = target->createTargetMachine(
        target_triple, cpu, features, opt, reloc);

    g_module->setDataLayout(target_machine->createDataLayout());

    // compilation
    std::error_code ec;
    auto filename = "output.o";
    raw_fd_ostream dest(filename, ec, sys::fs::OF_None);

    if (ec) {
        errs() << "Could not open file: " << ec.message();
        return 1;
    }

    legacy::PassManager pass;
    auto file_type = CGFT_ObjectFile;

    if (target_machine->addPassesToEmitFile(pass, dest, nullptr, file_type)) {
        errs() << "Target machine can't emit a file of this type";
        return 1;
    }

    pass.run(*g_module);
    dest.flush();

    outs() << "Wrote " << filename << "\n";

    return 0;
}

void setup_debug() {
    // debug info version
    g_module->addModuleFlag(
        Module::Warning, "Debug Info Version", DEBUG_METADATA_VERSION);
    // darwin supports only dwarf2
    if (Triple(sys::getProcessTriple()).isOSDarwin())
        g_module->addModuleFlag(
            llvm::Module::Warning, "Dwarf Version", 2);

    // construct DIBuilder, we do this here because we need the module
    g_dbuilder = std::make_unique<DIBuilder>(*g_module);

    // create the compile unit for the module
    // currently down as "fib.ks" as a filename since we're redirecting
    // stdin but we'd like actual source locations
    KSDbgInfo.compile_unit = g_dbuilder->createCompileUnit(
        dwarf::DW_LANG_C,
        g_dbuilder->createFile("fib.ks", "."),
        "Kaleidoscope Compiler",
        not g_dbg_mode,
        "",
        0
    );
}

int main(int argc, char** argv) {
    // IR emiting mode
    if (argc > 1 and strcmp(argv[1], "-ir") == 0)
        g_ir_mode = true;
    else if (argc > 1 and strcmp(argv[1], "-dbgir") == 0)
        g_dbg_mode = g_ir_mode = true;
    else if (argc > 1 and strcmp(argv[1], "-o") == 0)
        g_obj_mode = true;

    InitializeAllTargetInfos();
    InitializeAllTargets();
    InitializeAllTargetMCs();
    InitializeAllAsmParsers();
    InitializeAllAsmPrinters();

    // 1 is the lowest precedence
    g_binop_precedence['='] = 2;
    g_binop_precedence['<'] = 10;
    g_binop_precedence['+'] = 20;
    g_binop_precedence['-'] = 20;
    g_binop_precedence['*'] = 40;
    g_binop_precedence['/'] = 40;

    // repl loop
    print_prompt();
    get_next_token();

    g_jit = std::make_unique<KaleidoscopeJIT>();
    initialize_module_and_passmanager();

    setup_debug();

    main_loop();

    g_dbuilder->finalize();

    if (g_ir_mode)
        return emit_ir();
    else if (g_obj_mode)
        return emit_obj();

    return 0;
}